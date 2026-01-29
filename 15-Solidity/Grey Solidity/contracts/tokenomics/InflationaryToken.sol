// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Burnable.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title InflationaryToken
 * @author Grey Protocol
 * @notice ERC20 token with programmable inflation mechanics
 * @dev Implements various inflation models with distribution controls
 * 
 * Features:
 * - Configurable inflation rate with decay
 * - Epoch-based emission schedule
 * - Multiple distribution streams (staking, treasury, burns)
 * - Halving mechanism
 * - Supply caps with soft/hard limits
 * - Inflation targeting based on utilization
 */
contract InflationaryToken is ERC20, ERC20Burnable, AccessControl, ReentrancyGuard {
    
    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    bytes32 public constant INFLATION_MANAGER_ROLE = keccak256("INFLATION_MANAGER_ROLE");

    uint256 public constant BPS_DENOMINATOR = 10000;
    uint256 public constant YEAR_IN_SECONDS = 365 days;
    uint256 public constant MAX_INFLATION_RATE = 2000; // 20% max annual

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Inflation configuration
    struct InflationConfig {
        uint256 annualRate;          // Annual inflation rate in BPS
        uint256 decayRate;           // Rate decay per epoch in BPS
        uint256 epochDuration;       // Duration of each epoch
        uint256 minimumRate;         // Floor for inflation rate
        uint256 targetUtilization;   // Target utilization rate in BPS
    }

    /// @notice Distribution allocation
    struct DistributionConfig {
        uint256 stakingShare;        // Share to staking rewards (BPS)
        uint256 treasuryShare;       // Share to treasury (BPS)
        uint256 ecosystemShare;      // Share to ecosystem fund (BPS)
        uint256 burnShare;           // Share to burn (BPS)
    }

    /// @notice Epoch data
    struct Epoch {
        uint256 epochNumber;
        uint256 startTime;
        uint256 endTime;
        uint256 inflationRate;
        uint256 mintedAmount;
        uint256 distributedAmount;
        bool finalized;
    }

    /// @notice Halving schedule
    struct HalvingConfig {
        uint256 epochsPerHalving;
        uint256 halvingCount;
        uint256 nextHalvingEpoch;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Inflation configuration
    InflationConfig public inflationConfig;

    /// @notice Distribution configuration
    DistributionConfig public distributionConfig;

    /// @notice Halving configuration
    HalvingConfig public halvingConfig;

    /// @notice Current epoch number
    uint256 public currentEpoch;

    /// @notice Epoch data
    mapping(uint256 => Epoch) public epochs;

    /// @notice Genesis timestamp
    uint256 public genesisTimestamp;

    /// @notice Initial supply
    uint256 public immutable initialSupply;

    /// @notice Hard cap (0 = no cap)
    uint256 public hardCap;

    /// @notice Soft cap for warnings
    uint256 public softCap;

    /// @notice Total minted through inflation
    uint256 public totalInflationMinted;

    /// @notice Total burned
    uint256 public totalBurned;

    /// @notice Staking contract address
    address public stakingContract;

    /// @notice Treasury address
    address public treasury;

    /// @notice Ecosystem fund address
    address public ecosystemFund;

    /// @notice Utilization metric (set externally)
    uint256 public currentUtilization;

    // ============================================
    // EVENTS
    // ============================================

    event InflationMinted(
        uint256 indexed epoch,
        uint256 amount,
        uint256 rate
    );

    event InflationDistributed(
        uint256 indexed epoch,
        uint256 stakingAmount,
        uint256 treasuryAmount,
        uint256 ecosystemAmount,
        uint256 burnAmount
    );

    event EpochFinalized(uint256 indexed epoch);

    event HalvingOccurred(uint256 indexed epoch, uint256 newRate);

    event InflationRateUpdated(uint256 oldRate, uint256 newRate);

    event SoftCapReached(uint256 totalSupply, uint256 softCap);

    // ============================================
    // ERRORS
    // ============================================

    error HardCapExceeded();
    error InvalidRate();
    error EpochNotReady();
    error EpochAlreadyFinalized();
    error InvalidDistribution();
    error InvalidAddress();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        string memory name,
        string memory symbol,
        uint256 _initialSupply,
        uint256 _hardCap,
        uint256 _initialInflationRate
    ) ERC20(name, symbol) {
        initialSupply = _initialSupply;
        hardCap = _hardCap;
        softCap = (_hardCap * 90) / 100; // 90% of hard cap

        genesisTimestamp = block.timestamp;

        // Default inflation config
        inflationConfig = InflationConfig({
            annualRate: _initialInflationRate,
            decayRate: 100, // 1% decay per epoch
            epochDuration: 7 days,
            minimumRate: 100, // 1% minimum
            targetUtilization: 7000 // 70% target
        });

        // Default distribution (must sum to 10000)
        distributionConfig = DistributionConfig({
            stakingShare: 6000,   // 60% to staking
            treasuryShare: 2000,  // 20% to treasury
            ecosystemShare: 1500, // 15% to ecosystem
            burnShare: 500        // 5% burned
        });

        // Halving every 52 epochs (approx 1 year with 7-day epochs)
        halvingConfig = HalvingConfig({
            epochsPerHalving: 52,
            halvingCount: 0,
            nextHalvingEpoch: 52
        });

        // Initialize first epoch
        epochs[0] = Epoch({
            epochNumber: 0,
            startTime: block.timestamp,
            endTime: block.timestamp + inflationConfig.epochDuration,
            inflationRate: _initialInflationRate,
            mintedAmount: 0,
            distributedAmount: 0,
            finalized: false
        });

        // Mint initial supply
        _mint(msg.sender, _initialSupply);

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(MINTER_ROLE, msg.sender);
        _grantRole(INFLATION_MANAGER_ROLE, msg.sender);
    }

    // ============================================
    // INFLATION MECHANICS
    // ============================================

    /**
     * @notice Process inflation for current epoch
     * @return amount Amount minted
     */
    function processInflation() external nonReentrant returns (uint256 amount) {
        Epoch storage epoch = epochs[currentEpoch];
        
        if (block.timestamp < epoch.endTime) revert EpochNotReady();
        if (epoch.finalized) revert EpochAlreadyFinalized();

        // Calculate inflation amount
        uint256 currentSupply = totalSupply();
        uint256 epochsPerYear = YEAR_IN_SECONDS / inflationConfig.epochDuration;
        uint256 epochRate = epoch.inflationRate / epochsPerYear;
        
        amount = (currentSupply * epochRate) / BPS_DENOMINATOR;

        // Check hard cap
        if (hardCap > 0 && currentSupply + amount > hardCap) {
            amount = hardCap - currentSupply;
        }

        if (amount > 0) {
            _mint(address(this), amount);
            epoch.mintedAmount = amount;
            totalInflationMinted += amount;

            emit InflationMinted(currentEpoch, amount, epoch.inflationRate);

            // Check soft cap warning
            if (totalSupply() >= softCap) {
                emit SoftCapReached(totalSupply(), softCap);
            }

            // Distribute
            _distributeInflation(amount);
        }

        // Finalize epoch
        epoch.finalized = true;
        emit EpochFinalized(currentEpoch);

        // Start next epoch
        _startNextEpoch();
    }

    /**
     * @notice Distribute minted inflation
     */
    function _distributeInflation(uint256 amount) internal {
        uint256 stakingAmount = (amount * distributionConfig.stakingShare) / BPS_DENOMINATOR;
        uint256 treasuryAmount = (amount * distributionConfig.treasuryShare) / BPS_DENOMINATOR;
        uint256 ecosystemAmount = (amount * distributionConfig.ecosystemShare) / BPS_DENOMINATOR;
        uint256 burnAmount = (amount * distributionConfig.burnShare) / BPS_DENOMINATOR;

        // Distribute to recipients
        if (stakingAmount > 0 && stakingContract != address(0)) {
            _transfer(address(this), stakingContract, stakingAmount);
        }
        if (treasuryAmount > 0 && treasury != address(0)) {
            _transfer(address(this), treasury, treasuryAmount);
        }
        if (ecosystemAmount > 0 && ecosystemFund != address(0)) {
            _transfer(address(this), ecosystemFund, ecosystemAmount);
        }
        if (burnAmount > 0) {
            _burn(address(this), burnAmount);
            totalBurned += burnAmount;
        }

        epochs[currentEpoch].distributedAmount = amount;

        emit InflationDistributed(
            currentEpoch,
            stakingAmount,
            treasuryAmount,
            ecosystemAmount,
            burnAmount
        );
    }

    /**
     * @notice Start the next epoch with updated parameters
     */
    function _startNextEpoch() internal {
        currentEpoch++;
        
        // Calculate new inflation rate
        uint256 newRate = _calculateNewInflationRate();

        // Check for halving
        if (currentEpoch >= halvingConfig.nextHalvingEpoch) {
            newRate = newRate / 2;
            if (newRate < inflationConfig.minimumRate) {
                newRate = inflationConfig.minimumRate;
            }
            halvingConfig.halvingCount++;
            halvingConfig.nextHalvingEpoch = currentEpoch + halvingConfig.epochsPerHalving;
            emit HalvingOccurred(currentEpoch, newRate);
        }

        epochs[currentEpoch] = Epoch({
            epochNumber: currentEpoch,
            startTime: block.timestamp,
            endTime: block.timestamp + inflationConfig.epochDuration,
            inflationRate: newRate,
            mintedAmount: 0,
            distributedAmount: 0,
            finalized: false
        });
    }

    /**
     * @notice Calculate new inflation rate based on utilization
     */
    function _calculateNewInflationRate() internal view returns (uint256) {
        uint256 currentRate = epochs[currentEpoch - 1].inflationRate;

        // Adjust based on utilization
        if (currentUtilization < inflationConfig.targetUtilization) {
            // Below target: decrease rate to reduce supply growth
            uint256 decrease = (currentRate * inflationConfig.decayRate) / BPS_DENOMINATOR;
            if (currentRate > decrease + inflationConfig.minimumRate) {
                return currentRate - decrease;
            }
            return inflationConfig.minimumRate;
        } else if (currentUtilization > inflationConfig.targetUtilization) {
            // Above target: slightly increase rate
            uint256 increase = (currentRate * inflationConfig.decayRate) / (BPS_DENOMINATOR * 2);
            uint256 newRate = currentRate + increase;
            if (newRate > MAX_INFLATION_RATE) {
                return MAX_INFLATION_RATE;
            }
            return newRate;
        }

        return currentRate;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get current effective inflation rate
     */
    function getEffectiveInflationRate() external view returns (uint256) {
        return epochs[currentEpoch].inflationRate;
    }

    /**
     * @notice Get epoch details
     */
    function getEpoch(uint256 epochNum) external view returns (Epoch memory) {
        return epochs[epochNum];
    }

    /**
     * @notice Calculate projected supply at future epoch
     */
    function projectedSupply(uint256 futureEpoch) external view returns (uint256) {
        uint256 supply = totalSupply();
        uint256 rate = epochs[currentEpoch].inflationRate;
        uint256 epochsPerYear = YEAR_IN_SECONDS / inflationConfig.epochDuration;

        for (uint256 i = currentEpoch; i < futureEpoch; i++) {
            uint256 epochRate = rate / epochsPerYear;
            supply += (supply * epochRate) / BPS_DENOMINATOR;

            // Apply decay
            rate = (rate * (BPS_DENOMINATOR - inflationConfig.decayRate)) / BPS_DENOMINATOR;
            if (rate < inflationConfig.minimumRate) rate = inflationConfig.minimumRate;

            // Check halving
            if ((i + 1) % halvingConfig.epochsPerHalving == 0) {
                rate = rate / 2;
                if (rate < inflationConfig.minimumRate) rate = inflationConfig.minimumRate;
            }

            if (hardCap > 0 && supply > hardCap) {
                return hardCap;
            }
        }

        return supply;
    }

    /**
     * @notice Time until next epoch
     */
    function timeUntilNextEpoch() external view returns (uint256) {
        Epoch storage epoch = epochs[currentEpoch];
        if (block.timestamp >= epoch.endTime) return 0;
        return epoch.endTime - block.timestamp;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setInflationConfig(
        uint256 annualRate,
        uint256 decayRate,
        uint256 epochDuration,
        uint256 minimumRate,
        uint256 targetUtilization
    ) external onlyRole(INFLATION_MANAGER_ROLE) {
        if (annualRate > MAX_INFLATION_RATE) revert InvalidRate();
        
        uint256 oldRate = inflationConfig.annualRate;
        inflationConfig = InflationConfig({
            annualRate: annualRate,
            decayRate: decayRate,
            epochDuration: epochDuration,
            minimumRate: minimumRate,
            targetUtilization: targetUtilization
        });
        
        emit InflationRateUpdated(oldRate, annualRate);
    }

    function setDistributionConfig(
        uint256 stakingShare,
        uint256 treasuryShare,
        uint256 ecosystemShare,
        uint256 burnShare
    ) external onlyRole(INFLATION_MANAGER_ROLE) {
        if (stakingShare + treasuryShare + ecosystemShare + burnShare != BPS_DENOMINATOR) {
            revert InvalidDistribution();
        }
        
        distributionConfig = DistributionConfig({
            stakingShare: stakingShare,
            treasuryShare: treasuryShare,
            ecosystemShare: ecosystemShare,
            burnShare: burnShare
        });
    }

    function setAddresses(
        address _stakingContract,
        address _treasury,
        address _ecosystemFund
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        stakingContract = _stakingContract;
        treasury = _treasury;
        ecosystemFund = _ecosystemFund;
    }

    function setUtilization(uint256 utilization) external onlyRole(INFLATION_MANAGER_ROLE) {
        currentUtilization = utilization;
    }

    function setHardCap(uint256 _hardCap) external onlyRole(DEFAULT_ADMIN_ROLE) {
        hardCap = _hardCap;
        softCap = (_hardCap * 90) / 100;
    }
}
