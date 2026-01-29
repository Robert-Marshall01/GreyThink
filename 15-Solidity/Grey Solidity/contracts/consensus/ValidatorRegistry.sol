// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title ValidatorRegistry
 * @author Grey Protocol
 * @notice Manages validator registration, staking, and validator set rotation
 * @dev Implements core consensus primitives for Proof-of-Stake validator management
 * 
 * Key features:
 * - Validator registration with minimum stake requirements
 * - Epoch-based validator set rotation
 * - Commission rate management
 * - Delegated staking support
 * - Jailing and unjailing mechanisms
 * - Validator performance tracking
 */
contract ValidatorRegistry is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");
    bytes32 public constant SLASHER_ROLE = keccak256("SLASHER_ROLE");

    uint256 public constant BPS_DENOMINATOR = 10000;
    uint256 public constant MAX_COMMISSION_RATE = 5000; // 50%
    uint256 public constant MAX_VALIDATORS = 100;
    uint256 public constant UNBONDING_PERIOD = 21 days;
    
    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice The staking token (governance token)
    IERC20 public immutable stakingToken;

    /// @notice Minimum stake required to become a validator
    uint256 public minValidatorStake;

    /// @notice Maximum stake per validator (concentration limit)
    uint256 public maxValidatorStake;

    /// @notice Current epoch number
    uint256 public currentEpoch;

    /// @notice Epoch duration in blocks
    uint256 public epochLength;

    /// @notice Block number when current epoch started
    uint256 public epochStartBlock;

    /// @notice Active validator count
    uint256 public activeValidatorCount;

    /// @notice Total staked across all validators
    uint256 public totalStaked;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Validator state and configuration
    struct Validator {
        address operator;           // Operator address for signing
        address withdrawalAddress;  // Address for reward/stake withdrawal
        uint256 selfStake;          // Validator's own stake
        uint256 delegatedStake;     // Total delegated stake
        uint256 commissionRate;     // Commission rate in BPS
        uint256 accumulatedRewards; // Pending rewards
        uint256 jailedUntil;        // Jail expiry timestamp
        uint256 slashCount;         // Number of times slashed
        uint256 registrationEpoch;  // Epoch when validator registered
        uint256 lastActiveEpoch;    // Last epoch with activity
        bool isActive;              // Currently in active set
        bool isRegistered;          // Has registered (even if inactive)
        bytes32 pubKeyHash;         // Hash of consensus public key
        string moniker;             // Human-readable name
        string website;             // Validator website
        string description;         // Validator description
    }

    /// @notice Delegation record
    struct Delegation {
        uint256 shares;             // Share of delegated stake
        uint256 stakedAmount;       // Original staked amount
        uint256 pendingRewards;     // Unclaimed rewards
        uint256 unbondingAmount;    // Amount being unbonded
        uint256 unbondingEnd;       // When unbonding completes
    }

    /// @notice Unbonding entry for delayed withdrawals
    struct UnbondingEntry {
        uint256 amount;
        uint256 completionTime;
    }

    /// @notice Epoch snapshot for historical queries
    struct EpochSnapshot {
        uint256 totalStake;
        uint256 validatorCount;
        bytes32 validatorSetHash;
        uint256 blockNumber;
        uint256 timestamp;
    }

    // ============================================
    // MAPPINGS
    // ============================================

    /// @notice Validator address => Validator data
    mapping(address => Validator) public validators;

    /// @notice Delegator => Validator => Delegation
    mapping(address => mapping(address => Delegation)) public delegations;

    /// @notice Delegator => Validator => Unbonding entries
    mapping(address => mapping(address => UnbondingEntry[])) public unbondingQueue;

    /// @notice Validator address => total shares issued
    mapping(address => uint256) public validatorTotalShares;

    /// @notice Epoch => Snapshot
    mapping(uint256 => EpochSnapshot) public epochSnapshots;

    /// @notice Active validator set (sorted by stake)
    address[] public activeValidatorSet;

    /// @notice All registered validators
    address[] public allValidators;

    // ============================================
    // EVENTS
    // ============================================

    event ValidatorRegistered(
        address indexed validator,
        address indexed operator,
        uint256 initialStake,
        string moniker
    );

    event ValidatorUpdated(
        address indexed validator,
        uint256 newCommissionRate,
        string newMoniker
    );

    event StakeDeposited(
        address indexed validator,
        address indexed delegator,
        uint256 amount,
        uint256 shares
    );

    event UnbondingInitiated(
        address indexed validator,
        address indexed delegator,
        uint256 amount,
        uint256 completionTime
    );

    event StakeWithdrawn(
        address indexed validator,
        address indexed delegator,
        uint256 amount
    );

    event ValidatorSlashed(
        address indexed validator,
        uint256 slashAmount,
        uint256 slashPercentage,
        string reason
    );

    event ValidatorJailed(
        address indexed validator,
        uint256 jailUntil,
        string reason
    );

    event ValidatorUnjailed(address indexed validator);

    event EpochAdvanced(
        uint256 indexed epoch,
        uint256 validatorCount,
        uint256 totalStake
    );

    event RewardsClaimed(
        address indexed validator,
        address indexed recipient,
        uint256 amount
    );

    event ValidatorSetUpdated(
        uint256 indexed epoch,
        address[] newValidatorSet
    );

    // ============================================
    // ERRORS
    // ============================================

    error InsufficientStake();
    error ExceedsMaxStake();
    error ValidatorNotRegistered();
    error ValidatorAlreadyRegistered();
    error ValidatorIsJailed();
    error ValidatorNotJailed();
    error InvalidCommissionRate();
    error UnbondingNotComplete();
    error NoUnbondingEntries();
    error InvalidOperator();
    error MaxValidatorsReached();
    error NotValidatorOrOperator();
    error EpochNotAdvanceable();
    error InvalidPubKey();
    error NoDelegation();
    error InsufficientShares();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address _stakingToken,
        uint256 _minValidatorStake,
        uint256 _maxValidatorStake,
        uint256 _epochLength
    ) {
        stakingToken = IERC20(_stakingToken);
        minValidatorStake = _minValidatorStake;
        maxValidatorStake = _maxValidatorStake;
        epochLength = _epochLength;
        epochStartBlock = block.number;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(GOVERNANCE_ROLE, msg.sender);
    }

    // ============================================
    // VALIDATOR REGISTRATION
    // ============================================

    /**
     * @notice Register as a new validator
     * @param operator Address authorized to sign on behalf of validator
     * @param pubKeyHash Hash of the consensus public key
     * @param commissionRate Commission rate in basis points
     * @param initialStake Amount of tokens to stake
     * @param moniker Human-readable validator name
     * @param website Validator website URL
     * @param description Validator description
     */
    function registerValidator(
        address operator,
        bytes32 pubKeyHash,
        uint256 commissionRate,
        uint256 initialStake,
        string calldata moniker,
        string calldata website,
        string calldata description
    ) external nonReentrant {
        if (validators[msg.sender].isRegistered) revert ValidatorAlreadyRegistered();
        if (initialStake < minValidatorStake) revert InsufficientStake();
        if (initialStake > maxValidatorStake) revert ExceedsMaxStake();
        if (commissionRate > MAX_COMMISSION_RATE) revert InvalidCommissionRate();
        if (operator == address(0)) revert InvalidOperator();
        if (pubKeyHash == bytes32(0)) revert InvalidPubKey();

        // Transfer stake
        stakingToken.safeTransferFrom(msg.sender, address(this), initialStake);

        // Create validator record
        validators[msg.sender] = Validator({
            operator: operator,
            withdrawalAddress: msg.sender,
            selfStake: initialStake,
            delegatedStake: 0,
            commissionRate: commissionRate,
            accumulatedRewards: 0,
            jailedUntil: 0,
            slashCount: 0,
            registrationEpoch: currentEpoch,
            lastActiveEpoch: currentEpoch,
            isActive: false,
            isRegistered: true,
            pubKeyHash: pubKeyHash,
            moniker: moniker,
            website: website,
            description: description
        });

        // Initialize shares (1:1 for self-stake)
        validatorTotalShares[msg.sender] = initialStake;

        allValidators.push(msg.sender);
        totalStaked += initialStake;

        emit ValidatorRegistered(msg.sender, operator, initialStake, moniker);

        // Attempt to add to active set
        _tryActivateValidator(msg.sender);
    }

    /**
     * @notice Update validator metadata and commission
     * @param newCommissionRate New commission rate (can only decrease or stay same)
     * @param newMoniker New validator name
     * @param newWebsite New website URL
     * @param newDescription New description
     */
    function updateValidator(
        uint256 newCommissionRate,
        string calldata newMoniker,
        string calldata newWebsite,
        string calldata newDescription
    ) external {
        Validator storage v = validators[msg.sender];
        if (!v.isRegistered) revert ValidatorNotRegistered();
        if (newCommissionRate > MAX_COMMISSION_RATE) revert InvalidCommissionRate();

        // Commission can only decrease (prevents bait-and-switch)
        if (newCommissionRate <= v.commissionRate) {
            v.commissionRate = newCommissionRate;
        }

        v.moniker = newMoniker;
        v.website = newWebsite;
        v.description = newDescription;

        emit ValidatorUpdated(msg.sender, newCommissionRate, newMoniker);
    }

    /**
     * @notice Update validator operator address
     * @param newOperator New operator address
     */
    function updateOperator(address newOperator) external {
        Validator storage v = validators[msg.sender];
        if (!v.isRegistered) revert ValidatorNotRegistered();
        if (newOperator == address(0)) revert InvalidOperator();

        v.operator = newOperator;
    }

    // ============================================
    // STAKING & DELEGATION
    // ============================================

    /**
     * @notice Delegate stake to a validator
     * @param validator Validator address to delegate to
     * @param amount Amount of tokens to delegate
     */
    function delegate(address validator, uint256 amount) external nonReentrant {
        Validator storage v = validators[validator];
        if (!v.isRegistered) revert ValidatorNotRegistered();
        if (v.jailedUntil > block.timestamp) revert ValidatorIsJailed();

        uint256 totalStakeAfter = v.selfStake + v.delegatedStake + amount;
        if (totalStakeAfter > maxValidatorStake) revert ExceedsMaxStake();

        // Transfer tokens
        stakingToken.safeTransferFrom(msg.sender, address(this), amount);

        // Calculate shares
        uint256 shares;
        uint256 currentTotalShares = validatorTotalShares[validator];
        uint256 currentTotalStake = v.selfStake + v.delegatedStake;

        if (currentTotalShares == 0 || currentTotalStake == 0) {
            shares = amount;
        } else {
            shares = (amount * currentTotalShares) / currentTotalStake;
        }

        // Update delegation
        Delegation storage d = delegations[msg.sender][validator];
        d.shares += shares;
        d.stakedAmount += amount;

        // Update validator
        v.delegatedStake += amount;
        validatorTotalShares[validator] += shares;
        totalStaked += amount;

        emit StakeDeposited(validator, msg.sender, amount, shares);

        // Try to activate validator if needed
        _tryActivateValidator(validator);
    }

    /**
     * @notice Self-stake additional tokens (validator only)
     * @param amount Amount to add to self-stake
     */
    function addSelfStake(uint256 amount) external nonReentrant {
        Validator storage v = validators[msg.sender];
        if (!v.isRegistered) revert ValidatorNotRegistered();

        uint256 totalStakeAfter = v.selfStake + v.delegatedStake + amount;
        if (totalStakeAfter > maxValidatorStake) revert ExceedsMaxStake();

        stakingToken.safeTransferFrom(msg.sender, address(this), amount);

        v.selfStake += amount;
        validatorTotalShares[msg.sender] += amount;
        totalStaked += amount;

        emit StakeDeposited(msg.sender, msg.sender, amount, amount);

        _tryActivateValidator(msg.sender);
    }

    /**
     * @notice Initiate unbonding of delegated stake
     * @param validator Validator to unbond from
     * @param shares Number of shares to unbond
     */
    function undelegate(address validator, uint256 shares) external nonReentrant {
        Delegation storage d = delegations[msg.sender][validator];
        if (d.shares == 0) revert NoDelegation();
        if (shares > d.shares) revert InsufficientShares();

        Validator storage v = validators[validator];

        // Calculate token amount for shares
        uint256 totalShares = validatorTotalShares[validator];
        uint256 totalStake = v.selfStake + v.delegatedStake;
        uint256 amount = (shares * totalStake) / totalShares;

        // Update state
        d.shares -= shares;
        v.delegatedStake -= amount;
        validatorTotalShares[validator] -= shares;
        totalStaked -= amount;

        // Create unbonding entry
        uint256 completionTime = block.timestamp + UNBONDING_PERIOD;
        unbondingQueue[msg.sender][validator].push(UnbondingEntry({
            amount: amount,
            completionTime: completionTime
        }));

        d.unbondingAmount += amount;
        d.unbondingEnd = completionTime;

        emit UnbondingInitiated(validator, msg.sender, amount, completionTime);

        // Check if validator should be deactivated
        _checkValidatorStatus(validator);
    }

    /**
     * @notice Complete unbonding and withdraw tokens
     * @param validator Validator the stake was delegated to
     */
    function completeUnbonding(address validator) external nonReentrant {
        UnbondingEntry[] storage entries = unbondingQueue[msg.sender][validator];
        if (entries.length == 0) revert NoUnbondingEntries();

        uint256 totalWithdrawable = 0;
        uint256 writeIndex = 0;

        for (uint256 i = 0; i < entries.length; i++) {
            if (entries[i].completionTime <= block.timestamp) {
                totalWithdrawable += entries[i].amount;
            } else {
                if (writeIndex != i) {
                    entries[writeIndex] = entries[i];
                }
                writeIndex++;
            }
        }

        // Resize array
        while (entries.length > writeIndex) {
            entries.pop();
        }

        if (totalWithdrawable == 0) revert UnbondingNotComplete();

        // Update delegation tracking
        Delegation storage d = delegations[msg.sender][validator];
        d.unbondingAmount -= totalWithdrawable;
        d.stakedAmount -= totalWithdrawable;

        // Transfer tokens
        stakingToken.safeTransfer(msg.sender, totalWithdrawable);

        emit StakeWithdrawn(validator, msg.sender, totalWithdrawable);
    }

    // ============================================
    // SLASHING & JAILING
    // ============================================

    /**
     * @notice Slash a validator for misbehavior
     * @param validator Address of validator to slash
     * @param slashPercentage Percentage to slash (in BPS)
     * @param reason Reason for slashing
     */
    function slashValidator(
        address validator,
        uint256 slashPercentage,
        string calldata reason
    ) external onlyRole(SLASHER_ROLE) {
        Validator storage v = validators[validator];
        if (!v.isRegistered) revert ValidatorNotRegistered();

        uint256 totalStake = v.selfStake + v.delegatedStake;
        uint256 slashAmount = (totalStake * slashPercentage) / BPS_DENOMINATOR;

        // Slash proportionally from self-stake and delegated stake
        uint256 selfSlash = (v.selfStake * slashPercentage) / BPS_DENOMINATOR;
        uint256 delegatedSlash = slashAmount - selfSlash;

        v.selfStake -= selfSlash;
        v.delegatedStake -= delegatedSlash;
        totalStaked -= slashAmount;
        v.slashCount++;

        // Reduce total shares proportionally
        uint256 sharesToBurn = (validatorTotalShares[validator] * slashPercentage) / BPS_DENOMINATOR;
        validatorTotalShares[validator] -= sharesToBurn;

        // Transfer slashed tokens to treasury or burn
        // For now, tokens stay in contract as slashed pool

        emit ValidatorSlashed(validator, slashAmount, slashPercentage, reason);

        _checkValidatorStatus(validator);
    }

    /**
     * @notice Jail a validator (temporary removal from active set)
     * @param validator Address of validator to jail
     * @param duration How long to jail for
     * @param reason Reason for jailing
     */
    function jailValidator(
        address validator,
        uint256 duration,
        string calldata reason
    ) external onlyRole(SLASHER_ROLE) {
        Validator storage v = validators[validator];
        if (!v.isRegistered) revert ValidatorNotRegistered();

        v.jailedUntil = block.timestamp + duration;
        
        if (v.isActive) {
            v.isActive = false;
            _removeFromActiveSet(validator);
        }

        emit ValidatorJailed(validator, v.jailedUntil, reason);
    }

    /**
     * @notice Unjail a validator (validator must call after jail period)
     */
    function unjail() external {
        Validator storage v = validators[msg.sender];
        if (!v.isRegistered) revert ValidatorNotRegistered();
        if (v.jailedUntil > block.timestamp) revert ValidatorIsJailed();
        if (v.jailedUntil == 0) revert ValidatorNotJailed();

        v.jailedUntil = 0;

        emit ValidatorUnjailed(msg.sender);

        _tryActivateValidator(msg.sender);
    }

    // ============================================
    // EPOCH MANAGEMENT
    // ============================================

    /**
     * @notice Advance to the next epoch and update validator set
     */
    function advanceEpoch() external {
        if (block.number < epochStartBlock + epochLength) revert EpochNotAdvanceable();

        // Save current epoch snapshot
        epochSnapshots[currentEpoch] = EpochSnapshot({
            totalStake: totalStaked,
            validatorCount: activeValidatorCount,
            validatorSetHash: keccak256(abi.encode(activeValidatorSet)),
            blockNumber: block.number,
            timestamp: block.timestamp
        });

        currentEpoch++;
        epochStartBlock = block.number;

        // Rebuild active validator set
        _rebuildActiveValidatorSet();

        emit EpochAdvanced(currentEpoch, activeValidatorCount, totalStaked);
    }

    /**
     * @notice Distribute rewards to validators for the epoch
     * @param rewards Total rewards to distribute
     */
    function distributeEpochRewards(uint256 rewards) external onlyRole(GOVERNANCE_ROLE) {
        if (activeValidatorCount == 0) return;

        stakingToken.safeTransferFrom(msg.sender, address(this), rewards);

        // Distribute proportionally to stake
        for (uint256 i = 0; i < activeValidatorSet.length; i++) {
            address validator = activeValidatorSet[i];
            Validator storage v = validators[validator];
            
            uint256 validatorStake = v.selfStake + v.delegatedStake;
            uint256 validatorReward = (rewards * validatorStake) / totalStaked;
            
            // Validator takes commission
            uint256 commission = (validatorReward * v.commissionRate) / BPS_DENOMINATOR;
            v.accumulatedRewards += commission;

            // Rest goes to stake pool (increases share value)
            // This is automatically handled by share/stake ratio
        }
    }

    /**
     * @notice Claim accumulated rewards (validator only)
     */
    function claimValidatorRewards() external nonReentrant {
        Validator storage v = validators[msg.sender];
        if (!v.isRegistered) revert ValidatorNotRegistered();
        
        uint256 rewards = v.accumulatedRewards;
        v.accumulatedRewards = 0;

        stakingToken.safeTransfer(v.withdrawalAddress, rewards);

        emit RewardsClaimed(msg.sender, v.withdrawalAddress, rewards);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get the current active validator set
     * @return Array of active validator addresses
     */
    function getActiveValidatorSet() external view returns (address[] memory) {
        return activeValidatorSet;
    }

    /**
     * @notice Get validator's total stake (self + delegated)
     * @param validator Validator address
     * @return Total stake amount
     */
    function getValidatorTotalStake(address validator) external view returns (uint256) {
        Validator storage v = validators[validator];
        return v.selfStake + v.delegatedStake;
    }

    /**
     * @notice Get delegator's stake value with a validator
     * @param delegator Delegator address
     * @param validator Validator address
     * @return Current stake value
     */
    function getDelegationValue(address delegator, address validator) external view returns (uint256) {
        Delegation storage d = delegations[delegator][validator];
        if (d.shares == 0) return 0;

        Validator storage v = validators[validator];
        uint256 totalShares = validatorTotalShares[validator];
        uint256 totalStake = v.selfStake + v.delegatedStake;

        return (d.shares * totalStake) / totalShares;
    }

    /**
     * @notice Check if validator is eligible for active set
     * @param validator Validator address
     * @return Whether validator is eligible
     */
    function isEligibleValidator(address validator) public view returns (bool) {
        Validator storage v = validators[validator];
        return v.isRegistered &&
               v.jailedUntil <= block.timestamp &&
               v.selfStake + v.delegatedStake >= minValidatorStake;
    }

    /**
     * @notice Get voting power for consensus
     * @param validator Validator address
     * @return Voting power (stake-weighted)
     */
    function getVotingPower(address validator) external view returns (uint256) {
        if (!validators[validator].isActive) return 0;
        Validator storage v = validators[validator];
        return v.selfStake + v.delegatedStake;
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    function _tryActivateValidator(address validator) internal {
        if (activeValidatorCount >= MAX_VALIDATORS) {
            // Check if new validator has more stake than weakest
            if (activeValidatorSet.length > 0) {
                address weakest = activeValidatorSet[activeValidatorSet.length - 1];
                Validator storage weakestV = validators[weakest];
                Validator storage newV = validators[validator];
                
                if (newV.selfStake + newV.delegatedStake > 
                    weakestV.selfStake + weakestV.delegatedStake) {
                    _removeFromActiveSet(weakest);
                    _addToActiveSet(validator);
                }
            }
        } else if (isEligibleValidator(validator)) {
            _addToActiveSet(validator);
        }
    }

    function _addToActiveSet(address validator) internal {
        Validator storage v = validators[validator];
        if (v.isActive) return;
        
        v.isActive = true;
        activeValidatorSet.push(validator);
        activeValidatorCount++;
        
        // Sort by stake (simple insertion)
        _sortActiveSet();
    }

    function _removeFromActiveSet(address validator) internal {
        Validator storage v = validators[validator];
        if (!v.isActive) return;
        
        v.isActive = false;
        activeValidatorCount--;
        
        for (uint256 i = 0; i < activeValidatorSet.length; i++) {
            if (activeValidatorSet[i] == validator) {
                activeValidatorSet[i] = activeValidatorSet[activeValidatorSet.length - 1];
                activeValidatorSet.pop();
                break;
            }
        }
        
        _sortActiveSet();
    }

    function _sortActiveSet() internal {
        // Simple bubble sort (acceptable for small validator sets)
        uint256 n = activeValidatorSet.length;
        for (uint256 i = 0; i < n; i++) {
            for (uint256 j = i + 1; j < n; j++) {
                Validator storage vi = validators[activeValidatorSet[i]];
                Validator storage vj = validators[activeValidatorSet[j]];
                
                uint256 stakeI = vi.selfStake + vi.delegatedStake;
                uint256 stakeJ = vj.selfStake + vj.delegatedStake;
                
                if (stakeJ > stakeI) {
                    address temp = activeValidatorSet[i];
                    activeValidatorSet[i] = activeValidatorSet[j];
                    activeValidatorSet[j] = temp;
                }
            }
        }
    }

    function _rebuildActiveValidatorSet() internal {
        // Clear current set
        while (activeValidatorSet.length > 0) {
            validators[activeValidatorSet[activeValidatorSet.length - 1]].isActive = false;
            activeValidatorSet.pop();
        }
        activeValidatorCount = 0;

        // Re-add eligible validators
        for (uint256 i = 0; i < allValidators.length && activeValidatorCount < MAX_VALIDATORS; i++) {
            if (isEligibleValidator(allValidators[i])) {
                _addToActiveSet(allValidators[i]);
            }
        }

        emit ValidatorSetUpdated(currentEpoch, activeValidatorSet);
    }

    function _checkValidatorStatus(address validator) internal {
        if (!isEligibleValidator(validator) && validators[validator].isActive) {
            _removeFromActiveSet(validator);
        }
    }

    // ============================================
    // GOVERNANCE FUNCTIONS
    // ============================================

    function setMinValidatorStake(uint256 newMin) external onlyRole(GOVERNANCE_ROLE) {
        minValidatorStake = newMin;
    }

    function setMaxValidatorStake(uint256 newMax) external onlyRole(GOVERNANCE_ROLE) {
        maxValidatorStake = newMax;
    }

    function setEpochLength(uint256 newLength) external onlyRole(GOVERNANCE_ROLE) {
        epochLength = newLength;
    }
}
