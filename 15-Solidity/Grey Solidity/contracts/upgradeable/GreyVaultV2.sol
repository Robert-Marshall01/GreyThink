// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts/proxy/ERC1967/ERC1967Utils.sol";

/**
 * @title GreyVaultV2
 * @author Grey Solidity Project
 * @notice UUPS upgradeable vault implementation V2 with interest
 * @dev Demonstrates upgrade with new features while preserving state
 */
contract GreyVaultV2 is 
    Initializable,
    UUPSUpgradeable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable 
{
    /// @notice Role for upgrades
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Deposit structure (must match V1 exactly)
    struct Deposit {
        uint256 amount;
        uint256 timestamp;
    }

    /// @notice Total deposits (inherited from V1)
    uint256 public totalDeposits;

    /// @notice User deposits mapping (inherited from V1)
    mapping(address => Deposit) public deposits;

    // ========== V2 New Storage ==========

    /// @notice Annual interest rate in basis points
    uint256 public interestRate;

    /// @notice Last interest accrual timestamp
    mapping(address => uint256) public lastAccrual;

    /// @notice Accrued interest
    mapping(address => uint256) public accruedInterest;

    /// @notice Withdrawal fee in basis points
    uint256 public withdrawalFee;

    /// @notice Fee recipient
    address public feeRecipient;

    /// @notice Total fees collected
    uint256 public totalFeesCollected;

    /// @notice Version string
    string public constant VERSION = "2.0.0";

    // Storage gap for future upgrades (reduced due to new storage)
    uint256[43] private __gap;

    // ============ Events ============

    event Deposited(address indexed user, uint256 amount);
    event Withdrawn(address indexed user, uint256 amount, uint256 fee);
    event InterestAccrued(address indexed user, uint256 amount);
    event InterestRateUpdated(uint256 oldRate, uint256 newRate);
    event WithdrawalFeeUpdated(uint256 oldFee, uint256 newFee);
    event FeesWithdrawn(address indexed recipient, uint256 amount);

    // ============ Errors ============

    error InsufficientBalance();
    error ZeroAmount();
    error ZeroAddress();

    /// @custom:oz-upgrades-unsafe-allow constructor
    constructor() {
        _disableInitializers();
    }

    /**
     * @notice Reinitializer for V2 upgrade
     * @param interestRate_ Initial interest rate
     * @param withdrawalFee_ Initial withdrawal fee
     * @param feeRecipient_ Fee recipient address
     */
    function initializeV2(
        uint256 interestRate_,
        uint256 withdrawalFee_,
        address feeRecipient_
    ) public reinitializer(2) {
        require(interestRate_ <= 2000, "Rate too high"); // Max 20%
        require(withdrawalFee_ <= 500, "Fee too high");  // Max 5%
        require(feeRecipient_ != address(0), "Zero address");

        interestRate = interestRate_;
        withdrawalFee = withdrawalFee_;
        feeRecipient = feeRecipient_;
    }

    // ============ Core Functions ============

    /**
     * @notice Deposits ETH
     */
    function deposit() external payable nonReentrant whenNotPaused {
        if (msg.value == 0) revert ZeroAmount();

        // Accrue interest before deposit
        _accrueInterest(msg.sender);

        deposits[msg.sender].amount += msg.value;
        deposits[msg.sender].timestamp = block.timestamp;
        lastAccrual[msg.sender] = block.timestamp;
        totalDeposits += msg.value;

        emit Deposited(msg.sender, msg.value);
    }

    /**
     * @notice Withdraws ETH with interest
     * @param amount Amount to withdraw (principal)
     */
    function withdraw(uint256 amount) external nonReentrant whenNotPaused {
        if (amount == 0) revert ZeroAmount();
        
        // Accrue interest first
        _accrueInterest(msg.sender);

        uint256 totalBalance = deposits[msg.sender].amount + accruedInterest[msg.sender];
        if (amount > totalBalance) revert InsufficientBalance();

        // Deduct from interest first, then principal
        uint256 remainingAmount = amount;
        if (accruedInterest[msg.sender] >= remainingAmount) {
            accruedInterest[msg.sender] -= remainingAmount;
        } else {
            remainingAmount -= accruedInterest[msg.sender];
            accruedInterest[msg.sender] = 0;
            deposits[msg.sender].amount -= remainingAmount;
            totalDeposits -= remainingAmount;
        }

        // Calculate fee
        uint256 fee = (amount * withdrawalFee) / 10000;
        uint256 netAmount = amount - fee;
        totalFeesCollected += fee;

        (bool success, ) = msg.sender.call{value: netAmount}("");
        require(success, "Transfer failed");

        emit Withdrawn(msg.sender, netAmount, fee);
    }

    /**
     * @notice Claims accrued interest without withdrawing principal
     */
    function claimInterest() external nonReentrant whenNotPaused {
        _accrueInterest(msg.sender);

        uint256 interest = accruedInterest[msg.sender];
        if (interest == 0) revert ZeroAmount();

        accruedInterest[msg.sender] = 0;

        // Calculate fee
        uint256 fee = (interest * withdrawalFee) / 10000;
        uint256 netInterest = interest - fee;
        totalFeesCollected += fee;

        (bool success, ) = msg.sender.call{value: netInterest}("");
        require(success, "Transfer failed");

        emit Withdrawn(msg.sender, netInterest, fee);
    }

    // ============ View Functions ============

    /**
     * @notice Returns user's total balance including interest
     * @param user User address
     * @return balance Total balance
     */
    function balanceOf(address user) external view returns (uint256 balance) {
        uint256 pendingInterest = _calculatePendingInterest(user);
        return deposits[user].amount + accruedInterest[user] + pendingInterest;
    }

    /**
     * @notice Returns detailed user info
     * @param user User address
     * @return principal Principal amount
     * @return interest Accrued interest
     * @return pending Pending interest
     */
    function getUserInfo(address user) external view returns (
        uint256 principal,
        uint256 interest,
        uint256 pending
    ) {
        principal = deposits[user].amount;
        interest = accruedInterest[user];
        pending = _calculatePendingInterest(user);
    }

    /**
     * @notice Calculates pending interest for a user
     * @param user User address
     * @return Pending interest amount
     */
    function pendingInterest(address user) external view returns (uint256) {
        return _calculatePendingInterest(user);
    }

    // ============ Admin Functions ============

    /**
     * @notice Sets the interest rate
     * @param newRate New rate in basis points
     */
    function setInterestRate(uint256 newRate) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newRate <= 2000, "Rate too high");
        uint256 oldRate = interestRate;
        interestRate = newRate;
        emit InterestRateUpdated(oldRate, newRate);
    }

    /**
     * @notice Sets the withdrawal fee
     * @param newFee New fee in basis points
     */
    function setWithdrawalFee(uint256 newFee) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(newFee <= 500, "Fee too high");
        uint256 oldFee = withdrawalFee;
        withdrawalFee = newFee;
        emit WithdrawalFeeUpdated(oldFee, newFee);
    }

    /**
     * @notice Sets the fee recipient
     * @param newRecipient New recipient address
     */
    function setFeeRecipient(address newRecipient) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (newRecipient == address(0)) revert ZeroAddress();
        feeRecipient = newRecipient;
    }

    /**
     * @notice Withdraws collected fees
     */
    function withdrawFees() external onlyRole(DEFAULT_ADMIN_ROLE) nonReentrant {
        uint256 fees = totalFeesCollected;
        totalFeesCollected = 0;

        (bool success, ) = feeRecipient.call{value: fees}("");
        require(success, "Transfer failed");

        emit FeesWithdrawn(feeRecipient, fees);
    }

    /**
     * @notice Pauses the vault
     */
    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the vault
     */
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        _unpause();
    }

    // ============ Internal Functions ============

    /**
     * @dev Accrues interest for a user
     */
    function _accrueInterest(address user) private {
        uint256 pending = _calculatePendingInterest(user);
        if (pending > 0) {
            accruedInterest[user] += pending;
            emit InterestAccrued(user, pending);
        }
        lastAccrual[user] = block.timestamp;
    }

    /**
     * @dev Calculates pending interest
     */
    function _calculatePendingInterest(address user) private view returns (uint256) {
        uint256 principal = deposits[user].amount;
        if (principal == 0 || interestRate == 0) {
            return 0;
        }

        uint256 lastTime = lastAccrual[user];
        if (lastTime == 0) {
            lastTime = deposits[user].timestamp;
        }
        if (lastTime == 0 || block.timestamp <= lastTime) {
            return 0;
        }

        uint256 timeElapsed = block.timestamp - lastTime;
        // Simple interest: principal * rate * time / (365 days * 10000)
        return (principal * interestRate * timeElapsed) / (365 days * 10000);
    }

    // ============ UUPS ============

    /**
     * @dev Authorizes upgrade
     */
    function _authorizeUpgrade(address newImplementation) internal override onlyRole(UPGRADER_ROLE) {}

    /**
     * @notice Returns the implementation address
     */
    function getImplementation() external view returns (address) {
        return ERC1967Utils.getImplementation();
    }

    // Receive ETH
    receive() external payable {
        _accrueInterest(msg.sender);
        deposits[msg.sender].amount += msg.value;
        deposits[msg.sender].timestamp = block.timestamp;
        lastAccrual[msg.sender] = block.timestamp;
        totalDeposits += msg.value;
        emit Deposited(msg.sender, msg.value);
    }
}
