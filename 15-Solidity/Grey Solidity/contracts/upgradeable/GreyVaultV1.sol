// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts/proxy/ERC1967/ERC1967Utils.sol";

/**
 * @title GreyVaultV1
 * @author Grey Solidity Project
 * @notice UUPS upgradeable vault implementation V1
 * @dev Demonstrates UUPS upgrade pattern with storage gaps
 */
contract GreyVaultV1 is 
    Initializable,
    UUPSUpgradeable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable 
{
    /// @notice Role for upgrades
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Deposit structure
    struct Deposit {
        uint256 amount;
        uint256 timestamp;
    }

    /// @notice Total deposits
    uint256 public totalDeposits;

    /// @notice User deposits mapping
    mapping(address => Deposit) public deposits;

    /// @notice Version string
    string public constant VERSION = "1.0.0";

    // Storage gap for future upgrades
    uint256[48] private __gap;

    // ============ Events ============

    event Deposited(address indexed user, uint256 amount);
    event Withdrawn(address indexed user, uint256 amount);

    // ============ Errors ============

    error InsufficientBalance();
    error ZeroAmount();

    /// @custom:oz-upgrades-unsafe-allow constructor
    constructor() {
        _disableInitializers();
    }

    /**
     * @notice Initializes the vault
     * @param admin The admin address
     */
    function initialize(address admin) public initializer {
        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();
        __UUPSUpgradeable_init();

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
    }

    // ============ Core Functions ============

    /**
     * @notice Deposits ETH
     */
    function deposit() external payable nonReentrant whenNotPaused {
        if (msg.value == 0) revert ZeroAmount();

        deposits[msg.sender].amount += msg.value;
        deposits[msg.sender].timestamp = block.timestamp;
        totalDeposits += msg.value;

        emit Deposited(msg.sender, msg.value);
    }

    /**
     * @notice Withdraws ETH
     * @param amount Amount to withdraw
     */
    function withdraw(uint256 amount) external nonReentrant whenNotPaused {
        if (amount == 0) revert ZeroAmount();
        if (deposits[msg.sender].amount < amount) revert InsufficientBalance();

        deposits[msg.sender].amount -= amount;
        totalDeposits -= amount;

        (bool success, ) = msg.sender.call{value: amount}("");
        require(success, "Transfer failed");

        emit Withdrawn(msg.sender, amount);
    }

    // ============ View Functions ============

    /**
     * @notice Returns user balance
     * @param user User address
     * @return The balance
     */
    function balanceOf(address user) external view returns (uint256) {
        return deposits[user].amount;
    }

    /**
     * @notice Returns user deposit info
     * @param user User address
     * @return amount The deposited amount
     * @return timestamp The deposit timestamp
     */
    function getDeposit(address user) external view returns (uint256 amount, uint256 timestamp) {
        Deposit storage d = deposits[user];
        return (d.amount, d.timestamp);
    }

    // ============ Admin Functions ============

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

    // ============ UUPS ============

    /**
     * @dev Authorizes upgrade
     */
    function _authorizeUpgrade(address newImplementation) internal override onlyRole(UPGRADER_ROLE) {}

    /**
     * @notice Returns the implementation address
     * @return The implementation address
     */
    function getImplementation() external view returns (address) {
        return ERC1967Utils.getImplementation();
    }

    // Receive ETH
    receive() external payable {
        deposits[msg.sender].amount += msg.value;
        deposits[msg.sender].timestamp = block.timestamp;
        totalDeposits += msg.value;
        emit Deposited(msg.sender, msg.value);
    }
}
