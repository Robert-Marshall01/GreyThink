// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/access/OwnableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";

/**
 * @title TransparentRegistry
 * @author Grey Solidity Project
 * @notice Upgradeable registry for transparent proxy pattern
 * @dev Used with OpenZeppelin TransparentUpgradeableProxy
 */
contract TransparentRegistry is 
    Initializable,
    OwnableUpgradeable,
    ReentrancyGuardUpgradeable 
{
    /// @notice Record structure
    struct Record {
        string name;
        string data;
        address owner;
        uint256 createdAt;
        uint256 updatedAt;
        bool active;
    }

    /// @notice Mapping of record ID to record
    mapping(bytes32 => Record) public records;

    /// @notice User's record IDs
    mapping(address => bytes32[]) public userRecords;

    /// @notice Total record count
    uint256 public recordCount;

    /// @notice Registration fee
    uint256 public registrationFee;

    /// @notice Version
    string public constant VERSION = "1.0.0";

    // Storage gap
    uint256[47] private __gap;

    // ============ Events ============

    event RecordCreated(bytes32 indexed id, string name, address indexed owner);
    event RecordUpdated(bytes32 indexed id, string newData);
    event RecordTransferred(bytes32 indexed id, address indexed from, address indexed to);
    event RecordDeactivated(bytes32 indexed id);
    event RegistrationFeeUpdated(uint256 oldFee, uint256 newFee);
    event FeesWithdrawn(address indexed to, uint256 amount);

    // ============ Errors ============

    error RecordExists(bytes32 id);
    error RecordNotFound(bytes32 id);
    error NotRecordOwner();
    error InsufficientFee(uint256 required, uint256 provided);
    error ZeroAddress();

    /// @custom:oz-upgrades-unsafe-allow constructor
    constructor() {
        _disableInitializers();
    }

    /**
     * @notice Initializes the registry
     * @param owner_ The owner address
     * @param fee_ Initial registration fee
     */
    function initialize(address owner_, uint256 fee_) public initializer {
        __Ownable_init(owner_);
        __ReentrancyGuard_init();
        registrationFee = fee_;
    }

    // ============ Core Functions ============

    /**
     * @notice Creates a new record
     * @param name Record name
     * @param data Record data
     * @return id The record ID
     */
    function createRecord(
        string calldata name,
        string calldata data
    ) external payable nonReentrant returns (bytes32 id) {
        if (msg.value < registrationFee) {
            revert InsufficientFee(registrationFee, msg.value);
        }

        id = keccak256(abi.encodePacked(name, msg.sender, block.timestamp));
        
        if (records[id].createdAt > 0) {
            revert RecordExists(id);
        }

        records[id] = Record({
            name: name,
            data: data,
            owner: msg.sender,
            createdAt: block.timestamp,
            updatedAt: block.timestamp,
            active: true
        });

        userRecords[msg.sender].push(id);
        recordCount++;

        emit RecordCreated(id, name, msg.sender);

        // Refund excess
        if (msg.value > registrationFee) {
            (bool success, ) = msg.sender.call{value: msg.value - registrationFee}("");
            require(success, "Refund failed");
        }
    }

    /**
     * @notice Updates a record
     * @param id Record ID
     * @param newData New data
     */
    function updateRecord(bytes32 id, string calldata newData) external {
        Record storage record = records[id];
        
        if (record.createdAt == 0) revert RecordNotFound(id);
        if (record.owner != msg.sender) revert NotRecordOwner();

        record.data = newData;
        record.updatedAt = block.timestamp;

        emit RecordUpdated(id, newData);
    }

    /**
     * @notice Transfers record ownership
     * @param id Record ID
     * @param newOwner New owner address
     */
    function transferRecord(bytes32 id, address newOwner) external {
        if (newOwner == address(0)) revert ZeroAddress();
        
        Record storage record = records[id];
        
        if (record.createdAt == 0) revert RecordNotFound(id);
        if (record.owner != msg.sender) revert NotRecordOwner();

        address previousOwner = record.owner;
        record.owner = newOwner;
        record.updatedAt = block.timestamp;

        userRecords[newOwner].push(id);

        emit RecordTransferred(id, previousOwner, newOwner);
    }

    /**
     * @notice Deactivates a record
     * @param id Record ID
     */
    function deactivateRecord(bytes32 id) external {
        Record storage record = records[id];
        
        if (record.createdAt == 0) revert RecordNotFound(id);
        if (record.owner != msg.sender) revert NotRecordOwner();

        record.active = false;
        record.updatedAt = block.timestamp;

        emit RecordDeactivated(id);
    }

    // ============ View Functions ============

    /**
     * @notice Gets a record
     * @param id Record ID
     * @return The record struct
     */
    function getRecord(bytes32 id) external view returns (Record memory) {
        return records[id];
    }

    /**
     * @notice Gets user's records
     * @param user User address
     * @return Array of record IDs
     */
    function getUserRecords(address user) external view returns (bytes32[] memory) {
        return userRecords[user];
    }

    /**
     * @notice Checks if a record exists
     * @param id Record ID
     * @return True if exists
     */
    function recordExists(bytes32 id) external view returns (bool) {
        return records[id].createdAt > 0;
    }

    /**
     * @notice Computes record ID
     * @param name Record name
     * @param owner Owner address
     * @param timestamp Creation timestamp
     * @return The record ID
     */
    function computeRecordId(
        string calldata name,
        address owner,
        uint256 timestamp
    ) external pure returns (bytes32) {
        return keccak256(abi.encodePacked(name, owner, timestamp));
    }

    // ============ Admin Functions ============

    /**
     * @notice Sets registration fee
     * @param newFee New fee
     */
    function setRegistrationFee(uint256 newFee) external onlyOwner {
        uint256 oldFee = registrationFee;
        registrationFee = newFee;
        emit RegistrationFeeUpdated(oldFee, newFee);
    }

    /**
     * @notice Withdraws collected fees
     * @param to Recipient address
     */
    function withdrawFees(address to) external onlyOwner nonReentrant {
        if (to == address(0)) revert ZeroAddress();
        
        uint256 balance = address(this).balance;
        (bool success, ) = to.call{value: balance}("");
        require(success, "Transfer failed");

        emit FeesWithdrawn(to, balance);
    }

    // Receive ETH
    receive() external payable {}
}
