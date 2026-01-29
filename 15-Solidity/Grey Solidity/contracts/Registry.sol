// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "./AccessControlManager.sol";

/**
 * @title Registry
 * @author Grey Solidity Project
 * @notice A registry for storing and managing structured records
 * @dev Integrates with AccessControlManager for permission control
 */
contract Registry {
    /// @notice Reference to the access control contract
    AccessControlManager public immutable accessControl;

    /// @notice Structure representing a record in the registry
    struct Record {
        uint256 id;
        string name;
        string data;
        address owner;
        uint256 createdAt;
        uint256 updatedAt;
        bool active;
    }

    /// @notice Counter for generating unique record IDs
    uint256 private _recordIdCounter;

    /// @notice Mapping from record ID to Record struct
    mapping(uint256 => Record) private _records;

    /// @notice Mapping from owner address to their record IDs
    mapping(address => uint256[]) private _ownerRecords;

    /// @notice Array of all record IDs
    uint256[] private _allRecordIds;

    /// @notice Emitted when a new record is created
    /// @param id The unique identifier of the record
    /// @param name The name of the record
    /// @param owner The owner of the record
    event RecordCreated(uint256 indexed id, string name, address indexed owner);

    /// @notice Emitted when a record is updated
    /// @param id The unique identifier of the record
    /// @param name The new name of the record
    /// @param data The new data of the record
    event RecordUpdated(uint256 indexed id, string name, string data);

    /// @notice Emitted when a record is deactivated
    /// @param id The unique identifier of the record
    event RecordDeactivated(uint256 indexed id);

    /// @notice Emitted when a record is reactivated
    /// @param id The unique identifier of the record
    event RecordReactivated(uint256 indexed id);

    /// @notice Error thrown when a record is not found
    error RecordNotFound(uint256 id);

    /// @notice Error thrown when caller is not the record owner
    error NotRecordOwner(uint256 id, address caller);

    /// @notice Error thrown when a name is empty
    error EmptyName();

    /// @notice Restricts access to users with USER_ROLE or ADMIN_ROLE
    modifier onlyUser() {
        bool isUser = accessControl.hasRole(accessControl.USER_ROLE(), msg.sender);
        bool isAdmin = accessControl.hasRole(accessControl.ADMIN_ROLE(), msg.sender);
        require(isUser || isAdmin, "Registry: caller is not a user or admin");
        _;
    }

    /// @notice Restricts access to admins only
    modifier onlyAdmin() {
        require(
            accessControl.hasRole(accessControl.ADMIN_ROLE(), msg.sender),
            "Registry: caller is not an admin"
        );
        _;
    }

    /// @notice Ensures the record exists
    modifier recordExists(uint256 id) {
        if (_records[id].createdAt == 0) {
            revert RecordNotFound(id);
        }
        _;
    }

    /// @notice Ensures caller is the record owner or an admin
    modifier onlyRecordOwnerOrAdmin(uint256 id) {
        bool isOwner = _records[id].owner == msg.sender;
        bool isAdmin = accessControl.hasRole(accessControl.ADMIN_ROLE(), msg.sender);
        if (!isOwner && !isAdmin) {
            revert NotRecordOwner(id, msg.sender);
        }
        _;
    }

    /**
     * @notice Initializes the Registry with an AccessControlManager reference
     * @param _accessControl Address of the AccessControlManager contract
     */
    constructor(address _accessControl) {
        require(_accessControl != address(0), "Registry: invalid access control address");
        accessControl = AccessControlManager(_accessControl);
    }

    /**
     * @notice Creates a new record in the registry
     * @dev Caller must have USER_ROLE or ADMIN_ROLE
     * @param name The name of the record
     * @param data The data to store in the record
     * @return id The unique identifier of the created record
     */
    function createRecord(string calldata name, string calldata data) 
        external 
        onlyUser 
        returns (uint256 id) 
    {
        if (bytes(name).length == 0) {
            revert EmptyName();
        }

        id = ++_recordIdCounter;
        
        _records[id] = Record({
            id: id,
            name: name,
            data: data,
            owner: msg.sender,
            createdAt: block.timestamp,
            updatedAt: block.timestamp,
            active: true
        });

        _ownerRecords[msg.sender].push(id);
        _allRecordIds.push(id);

        emit RecordCreated(id, name, msg.sender);
    }

    /**
     * @notice Updates an existing record
     * @dev Caller must be the record owner or an admin
     * @param id The ID of the record to update
     * @param name The new name for the record
     * @param data The new data for the record
     */
    function updateRecord(uint256 id, string calldata name, string calldata data)
        external
        recordExists(id)
        onlyRecordOwnerOrAdmin(id)
    {
        if (bytes(name).length == 0) {
            revert EmptyName();
        }

        Record storage record = _records[id];
        record.name = name;
        record.data = data;
        record.updatedAt = block.timestamp;

        emit RecordUpdated(id, name, data);
    }

    /**
     * @notice Deactivates a record
     * @dev Caller must be the record owner or an admin
     * @param id The ID of the record to deactivate
     */
    function deactivateRecord(uint256 id)
        external
        recordExists(id)
        onlyRecordOwnerOrAdmin(id)
    {
        _records[id].active = false;
        _records[id].updatedAt = block.timestamp;
        emit RecordDeactivated(id);
    }

    /**
     * @notice Reactivates a deactivated record
     * @dev Only admins can reactivate records
     * @param id The ID of the record to reactivate
     */
    function reactivateRecord(uint256 id)
        external
        recordExists(id)
        onlyAdmin
    {
        _records[id].active = true;
        _records[id].updatedAt = block.timestamp;
        emit RecordReactivated(id);
    }

    /**
     * @notice Retrieves a record by its ID
     * @param id The ID of the record
     * @return The Record struct
     */
    function getRecord(uint256 id) 
        external 
        view 
        recordExists(id) 
        returns (Record memory) 
    {
        return _records[id];
    }

    /**
     * @notice Gets all record IDs owned by a specific address
     * @param owner The owner address to query
     * @return Array of record IDs
     */
    function getRecordsByOwner(address owner) external view returns (uint256[] memory) {
        return _ownerRecords[owner];
    }

    /**
     * @notice Gets the total number of records
     * @return The total count of records
     */
    function getTotalRecords() external view returns (uint256) {
        return _recordIdCounter;
    }

    /**
     * @notice Gets all record IDs
     * @return Array of all record IDs
     */
    function getAllRecordIds() external view returns (uint256[] memory) {
        return _allRecordIds;
    }
}
