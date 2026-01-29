// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "../interfaces/IAccessControlExtended.sol";
import "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";

/**
 * @title AccessControlExtended
 * @author Grey Solidity Project
 * @notice Extended role-based access control with identity management
 * @dev Implements granular permissions with ADMIN, OPERATOR, and USER roles
 */
contract AccessControlExtended is IAccessControlExtended {
    using EnumerableSet for EnumerableSet.AddressSet;

    /// @inheritdoc IAccessControlExtended
    bytes32 public constant override DEFAULT_ADMIN_ROLE = 0x00;
    
    /// @inheritdoc IAccessControlExtended
    bytes32 public constant override ADMIN_ROLE = keccak256("ADMIN_ROLE");
    
    /// @inheritdoc IAccessControlExtended
    bytes32 public constant override OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    
    /// @inheritdoc IAccessControlExtended
    bytes32 public constant override USER_ROLE = keccak256("USER_ROLE");

    /// @notice Maps role => accounts with that role
    mapping(bytes32 => EnumerableSet.AddressSet) private _roleMembers;

    /// @notice Maps role => admin role
    mapping(bytes32 => bytes32) private _roleAdmin;

    /// @notice Maps account => identity info
    mapping(address => Identity) private _identities;

    /// @notice Tracks which accounts have registered identities
    EnumerableSet.AddressSet private _registeredAccounts;

    /// @notice Error for unauthorized access
    error AccessControlUnauthorizedAccount(address account, bytes32 role);

    /// @notice Error for invalid identity operation
    error IdentityError(string reason);

    /// @notice Modifier to restrict access to role holders
    modifier onlyRole(bytes32 role) {
        _checkRole(role, msg.sender);
        _;
    }

    /// @notice Modifier to restrict to admin or operator
    modifier onlyAdminOrOperator() {
        require(
            hasRole(ADMIN_ROLE, msg.sender) || hasRole(OPERATOR_ROLE, msg.sender),
            "AccessControl: caller is not admin or operator"
        );
        _;
    }

    /**
     * @notice Initializes the access control with deployer as admin
     */
    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(ADMIN_ROLE, msg.sender);
        
        _setRoleAdmin(ADMIN_ROLE, DEFAULT_ADMIN_ROLE);
        _setRoleAdmin(OPERATOR_ROLE, ADMIN_ROLE);
        _setRoleAdmin(USER_ROLE, OPERATOR_ROLE);
    }

    // ============ Role Management ============

    /// @inheritdoc IAccessControlExtended
    function hasRole(bytes32 role, address account) public view override returns (bool) {
        return _roleMembers[role].contains(account);
    }

    /// @inheritdoc IAccessControlExtended
    function getRoleAdmin(bytes32 role) public view override returns (bytes32) {
        return _roleAdmin[role];
    }

    /// @inheritdoc IAccessControlExtended
    function grantRole(bytes32 role, address account) external override onlyRole(getRoleAdmin(role)) {
        _grantRole(role, account);
    }

    /// @inheritdoc IAccessControlExtended
    function revokeRole(bytes32 role, address account) external override onlyRole(getRoleAdmin(role)) {
        _revokeRole(role, account);
    }

    /// @inheritdoc IAccessControlExtended
    function renounceRole(bytes32 role) external override {
        _revokeRole(role, msg.sender);
    }

    /// @inheritdoc IAccessControlExtended
    function getRoleMemberCount(bytes32 role) external view override returns (uint256) {
        return _roleMembers[role].length();
    }

    /// @inheritdoc IAccessControlExtended
    function getRoleMember(bytes32 role, uint256 index) external view override returns (address) {
        return _roleMembers[role].at(index);
    }

    // ============ Identity Management ============

    /// @inheritdoc IAccessControlExtended
    function registerIdentity(
        address account,
        bytes32 identityHash,
        string calldata metadataUri
    ) external override onlyAdminOrOperator {
        if (_identities[account].registeredAt != 0) {
            revert IdentityError("Identity already registered");
        }
        if (account == address(0)) {
            revert IdentityError("Invalid account address");
        }
        if (identityHash == bytes32(0)) {
            revert IdentityError("Invalid identity hash");
        }

        _identities[account] = Identity({
            identityHash: identityHash,
            metadataUri: metadataUri,
            registeredAt: block.timestamp,
            updatedAt: block.timestamp,
            verified: false,
            active: true
        });

        _registeredAccounts.add(account);

        emit IdentityRegistered(account, identityHash, metadataUri);
    }

    /// @inheritdoc IAccessControlExtended
    function updateIdentityMetadata(
        address account,
        string calldata metadataUri
    ) external override onlyAdminOrOperator {
        if (_identities[account].registeredAt == 0) {
            revert IdentityError("Identity not registered");
        }

        string memory oldUri = _identities[account].metadataUri;
        _identities[account].metadataUri = metadataUri;
        _identities[account].updatedAt = block.timestamp;

        emit IdentityUpdated(account, oldUri, metadataUri);
    }

    /// @inheritdoc IAccessControlExtended
    function verifyIdentity(address account) external override onlyRole(ADMIN_ROLE) {
        if (_identities[account].registeredAt == 0) {
            revert IdentityError("Identity not registered");
        }
        if (_identities[account].verified) {
            revert IdentityError("Identity already verified");
        }

        _identities[account].verified = true;
        _identities[account].updatedAt = block.timestamp;

        emit IdentityVerified(account, msg.sender);
    }

    /// @inheritdoc IAccessControlExtended
    function revokeIdentityVerification(address account) external override onlyRole(ADMIN_ROLE) {
        if (_identities[account].registeredAt == 0) {
            revert IdentityError("Identity not registered");
        }
        if (!_identities[account].verified) {
            revert IdentityError("Identity not verified");
        }

        _identities[account].verified = false;
        _identities[account].updatedAt = block.timestamp;

        emit IdentityVerificationRevoked(account, msg.sender);
    }

    /// @inheritdoc IAccessControlExtended
    function deactivateIdentity(address account) external override onlyRole(ADMIN_ROLE) {
        if (_identities[account].registeredAt == 0) {
            revert IdentityError("Identity not registered");
        }
        if (!_identities[account].active) {
            revert IdentityError("Identity already deactivated");
        }

        _identities[account].active = false;
        _identities[account].updatedAt = block.timestamp;

        emit IdentityDeactivated(account);
    }

    /// @inheritdoc IAccessControlExtended
    function reactivateIdentity(address account) external override onlyRole(ADMIN_ROLE) {
        if (_identities[account].registeredAt == 0) {
            revert IdentityError("Identity not registered");
        }
        if (_identities[account].active) {
            revert IdentityError("Identity already active");
        }

        _identities[account].active = true;
        _identities[account].updatedAt = block.timestamp;

        emit IdentityReactivated(account);
    }

    /// @inheritdoc IAccessControlExtended
    function getIdentity(address account) external view override returns (Identity memory) {
        return _identities[account];
    }

    /// @inheritdoc IAccessControlExtended
    function hasIdentity(address account) external view override returns (bool) {
        return _identities[account].registeredAt != 0;
    }

    /// @inheritdoc IAccessControlExtended
    function isVerified(address account) external view override returns (bool) {
        return _identities[account].verified && _identities[account].active;
    }

    // ============ Internal Functions ============

    /**
     * @notice Internal function to check role
     * @param role The role to check
     * @param account The account to check
     */
    function _checkRole(bytes32 role, address account) internal view {
        if (!hasRole(role, account)) {
            revert AccessControlUnauthorizedAccount(account, role);
        }
    }

    /**
     * @notice Internal function to grant a role
     * @param role The role to grant
     * @param account The account to receive the role
     */
    function _grantRole(bytes32 role, address account) internal {
        if (!hasRole(role, account)) {
            _roleMembers[role].add(account);
            emit RoleGranted(role, account, msg.sender);
        }
    }

    /**
     * @notice Internal function to revoke a role
     * @param role The role to revoke
     * @param account The account to lose the role
     */
    function _revokeRole(bytes32 role, address account) internal {
        if (hasRole(role, account)) {
            _roleMembers[role].remove(account);
            emit RoleRevoked(role, account, msg.sender);
        }
    }

    /**
     * @notice Internal function to set role admin
     * @param role The role to configure
     * @param adminRole The admin role
     */
    function _setRoleAdmin(bytes32 role, bytes32 adminRole) internal {
        bytes32 previousAdminRole = getRoleAdmin(role);
        _roleAdmin[role] = adminRole;
        emit RoleAdminChanged(role, previousAdminRole, adminRole);
    }

    // ============ Batch Operations ============

    /**
     * @notice Grants roles to multiple accounts
     * @param role The role to grant
     * @param accounts The accounts to receive the role
     */
    function batchGrantRole(
        bytes32 role,
        address[] calldata accounts
    ) external onlyRole(getRoleAdmin(role)) {
        for (uint256 i = 0; i < accounts.length; i++) {
            _grantRole(role, accounts[i]);
        }
    }

    /**
     * @notice Revokes roles from multiple accounts
     * @param role The role to revoke
     * @param accounts The accounts to lose the role
     */
    function batchRevokeRole(
        bytes32 role,
        address[] calldata accounts
    ) external onlyRole(getRoleAdmin(role)) {
        for (uint256 i = 0; i < accounts.length; i++) {
            _revokeRole(role, accounts[i]);
        }
    }

    /**
     * @notice Returns all registered account addresses
     * @return Array of registered accounts
     */
    function getRegisteredAccounts() external view returns (address[] memory) {
        uint256 length = _registeredAccounts.length();
        address[] memory accounts = new address[](length);
        
        for (uint256 i = 0; i < length; i++) {
            accounts[i] = _registeredAccounts.at(i);
        }
        
        return accounts;
    }

    /**
     * @notice Returns the count of registered identities
     * @return The count
     */
    function getRegisteredCount() external view returns (uint256) {
        return _registeredAccounts.length();
    }

    /**
     * @notice Checks if an account has any role
     * @param account The account to check
     * @return True if has any role
     */
    function hasAnyRole(address account) external view returns (bool) {
        return hasRole(ADMIN_ROLE, account) || 
               hasRole(OPERATOR_ROLE, account) || 
               hasRole(USER_ROLE, account);
    }

    /**
     * @notice Gets all roles for an account
     * @param account The account to query
     * @return roles Array of role hashes
     */
    function getRolesForAccount(address account) external view returns (bytes32[] memory roles) {
        uint256 count;
        if (hasRole(DEFAULT_ADMIN_ROLE, account)) count++;
        if (hasRole(ADMIN_ROLE, account)) count++;
        if (hasRole(OPERATOR_ROLE, account)) count++;
        if (hasRole(USER_ROLE, account)) count++;

        roles = new bytes32[](count);
        uint256 index;

        if (hasRole(DEFAULT_ADMIN_ROLE, account)) {
            roles[index++] = DEFAULT_ADMIN_ROLE;
        }
        if (hasRole(ADMIN_ROLE, account)) {
            roles[index++] = ADMIN_ROLE;
        }
        if (hasRole(OPERATOR_ROLE, account)) {
            roles[index++] = OPERATOR_ROLE;
        }
        if (hasRole(USER_ROLE, account)) {
            roles[index++] = USER_ROLE;
        }
    }
}
