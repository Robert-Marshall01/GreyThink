// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title AccessControlManager
 * @author Grey Solidity Project
 * @notice Manages role-based access control for the system
 * @dev Implements a simple role-based permission system with ADMIN and USER roles
 */
contract AccessControlManager {
    /// @notice Role identifier for administrators
    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
    
    /// @notice Role identifier for regular users
    bytes32 public constant USER_ROLE = keccak256("USER_ROLE");

    /// @notice Maps role => account => hasRole
    mapping(bytes32 => mapping(address => bool)) private _roles;

    /// @notice Maps role => admin role that can grant/revoke it
    mapping(bytes32 => bytes32) private _roleAdmin;

    /// @notice Emitted when a role is granted to an account
    /// @param role The role that was granted
    /// @param account The account that received the role
    /// @param sender The account that granted the role
    event RoleGranted(bytes32 indexed role, address indexed account, address indexed sender);

    /// @notice Emitted when a role is revoked from an account
    /// @param role The role that was revoked
    /// @param account The account that lost the role
    /// @param sender The account that revoked the role
    event RoleRevoked(bytes32 indexed role, address indexed account, address indexed sender);

    /// @notice Error thrown when an account lacks a required role
    /// @param account The account that lacks the role
    /// @param role The role that was required
    error AccessControlUnauthorized(address account, bytes32 role);

    /// @notice Restricts function access to accounts with the specified role
    /// @param role The required role
    modifier onlyRole(bytes32 role) {
        _checkRole(role, msg.sender);
        _;
    }

    /**
     * @notice Sets up the contract with the deployer as the initial admin
     * @dev Grants ADMIN_ROLE to the deployer and sets ADMIN_ROLE as the admin of all roles
     */
    constructor() {
        _grantRole(ADMIN_ROLE, msg.sender);
        _setRoleAdmin(ADMIN_ROLE, ADMIN_ROLE);
        _setRoleAdmin(USER_ROLE, ADMIN_ROLE);
    }

    /**
     * @notice Checks if an account has a specific role
     * @param role The role to check
     * @param account The account to check
     * @return bool True if the account has the role
     */
    function hasRole(bytes32 role, address account) public view returns (bool) {
        return _roles[role][account];
    }

    /**
     * @notice Returns the admin role that controls a given role
     * @param role The role to query
     * @return bytes32 The admin role
     */
    function getRoleAdmin(bytes32 role) public view returns (bytes32) {
        return _roleAdmin[role];
    }

    /**
     * @notice Grants a role to an account
     * @dev Caller must have the admin role for the target role
     * @param role The role to grant
     * @param account The account to receive the role
     */
    function grantRole(bytes32 role, address account) external onlyRole(getRoleAdmin(role)) {
        _grantRole(role, account);
    }

    /**
     * @notice Revokes a role from an account
     * @dev Caller must have the admin role for the target role
     * @param role The role to revoke
     * @param account The account to lose the role
     */
    function revokeRole(bytes32 role, address account) external onlyRole(getRoleAdmin(role)) {
        _revokeRole(role, account);
    }

    /**
     * @notice Allows an account to renounce a role they hold
     * @dev Can only renounce roles for self
     * @param role The role to renounce
     */
    function renounceRole(bytes32 role) external {
        _revokeRole(role, msg.sender);
    }

    /**
     * @notice Internal function to check if an account has a role
     * @param role The role to check
     * @param account The account to check
     */
    function _checkRole(bytes32 role, address account) internal view {
        if (!hasRole(role, account)) {
            revert AccessControlUnauthorized(account, role);
        }
    }

    /**
     * @notice Internal function to grant a role
     * @param role The role to grant
     * @param account The account to receive the role
     */
    function _grantRole(bytes32 role, address account) internal {
        if (!hasRole(role, account)) {
            _roles[role][account] = true;
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
            _roles[role][account] = false;
            emit RoleRevoked(role, account, msg.sender);
        }
    }

    /**
     * @notice Internal function to set the admin role for a given role
     * @param role The role to configure
     * @param adminRole The admin role
     */
    function _setRoleAdmin(bytes32 role, bytes32 adminRole) internal {
        _roleAdmin[role] = adminRole;
    }
}
