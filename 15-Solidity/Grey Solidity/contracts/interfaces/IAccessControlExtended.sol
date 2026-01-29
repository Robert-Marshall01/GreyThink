// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IAccessControlExtended
 * @notice Extended access control interface with roles and identity
 * @dev Supports ADMIN, OPERATOR, USER roles with identity metadata
 */
interface IAccessControlExtended {
    /// @notice Role identifier for administrators
    function ADMIN_ROLE() external view returns (bytes32);

    /// @notice Role identifier for operators
    function OPERATOR_ROLE() external view returns (bytes32);

    /// @notice Role identifier for regular users
    function USER_ROLE() external view returns (bytes32);

    /// @notice The default admin role
    function DEFAULT_ADMIN_ROLE() external view returns (bytes32);

    /**
     * @notice Identity metadata structure
     */
    struct Identity {
        bytes32 identityHash;
        string metadataUri;
        uint256 registeredAt;
        uint256 updatedAt;
        bool verified;
        bool active;
    }

    /**
     * @notice Checks if an account has a specific role
     * @param role The role to check
     * @param account The account to check
     * @return True if the account has the role
     */
    function hasRole(bytes32 role, address account) external view returns (bool);

    /**
     * @notice Returns the admin role for a given role
     * @param role The role to query
     * @return The admin role
     */
    function getRoleAdmin(bytes32 role) external view returns (bytes32);

    /**
     * @notice Grants a role to an account
     * @param role The role to grant
     * @param account The account to receive the role
     */
    function grantRole(bytes32 role, address account) external;

    /**
     * @notice Revokes a role from an account
     * @param role The role to revoke
     * @param account The account to lose the role
     */
    function revokeRole(bytes32 role, address account) external;

    /**
     * @notice Renounces a role
     * @param role The role to renounce
     */
    function renounceRole(bytes32 role) external;

    /**
     * @notice Registers an identity for an account
     * @param account The account to register
     * @param identityHash The identity hash
     * @param metadataUri The metadata URI
     */
    function registerIdentity(
        address account,
        bytes32 identityHash,
        string calldata metadataUri
    ) external;

    /**
     * @notice Updates identity metadata
     * @param account The account to update
     * @param metadataUri The new metadata URI
     */
    function updateIdentityMetadata(address account, string calldata metadataUri) external;

    /**
     * @notice Verifies an identity
     * @param account The account to verify
     */
    function verifyIdentity(address account) external;

    /**
     * @notice Revokes identity verification
     * @param account The account to revoke
     */
    function revokeIdentityVerification(address account) external;

    /**
     * @notice Deactivates an identity
     * @param account The account to deactivate
     */
    function deactivateIdentity(address account) external;

    /**
     * @notice Reactivates an identity
     * @param account The account to reactivate
     */
    function reactivateIdentity(address account) external;

    /**
     * @notice Gets identity info for an account
     * @param account The account to query
     * @return The identity struct
     */
    function getIdentity(address account) external view returns (Identity memory);

    /**
     * @notice Checks if an account has a registered identity
     * @param account The account to check
     * @return True if registered
     */
    function hasIdentity(address account) external view returns (bool);

    /**
     * @notice Checks if an account's identity is verified
     * @param account The account to check
     * @return True if verified
     */
    function isVerified(address account) external view returns (bool);

    /**
     * @notice Returns the number of members for a role
     * @param role The role to query
     * @return The member count
     */
    function getRoleMemberCount(bytes32 role) external view returns (uint256);

    /**
     * @notice Returns a role member at an index
     * @param role The role to query
     * @param index The index
     * @return The member address
     */
    function getRoleMember(bytes32 role, uint256 index) external view returns (address);

    // Events
    event RoleGranted(bytes32 indexed role, address indexed account, address indexed sender);
    event RoleRevoked(bytes32 indexed role, address indexed account, address indexed sender);
    event RoleAdminChanged(bytes32 indexed role, bytes32 indexed previousAdminRole, bytes32 indexed newAdminRole);
    event IdentityRegistered(address indexed account, bytes32 identityHash, string metadataUri);
    event IdentityUpdated(address indexed account, string oldMetadataUri, string newMetadataUri);
    event IdentityVerified(address indexed account, address indexed verifier);
    event IdentityVerificationRevoked(address indexed account, address indexed revoker);
    event IdentityDeactivated(address indexed account);
    event IdentityReactivated(address indexed account);
}
