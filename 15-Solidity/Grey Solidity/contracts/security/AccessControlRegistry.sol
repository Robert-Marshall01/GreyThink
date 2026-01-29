// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/structs/EnumerableSet.sol";

/**
 * @title AccessControlRegistry
 * @author Grey Protocol Team
 * @notice Centralized access control registry for the entire protocol
 * @dev Provides granular role-based access control with hierarchical permissions
 * 
 * Features:
 * - Hierarchical role system with inheritance
 * - Time-bounded role assignments
 * - Role delegation with constraints
 * - Role request and approval workflow
 * - Comprehensive audit logging
 * - Emergency role revocation
 * - KYC/AML simulation support
 */
contract AccessControlRegistry is AccessControl, Pausable, ReentrancyGuard {
    using EnumerableSet for EnumerableSet.AddressSet;
    using EnumerableSet for EnumerableSet.Bytes32Set;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    /// @notice Super admin role - can manage all other roles
    bytes32 public constant SUPER_ADMIN_ROLE = keccak256("SUPER_ADMIN_ROLE");
    
    /// @notice Protocol admin role - manages protocol settings
    bytes32 public constant PROTOCOL_ADMIN_ROLE = keccak256("PROTOCOL_ADMIN_ROLE");
    
    /// @notice Operator role - can perform operational tasks
    bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
    
    /// @notice Guardian role - can pause/unpause
    bytes32 public constant GUARDIAN_ROLE = keccak256("GUARDIAN_ROLE");
    
    /// @notice Minter role - can mint tokens
    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    
    /// @notice Burner role - can burn tokens
    bytes32 public constant BURNER_ROLE = keccak256("BURNER_ROLE");
    
    /// @notice Treasury role - can manage treasury
    bytes32 public constant TREASURY_ROLE = keccak256("TREASURY_ROLE");
    
    /// @notice Governance role - can execute governance actions
    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");
    
    /// @notice Upgrader role - can upgrade contracts
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");
    
    /// @notice Pauser role - can pause contracts
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    
    /// @notice Oracle role - can update oracle data
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    
    /// @notice Liquidator role - can perform liquidations
    bytes32 public constant LIQUIDATOR_ROLE = keccak256("LIQUIDATOR_ROLE");
    
    /// @notice Keeper role - can execute keeper functions
    bytes32 public constant KEEPER_ROLE = keccak256("KEEPER_ROLE");
    
    /// @notice Relayer role - can relay transactions
    bytes32 public constant RELAYER_ROLE = keccak256("RELAYER_ROLE");
    
    /// @notice KYC verified role
    bytes32 public constant KYC_VERIFIED_ROLE = keccak256("KYC_VERIFIED_ROLE");
    
    /// @notice Maximum role duration (365 days)
    uint256 public constant MAX_ROLE_DURATION = 365 days;
    
    /// @notice Minimum role duration (1 hour)
    uint256 public constant MIN_ROLE_DURATION = 1 hours;
    
    /// @notice Maximum pending requests per user
    uint256 public constant MAX_PENDING_REQUESTS = 10;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Request status for role requests
     */
    enum RequestStatus {
        Pending,
        Approved,
        Rejected,
        Expired,
        Cancelled
    }

    /**
     * @notice KYC/AML verification level
     */
    enum VerificationLevel {
        None,
        Basic,
        Enhanced,
        Full,
        Institutional
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Role assignment with time bounds
     */
    struct RoleAssignment {
        bytes32 role;
        address account;
        uint256 grantedAt;
        uint256 expiresAt;
        address grantedBy;
        string reason;
        bool isActive;
    }

    /**
     * @notice Role request for approval workflow
     */
    struct RoleRequest {
        uint256 requestId;
        bytes32 role;
        address requester;
        uint256 requestedAt;
        uint256 expiresAt;
        uint256 requestedDuration;
        string reason;
        RequestStatus status;
        address processedBy;
        uint256 processedAt;
        string processReason;
    }

    /**
     * @notice Role delegation
     */
    struct RoleDelegation {
        bytes32 role;
        address delegator;
        address delegatee;
        uint256 delegatedAt;
        uint256 expiresAt;
        bool canSubDelegate;
        bool isActive;
    }

    /**
     * @notice Identity record for KYC/AML
     */
    struct IdentityRecord {
        address account;
        VerificationLevel level;
        bytes32 identityHash;
        uint256 verifiedAt;
        uint256 expiresAt;
        address verifiedBy;
        string countryCode;
        bool isBlacklisted;
        string blacklistReason;
    }

    /**
     * @notice Role configuration
     */
    struct RoleConfig {
        bytes32 role;
        bytes32 adminRole;
        uint256 maxMembers;
        uint256 currentMembers;
        bool requiresKYC;
        VerificationLevel minVerificationLevel;
        bool isActive;
        bool allowDelegation;
        uint256 defaultDuration;
    }

    /**
     * @notice Audit log entry
     */
    struct AuditEntry {
        uint256 entryId;
        bytes32 action;
        address actor;
        address subject;
        bytes32 role;
        uint256 timestamp;
        bytes data;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Role assignments by account and role
    mapping(address => mapping(bytes32 => RoleAssignment)) public roleAssignments;
    
    /// @notice All role assignments for an account
    mapping(address => EnumerableSet.Bytes32Set) private accountRoles;
    
    /// @notice All members of a role
    mapping(bytes32 => EnumerableSet.AddressSet) private roleMembers;
    
    /// @notice Role configurations
    mapping(bytes32 => RoleConfig) public roleConfigs;
    
    /// @notice All registered roles
    EnumerableSet.Bytes32Set private registeredRoles;
    
    /// @notice Role requests
    mapping(uint256 => RoleRequest) public roleRequests;
    
    /// @notice Pending requests by account
    mapping(address => uint256[]) public pendingRequests;
    
    /// @notice Request counter
    uint256 public requestCounter;
    
    /// @notice Role delegations
    mapping(bytes32 => RoleDelegation) public delegations;
    
    /// @notice Delegation key = keccak256(role, delegator, delegatee)
    mapping(address => EnumerableSet.Bytes32Set) private accountDelegations;
    
    /// @notice Identity records
    mapping(address => IdentityRecord) public identityRecords;
    
    /// @notice Audit log
    AuditEntry[] public auditLog;
    
    /// @notice Audit log by account
    mapping(address => uint256[]) public accountAuditLog;
    
    /// @notice Request expiry duration
    uint256 public requestExpiryDuration = 7 days;
    
    /// @notice Whether role requests are enabled
    bool public roleRequestsEnabled = true;
    
    /// @notice Global blacklist
    mapping(address => bool) public globalBlacklist;
    
    /// @notice Contract registry for permissioned contracts
    mapping(address => bool) public registeredContracts;

    // ============================================
    // EVENTS
    // ============================================

    event RoleAssigned(
        bytes32 indexed role,
        address indexed account,
        address indexed grantedBy,
        uint256 expiresAt,
        string reason
    );
    
    event RoleRevoked(
        bytes32 indexed role,
        address indexed account,
        address indexed revokedBy,
        string reason
    );
    
    event RoleExpired(bytes32 indexed role, address indexed account);
    
    event RoleRequested(
        uint256 indexed requestId,
        bytes32 indexed role,
        address indexed requester,
        uint256 duration,
        string reason
    );
    
    event RoleRequestProcessed(
        uint256 indexed requestId,
        bytes32 indexed role,
        address indexed requester,
        RequestStatus status,
        address processedBy
    );
    
    event RoleDelegated(
        bytes32 indexed role,
        address indexed delegator,
        address indexed delegatee,
        uint256 expiresAt
    );
    
    event DelegationRevoked(
        bytes32 indexed role,
        address indexed delegator,
        address indexed delegatee
    );
    
    event IdentityVerified(
        address indexed account,
        VerificationLevel level,
        address indexed verifiedBy
    );
    
    event IdentityRevoked(address indexed account, string reason);
    
    event AccountBlacklisted(address indexed account, string reason);
    
    event AccountWhitelisted(address indexed account);
    
    event RoleConfigured(bytes32 indexed role, uint256 maxMembers, bool requiresKYC);
    
    event ContractRegistered(address indexed contractAddress, bool status);
    
    event AuditLogged(uint256 indexed entryId, bytes32 indexed action, address indexed actor);

    // ============================================
    // ERRORS
    // ============================================

    error AccountBlacklistedError(address account);
    error RoleNotActive(bytes32 role);
    error RoleExpiredError(bytes32 role, address account);
    error MaxMembersReached(bytes32 role);
    error InsufficientVerificationLevel(VerificationLevel required, VerificationLevel actual);
    error KYCRequired(bytes32 role);
    error InvalidDuration(uint256 duration);
    error RequestNotPending(uint256 requestId);
    error RequestExpired(uint256 requestId);
    error TooManyPendingRequests(address account);
    error DelegationNotAllowed(bytes32 role);
    error NotDelegator(address caller, address delegator);
    error DelegationExpired(bytes32 role, address delegator, address delegatee);
    error InvalidRoleConfiguration();
    error RoleRequestsDisabled();
    error NotRegisteredContract(address caller);
    error SelfDelegationNotAllowed();

    // ============================================
    // MODIFIERS
    // ============================================

    /**
     * @notice Ensures account is not blacklisted
     */
    modifier notBlacklisted(address account) {
        if (globalBlacklist[account]) {
            revert AccountBlacklistedError(account);
        }
        _;
    }

    /**
     * @notice Ensures role is active and configured
     */
    modifier roleActive(bytes32 role) {
        if (!roleConfigs[role].isActive) {
            revert RoleNotActive(role);
        }
        _;
    }

    /**
     * @notice Ensures caller is a registered contract
     */
    modifier onlyRegisteredContract() {
        if (!registeredContracts[msg.sender]) {
            revert NotRegisteredContract(msg.sender);
        }
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(SUPER_ADMIN_ROLE, msg.sender);
        _grantRole(GUARDIAN_ROLE, msg.sender);
        
        // Initialize core roles
        _initializeRoles();
    }

    /**
     * @notice Initializes default role configurations
     */
    function _initializeRoles() internal {
        bytes32[15] memory roles = [
            SUPER_ADMIN_ROLE,
            PROTOCOL_ADMIN_ROLE,
            OPERATOR_ROLE,
            GUARDIAN_ROLE,
            MINTER_ROLE,
            BURNER_ROLE,
            TREASURY_ROLE,
            GOVERNANCE_ROLE,
            UPGRADER_ROLE,
            PAUSER_ROLE,
            ORACLE_ROLE,
            LIQUIDATOR_ROLE,
            KEEPER_ROLE,
            RELAYER_ROLE,
            KYC_VERIFIED_ROLE
        ];

        for (uint256 i = 0; i < roles.length; i++) {
            registeredRoles.add(roles[i]);
            roleConfigs[roles[i]] = RoleConfig({
                role: roles[i],
                adminRole: SUPER_ADMIN_ROLE,
                maxMembers: type(uint256).max,
                currentMembers: 0,
                requiresKYC: false,
                minVerificationLevel: VerificationLevel.None,
                isActive: true,
                allowDelegation: true,
                defaultDuration: 180 days
            });
        }

        // Set role hierarchy
        _setRoleAdmin(PROTOCOL_ADMIN_ROLE, SUPER_ADMIN_ROLE);
        _setRoleAdmin(OPERATOR_ROLE, PROTOCOL_ADMIN_ROLE);
        _setRoleAdmin(GUARDIAN_ROLE, SUPER_ADMIN_ROLE);
        _setRoleAdmin(MINTER_ROLE, PROTOCOL_ADMIN_ROLE);
        _setRoleAdmin(BURNER_ROLE, PROTOCOL_ADMIN_ROLE);
        _setRoleAdmin(TREASURY_ROLE, SUPER_ADMIN_ROLE);
        _setRoleAdmin(GOVERNANCE_ROLE, SUPER_ADMIN_ROLE);
        _setRoleAdmin(UPGRADER_ROLE, SUPER_ADMIN_ROLE);
        _setRoleAdmin(PAUSER_ROLE, GUARDIAN_ROLE);
        _setRoleAdmin(ORACLE_ROLE, OPERATOR_ROLE);
        _setRoleAdmin(LIQUIDATOR_ROLE, OPERATOR_ROLE);
        _setRoleAdmin(KEEPER_ROLE, OPERATOR_ROLE);
        _setRoleAdmin(RELAYER_ROLE, OPERATOR_ROLE);
    }

    // ============================================
    // ROLE MANAGEMENT
    // ============================================

    /**
     * @notice Assigns a role to an account with time bounds
     * @param role Role to assign
     * @param account Account to assign role to
     * @param duration Duration of the role assignment
     * @param reason Reason for assignment
     */
    function assignRole(
        bytes32 role,
        address account,
        uint256 duration,
        string calldata reason
    ) 
        external 
        onlyRole(getRoleAdmin(role))
        notBlacklisted(account)
        roleActive(role)
        whenNotPaused
    {
        _assignRole(role, account, duration, reason);
    }

    /**
     * @notice Batch assigns roles
     * @param roles Array of roles
     * @param accounts Array of accounts
     * @param durations Array of durations
     * @param reasons Array of reasons
     */
    function batchAssignRoles(
        bytes32[] calldata roles,
        address[] calldata accounts,
        uint256[] calldata durations,
        string[] calldata reasons
    ) external whenNotPaused {
        require(
            roles.length == accounts.length &&
            roles.length == durations.length &&
            roles.length == reasons.length,
            "Array length mismatch"
        );

        for (uint256 i = 0; i < roles.length; i++) {
            require(hasRole(getRoleAdmin(roles[i]), msg.sender), "Not admin for role");
            if (!globalBlacklist[accounts[i]] && roleConfigs[roles[i]].isActive) {
                _assignRole(roles[i], accounts[i], durations[i], reasons[i]);
            }
        }
    }

    /**
     * @notice Internal role assignment
     */
    function _assignRole(
        bytes32 role,
        address account,
        uint256 duration,
        string memory reason
    ) internal {
        RoleConfig storage config = roleConfigs[role];
        
        // Check duration
        if (duration < MIN_ROLE_DURATION || duration > MAX_ROLE_DURATION) {
            revert InvalidDuration(duration);
        }

        // Check max members
        if (config.currentMembers >= config.maxMembers) {
            revert MaxMembersReached(role);
        }

        // Check KYC requirements
        if (config.requiresKYC) {
            IdentityRecord storage identity = identityRecords[account];
            if (identity.level == VerificationLevel.None || identity.expiresAt < block.timestamp) {
                revert KYCRequired(role);
            }
            if (uint8(identity.level) < uint8(config.minVerificationLevel)) {
                revert InsufficientVerificationLevel(config.minVerificationLevel, identity.level);
            }
        }

        uint256 expiresAt = block.timestamp + duration;

        // Create assignment
        roleAssignments[account][role] = RoleAssignment({
            role: role,
            account: account,
            grantedAt: block.timestamp,
            expiresAt: expiresAt,
            grantedBy: msg.sender,
            reason: reason,
            isActive: true
        });

        // Update tracking
        if (!accountRoles[account].contains(role)) {
            accountRoles[account].add(role);
            roleMembers[role].add(account);
            config.currentMembers++;
        }

        // Grant underlying role
        if (!hasRole(role, account)) {
            _grantRole(role, account);
        }

        // Log audit
        _logAudit(keccak256("ROLE_ASSIGNED"), account, role, "");

        emit RoleAssigned(role, account, msg.sender, expiresAt, reason);
    }

    /**
     * @notice Revokes a role from an account
     * @param role Role to revoke
     * @param account Account to revoke role from
     * @param reason Reason for revocation
     */
    function revokeRoleWithReason(
        bytes32 role,
        address account,
        string calldata reason
    ) external onlyRole(getRoleAdmin(role)) {
        _revokeRoleInternal(role, account, reason);
    }

    /**
     * @notice Emergency revocation by guardian
     * @param role Role to revoke
     * @param account Account to revoke from
     */
    function emergencyRevoke(
        bytes32 role,
        address account
    ) external onlyRole(GUARDIAN_ROLE) {
        _revokeRoleInternal(role, account, "Emergency revocation");
    }

    /**
     * @notice Internal role revocation
     */
    function _revokeRoleInternal(
        bytes32 role,
        address account,
        string memory reason
    ) internal {
        RoleAssignment storage assignment = roleAssignments[account][role];
        assignment.isActive = false;

        // Update tracking
        if (accountRoles[account].contains(role)) {
            accountRoles[account].remove(role);
            roleMembers[role].remove(account);
            roleConfigs[role].currentMembers--;
        }

        // Revoke underlying role
        if (hasRole(role, account)) {
            _revokeRole(role, account);
        }

        // Revoke any delegations
        _revokeDelegationsForAccount(role, account);

        // Log audit
        _logAudit(keccak256("ROLE_REVOKED"), account, role, bytes(reason));

        emit RoleRevoked(role, account, msg.sender, reason);
    }

    /**
     * @notice Checks and handles expired roles
     * @param account Account to check
     */
    function checkAndRevokeExpiredRoles(address account) external {
        bytes32[] memory roles = getAccountRoles(account);
        
        for (uint256 i = 0; i < roles.length; i++) {
            RoleAssignment storage assignment = roleAssignments[account][roles[i]];
            if (assignment.isActive && assignment.expiresAt < block.timestamp) {
                _revokeRoleInternal(roles[i], account, "Role expired");
                emit RoleExpired(roles[i], account);
            }
        }
    }

    /**
     * @notice Renews a role assignment
     * @param role Role to renew
     * @param account Account to renew for
     * @param additionalDuration Additional duration to add
     */
    function renewRole(
        bytes32 role,
        address account,
        uint256 additionalDuration
    ) 
        external 
        onlyRole(getRoleAdmin(role))
        notBlacklisted(account)
    {
        RoleAssignment storage assignment = roleAssignments[account][role];
        require(assignment.isActive, "Role not active");
        
        uint256 newExpiry = assignment.expiresAt + additionalDuration;
        if (newExpiry > block.timestamp + MAX_ROLE_DURATION) {
            newExpiry = block.timestamp + MAX_ROLE_DURATION;
        }
        
        assignment.expiresAt = newExpiry;
        
        _logAudit(keccak256("ROLE_RENEWED"), account, role, abi.encode(newExpiry));
    }

    // ============================================
    // ROLE REQUESTS
    // ============================================

    /**
     * @notice Requests a role assignment
     * @param role Role to request
     * @param duration Requested duration
     * @param reason Reason for request
     */
    function requestRole(
        bytes32 role,
        uint256 duration,
        string calldata reason
    ) 
        external 
        notBlacklisted(msg.sender)
        roleActive(role)
        whenNotPaused
        returns (uint256 requestId)
    {
        if (!roleRequestsEnabled) {
            revert RoleRequestsDisabled();
        }

        if (pendingRequests[msg.sender].length >= MAX_PENDING_REQUESTS) {
            revert TooManyPendingRequests(msg.sender);
        }

        if (duration < MIN_ROLE_DURATION || duration > MAX_ROLE_DURATION) {
            revert InvalidDuration(duration);
        }

        requestId = ++requestCounter;

        roleRequests[requestId] = RoleRequest({
            requestId: requestId,
            role: role,
            requester: msg.sender,
            requestedAt: block.timestamp,
            expiresAt: block.timestamp + requestExpiryDuration,
            requestedDuration: duration,
            reason: reason,
            status: RequestStatus.Pending,
            processedBy: address(0),
            processedAt: 0,
            processReason: ""
        });

        pendingRequests[msg.sender].push(requestId);

        emit RoleRequested(requestId, role, msg.sender, duration, reason);
    }

    /**
     * @notice Approves a role request
     * @param requestId Request ID to approve
     * @param grantDuration Actual duration to grant (can differ from requested)
     * @param reason Reason for approval
     */
    function approveRoleRequest(
        uint256 requestId,
        uint256 grantDuration,
        string calldata reason
    ) external whenNotPaused {
        RoleRequest storage request = roleRequests[requestId];
        
        if (request.status != RequestStatus.Pending) {
            revert RequestNotPending(requestId);
        }
        
        if (request.expiresAt < block.timestamp) {
            request.status = RequestStatus.Expired;
            revert RequestExpired(requestId);
        }

        require(hasRole(getRoleAdmin(request.role), msg.sender), "Not admin for role");

        // Assign the role
        _assignRole(request.role, request.requester, grantDuration, request.reason);

        // Update request
        request.status = RequestStatus.Approved;
        request.processedBy = msg.sender;
        request.processedAt = block.timestamp;
        request.processReason = reason;

        // Remove from pending
        _removePendingRequest(request.requester, requestId);

        emit RoleRequestProcessed(requestId, request.role, request.requester, RequestStatus.Approved, msg.sender);
    }

    /**
     * @notice Rejects a role request
     * @param requestId Request ID to reject
     * @param reason Reason for rejection
     */
    function rejectRoleRequest(
        uint256 requestId,
        string calldata reason
    ) external {
        RoleRequest storage request = roleRequests[requestId];
        
        if (request.status != RequestStatus.Pending) {
            revert RequestNotPending(requestId);
        }

        require(hasRole(getRoleAdmin(request.role), msg.sender), "Not admin for role");

        request.status = RequestStatus.Rejected;
        request.processedBy = msg.sender;
        request.processedAt = block.timestamp;
        request.processReason = reason;

        _removePendingRequest(request.requester, requestId);

        emit RoleRequestProcessed(requestId, request.role, request.requester, RequestStatus.Rejected, msg.sender);
    }

    /**
     * @notice Cancels own role request
     * @param requestId Request ID to cancel
     */
    function cancelRoleRequest(uint256 requestId) external {
        RoleRequest storage request = roleRequests[requestId];
        
        require(request.requester == msg.sender, "Not requester");
        if (request.status != RequestStatus.Pending) {
            revert RequestNotPending(requestId);
        }

        request.status = RequestStatus.Cancelled;
        request.processedAt = block.timestamp;

        _removePendingRequest(msg.sender, requestId);

        emit RoleRequestProcessed(requestId, request.role, msg.sender, RequestStatus.Cancelled, msg.sender);
    }

    /**
     * @notice Removes request from pending array
     */
    function _removePendingRequest(address account, uint256 requestId) internal {
        uint256[] storage pending = pendingRequests[account];
        for (uint256 i = 0; i < pending.length; i++) {
            if (pending[i] == requestId) {
                pending[i] = pending[pending.length - 1];
                pending.pop();
                break;
            }
        }
    }

    // ============================================
    // DELEGATION
    // ============================================

    /**
     * @notice Delegates a role to another account
     * @param role Role to delegate
     * @param delegatee Account to delegate to
     * @param duration Duration of delegation
     * @param canSubDelegate Whether delegatee can further delegate
     */
    function delegateRole(
        bytes32 role,
        address delegatee,
        uint256 duration,
        bool canSubDelegate
    ) 
        external 
        notBlacklisted(delegatee)
        whenNotPaused
    {
        if (delegatee == msg.sender) {
            revert SelfDelegationNotAllowed();
        }

        RoleConfig storage config = roleConfigs[role];
        if (!config.allowDelegation) {
            revert DelegationNotAllowed(role);
        }

        require(hasRole(role, msg.sender), "Caller doesn't have role");

        if (duration < MIN_ROLE_DURATION || duration > MAX_ROLE_DURATION) {
            revert InvalidDuration(duration);
        }

        bytes32 delegationKey = keccak256(abi.encodePacked(role, msg.sender, delegatee));
        
        delegations[delegationKey] = RoleDelegation({
            role: role,
            delegator: msg.sender,
            delegatee: delegatee,
            delegatedAt: block.timestamp,
            expiresAt: block.timestamp + duration,
            canSubDelegate: canSubDelegate,
            isActive: true
        });

        accountDelegations[delegatee].add(delegationKey);

        // Grant role to delegatee
        if (!hasRole(role, delegatee)) {
            _grantRole(role, delegatee);
            roleMembers[role].add(delegatee);
        }

        _logAudit(keccak256("ROLE_DELEGATED"), delegatee, role, abi.encodePacked(msg.sender));

        emit RoleDelegated(role, msg.sender, delegatee, block.timestamp + duration);
    }

    /**
     * @notice Revokes a delegation
     * @param role Role to revoke delegation for
     * @param delegatee Account to revoke from
     */
    function revokeDelegation(bytes32 role, address delegatee) external {
        bytes32 delegationKey = keccak256(abi.encodePacked(role, msg.sender, delegatee));
        RoleDelegation storage delegation = delegations[delegationKey];

        if (delegation.delegator != msg.sender) {
            revert NotDelegator(msg.sender, delegation.delegator);
        }

        _revokeDelegation(delegationKey, delegation);
    }

    /**
     * @notice Internal delegation revocation
     */
    function _revokeDelegation(bytes32 delegationKey, RoleDelegation storage delegation) internal {
        delegation.isActive = false;

        accountDelegations[delegation.delegatee].remove(delegationKey);

        // Check if delegatee still has role from other sources
        if (!_hasRoleFromOtherSource(delegation.role, delegation.delegatee)) {
            _revokeRole(delegation.role, delegation.delegatee);
            roleMembers[delegation.role].remove(delegation.delegatee);
        }

        emit DelegationRevoked(delegation.role, delegation.delegator, delegation.delegatee);
    }

    /**
     * @notice Revokes all delegations for an account's role
     */
    function _revokeDelegationsForAccount(bytes32 role, address account) internal {
        // This is a simplified version - in production would need more tracking
    }

    /**
     * @notice Checks if account has role from another source
     */
    function _hasRoleFromOtherSource(bytes32 role, address account) internal view returns (bool) {
        // Check direct assignment
        if (roleAssignments[account][role].isActive && 
            roleAssignments[account][role].expiresAt > block.timestamp) {
            return true;
        }

        // Check other delegations
        bytes32[] memory delegationKeys = accountDelegations[account].values();
        for (uint256 i = 0; i < delegationKeys.length; i++) {
            RoleDelegation storage d = delegations[delegationKeys[i]];
            if (d.role == role && d.isActive && d.expiresAt > block.timestamp) {
                return true;
            }
        }

        return false;
    }

    // ============================================
    // IDENTITY / KYC
    // ============================================

    /**
     * @notice Verifies identity for KYC purposes
     * @param account Account to verify
     * @param level Verification level
     * @param identityHash Hash of identity documents
     * @param countryCode Country code
     * @param validityDuration How long verification is valid
     */
    function verifyIdentity(
        address account,
        VerificationLevel level,
        bytes32 identityHash,
        string calldata countryCode,
        uint256 validityDuration
    ) external onlyRole(OPERATOR_ROLE) notBlacklisted(account) {
        require(level != VerificationLevel.None, "Invalid level");
        require(validityDuration > 0 && validityDuration <= 365 days, "Invalid validity");

        identityRecords[account] = IdentityRecord({
            account: account,
            level: level,
            identityHash: identityHash,
            verifiedAt: block.timestamp,
            expiresAt: block.timestamp + validityDuration,
            verifiedBy: msg.sender,
            countryCode: countryCode,
            isBlacklisted: false,
            blacklistReason: ""
        });

        // Grant KYC role
        if (!hasRole(KYC_VERIFIED_ROLE, account)) {
            _grantRole(KYC_VERIFIED_ROLE, account);
        }

        _logAudit(keccak256("IDENTITY_VERIFIED"), account, bytes32(0), abi.encode(level));

        emit IdentityVerified(account, level, msg.sender);
    }

    /**
     * @notice Revokes identity verification
     * @param account Account to revoke
     * @param reason Reason for revocation
     */
    function revokeIdentity(
        address account,
        string calldata reason
    ) external onlyRole(OPERATOR_ROLE) {
        IdentityRecord storage record = identityRecords[account];
        record.level = VerificationLevel.None;
        record.expiresAt = block.timestamp;

        if (hasRole(KYC_VERIFIED_ROLE, account)) {
            _revokeRole(KYC_VERIFIED_ROLE, account);
        }

        _logAudit(keccak256("IDENTITY_REVOKED"), account, bytes32(0), bytes(reason));

        emit IdentityRevoked(account, reason);
    }

    /**
     * @notice Blacklists an account
     * @param account Account to blacklist
     * @param reason Reason for blacklisting
     */
    function blacklistAccount(
        address account,
        string calldata reason
    ) external onlyRole(GUARDIAN_ROLE) {
        globalBlacklist[account] = true;
        identityRecords[account].isBlacklisted = true;
        identityRecords[account].blacklistReason = reason;

        // Revoke all roles
        bytes32[] memory roles = getAccountRoles(account);
        for (uint256 i = 0; i < roles.length; i++) {
            _revokeRoleInternal(roles[i], account, "Account blacklisted");
        }

        _logAudit(keccak256("ACCOUNT_BLACKLISTED"), account, bytes32(0), bytes(reason));

        emit AccountBlacklisted(account, reason);
    }

    /**
     * @notice Removes account from blacklist
     * @param account Account to whitelist
     */
    function whitelistAccount(address account) external onlyRole(SUPER_ADMIN_ROLE) {
        globalBlacklist[account] = false;
        identityRecords[account].isBlacklisted = false;
        identityRecords[account].blacklistReason = "";

        _logAudit(keccak256("ACCOUNT_WHITELISTED"), account, bytes32(0), "");

        emit AccountWhitelisted(account);
    }

    // ============================================
    // ROLE CONFIGURATION
    // ============================================

    /**
     * @notice Configures a role
     * @param role Role to configure
     * @param maxMembers Maximum members allowed
     * @param requiresKYC Whether KYC is required
     * @param minLevel Minimum verification level
     * @param allowDelegation Whether delegation is allowed
     * @param defaultDuration Default role duration
     */
    function configureRole(
        bytes32 role,
        uint256 maxMembers,
        bool requiresKYC,
        VerificationLevel minLevel,
        bool allowDelegation,
        uint256 defaultDuration
    ) external onlyRole(SUPER_ADMIN_ROLE) {
        if (defaultDuration < MIN_ROLE_DURATION || defaultDuration > MAX_ROLE_DURATION) {
            revert InvalidDuration(defaultDuration);
        }

        RoleConfig storage config = roleConfigs[role];
        config.maxMembers = maxMembers;
        config.requiresKYC = requiresKYC;
        config.minVerificationLevel = minLevel;
        config.allowDelegation = allowDelegation;
        config.defaultDuration = defaultDuration;

        if (!registeredRoles.contains(role)) {
            registeredRoles.add(role);
            config.role = role;
            config.adminRole = SUPER_ADMIN_ROLE;
            config.isActive = true;
        }

        emit RoleConfigured(role, maxMembers, requiresKYC);
    }

    /**
     * @notice Activates or deactivates a role
     * @param role Role to update
     * @param isActive New active status
     */
    function setRoleActive(bytes32 role, bool isActive) external onlyRole(SUPER_ADMIN_ROLE) {
        roleConfigs[role].isActive = isActive;
    }

    /**
     * @notice Registers a contract for permissioned access
     * @param contractAddress Contract to register
     * @param status Registration status
     */
    function registerContract(address contractAddress, bool status) external onlyRole(PROTOCOL_ADMIN_ROLE) {
        registeredContracts[contractAddress] = status;
        emit ContractRegistered(contractAddress, status);
    }

    // ============================================
    // AUDIT LOGGING
    // ============================================

    /**
     * @notice Logs an audit entry
     */
    function _logAudit(
        bytes32 action,
        address subject,
        bytes32 role,
        bytes memory data
    ) internal {
        uint256 entryId = auditLog.length;
        
        auditLog.push(AuditEntry({
            entryId: entryId,
            action: action,
            actor: msg.sender,
            subject: subject,
            role: role,
            timestamp: block.timestamp,
            data: data
        }));

        accountAuditLog[subject].push(entryId);

        emit AuditLogged(entryId, action, msg.sender);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Returns all roles for an account
     * @param account Account to query
     * @return Array of role identifiers
     */
    function getAccountRoles(address account) public view returns (bytes32[] memory) {
        return accountRoles[account].values();
    }

    /**
     * @notice Returns all members of a role
     * @param role Role to query
     * @return Array of member addresses
     */
    function getRoleMembers(bytes32 role) external view returns (address[] memory) {
        return roleMembers[role].values();
    }

    /**
     * @notice Returns the number of members for a role
     * @param role Role to query
     * @return Number of members
     */
    function getRoleMemberCount(bytes32 role) external view returns (uint256) {
        return roleMembers[role].length();
    }

    /**
     * @notice Returns all registered roles
     * @return Array of role identifiers
     */
    function getRegisteredRoles() external view returns (bytes32[] memory) {
        return registeredRoles.values();
    }

    /**
     * @notice Checks if role assignment is valid (not expired)
     * @param role Role to check
     * @param account Account to check
     * @return Whether assignment is valid
     */
    function isRoleAssignmentValid(bytes32 role, address account) external view returns (bool) {
        RoleAssignment storage assignment = roleAssignments[account][role];
        return assignment.isActive && assignment.expiresAt > block.timestamp;
    }

    /**
     * @notice Gets identity verification level
     * @param account Account to query
     * @return Current verification level
     */
    function getVerificationLevel(address account) external view returns (VerificationLevel) {
        IdentityRecord storage record = identityRecords[account];
        if (record.expiresAt < block.timestamp || record.isBlacklisted) {
            return VerificationLevel.None;
        }
        return record.level;
    }

    /**
     * @notice Gets pending requests for an account
     * @param account Account to query
     * @return Array of request IDs
     */
    function getPendingRequests(address account) external view returns (uint256[] memory) {
        return pendingRequests[account];
    }

    /**
     * @notice Gets audit log entries for an account
     * @param account Account to query
     * @param offset Starting index
     * @param limit Maximum entries to return
     * @return entries Array of audit entries
     */
    function getAccountAuditLog(
        address account,
        uint256 offset,
        uint256 limit
    ) external view returns (AuditEntry[] memory entries) {
        uint256[] storage indices = accountAuditLog[account];
        
        if (offset >= indices.length) {
            return new AuditEntry[](0);
        }

        uint256 end = offset + limit;
        if (end > indices.length) {
            end = indices.length;
        }

        entries = new AuditEntry[](end - offset);
        for (uint256 i = offset; i < end; i++) {
            entries[i - offset] = auditLog[indices[i]];
        }
    }

    /**
     * @notice Gets total audit log size
     * @return Total number of entries
     */
    function getAuditLogSize() external view returns (uint256) {
        return auditLog.length;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    /**
     * @notice Enables or disables role requests
     * @param enabled New status
     */
    function setRoleRequestsEnabled(bool enabled) external onlyRole(SUPER_ADMIN_ROLE) {
        roleRequestsEnabled = enabled;
    }

    /**
     * @notice Updates request expiry duration
     * @param duration New duration
     */
    function setRequestExpiryDuration(uint256 duration) external onlyRole(SUPER_ADMIN_ROLE) {
        require(duration >= 1 days && duration <= 30 days, "Invalid duration");
        requestExpiryDuration = duration;
    }

    /**
     * @notice Pauses the registry
     */
    function pause() external onlyRole(GUARDIAN_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses the registry
     */
    function unpause() external onlyRole(GUARDIAN_ROLE) {
        _unpause();
    }
}
