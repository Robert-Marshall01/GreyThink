// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/ReentrancyGuardUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts/token/ERC1155/IERC1155.sol";

/**
 * @title EmergencyWithdraw
 * @author Grey Protocol Team
 * @notice Emergency rescue system for recovering stuck funds from protocol contracts
 * @dev Implements timelocked recovery mechanisms with multi-sig requirements
 * 
 * Features:
 * - Timelock-protected withdrawals
 * - Multi-signature approval for large amounts
 * - Support for ETH, ERC20, ERC721, ERC1155
 * - Whitelisted destination addresses
 * - Emergency bypass with high threshold
 * - Audit trail for all operations
 * - Configurable cooldown periods
 */
contract EmergencyWithdraw is
    Initializable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    ReentrancyGuardUpgradeable,
    UUPSUpgradeable
{
    using SafeERC20 for IERC20;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant RESCUER_ROLE = keccak256("RESCUER_ROLE");
    bytes32 public constant APPROVER_ROLE = keccak256("APPROVER_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");
    bytes32 public constant EMERGENCY_ROLE = keccak256("EMERGENCY_ROLE");

    /// @notice Minimum timelock delay
    uint256 public constant MIN_DELAY = 12 hours;

    /// @notice Maximum timelock delay
    uint256 public constant MAX_DELAY = 30 days;

    /// @notice Grace period after timelock expires
    uint256 public constant GRACE_PERIOD = 7 days;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Asset types supported
     */
    enum AssetType {
        NATIVE,
        ERC20,
        ERC721,
        ERC1155
    }

    /**
     * @notice Request status
     */
    enum RequestStatus {
        PENDING,
        APPROVED,
        EXECUTED,
        CANCELLED,
        EXPIRED
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Withdrawal request
     * @param id Request ID
     * @param requester Who initiated the request
     * @param targetContract Contract to withdraw from
     * @param recipient Funds destination
     * @param assetType Type of asset
     * @param asset Asset address (zero for native)
     * @param amount Amount or token ID
     * @param data Additional data (for ERC1155)
     * @param status Current status
     * @param createdAt Creation timestamp
     * @param executeAfter Earliest execution time
     * @param expiresAt Expiration timestamp
     * @param approvalCount Number of approvals
     * @param reason Reason for withdrawal
     */
    struct WithdrawalRequest {
        uint256 id;
        address requester;
        address targetContract;
        address recipient;
        AssetType assetType;
        address asset;
        uint256 amount;
        bytes data;
        RequestStatus status;
        uint256 createdAt;
        uint256 executeAfter;
        uint256 expiresAt;
        uint256 approvalCount;
        string reason;
    }

    /**
     * @notice Configuration for protected contracts
     * @param isProtected Whether contract is registered
     * @param timelockDelay Delay before execution
     * @param requiredApprovals Required approvals for high-value
     * @param highValueThreshold Threshold for multi-approval
     * @param cooldownPeriod Cooldown between withdrawals
     * @param lastWithdrawal Last withdrawal timestamp
     * @param totalWithdrawn Total value withdrawn
     */
    struct ContractConfig {
        bool isProtected;
        uint256 timelockDelay;
        uint256 requiredApprovals;
        uint256 highValueThreshold;
        uint256 cooldownPeriod;
        uint256 lastWithdrawal;
        uint256 totalWithdrawn;
    }

    /**
     * @notice Audit log entry
     */
    struct AuditEntry {
        uint256 timestamp;
        address actor;
        bytes32 action;
        uint256 requestId;
        bytes data;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice All withdrawal requests
    mapping(uint256 => WithdrawalRequest) public requests;

    /// @notice Request count / next ID
    uint256 public requestCount;

    /// @notice Contract configurations
    mapping(address => ContractConfig) public contractConfigs;

    /// @notice Approvals: requestId => approver => approved
    mapping(uint256 => mapping(address => bool)) public approvals;

    /// @notice Whitelisted recipients
    mapping(address => bool) public whitelistedRecipients;

    /// @notice Audit log
    AuditEntry[] public auditLog;

    /// @notice Global cooldown period
    uint256 public globalCooldown;

    /// @notice Last global withdrawal
    uint256 public lastGlobalWithdrawal;

    /// @notice Default timelock delay
    uint256 public defaultDelay;

    /// @notice Default approval requirement
    uint256 public defaultApprovals;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event WithdrawalRequested(
        uint256 indexed requestId,
        address indexed targetContract,
        address indexed requester,
        AssetType assetType,
        address asset,
        uint256 amount
    );

    event WithdrawalApproved(uint256 indexed requestId, address indexed approver, uint256 approvalCount);
    event WithdrawalExecuted(uint256 indexed requestId, address indexed recipient, uint256 amount);
    event WithdrawalCancelled(uint256 indexed requestId, address indexed canceller);
    event ContractRegistered(address indexed targetContract, uint256 timelockDelay);
    event ContractRemoved(address indexed targetContract);
    event RecipientWhitelisted(address indexed recipient, bool status);
    event EmergencyWithdrawalExecuted(uint256 indexed requestId, address indexed executor);

    // ============================================
    // ERRORS
    // ============================================

    error RequestNotFound(uint256 requestId);
    error RequestNotPending(uint256 requestId, RequestStatus status);
    error RequestExpired(uint256 requestId);
    error TimelockNotMet(uint256 executeAfter);
    error InsufficientApprovals(uint256 current, uint256 required);
    error AlreadyApproved(address approver);
    error RecipientNotWhitelisted(address recipient);
    error ContractNotRegistered(address targetContract);
    error CooldownActive(uint256 until);
    error InvalidDelay(uint256 delay);
    error InvalidAmount();
    error ZeroAddress();
    error TransferFailed();

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the emergency withdraw system
     * @param admin Admin address
     */
    function initialize(address admin) public initializer {
        if (admin == address(0)) revert ZeroAddress();

        __AccessControl_init();
        __Pausable_init();
        __ReentrancyGuard_init();
        __UUPSUpgradeable_init();

        defaultDelay = 24 hours;
        defaultApprovals = 2;
        globalCooldown = 1 hours;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(RESCUER_ROLE, admin);
        _grantRole(APPROVER_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
        _grantRole(EMERGENCY_ROLE, admin);

        // Whitelist admin as default recipient
        whitelistedRecipients[admin] = true;
    }

    // ============================================
    // REQUEST MANAGEMENT
    // ============================================

    /**
     * @notice Creates a withdrawal request for native currency
     */
    function requestNativeWithdrawal(
        address targetContract,
        address recipient,
        uint256 amount,
        string calldata reason
    ) external onlyRole(RESCUER_ROLE) returns (uint256 requestId) {
        return _createRequest(
            targetContract,
            recipient,
            AssetType.NATIVE,
            address(0),
            amount,
            "",
            reason
        );
    }

    /**
     * @notice Creates a withdrawal request for ERC20 tokens
     */
    function requestERC20Withdrawal(
        address targetContract,
        address recipient,
        address token,
        uint256 amount,
        string calldata reason
    ) external onlyRole(RESCUER_ROLE) returns (uint256 requestId) {
        return _createRequest(
            targetContract,
            recipient,
            AssetType.ERC20,
            token,
            amount,
            "",
            reason
        );
    }

    /**
     * @notice Creates a withdrawal request for ERC721 tokens
     */
    function requestERC721Withdrawal(
        address targetContract,
        address recipient,
        address token,
        uint256 tokenId,
        string calldata reason
    ) external onlyRole(RESCUER_ROLE) returns (uint256 requestId) {
        return _createRequest(
            targetContract,
            recipient,
            AssetType.ERC721,
            token,
            tokenId,
            "",
            reason
        );
    }

    /**
     * @notice Creates a withdrawal request for ERC1155 tokens
     */
    function requestERC1155Withdrawal(
        address targetContract,
        address recipient,
        address token,
        uint256 tokenId,
        uint256 amount,
        string calldata reason
    ) external onlyRole(RESCUER_ROLE) returns (uint256 requestId) {
        return _createRequest(
            targetContract,
            recipient,
            AssetType.ERC1155,
            token,
            tokenId,
            abi.encode(amount),
            reason
        );
    }

    /**
     * @notice Internal request creation
     */
    function _createRequest(
        address targetContract,
        address recipient,
        AssetType assetType,
        address asset,
        uint256 amountOrId,
        bytes memory data,
        string calldata reason
    ) internal returns (uint256 requestId) {
        if (!whitelistedRecipients[recipient]) revert RecipientNotWhitelisted(recipient);
        if (amountOrId == 0 && assetType != AssetType.ERC721) revert InvalidAmount();

        ContractConfig storage config = contractConfigs[targetContract];
        uint256 delay = config.isProtected ? config.timelockDelay : defaultDelay;

        requestId = ++requestCount;

        requests[requestId] = WithdrawalRequest({
            id: requestId,
            requester: msg.sender,
            targetContract: targetContract,
            recipient: recipient,
            assetType: assetType,
            asset: asset,
            amount: amountOrId,
            data: data,
            status: RequestStatus.PENDING,
            createdAt: block.timestamp,
            executeAfter: block.timestamp + delay,
            expiresAt: block.timestamp + delay + GRACE_PERIOD,
            approvalCount: 0,
            reason: reason
        });

        _audit(keccak256("REQUEST_CREATED"), requestId, abi.encode(targetContract, asset, amountOrId));

        emit WithdrawalRequested(requestId, targetContract, msg.sender, assetType, asset, amountOrId);
    }

    // ============================================
    // APPROVAL & EXECUTION
    // ============================================

    /**
     * @notice Approves a withdrawal request
     */
    function approveRequest(uint256 requestId) external onlyRole(APPROVER_ROLE) {
        WithdrawalRequest storage request = requests[requestId];
        if (request.id == 0) revert RequestNotFound(requestId);
        if (request.status != RequestStatus.PENDING) revert RequestNotPending(requestId, request.status);
        if (block.timestamp > request.expiresAt) revert RequestExpired(requestId);
        if (approvals[requestId][msg.sender]) revert AlreadyApproved(msg.sender);

        approvals[requestId][msg.sender] = true;
        request.approvalCount++;

        _audit(keccak256("REQUEST_APPROVED"), requestId, abi.encode(msg.sender));

        emit WithdrawalApproved(requestId, msg.sender, request.approvalCount);
    }

    /**
     * @notice Executes a withdrawal request
     */
    function executeRequest(uint256 requestId) external nonReentrant onlyRole(RESCUER_ROLE) {
        WithdrawalRequest storage request = requests[requestId];
        if (request.id == 0) revert RequestNotFound(requestId);
        if (request.status != RequestStatus.PENDING) revert RequestNotPending(requestId, request.status);
        if (block.timestamp < request.executeAfter) revert TimelockNotMet(request.executeAfter);
        if (block.timestamp > request.expiresAt) revert RequestExpired(requestId);

        // Check approval requirements
        ContractConfig storage config = contractConfigs[request.targetContract];
        uint256 required = config.isProtected ? config.requiredApprovals : defaultApprovals;
        
        if (request.approvalCount < required) {
            revert InsufficientApprovals(request.approvalCount, required);
        }

        // Check cooldowns
        if (config.isProtected && config.cooldownPeriod > 0) {
            if (block.timestamp < config.lastWithdrawal + config.cooldownPeriod) {
                revert CooldownActive(config.lastWithdrawal + config.cooldownPeriod);
            }
        }

        if (block.timestamp < lastGlobalWithdrawal + globalCooldown) {
            revert CooldownActive(lastGlobalWithdrawal + globalCooldown);
        }

        // Execute withdrawal
        request.status = RequestStatus.EXECUTED;
        config.lastWithdrawal = block.timestamp;
        config.totalWithdrawn += request.amount;
        lastGlobalWithdrawal = block.timestamp;

        _executeWithdrawal(request);

        _audit(keccak256("REQUEST_EXECUTED"), requestId, abi.encode(msg.sender));

        emit WithdrawalExecuted(requestId, request.recipient, request.amount);
    }

    /**
     * @notice Emergency execution (bypasses timelock)
     * @dev Requires EMERGENCY_ROLE and higher approval threshold
     */
    function emergencyExecute(uint256 requestId) external nonReentrant onlyRole(EMERGENCY_ROLE) {
        WithdrawalRequest storage request = requests[requestId];
        if (request.id == 0) revert RequestNotFound(requestId);
        if (request.status != RequestStatus.PENDING) revert RequestNotPending(requestId, request.status);

        // Emergency requires 3 approvals minimum
        if (request.approvalCount < 3) {
            revert InsufficientApprovals(request.approvalCount, 3);
        }

        request.status = RequestStatus.EXECUTED;
        lastGlobalWithdrawal = block.timestamp;

        _executeWithdrawal(request);

        _audit(keccak256("EMERGENCY_EXECUTED"), requestId, abi.encode(msg.sender));

        emit EmergencyWithdrawalExecuted(requestId, msg.sender);
    }

    /**
     * @notice Internal withdrawal execution
     */
    function _executeWithdrawal(WithdrawalRequest storage request) internal {
        if (request.assetType == AssetType.NATIVE) {
            _withdrawNative(request.targetContract, request.recipient, request.amount);
        } else if (request.assetType == AssetType.ERC20) {
            _withdrawERC20(request.targetContract, request.asset, request.recipient, request.amount);
        } else if (request.assetType == AssetType.ERC721) {
            _withdrawERC721(request.targetContract, request.asset, request.recipient, request.amount);
        } else if (request.assetType == AssetType.ERC1155) {
            uint256 amount = abi.decode(request.data, (uint256));
            _withdrawERC1155(request.targetContract, request.asset, request.recipient, request.amount, amount);
        }
    }

    /**
     * @notice Withdraws native currency from target
     */
    function _withdrawNative(address target, address recipient, uint256 amount) internal {
        (bool success, ) = target.call(
            abi.encodeWithSignature("emergencyWithdrawNative(address,uint256)", recipient, amount)
        );
        if (!success) {
            // Try direct transfer if contract has receive
            (success, ) = target.call{value: 0}(
                abi.encodeWithSignature("transfer(address,uint256)", recipient, amount)
            );
        }
        if (!success) revert TransferFailed();
    }

    /**
     * @notice Withdraws ERC20 from target
     */
    function _withdrawERC20(address target, address token, address recipient, uint256 amount) internal {
        (bool success, ) = target.call(
            abi.encodeWithSignature("emergencyWithdrawERC20(address,address,uint256)", token, recipient, amount)
        );
        if (!success) revert TransferFailed();
    }

    /**
     * @notice Withdraws ERC721 from target
     */
    function _withdrawERC721(address target, address token, address recipient, uint256 tokenId) internal {
        (bool success, ) = target.call(
            abi.encodeWithSignature("emergencyWithdrawERC721(address,address,uint256)", token, recipient, tokenId)
        );
        if (!success) revert TransferFailed();
    }

    /**
     * @notice Withdraws ERC1155 from target
     */
    function _withdrawERC1155(
        address target,
        address token,
        address recipient,
        uint256 tokenId,
        uint256 amount
    ) internal {
        (bool success, ) = target.call(
            abi.encodeWithSignature(
                "emergencyWithdrawERC1155(address,address,uint256,uint256)",
                token, recipient, tokenId, amount
            )
        );
        if (!success) revert TransferFailed();
    }

    /**
     * @notice Cancels a withdrawal request
     */
    function cancelRequest(uint256 requestId) external {
        WithdrawalRequest storage request = requests[requestId];
        if (request.id == 0) revert RequestNotFound(requestId);
        if (request.status != RequestStatus.PENDING) revert RequestNotPending(requestId, request.status);

        // Only requester or admin can cancel
        require(
            msg.sender == request.requester || hasRole(DEFAULT_ADMIN_ROLE, msg.sender),
            "Not authorized"
        );

        request.status = RequestStatus.CANCELLED;

        _audit(keccak256("REQUEST_CANCELLED"), requestId, abi.encode(msg.sender));

        emit WithdrawalCancelled(requestId, msg.sender);
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Registers a contract for protection
     */
    function registerContract(
        address targetContract,
        uint256 timelockDelay,
        uint256 requiredApprovals,
        uint256 highValueThreshold,
        uint256 cooldownPeriod
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (targetContract == address(0)) revert ZeroAddress();
        if (timelockDelay < MIN_DELAY || timelockDelay > MAX_DELAY) revert InvalidDelay(timelockDelay);

        contractConfigs[targetContract] = ContractConfig({
            isProtected: true,
            timelockDelay: timelockDelay,
            requiredApprovals: requiredApprovals,
            highValueThreshold: highValueThreshold,
            cooldownPeriod: cooldownPeriod,
            lastWithdrawal: 0,
            totalWithdrawn: 0
        });

        emit ContractRegistered(targetContract, timelockDelay);
    }

    /**
     * @notice Removes a contract from protection
     */
    function removeContract(address targetContract) external onlyRole(DEFAULT_ADMIN_ROLE) {
        delete contractConfigs[targetContract];
        emit ContractRemoved(targetContract);
    }

    /**
     * @notice Whitelists a recipient
     */
    function setRecipientWhitelist(address recipient, bool status) external onlyRole(DEFAULT_ADMIN_ROLE) {
        whitelistedRecipients[recipient] = status;
        emit RecipientWhitelisted(recipient, status);
    }

    /**
     * @notice Sets global parameters
     */
    function setGlobalParams(
        uint256 _defaultDelay,
        uint256 _defaultApprovals,
        uint256 _globalCooldown
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (_defaultDelay < MIN_DELAY || _defaultDelay > MAX_DELAY) revert InvalidDelay(_defaultDelay);
        
        defaultDelay = _defaultDelay;
        defaultApprovals = _defaultApprovals;
        globalCooldown = _globalCooldown;
    }

    // ============================================
    // AUDIT
    // ============================================

    /**
     * @notice Records an audit entry
     */
    function _audit(bytes32 action, uint256 requestId, bytes memory data) internal {
        auditLog.push(AuditEntry({
            timestamp: block.timestamp,
            actor: msg.sender,
            action: action,
            requestId: requestId,
            data: data
        }));
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getRequest(uint256 requestId) external view returns (WithdrawalRequest memory) {
        return requests[requestId];
    }

    function getContractConfig(address targetContract) external view returns (ContractConfig memory) {
        return contractConfigs[targetContract];
    }

    function getAuditLog(uint256 offset, uint256 limit) external view returns (AuditEntry[] memory) {
        uint256 total = auditLog.length;
        if (offset >= total) return new AuditEntry[](0);

        uint256 end = offset + limit > total ? total : offset + limit;
        AuditEntry[] memory entries = new AuditEntry[](end - offset);
        
        for (uint256 i = offset; i < end; i++) {
            entries[i - offset] = auditLog[i];
        }
        
        return entries;
    }

    function hasApproved(uint256 requestId, address approver) external view returns (bool) {
        return approvals[requestId][approver];
    }

    function getAuditLogLength() external view returns (uint256) {
        return auditLog.length;
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(DEFAULT_ADMIN_ROLE) { _pause(); }
    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}

    receive() external payable {}
}
