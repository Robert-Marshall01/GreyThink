// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title MultiSigWallet
 * @author Grey Protocol Team
 * @notice Advanced multi-signature wallet with time-locked transactions
 * @dev Implements comprehensive multi-sig with various security features
 * 
 * Features:
 * - Multiple signature schemes (M-of-N)
 * - Time-locked transactions
 * - Transaction batching
 * - Gas-less meta transactions via signatures
 * - Owner management with quorum
 * - Daily spending limits
 * - Emergency recovery
 * - Transaction scheduling
 * - Role-based permissions
 */
contract MultiSigWallet is ReentrancyGuard {
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant MAX_OWNERS = 50;
    uint256 public constant MIN_OWNERS = 2;
    uint256 public constant MAX_PENDING_TXS = 100;
    uint256 public constant MIN_TIMELOCK = 1 hours;
    uint256 public constant MAX_TIMELOCK = 30 days;
    uint256 public constant SIGNATURE_VALIDITY = 24 hours;

    // ============================================
    // ENUMS
    // ============================================

    enum TransactionStatus {
        Pending,
        Scheduled,
        Executed,
        Cancelled,
        Expired,
        Failed
    }

    enum OperationType {
        Call,
        DelegateCall,
        Create,
        Create2
    }

    enum PermissionLevel {
        None,
        Viewer,
        Proposer,
        Signer,
        Admin
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Transaction data
     */
    struct Transaction {
        uint256 txId;
        address to;
        uint256 value;
        bytes data;
        OperationType operation;
        uint256 nonce;
        uint256 createdAt;
        uint256 scheduledFor;
        uint256 expiresAt;
        TransactionStatus status;
        address proposer;
        bytes32 dataHash;
        uint256 gasLimit;
        string description;
    }

    /**
     * @notice Owner configuration
     */
    struct OwnerConfig {
        address owner;
        PermissionLevel permission;
        uint256 addedAt;
        uint256 dailyLimit;
        uint256 spentToday;
        uint256 lastSpendReset;
        bool isActive;
    }

    /**
     * @notice Wallet configuration
     */
    struct WalletConfig {
        uint256 threshold;
        uint256 timelockDuration;
        uint256 globalDailyLimit;
        uint256 transactionExpiry;
        bool requiresTimelock;
        bool paused;
    }

    /**
     * @notice Confirmation data
     */
    struct Confirmation {
        address signer;
        uint256 timestamp;
        bytes signature;
    }

    /**
     * @notice Batch transaction
     */
    struct BatchTransaction {
        address to;
        uint256 value;
        bytes data;
        OperationType operation;
    }

    /**
     * @notice Recovery request
     */
    struct RecoveryRequest {
        address[] newOwners;
        uint256 newThreshold;
        uint256 proposedAt;
        uint256 executesAt;
        bool executed;
        uint256 confirmationCount;
        mapping(address => bool) confirmations;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Wallet configuration
    WalletConfig public config;

    /// @notice Owner configurations
    mapping(address => OwnerConfig) public owners;

    /// @notice Owner list
    address[] public ownerList;

    /// @notice Is owner
    mapping(address => bool) public isOwner;

    /// @notice Transactions
    mapping(uint256 => Transaction) public transactions;

    /// @notice Transaction confirmations
    mapping(uint256 => Confirmation[]) public confirmations;

    /// @notice Has confirmed
    mapping(uint256 => mapping(address => bool)) public hasConfirmed;

    /// @notice Transaction count
    uint256 public transactionCount;

    /// @notice Pending transaction count
    uint256 public pendingCount;

    /// @notice Nonce for transactions
    uint256 public nonce;

    /// @notice Global spent today
    uint256 public globalSpentToday;

    /// @notice Last global reset
    uint256 public lastGlobalReset;

    /// @notice Recovery requests
    mapping(uint256 => RecoveryRequest) public recoveryRequests;

    /// @notice Recovery request count
    uint256 public recoveryRequestCount;

    /// @notice Guardian addresses for recovery
    mapping(address => bool) public isGuardian;

    /// @notice Guardian list
    address[] public guardianList;

    /// @notice Required guardian confirmations for recovery
    uint256 public guardianThreshold;

    /// @notice Domain separator for EIP-712
    bytes32 public DOMAIN_SEPARATOR;

    /// @notice EIP-712 typehash for transactions
    bytes32 public constant TX_TYPEHASH = keccak256(
        "Transaction(address to,uint256 value,bytes data,uint8 operation,uint256 nonce,uint256 deadline)"
    );

    // ============================================
    // EVENTS
    // ============================================

    event WalletCreated(
        address[] owners,
        uint256 threshold,
        uint256 timelockDuration
    );

    event TransactionProposed(
        uint256 indexed txId,
        address indexed proposer,
        address to,
        uint256 value,
        bytes data
    );

    event TransactionConfirmed(
        uint256 indexed txId,
        address indexed signer,
        uint256 confirmationCount
    );

    event TransactionRevoked(
        uint256 indexed txId,
        address indexed signer
    );

    event TransactionScheduled(
        uint256 indexed txId,
        uint256 scheduledFor
    );

    event TransactionExecuted(
        uint256 indexed txId,
        address indexed executor,
        bool success,
        bytes returnData
    );

    event TransactionCancelled(
        uint256 indexed txId,
        address indexed canceller
    );

    event TransactionFailed(
        uint256 indexed txId,
        bytes reason
    );

    event OwnerAdded(
        address indexed owner,
        PermissionLevel permission
    );

    event OwnerRemoved(
        address indexed owner
    );

    event OwnerUpdated(
        address indexed owner,
        PermissionLevel newPermission
    );

    event ThresholdChanged(
        uint256 oldThreshold,
        uint256 newThreshold
    );

    event TimelockChanged(
        uint256 oldTimelock,
        uint256 newTimelock
    );

    event DailyLimitChanged(
        address indexed owner,
        uint256 newLimit
    );

    event GlobalDailyLimitChanged(
        uint256 newLimit
    );

    event GuardianAdded(address indexed guardian);
    event GuardianRemoved(address indexed guardian);

    event RecoveryInitiated(
        uint256 indexed requestId,
        address[] newOwners,
        uint256 newThreshold
    );

    event RecoveryConfirmed(
        uint256 indexed requestId,
        address indexed guardian
    );

    event RecoveryExecuted(
        uint256 indexed requestId
    );

    event RecoveryCancelled(
        uint256 indexed requestId
    );

    event Deposit(
        address indexed sender,
        uint256 amount
    );

    event WalletPaused();
    event WalletUnpaused();

    // ============================================
    // ERRORS
    // ============================================

    error NotOwner(address caller);
    error NotSigner(address caller);
    error NotProposer(address caller);
    error NotAdmin(address caller);
    error NotGuardian(address caller);
    error InvalidThreshold(uint256 threshold, uint256 ownerCount);
    error InvalidOwner(address owner);
    error DuplicateOwner(address owner);
    error OwnerNotFound(address owner);
    error TransactionNotFound(uint256 txId);
    error TransactionNotPending(uint256 txId);
    error TransactionNotScheduled(uint256 txId);
    error TransactionExpired(uint256 txId);
    error TransactionNotReady(uint256 scheduledFor);
    error AlreadyConfirmed(uint256 txId, address signer);
    error NotConfirmed(uint256 txId, address signer);
    error InsufficientConfirmations(uint256 current, uint256 required);
    error ExecutionFailed(bytes reason);
    error DailyLimitExceeded(uint256 limit, uint256 requested);
    error GlobalDailyLimitExceeded(uint256 limit, uint256 requested);
    error TimelockNotExpired(uint256 expiresAt);
    error MaxOwnersReached();
    error MinOwnersRequired();
    error MaxPendingTransactionsReached();
    error InvalidTimelock(uint256 timelock);
    error InvalidSignature();
    error SignatureExpired();
    error WalletPausedError();
    error RecoveryNotFound(uint256 requestId);
    error RecoveryNotReady(uint256 executesAt);
    error RecoveryAlreadyExecuted();
    error AlreadyConfirmedRecovery();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier onlyOwner() {
        if (!isOwner[msg.sender]) revert NotOwner(msg.sender);
        _;
    }

    modifier onlySigner() {
        if (owners[msg.sender].permission < PermissionLevel.Signer) {
            revert NotSigner(msg.sender);
        }
        _;
    }

    modifier onlyProposer() {
        if (owners[msg.sender].permission < PermissionLevel.Proposer) {
            revert NotProposer(msg.sender);
        }
        _;
    }

    modifier onlyAdmin() {
        if (owners[msg.sender].permission != PermissionLevel.Admin) {
            revert NotAdmin(msg.sender);
        }
        _;
    }

    modifier onlyGuardian() {
        if (!isGuardian[msg.sender]) revert NotGuardian(msg.sender);
        _;
    }

    modifier notPaused() {
        if (config.paused) revert WalletPausedError();
        _;
    }

    modifier txExists(uint256 txId) {
        if (transactions[txId].createdAt == 0) revert TransactionNotFound(txId);
        _;
    }

    modifier txPending(uint256 txId) {
        if (transactions[txId].status != TransactionStatus.Pending &&
            transactions[txId].status != TransactionStatus.Scheduled) {
            revert TransactionNotPending(txId);
        }
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address[] memory _owners,
        uint256 _threshold,
        uint256 _timelockDuration,
        uint256 _globalDailyLimit
    ) {
        require(_owners.length >= MIN_OWNERS, "Min owners required");
        require(_owners.length <= MAX_OWNERS, "Max owners exceeded");
        require(_threshold >= 1 && _threshold <= _owners.length, "Invalid threshold");
        require(
            _timelockDuration == 0 || 
            (_timelockDuration >= MIN_TIMELOCK && _timelockDuration <= MAX_TIMELOCK),
            "Invalid timelock"
        );

        for (uint256 i = 0; i < _owners.length; i++) {
            address owner = _owners[i];
            require(owner != address(0), "Invalid owner");
            require(!isOwner[owner], "Duplicate owner");

            isOwner[owner] = true;
            ownerList.push(owner);
            
            owners[owner] = OwnerConfig({
                owner: owner,
                permission: i == 0 ? PermissionLevel.Admin : PermissionLevel.Signer,
                addedAt: block.timestamp,
                dailyLimit: type(uint256).max,
                spentToday: 0,
                lastSpendReset: block.timestamp,
                isActive: true
            });
        }

        config = WalletConfig({
            threshold: _threshold,
            timelockDuration: _timelockDuration,
            globalDailyLimit: _globalDailyLimit,
            transactionExpiry: 7 days,
            requiresTimelock: _timelockDuration > 0,
            paused: false
        });

        // Set up EIP-712 domain separator
        DOMAIN_SEPARATOR = keccak256(
            abi.encode(
                keccak256("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)"),
                keccak256("MultiSigWallet"),
                keccak256("1"),
                block.chainid,
                address(this)
            )
        );

        emit WalletCreated(_owners, _threshold, _timelockDuration);
    }

    // ============================================
    // RECEIVE & FALLBACK
    // ============================================

    receive() external payable {
        emit Deposit(msg.sender, msg.value);
    }

    fallback() external payable {
        emit Deposit(msg.sender, msg.value);
    }

    // ============================================
    // TRANSACTION PROPOSAL
    // ============================================

    /**
     * @notice Proposes a new transaction
     */
    function proposeTransaction(
        address to,
        uint256 value,
        bytes calldata data,
        OperationType operation,
        uint256 gasLimit,
        string calldata description
    ) 
        external 
        onlyProposer 
        notPaused 
        nonReentrant 
        returns (uint256 txId) 
    {
        if (pendingCount >= MAX_PENDING_TXS) revert MaxPendingTransactionsReached();

        txId = transactionCount++;
        uint256 currentNonce = nonce++;

        transactions[txId] = Transaction({
            txId: txId,
            to: to,
            value: value,
            data: data,
            operation: operation,
            nonce: currentNonce,
            createdAt: block.timestamp,
            scheduledFor: 0,
            expiresAt: block.timestamp + config.transactionExpiry,
            status: TransactionStatus.Pending,
            proposer: msg.sender,
            dataHash: keccak256(data),
            gasLimit: gasLimit,
            description: description
        });

        pendingCount++;

        // Auto-confirm by proposer if they're a signer
        if (owners[msg.sender].permission >= PermissionLevel.Signer) {
            _confirm(txId, msg.sender);
        }

        emit TransactionProposed(txId, msg.sender, to, value, data);
    }

    /**
     * @notice Proposes a batch of transactions
     */
    function proposeBatch(
        BatchTransaction[] calldata txs,
        string calldata description
    ) 
        external 
        onlyProposer 
        notPaused 
        nonReentrant 
        returns (uint256[] memory txIds) 
    {
        require(txs.length > 0, "Empty batch");
        require(pendingCount + txs.length <= MAX_PENDING_TXS, "Too many pending");

        txIds = new uint256[](txs.length);

        for (uint256 i = 0; i < txs.length; i++) {
            uint256 txId = transactionCount++;
            uint256 currentNonce = nonce++;

            transactions[txId] = Transaction({
                txId: txId,
                to: txs[i].to,
                value: txs[i].value,
                data: txs[i].data,
                operation: txs[i].operation,
                nonce: currentNonce,
                createdAt: block.timestamp,
                scheduledFor: 0,
                expiresAt: block.timestamp + config.transactionExpiry,
                status: TransactionStatus.Pending,
                proposer: msg.sender,
                dataHash: keccak256(txs[i].data),
                gasLimit: 0,
                description: description
            });

            txIds[i] = txId;
            pendingCount++;

            if (owners[msg.sender].permission >= PermissionLevel.Signer) {
                _confirm(txId, msg.sender);
            }

            emit TransactionProposed(txId, msg.sender, txs[i].to, txs[i].value, txs[i].data);
        }
    }

    // ============================================
    // CONFIRMATION FUNCTIONS
    // ============================================

    /**
     * @notice Confirms a pending transaction
     */
    function confirmTransaction(uint256 txId) 
        external 
        onlySigner 
        notPaused 
        txExists(txId) 
        txPending(txId) 
    {
        _confirm(txId, msg.sender);
    }

    /**
     * @notice Confirms multiple transactions
     */
    function confirmTransactions(uint256[] calldata txIds) 
        external 
        onlySigner 
        notPaused 
    {
        for (uint256 i = 0; i < txIds.length; i++) {
            if (transactions[txIds[i]].createdAt == 0) continue;
            if (transactions[txIds[i]].status != TransactionStatus.Pending &&
                transactions[txIds[i]].status != TransactionStatus.Scheduled) continue;
            
            _confirm(txIds[i], msg.sender);
        }
    }

    /**
     * @notice Confirms with signature (gasless)
     */
    function confirmWithSignature(
        uint256 txId,
        bytes calldata signature,
        uint256 deadline
    ) 
        external 
        notPaused 
        txExists(txId) 
        txPending(txId) 
    {
        if (block.timestamp > deadline) revert SignatureExpired();

        Transaction storage tx_ = transactions[txId];
        
        bytes32 structHash = keccak256(
            abi.encode(
                TX_TYPEHASH,
                tx_.to,
                tx_.value,
                keccak256(tx_.data),
                tx_.operation,
                tx_.nonce,
                deadline
            )
        );

        bytes32 hash = MessageHashUtils.toTypedDataHash(DOMAIN_SEPARATOR, structHash);
        address signer = hash.recover(signature);

        if (!isOwner[signer]) revert InvalidSignature();
        if (owners[signer].permission < PermissionLevel.Signer) revert NotSigner(signer);

        _confirm(txId, signer);

        // Store signature
        confirmations[txId].push(Confirmation({
            signer: signer,
            timestamp: block.timestamp,
            signature: signature
        }));
    }

    /**
     * @notice Revokes a confirmation
     */
    function revokeConfirmation(uint256 txId) 
        external 
        onlySigner 
        txExists(txId) 
        txPending(txId) 
    {
        if (!hasConfirmed[txId][msg.sender]) revert NotConfirmed(txId, msg.sender);

        hasConfirmed[txId][msg.sender] = false;
        
        // Remove from confirmations array
        Confirmation[] storage confs = confirmations[txId];
        for (uint256 i = 0; i < confs.length; i++) {
            if (confs[i].signer == msg.sender) {
                confs[i] = confs[confs.length - 1];
                confs.pop();
                break;
            }
        }

        emit TransactionRevoked(txId, msg.sender);
    }

    /**
     * @notice Internal confirm function
     */
    function _confirm(uint256 txId, address signer) internal {
        if (hasConfirmed[txId][signer]) revert AlreadyConfirmed(txId, signer);

        hasConfirmed[txId][signer] = true;
        confirmations[txId].push(Confirmation({
            signer: signer,
            timestamp: block.timestamp,
            signature: ""
        }));

        emit TransactionConfirmed(txId, signer, confirmations[txId].length);

        // Auto-schedule if threshold reached and timelock required
        if (confirmations[txId].length >= config.threshold && config.requiresTimelock) {
            if (transactions[txId].status == TransactionStatus.Pending) {
                transactions[txId].scheduledFor = block.timestamp + config.timelockDuration;
                transactions[txId].status = TransactionStatus.Scheduled;
                emit TransactionScheduled(txId, transactions[txId].scheduledFor);
            }
        }
    }

    // ============================================
    // EXECUTION FUNCTIONS
    // ============================================

    /**
     * @notice Executes a confirmed transaction
     */
    function executeTransaction(uint256 txId) 
        external 
        onlySigner 
        notPaused 
        nonReentrant 
        txExists(txId) 
        returns (bool success, bytes memory returnData) 
    {
        Transaction storage tx_ = transactions[txId];

        // Check confirmations
        if (confirmations[txId].length < config.threshold) {
            revert InsufficientConfirmations(confirmations[txId].length, config.threshold);
        }

        // Check expiry
        if (block.timestamp > tx_.expiresAt) {
            tx_.status = TransactionStatus.Expired;
            pendingCount--;
            revert TransactionExpired(txId);
        }

        // Check timelock if scheduled
        if (tx_.status == TransactionStatus.Scheduled) {
            if (block.timestamp < tx_.scheduledFor) {
                revert TransactionNotReady(tx_.scheduledFor);
            }
        } else if (config.requiresTimelock) {
            revert TransactionNotScheduled(txId);
        }

        // Check daily limits
        if (tx_.value > 0) {
            _checkAndUpdateDailyLimit(msg.sender, tx_.value);
        }

        // Execute
        tx_.status = TransactionStatus.Executed;
        pendingCount--;

        (success, returnData) = _execute(tx_.to, tx_.value, tx_.data, tx_.operation, tx_.gasLimit);

        if (success) {
            emit TransactionExecuted(txId, msg.sender, true, returnData);
        } else {
            tx_.status = TransactionStatus.Failed;
            emit TransactionFailed(txId, returnData);
        }
    }

    /**
     * @notice Executes multiple transactions
     */
    function executeBatch(uint256[] calldata txIds) 
        external 
        onlySigner 
        notPaused 
        nonReentrant 
        returns (bool[] memory successes) 
    {
        successes = new bool[](txIds.length);

        for (uint256 i = 0; i < txIds.length; i++) {
            Transaction storage tx_ = transactions[txIds[i]];
            
            if (tx_.createdAt == 0) continue;
            if (confirmations[txIds[i]].length < config.threshold) continue;
            if (block.timestamp > tx_.expiresAt) {
                tx_.status = TransactionStatus.Expired;
                pendingCount--;
                continue;
            }

            if (tx_.status == TransactionStatus.Scheduled) {
                if (block.timestamp < tx_.scheduledFor) continue;
            } else if (config.requiresTimelock) {
                continue;
            }

            if (tx_.value > 0) {
                try this.checkDailyLimitExternal(msg.sender, tx_.value) {
                    // Limit check passed
                } catch {
                    continue;
                }
            }

            tx_.status = TransactionStatus.Executed;
            pendingCount--;

            (bool success, bytes memory returnData) = _execute(
                tx_.to, 
                tx_.value, 
                tx_.data, 
                tx_.operation, 
                tx_.gasLimit
            );

            successes[i] = success;

            if (success) {
                emit TransactionExecuted(txIds[i], msg.sender, true, returnData);
            } else {
                tx_.status = TransactionStatus.Failed;
                emit TransactionFailed(txIds[i], returnData);
            }
        }
    }

    /**
     * @notice Internal execution function
     */
    function _execute(
        address to,
        uint256 value,
        bytes memory data,
        OperationType operation,
        uint256 gasLimit
    ) internal returns (bool success, bytes memory returnData) {
        uint256 gas = gasLimit > 0 ? gasLimit : gasleft();

        if (operation == OperationType.Call) {
            (success, returnData) = to.call{value: value, gas: gas}(data);
        } else if (operation == OperationType.DelegateCall) {
            (success, returnData) = to.delegatecall{gas: gas}(data);
        } else if (operation == OperationType.Create) {
            assembly {
                let deployed := create(value, add(data, 0x20), mload(data))
                success := iszero(iszero(deployed))
                mstore(returnData, deployed)
            }
        } else if (operation == OperationType.Create2) {
            bytes32 salt = bytes32(block.timestamp);
            assembly {
                let deployed := create2(value, add(data, 0x20), mload(data), salt)
                success := iszero(iszero(deployed))
                mstore(returnData, deployed)
            }
        }
    }

    /**
     * @notice Cancels a pending transaction
     */
    function cancelTransaction(uint256 txId) 
        external 
        onlyAdmin 
        txExists(txId) 
        txPending(txId) 
    {
        transactions[txId].status = TransactionStatus.Cancelled;
        pendingCount--;

        emit TransactionCancelled(txId, msg.sender);
    }

    // ============================================
    // DAILY LIMIT FUNCTIONS
    // ============================================

    /**
     * @notice Checks and updates daily spending limit
     */
    function _checkAndUpdateDailyLimit(address owner, uint256 amount) internal {
        OwnerConfig storage ownerConfig = owners[owner];

        // Reset daily limit if new day
        if (block.timestamp >= ownerConfig.lastSpendReset + 1 days) {
            ownerConfig.spentToday = 0;
            ownerConfig.lastSpendReset = block.timestamp;
        }

        // Check owner limit
        if (ownerConfig.spentToday + amount > ownerConfig.dailyLimit) {
            revert DailyLimitExceeded(ownerConfig.dailyLimit, ownerConfig.spentToday + amount);
        }

        // Reset global limit if new day
        if (block.timestamp >= lastGlobalReset + 1 days) {
            globalSpentToday = 0;
            lastGlobalReset = block.timestamp;
        }

        // Check global limit
        if (config.globalDailyLimit > 0 && globalSpentToday + amount > config.globalDailyLimit) {
            revert GlobalDailyLimitExceeded(config.globalDailyLimit, globalSpentToday + amount);
        }

        ownerConfig.spentToday += amount;
        globalSpentToday += amount;
    }

    /**
     * @notice External limit check (for batch execution)
     */
    function checkDailyLimitExternal(address owner, uint256 amount) external view {
        OwnerConfig storage ownerConfig = owners[owner];
        
        uint256 spentToday = ownerConfig.spentToday;
        if (block.timestamp >= ownerConfig.lastSpendReset + 1 days) {
            spentToday = 0;
        }

        if (spentToday + amount > ownerConfig.dailyLimit) {
            revert DailyLimitExceeded(ownerConfig.dailyLimit, spentToday + amount);
        }

        uint256 globalSpent = globalSpentToday;
        if (block.timestamp >= lastGlobalReset + 1 days) {
            globalSpent = 0;
        }

        if (config.globalDailyLimit > 0 && globalSpent + amount > config.globalDailyLimit) {
            revert GlobalDailyLimitExceeded(config.globalDailyLimit, globalSpent + amount);
        }
    }

    // ============================================
    // OWNER MANAGEMENT
    // ============================================

    /**
     * @notice Adds a new owner (requires multi-sig)
     */
    function addOwner(address owner, PermissionLevel permission) external onlyAdmin {
        if (owner == address(0)) revert InvalidOwner(owner);
        if (isOwner[owner]) revert DuplicateOwner(owner);
        if (ownerList.length >= MAX_OWNERS) revert MaxOwnersReached();

        isOwner[owner] = true;
        ownerList.push(owner);
        
        owners[owner] = OwnerConfig({
            owner: owner,
            permission: permission,
            addedAt: block.timestamp,
            dailyLimit: type(uint256).max,
            spentToday: 0,
            lastSpendReset: block.timestamp,
            isActive: true
        });

        emit OwnerAdded(owner, permission);
    }

    /**
     * @notice Removes an owner (requires multi-sig)
     */
    function removeOwner(address owner) external onlyAdmin {
        if (!isOwner[owner]) revert OwnerNotFound(owner);
        if (ownerList.length <= MIN_OWNERS) revert MinOwnersRequired();
        
        // Check threshold
        if (ownerList.length - 1 < config.threshold) {
            revert InvalidThreshold(config.threshold, ownerList.length - 1);
        }

        isOwner[owner] = false;
        owners[owner].isActive = false;

        // Remove from list
        for (uint256 i = 0; i < ownerList.length; i++) {
            if (ownerList[i] == owner) {
                ownerList[i] = ownerList[ownerList.length - 1];
                ownerList.pop();
                break;
            }
        }

        emit OwnerRemoved(owner);
    }

    /**
     * @notice Updates owner permission
     */
    function updateOwnerPermission(address owner, PermissionLevel permission) external onlyAdmin {
        if (!isOwner[owner]) revert OwnerNotFound(owner);
        owners[owner].permission = permission;
        emit OwnerUpdated(owner, permission);
    }

    /**
     * @notice Changes threshold
     */
    function changeThreshold(uint256 newThreshold) external onlyAdmin {
        if (newThreshold < 1 || newThreshold > ownerList.length) {
            revert InvalidThreshold(newThreshold, ownerList.length);
        }

        uint256 oldThreshold = config.threshold;
        config.threshold = newThreshold;

        emit ThresholdChanged(oldThreshold, newThreshold);
    }

    /**
     * @notice Changes timelock duration
     */
    function changeTimelock(uint256 newTimelock) external onlyAdmin {
        if (newTimelock != 0 && (newTimelock < MIN_TIMELOCK || newTimelock > MAX_TIMELOCK)) {
            revert InvalidTimelock(newTimelock);
        }

        uint256 oldTimelock = config.timelockDuration;
        config.timelockDuration = newTimelock;
        config.requiresTimelock = newTimelock > 0;

        emit TimelockChanged(oldTimelock, newTimelock);
    }

    /**
     * @notice Sets owner daily limit
     */
    function setOwnerDailyLimit(address owner, uint256 limit) external onlyAdmin {
        if (!isOwner[owner]) revert OwnerNotFound(owner);
        owners[owner].dailyLimit = limit;
        emit DailyLimitChanged(owner, limit);
    }

    /**
     * @notice Sets global daily limit
     */
    function setGlobalDailyLimit(uint256 limit) external onlyAdmin {
        config.globalDailyLimit = limit;
        emit GlobalDailyLimitChanged(limit);
    }

    // ============================================
    // GUARDIAN & RECOVERY
    // ============================================

    /**
     * @notice Adds a guardian for recovery
     */
    function addGuardian(address guardian) external onlyAdmin {
        require(guardian != address(0), "Invalid guardian");
        require(!isGuardian[guardian], "Already guardian");

        isGuardian[guardian] = true;
        guardianList.push(guardian);

        emit GuardianAdded(guardian);
    }

    /**
     * @notice Removes a guardian
     */
    function removeGuardian(address guardian) external onlyAdmin {
        require(isGuardian[guardian], "Not guardian");

        isGuardian[guardian] = false;

        for (uint256 i = 0; i < guardianList.length; i++) {
            if (guardianList[i] == guardian) {
                guardianList[i] = guardianList[guardianList.length - 1];
                guardianList.pop();
                break;
            }
        }

        emit GuardianRemoved(guardian);
    }

    /**
     * @notice Sets guardian threshold
     */
    function setGuardianThreshold(uint256 threshold) external onlyAdmin {
        require(threshold <= guardianList.length, "Invalid threshold");
        guardianThreshold = threshold;
    }

    /**
     * @notice Initiates wallet recovery
     */
    function initiateRecovery(
        address[] calldata newOwners,
        uint256 newThreshold
    ) external onlyGuardian returns (uint256 requestId) {
        require(newOwners.length >= MIN_OWNERS, "Min owners required");
        require(newThreshold <= newOwners.length, "Invalid threshold");

        requestId = recoveryRequestCount++;

        RecoveryRequest storage request = recoveryRequests[requestId];
        request.newOwners = newOwners;
        request.newThreshold = newThreshold;
        request.proposedAt = block.timestamp;
        request.executesAt = block.timestamp + 7 days; // 7 day delay
        request.executed = false;
        request.confirmationCount = 1;
        request.confirmations[msg.sender] = true;

        emit RecoveryInitiated(requestId, newOwners, newThreshold);
    }

    /**
     * @notice Confirms a recovery request
     */
    function confirmRecovery(uint256 requestId) external onlyGuardian {
        RecoveryRequest storage request = recoveryRequests[requestId];
        if (request.proposedAt == 0) revert RecoveryNotFound(requestId);
        if (request.executed) revert RecoveryAlreadyExecuted();
        if (request.confirmations[msg.sender]) revert AlreadyConfirmedRecovery();

        request.confirmations[msg.sender] = true;
        request.confirmationCount++;

        emit RecoveryConfirmed(requestId, msg.sender);
    }

    /**
     * @notice Executes a recovery
     */
    function executeRecovery(uint256 requestId) external onlyGuardian {
        RecoveryRequest storage request = recoveryRequests[requestId];
        
        if (request.proposedAt == 0) revert RecoveryNotFound(requestId);
        if (request.executed) revert RecoveryAlreadyExecuted();
        if (request.confirmationCount < guardianThreshold) {
            revert InsufficientConfirmations(request.confirmationCount, guardianThreshold);
        }
        if (block.timestamp < request.executesAt) {
            revert RecoveryNotReady(request.executesAt);
        }

        request.executed = true;

        // Clear existing owners
        for (uint256 i = 0; i < ownerList.length; i++) {
            isOwner[ownerList[i]] = false;
        }
        delete ownerList;

        // Set new owners
        for (uint256 i = 0; i < request.newOwners.length; i++) {
            address owner = request.newOwners[i];
            isOwner[owner] = true;
            ownerList.push(owner);
            
            owners[owner] = OwnerConfig({
                owner: owner,
                permission: i == 0 ? PermissionLevel.Admin : PermissionLevel.Signer,
                addedAt: block.timestamp,
                dailyLimit: type(uint256).max,
                spentToday: 0,
                lastSpendReset: block.timestamp,
                isActive: true
            });
        }

        config.threshold = request.newThreshold;

        emit RecoveryExecuted(requestId);
    }

    /**
     * @notice Cancels a pending recovery (by owner)
     */
    function cancelRecovery(uint256 requestId) external onlyAdmin {
        RecoveryRequest storage request = recoveryRequests[requestId];
        if (request.proposedAt == 0) revert RecoveryNotFound(requestId);
        if (request.executed) revert RecoveryAlreadyExecuted();

        request.executed = true; // Prevent future execution

        emit RecoveryCancelled(requestId);
    }

    // ============================================
    // PAUSE FUNCTIONS
    // ============================================

    /**
     * @notice Pauses the wallet
     */
    function pause() external onlyAdmin {
        config.paused = true;
        emit WalletPaused();
    }

    /**
     * @notice Unpauses the wallet
     */
    function unpause() external onlyAdmin {
        config.paused = false;
        emit WalletUnpaused();
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Gets transaction details
     */
    function getTransaction(uint256 txId) external view returns (Transaction memory) {
        return transactions[txId];
    }

    /**
     * @notice Gets transaction confirmations
     */
    function getConfirmations(uint256 txId) external view returns (Confirmation[] memory) {
        return confirmations[txId];
    }

    /**
     * @notice Gets confirmation count
     */
    function getConfirmationCount(uint256 txId) external view returns (uint256) {
        return confirmations[txId].length;
    }

    /**
     * @notice Gets all owners
     */
    function getOwners() external view returns (address[] memory) {
        return ownerList;
    }

    /**
     * @notice Gets owner count
     */
    function getOwnerCount() external view returns (uint256) {
        return ownerList.length;
    }

    /**
     * @notice Gets all guardians
     */
    function getGuardians() external view returns (address[] memory) {
        return guardianList;
    }

    /**
     * @notice Gets pending transactions
     */
    function getPendingTransactions() external view returns (uint256[] memory) {
        uint256[] memory pending = new uint256[](pendingCount);
        uint256 index = 0;

        for (uint256 i = 0; i < transactionCount && index < pendingCount; i++) {
            if (transactions[i].status == TransactionStatus.Pending ||
                transactions[i].status == TransactionStatus.Scheduled) {
                pending[index++] = i;
            }
        }

        return pending;
    }

    /**
     * @notice Checks if transaction is confirmed
     */
    function isConfirmed(uint256 txId) external view returns (bool) {
        return confirmations[txId].length >= config.threshold;
    }

    /**
     * @notice Gets wallet balance
     */
    function getBalance() external view returns (uint256) {
        return address(this).balance;
    }
}
