// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/cryptography/MerkleProof.sol";

/**
 * @title OptimisticRollupBridge
 * @notice L1 bridge contract for optimistic rollup with fraud proofs
 * @dev Implements the L1 side of an optimistic rollup system
 * 
 * Architecture:
 * - State roots submitted by sequencer
 * - Challenge period for fraud proofs
 * - Deposit and withdrawal queues
 * - Emergency exit mechanism
 * - Multi-token support
 * 
 * Security Model:
 * - 7-day challenge period for withdrawals
 * - Fraud proofs enable anyone to challenge invalid state
 * - Bond requirements for sequencers and challengers
 * - Emergency pause with guardian override
 */
contract OptimisticRollupBridge is AccessControl, ReentrancyGuard {
    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant SEQUENCER_ROLE = keccak256("SEQUENCER_ROLE");
    bytes32 public constant GUARDIAN_ROLE = keccak256("GUARDIAN_ROLE");
    bytes32 public constant CHALLENGER_ROLE = keccak256("CHALLENGER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice State batch submitted by sequencer
    struct StateBatch {
        bytes32 stateRoot;              // Merkle root of L2 state
        bytes32 withdrawalRoot;         // Merkle root of pending withdrawals
        bytes32 previousStateRoot;      // Previous state root (for chain)
        uint256 batchIndex;             // Sequential batch index
        uint256 l2BlockNumber;          // L2 block number
        uint256 timestamp;              // Submission timestamp
        uint256 challengeDeadline;      // When challenge period ends
        address proposer;               // Sequencer who submitted
        bool finalized;                 // Whether batch is finalized
        bool challenged;                // Whether batch has active challenge
    }

    /// @notice Challenge data
    struct Challenge {
        bytes32 batchRoot;              // State root being challenged
        uint256 batchIndex;             // Batch index being challenged
        address challenger;             // Who initiated challenge
        uint256 bondAmount;             // Bond posted by challenger
        uint256 deadline;               // Challenge resolution deadline
        ChallengeStatus status;
        bytes32 fraudProofHash;         // Hash of submitted fraud proof
    }

    /// @notice Pending deposit
    struct Deposit {
        address depositor;
        address token;
        uint256 amount;
        uint256 l2Recipient;            // L2 address (could be different format)
        uint256 timestamp;
        bool processed;
    }

    /// @notice Pending withdrawal
    struct Withdrawal {
        address recipient;
        address token;
        uint256 amount;
        bytes32 stateRoot;              // State root when withdrawal created
        uint256 batchIndex;             // Batch containing this withdrawal
        bytes32[] proof;                // Merkle proof
        bool claimed;
    }

    enum ChallengeStatus {
        None,
        Active,
        ChallengerWon,
        ProposerWon,
        Expired
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant CHALLENGE_PERIOD = 7 days;
    uint256 public constant CHALLENGE_BOND = 1 ether;
    uint256 public constant PROPOSER_BOND = 10 ether;
    uint256 public constant MAX_DEPOSIT_AMOUNT = 1000 ether;
    uint256 public constant FRAUD_PROOF_WINDOW = 1 days;

    // ============================================
    // STATE
    // ============================================

    /// @notice State batches by index
    mapping(uint256 => StateBatch) public stateBatches;

    /// @notice Current batch index
    uint256 public currentBatchIndex;

    /// @notice Latest finalized batch index
    uint256 public lastFinalizedBatch;

    /// @notice Challenges by batch index
    mapping(uint256 => Challenge) public challenges;

    /// @notice Deposits by ID
    mapping(uint256 => Deposit) public deposits;
    uint256 public depositCounter;

    /// @notice Withdrawals by hash
    mapping(bytes32 => Withdrawal) public withdrawals;

    /// @notice Proposer bonds
    mapping(address => uint256) public proposerBonds;

    /// @notice Processed withdrawal hashes (replay protection)
    mapping(bytes32 => bool) public processedWithdrawals;

    /// @notice Token support mapping
    mapping(address => bool) public supportedTokens;

    /// @notice L2 token mappings
    mapping(address => address) public l1ToL2Token;
    mapping(address => address) public l2ToL1Token;

    /// @notice Bridge paused state
    bool public paused;

    /// @notice Emergency mode (allows direct exits)
    bool public emergencyMode;

    // ============================================
    // EVENTS
    // ============================================

    event DepositInitiated(
        uint256 indexed depositId,
        address indexed depositor,
        address token,
        uint256 amount,
        uint256 l2Recipient
    );

    event StateBatchSubmitted(
        uint256 indexed batchIndex,
        bytes32 stateRoot,
        bytes32 withdrawalRoot,
        address proposer
    );

    event BatchFinalized(uint256 indexed batchIndex, bytes32 stateRoot);

    event ChallengeInitiated(
        uint256 indexed batchIndex,
        address indexed challenger,
        uint256 bondAmount
    );

    event ChallengeResolved(
        uint256 indexed batchIndex,
        ChallengeStatus status,
        address winner
    );

    event WithdrawalProven(
        bytes32 indexed withdrawalHash,
        address indexed recipient,
        address token,
        uint256 amount
    );

    event WithdrawalClaimed(
        bytes32 indexed withdrawalHash,
        address indexed recipient,
        uint256 amount
    );

    event EmergencyModeActivated(address indexed activator);

    event FraudProofSubmitted(
        uint256 indexed batchIndex,
        address indexed challenger,
        bytes32 fraudProofHash
    );

    // ============================================
    // ERRORS
    // ============================================

    error Paused();
    error NotInEmergencyMode();
    error InvalidStateRoot();
    error BatchNotFound();
    error BatchAlreadyFinalized();
    error ChallengePeriodActive();
    error ChallengePeriodExpired();
    error ChallengeAlreadyExists();
    error InvalidFraudProof();
    error InvalidWithdrawalProof();
    error WithdrawalNotReady();
    error WithdrawalAlreadyClaimed();
    error InsufficientBond();
    error TokenNotSupported();
    error AmountExceedsLimit();

    // ============================================
    // MODIFIERS
    // ============================================

    modifier whenNotPaused() {
        if (paused) revert Paused();
        _;
    }

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(GUARDIAN_ROLE, msg.sender);
    }

    // ============================================
    // DEPOSIT FUNCTIONS
    // ============================================

    /**
     * @notice Deposit ETH to L2
     * @param l2Recipient The recipient address on L2
     */
    function depositETH(uint256 l2Recipient) external payable nonReentrant whenNotPaused {
        if (msg.value == 0 || msg.value > MAX_DEPOSIT_AMOUNT) {
            revert AmountExceedsLimit();
        }

        uint256 depositId = ++depositCounter;
        
        deposits[depositId] = Deposit({
            depositor: msg.sender,
            token: address(0), // ETH
            amount: msg.value,
            l2Recipient: l2Recipient,
            timestamp: block.timestamp,
            processed: false
        });

        emit DepositInitiated(depositId, msg.sender, address(0), msg.value, l2Recipient);
    }

    /**
     * @notice Deposit ERC20 tokens to L2
     * @param token The L1 token address
     * @param amount Amount to deposit
     * @param l2Recipient The recipient address on L2
     */
    function depositERC20(
        address token,
        uint256 amount,
        uint256 l2Recipient
    ) external nonReentrant whenNotPaused {
        if (!supportedTokens[token]) revert TokenNotSupported();
        if (amount == 0) revert AmountExceedsLimit();

        // Transfer tokens to bridge
        // In production, use SafeERC20
        
        uint256 depositId = ++depositCounter;
        
        deposits[depositId] = Deposit({
            depositor: msg.sender,
            token: token,
            amount: amount,
            l2Recipient: l2Recipient,
            timestamp: block.timestamp,
            processed: false
        });

        emit DepositInitiated(depositId, msg.sender, token, amount, l2Recipient);
    }

    // ============================================
    // STATE SUBMISSION (SEQUENCER)
    // ============================================

    /**
     * @notice Submit a new state batch (sequencer only)
     * @param stateRoot The Merkle root of L2 state
     * @param withdrawalRoot The Merkle root of pending withdrawals
     * @param l2BlockNumber The L2 block number
     */
    function submitStateBatch(
        bytes32 stateRoot,
        bytes32 withdrawalRoot,
        uint256 l2BlockNumber
    ) external onlyRole(SEQUENCER_ROLE) whenNotPaused {
        if (proposerBonds[msg.sender] < PROPOSER_BOND) {
            revert InsufficientBond();
        }

        uint256 batchIndex = ++currentBatchIndex;
        bytes32 previousRoot = currentBatchIndex > 1 
            ? stateBatches[currentBatchIndex - 1].stateRoot 
            : bytes32(0);

        stateBatches[batchIndex] = StateBatch({
            stateRoot: stateRoot,
            withdrawalRoot: withdrawalRoot,
            previousStateRoot: previousRoot,
            batchIndex: batchIndex,
            l2BlockNumber: l2BlockNumber,
            timestamp: block.timestamp,
            challengeDeadline: block.timestamp + CHALLENGE_PERIOD,
            proposer: msg.sender,
            finalized: false,
            challenged: false
        });

        emit StateBatchSubmitted(batchIndex, stateRoot, withdrawalRoot, msg.sender);
    }

    /**
     * @notice Finalize a batch after challenge period
     * @param batchIndex The batch to finalize
     */
    function finalizeBatch(uint256 batchIndex) external {
        StateBatch storage batch = stateBatches[batchIndex];
        
        if (batch.stateRoot == bytes32(0)) revert BatchNotFound();
        if (batch.finalized) revert BatchAlreadyFinalized();
        if (block.timestamp < batch.challengeDeadline) revert ChallengePeriodActive();
        if (batch.challenged && challenges[batchIndex].status == ChallengeStatus.Active) {
            revert ChallengePeriodActive();
        }

        batch.finalized = true;
        lastFinalizedBatch = batchIndex;

        emit BatchFinalized(batchIndex, batch.stateRoot);
    }

    // ============================================
    // FRAUD PROOF SYSTEM
    // ============================================

    /**
     * @notice Initiate a challenge against a state batch
     * @param batchIndex The batch to challenge
     */
    function initiateChallenge(uint256 batchIndex) external payable nonReentrant {
        StateBatch storage batch = stateBatches[batchIndex];
        
        if (batch.stateRoot == bytes32(0)) revert BatchNotFound();
        if (batch.finalized) revert BatchAlreadyFinalized();
        if (block.timestamp >= batch.challengeDeadline) revert ChallengePeriodExpired();
        if (challenges[batchIndex].status == ChallengeStatus.Active) {
            revert ChallengeAlreadyExists();
        }
        if (msg.value < CHALLENGE_BOND) revert InsufficientBond();

        batch.challenged = true;
        
        challenges[batchIndex] = Challenge({
            batchRoot: batch.stateRoot,
            batchIndex: batchIndex,
            challenger: msg.sender,
            bondAmount: msg.value,
            deadline: block.timestamp + FRAUD_PROOF_WINDOW,
            status: ChallengeStatus.Active,
            fraudProofHash: bytes32(0)
        });

        emit ChallengeInitiated(batchIndex, msg.sender, msg.value);
    }

    /**
     * @notice Submit a fraud proof for an active challenge
     * @param batchIndex The challenged batch
     * @param preStateRoot State root before the fraudulent transition
     * @param postStateRoot Claimed post-state root
     * @param proof The fraud proof data
     */
    function submitFraudProof(
        uint256 batchIndex,
        bytes32 preStateRoot,
        bytes32 postStateRoot,
        bytes calldata proof
    ) external nonReentrant {
        Challenge storage challenge = challenges[batchIndex];
        
        if (challenge.status != ChallengeStatus.Active) {
            revert ChallengeAlreadyExists();
        }
        if (msg.sender != challenge.challenger) {
            revert InvalidFraudProof();
        }

        // Verify fraud proof
        // In production, this would verify:
        // 1. Pre-state root matches previous batch
        // 2. Post-state root doesn't match claimed state root
        // 3. State transition is invalid according to L2 rules
        
        bytes32 fraudProofHash = keccak256(abi.encodePacked(preStateRoot, postStateRoot, proof));
        bool isValid = _verifyFraudProof(batchIndex, preStateRoot, postStateRoot, proof);

        if (isValid) {
            // Fraud proven - challenger wins
            challenge.status = ChallengeStatus.ChallengerWon;
            challenge.fraudProofHash = fraudProofHash;

            // Slash proposer bond
            address proposer = stateBatches[batchIndex].proposer;
            uint256 slashedAmount = proposerBonds[proposer];
            proposerBonds[proposer] = 0;

            // Reward challenger
            uint256 reward = challenge.bondAmount + (slashedAmount / 2);
            payable(challenge.challenger).transfer(reward);

            // Invalidate batch and all subsequent batches
            _invalidateBatches(batchIndex);

            emit ChallengeResolved(batchIndex, ChallengeStatus.ChallengerWon, challenge.challenger);
        } else {
            revert InvalidFraudProof();
        }

        emit FraudProofSubmitted(batchIndex, challenge.challenger, fraudProofHash);
    }

    /**
     * @notice Resolve expired challenge (proposer wins by default)
     */
    function resolveExpiredChallenge(uint256 batchIndex) external {
        Challenge storage challenge = challenges[batchIndex];
        
        if (challenge.status != ChallengeStatus.Active) {
            revert ChallengeAlreadyExists();
        }
        if (block.timestamp <= challenge.deadline) {
            revert ChallengePeriodActive();
        }

        // Challenger failed to prove fraud
        challenge.status = ChallengeStatus.ProposerWon;
        
        // Proposer gets challenge bond
        address proposer = stateBatches[batchIndex].proposer;
        payable(proposer).transfer(challenge.bondAmount);

        emit ChallengeResolved(batchIndex, ChallengeStatus.ProposerWon, proposer);
    }

    /**
     * @notice Verify a fraud proof (simplified)
     */
    function _verifyFraudProof(
        uint256 batchIndex,
        bytes32 preStateRoot,
        bytes32 postStateRoot,
        bytes calldata proof
    ) internal view returns (bool) {
        StateBatch memory batch = stateBatches[batchIndex];

        // Verify pre-state matches previous batch
        if (batch.previousStateRoot != preStateRoot) {
            return false;
        }

        // Verify claimed post-state doesn't match actual
        if (postStateRoot == batch.stateRoot) {
            return false;
        }

        // In production, execute the disputed state transition
        // and verify the result doesn't match claimed state root
        
        // Simplified: check proof length (real implementation is complex)
        return proof.length > 0;
    }

    /**
     * @notice Invalidate batches from index onwards
     */
    function _invalidateBatches(uint256 fromIndex) internal {
        for (uint256 i = fromIndex; i <= currentBatchIndex; i++) {
            stateBatches[i].finalized = false;
            stateBatches[i].stateRoot = bytes32(0);
        }
        currentBatchIndex = fromIndex - 1;
    }

    // ============================================
    // WITHDRAWAL FUNCTIONS
    // ============================================

    /**
     * @notice Prove a withdrawal using Merkle proof
     * @param recipient The recipient address
     * @param token The token to withdraw
     * @param amount The amount to withdraw
     * @param batchIndex The batch containing this withdrawal
     * @param proof Merkle proof of inclusion
     */
    function proveWithdrawal(
        address recipient,
        address token,
        uint256 amount,
        uint256 batchIndex,
        bytes32[] calldata proof
    ) external nonReentrant {
        StateBatch memory batch = stateBatches[batchIndex];
        
        if (!batch.finalized) revert WithdrawalNotReady();

        // Create withdrawal hash
        bytes32 withdrawalHash = keccak256(abi.encodePacked(
            recipient,
            token,
            amount,
            batchIndex
        ));

        // Verify Merkle proof
        bytes32 leaf = keccak256(abi.encodePacked(withdrawalHash));
        if (!MerkleProof.verify(proof, batch.withdrawalRoot, leaf)) {
            revert InvalidWithdrawalProof();
        }

        // Store proven withdrawal
        withdrawals[withdrawalHash] = Withdrawal({
            recipient: recipient,
            token: token,
            amount: amount,
            stateRoot: batch.stateRoot,
            batchIndex: batchIndex,
            proof: proof,
            claimed: false
        });

        emit WithdrawalProven(withdrawalHash, recipient, token, amount);
    }

    /**
     * @notice Claim a proven withdrawal
     * @param withdrawalHash The withdrawal to claim
     */
    function claimWithdrawal(bytes32 withdrawalHash) external nonReentrant {
        Withdrawal storage withdrawal = withdrawals[withdrawalHash];
        
        if (withdrawal.claimed) revert WithdrawalAlreadyClaimed();
        if (withdrawal.recipient != msg.sender) revert InvalidWithdrawalProof();

        // Verify batch is still valid
        StateBatch memory batch = stateBatches[withdrawal.batchIndex];
        if (!batch.finalized || batch.stateRoot != withdrawal.stateRoot) {
            revert WithdrawalNotReady();
        }

        withdrawal.claimed = true;
        processedWithdrawals[withdrawalHash] = true;

        // Transfer funds
        if (withdrawal.token == address(0)) {
            payable(msg.sender).transfer(withdrawal.amount);
        } else {
            // Transfer ERC20 (use SafeERC20 in production)
        }

        emit WithdrawalClaimed(withdrawalHash, msg.sender, withdrawal.amount);
    }

    // ============================================
    // EMERGENCY FUNCTIONS
    // ============================================

    /**
     * @notice Activate emergency mode (guardian only)
     */
    function activateEmergencyMode() external onlyRole(GUARDIAN_ROLE) {
        emergencyMode = true;
        emit EmergencyModeActivated(msg.sender);
    }

    /**
     * @notice Emergency exit for users when bridge is compromised
     * @param depositId The deposit to exit
     */
    function emergencyExit(uint256 depositId) external nonReentrant {
        if (!emergencyMode) revert NotInEmergencyMode();

        Deposit storage deposit = deposits[depositId];
        if (deposit.depositor != msg.sender) revert InvalidWithdrawalProof();
        if (deposit.processed) revert WithdrawalAlreadyClaimed();

        deposit.processed = true;

        if (deposit.token == address(0)) {
            payable(msg.sender).transfer(deposit.amount);
        } else {
            // Transfer ERC20
        }
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function pause() external onlyRole(GUARDIAN_ROLE) {
        paused = true;
    }

    function unpause() external onlyRole(DEFAULT_ADMIN_ROLE) {
        paused = false;
    }

    function addSupportedToken(address token, address l2Token) external onlyRole(DEFAULT_ADMIN_ROLE) {
        supportedTokens[token] = true;
        l1ToL2Token[token] = l2Token;
        l2ToL1Token[l2Token] = token;
    }

    function depositProposerBond() external payable onlyRole(SEQUENCER_ROLE) {
        proposerBonds[msg.sender] += msg.value;
    }

    function withdrawProposerBond(uint256 amount) external onlyRole(SEQUENCER_ROLE) {
        require(proposerBonds[msg.sender] >= amount + PROPOSER_BOND, "Must maintain minimum bond");
        proposerBonds[msg.sender] -= amount;
        payable(msg.sender).transfer(amount);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getBatchInfo(uint256 batchIndex) external view returns (
        bytes32 stateRoot,
        bytes32 withdrawalRoot,
        uint256 timestamp,
        bool finalized,
        bool challenged
    ) {
        StateBatch memory batch = stateBatches[batchIndex];
        return (
            batch.stateRoot,
            batch.withdrawalRoot,
            batch.timestamp,
            batch.finalized,
            batch.challenged
        );
    }

    function getChallengeInfo(uint256 batchIndex) external view returns (
        address challenger,
        uint256 bondAmount,
        uint256 deadline,
        ChallengeStatus status
    ) {
        Challenge memory challenge = challenges[batchIndex];
        return (
            challenge.challenger,
            challenge.bondAmount,
            challenge.deadline,
            challenge.status
        );
    }

    receive() external payable {}
}
