// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title StateCommitmentChain
 * @notice Manages L2 state commitments on L1 with fraud proof verification
 * @dev Implements state root chain with bisection-based dispute resolution
 * 
 * Core Concepts:
 * - State commitments form a chain anchored to L1
 * - Interactive fraud proofs using bisection game
 * - Single-step execution for final verification
 * - Bonds required for proposers and challengers
 */
contract StateCommitmentChain is AccessControl, ReentrancyGuard {
    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant PROPOSER_ROLE = keccak256("PROPOSER_ROLE");
    bytes32 public constant VERIFIER_ROLE = keccak256("VERIFIER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice State commitment structure
    struct StateCommitment {
        bytes32 stateRoot;           // Root of L2 state trie
        bytes32 prevStateRoot;       // Previous state root
        bytes32 transactionRoot;     // Root of transactions in this commitment
        uint64 l2BlockStart;         // First L2 block in this commitment
        uint64 l2BlockEnd;           // Last L2 block in this commitment
        uint64 timestamp;            // L1 submission timestamp
        uint64 challengeDeadline;    // End of challenge period
        address proposer;
        CommitmentStatus status;
    }

    /// @notice Interactive dispute game
    struct DisputeGame {
        uint256 commitmentIndex;     // Commitment being disputed
        address challenger;
        address defender;            // Original proposer
        uint256 challengerBond;
        uint256 defenderBond;
        uint64 moveDeadline;         // Deadline for next move
        uint64 movesLeft;            // Remaining bisection steps
        GamePhase phase;
        bytes32 claimedStateRoot;    // Challenger's claimed correct root
        BisectionState bisection;
    }

    /// @notice Bisection state for interactive fraud proof
    struct BisectionState {
        uint64 lowerBound;           // Lower L2 block bound
        uint64 upperBound;           // Upper L2 block bound
        bytes32 lowerStateRoot;      // State at lower bound
        bytes32 upperStateRoot;      // State at upper bound
        bytes32 midpointStateRoot;   // Claimed state at midpoint
        bool defenderTurn;           // Whose turn it is
    }

    /// @notice Single-step execution proof
    struct ExecutionProof {
        bytes32 preStateRoot;
        bytes32 postStateRoot;
        bytes transaction;
        bytes stateProof;            // Merkle proof for accessed state
        bytes executionTrace;        // Trace for verification
    }

    enum CommitmentStatus {
        Pending,
        Finalized,
        Disputed,
        Invalid
    }

    enum GamePhase {
        NotStarted,
        Bisecting,
        SingleStepExecution,
        Resolved
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant MIN_BOND = 0.1 ether;
    uint256 public constant PROPOSAL_BOND = 1 ether;
    uint64 public constant CHALLENGE_PERIOD = 7 days;
    uint64 public constant MOVE_TIMEOUT = 1 days;
    uint64 public constant MAX_BISECTION_STEPS = 40; // log2(1e12)

    // ============================================
    // STATE
    // ============================================

    /// @notice State commitments by index
    mapping(uint256 => StateCommitment) public commitments;
    uint256 public commitmentCount;

    /// @notice Latest finalized commitment
    uint256 public lastFinalizedIndex;

    /// @notice Dispute games by commitment index
    mapping(uint256 => DisputeGame) public disputes;

    /// @notice Proposer bonds
    mapping(address => uint256) public proposerBonds;

    /// @notice Verified execution lookup
    mapping(bytes32 => bool) public verifiedExecutions;

    // ============================================
    // EVENTS
    // ============================================

    event StateCommitted(
        uint256 indexed index,
        bytes32 stateRoot,
        uint64 l2BlockStart,
        uint64 l2BlockEnd,
        address proposer
    );

    event CommitmentFinalized(uint256 indexed index, bytes32 stateRoot);

    event DisputeInitiated(
        uint256 indexed commitmentIndex,
        address challenger,
        bytes32 claimedCorrectRoot
    );

    event BisectionMove(
        uint256 indexed commitmentIndex,
        address mover,
        uint64 lowerBound,
        uint64 upperBound,
        bytes32 midpointRoot
    );

    event SingleStepChallenged(
        uint256 indexed commitmentIndex,
        uint64 blockNumber,
        bytes32 preState,
        bytes32 postState
    );

    event DisputeResolved(
        uint256 indexed commitmentIndex,
        address winner,
        uint256 reward
    );

    // ============================================
    // ERRORS
    // ============================================

    error InvalidCommitment();
    error CommitmentNotFound();
    error CommitmentAlreadyFinalized();
    error ChallengePeriodNotOver();
    error DisputeInProgress();
    error NotYourTurn();
    error MoveTimeout();
    error InvalidBisectionMove();
    error InvalidExecutionProof();
    error InsufficientBond();
    error GameNotInBisection();
    error GameNotInSingleStep();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
    }

    // ============================================
    // COMMITMENT FUNCTIONS
    // ============================================

    /**
     * @notice Submit a new state commitment
     * @param stateRoot The state root for this commitment
     * @param transactionRoot Root of transactions
     * @param l2BlockStart First L2 block
     * @param l2BlockEnd Last L2 block
     */
    function submitCommitment(
        bytes32 stateRoot,
        bytes32 transactionRoot,
        uint64 l2BlockStart,
        uint64 l2BlockEnd
    ) external onlyRole(PROPOSER_ROLE) {
        if (proposerBonds[msg.sender] < PROPOSAL_BOND) {
            revert InsufficientBond();
        }

        bytes32 prevRoot = commitmentCount > 0 
            ? commitments[commitmentCount].stateRoot 
            : bytes32(0);

        // Validate block range
        if (l2BlockEnd <= l2BlockStart) revert InvalidCommitment();
        if (commitmentCount > 0 && l2BlockStart != commitments[commitmentCount].l2BlockEnd + 1) {
            revert InvalidCommitment();
        }

        uint256 index = ++commitmentCount;

        commitments[index] = StateCommitment({
            stateRoot: stateRoot,
            prevStateRoot: prevRoot,
            transactionRoot: transactionRoot,
            l2BlockStart: l2BlockStart,
            l2BlockEnd: l2BlockEnd,
            timestamp: uint64(block.timestamp),
            challengeDeadline: uint64(block.timestamp) + CHALLENGE_PERIOD,
            proposer: msg.sender,
            status: CommitmentStatus.Pending
        });

        emit StateCommitted(index, stateRoot, l2BlockStart, l2BlockEnd, msg.sender);
    }

    /**
     * @notice Finalize a commitment after challenge period
     * @param index The commitment index
     */
    function finalizeCommitment(uint256 index) external {
        StateCommitment storage commitment = commitments[index];

        if (commitment.stateRoot == bytes32(0)) revert CommitmentNotFound();
        if (commitment.status == CommitmentStatus.Finalized) revert CommitmentAlreadyFinalized();
        if (commitment.status == CommitmentStatus.Disputed) revert DisputeInProgress();
        if (block.timestamp < commitment.challengeDeadline) revert ChallengePeriodNotOver();

        // Ensure all previous commitments are finalized
        if (index > 1 && commitments[index - 1].status != CommitmentStatus.Finalized) {
            revert InvalidCommitment();
        }

        commitment.status = CommitmentStatus.Finalized;
        lastFinalizedIndex = index;

        emit CommitmentFinalized(index, commitment.stateRoot);
    }

    // ============================================
    // DISPUTE GAME FUNCTIONS
    // ============================================

    /**
     * @notice Initiate a dispute against a commitment
     * @param commitmentIndex The commitment to dispute
     * @param claimedCorrectRoot The state root challenger claims is correct
     */
    function initiateDispute(
        uint256 commitmentIndex,
        bytes32 claimedCorrectRoot
    ) external payable nonReentrant {
        StateCommitment storage commitment = commitments[commitmentIndex];

        if (commitment.stateRoot == bytes32(0)) revert CommitmentNotFound();
        if (commitment.status != CommitmentStatus.Pending) revert InvalidCommitment();
        if (block.timestamp >= commitment.challengeDeadline) revert ChallengePeriodNotOver();
        if (msg.value < MIN_BOND) revert InsufficientBond();

        commitment.status = CommitmentStatus.Disputed;

        disputes[commitmentIndex] = DisputeGame({
            commitmentIndex: commitmentIndex,
            challenger: msg.sender,
            defender: commitment.proposer,
            challengerBond: msg.value,
            defenderBond: PROPOSAL_BOND,
            moveDeadline: uint64(block.timestamp) + MOVE_TIMEOUT,
            movesLeft: MAX_BISECTION_STEPS,
            phase: GamePhase.Bisecting,
            claimedStateRoot: claimedCorrectRoot,
            bisection: BisectionState({
                lowerBound: commitment.l2BlockStart,
                upperBound: commitment.l2BlockEnd,
                lowerStateRoot: commitment.prevStateRoot,
                upperStateRoot: commitment.stateRoot,
                midpointStateRoot: bytes32(0),
                defenderTurn: true
            })
        });

        emit DisputeInitiated(commitmentIndex, msg.sender, claimedCorrectRoot);
    }

    /**
     * @notice Make a bisection move in the dispute
     * @param commitmentIndex The disputed commitment
     * @param midpointStateRoot State root at midpoint of current range
     */
    function bisect(
        uint256 commitmentIndex,
        bytes32 midpointStateRoot
    ) external nonReentrant {
        DisputeGame storage game = disputes[commitmentIndex];

        if (game.phase != GamePhase.Bisecting) revert GameNotInBisection();
        if (block.timestamp > game.moveDeadline) revert MoveTimeout();

        bool isDefender = msg.sender == game.defender;
        bool isChallenger = msg.sender == game.challenger;

        if (game.bisection.defenderTurn) {
            if (!isDefender) revert NotYourTurn();
        } else {
            if (!isChallenger) revert NotYourTurn();
        }

        BisectionState storage bis = game.bisection;
        uint64 midpoint = (bis.lowerBound + bis.upperBound) / 2;

        if (game.bisection.defenderTurn) {
            // Defender provides midpoint state root
            bis.midpointStateRoot = midpointStateRoot;
        } else {
            // Challenger indicates which half is disputed
            // If challenger disputes lower half
            if (midpointStateRoot == bis.lowerStateRoot) {
                bis.upperBound = midpoint;
                bis.upperStateRoot = bis.midpointStateRoot;
            } else {
                // Challenger disputes upper half
                bis.lowerBound = midpoint;
                bis.lowerStateRoot = bis.midpointStateRoot;
            }
            bis.midpointStateRoot = bytes32(0);
        }

        // Switch turns
        bis.defenderTurn = !bis.defenderTurn;
        game.moveDeadline = uint64(block.timestamp) + MOVE_TIMEOUT;
        game.movesLeft--;

        // Check if we've narrowed to single step
        if (bis.upperBound - bis.lowerBound == 1) {
            game.phase = GamePhase.SingleStepExecution;
        }

        emit BisectionMove(
            commitmentIndex,
            msg.sender,
            bis.lowerBound,
            bis.upperBound,
            midpointStateRoot
        );
    }

    /**
     * @notice Execute single-step verification
     * @param commitmentIndex The disputed commitment
     * @param proof The execution proof
     */
    function executeSingleStep(
        uint256 commitmentIndex,
        ExecutionProof calldata proof
    ) external nonReentrant {
        DisputeGame storage game = disputes[commitmentIndex];

        if (game.phase != GamePhase.SingleStepExecution) revert GameNotInSingleStep();

        BisectionState memory bis = game.bisection;

        // Verify pre-state matches
        if (proof.preStateRoot != bis.lowerStateRoot) {
            revert InvalidExecutionProof();
        }

        // Execute the transaction and verify result
        bytes32 computedPostRoot = _executeTransaction(
            proof.preStateRoot,
            proof.transaction,
            proof.stateProof,
            proof.executionTrace
        );

        address winner;
        uint256 reward;

        if (computedPostRoot == bis.upperStateRoot) {
            // Defender was correct
            winner = game.defender;
            reward = game.challengerBond;
            proposerBonds[game.defender] += reward;
        } else {
            // Challenger was correct (fraud proven)
            winner = game.challenger;
            reward = game.challengerBond + game.defenderBond;
            payable(winner).transfer(reward);

            // Mark commitment as invalid
            commitments[commitmentIndex].status = CommitmentStatus.Invalid;

            // Slash proposer
            proposerBonds[game.defender] = 0;
        }

        game.phase = GamePhase.Resolved;

        emit DisputeResolved(commitmentIndex, winner, reward);
        emit SingleStepChallenged(
            commitmentIndex,
            bis.lowerBound,
            bis.lowerStateRoot,
            bis.upperStateRoot
        );
    }

    /**
     * @notice Claim victory on opponent timeout
     * @param commitmentIndex The disputed commitment
     */
    function claimTimeoutVictory(uint256 commitmentIndex) external nonReentrant {
        DisputeGame storage game = disputes[commitmentIndex];

        if (game.phase == GamePhase.Resolved) revert InvalidCommitment();
        if (block.timestamp <= game.moveDeadline) revert MoveTimeout();

        address winner;
        uint256 reward;

        if (game.bisection.defenderTurn) {
            // Defender timed out, challenger wins
            winner = game.challenger;
            reward = game.challengerBond + game.defenderBond;
            payable(winner).transfer(reward);
            commitments[commitmentIndex].status = CommitmentStatus.Invalid;
            proposerBonds[game.defender] = 0;
        } else {
            // Challenger timed out, defender wins
            winner = game.defender;
            reward = game.challengerBond;
            proposerBonds[game.defender] += reward;
            commitments[commitmentIndex].status = CommitmentStatus.Pending;
            commitments[commitmentIndex].challengeDeadline = uint64(block.timestamp) + CHALLENGE_PERIOD;
        }

        game.phase = GamePhase.Resolved;

        emit DisputeResolved(commitmentIndex, winner, reward);
    }

    /**
     * @notice Execute a transaction and compute resulting state root
     * @dev Simplified - real implementation would use EVM execution
     */
    function _executeTransaction(
        bytes32 preStateRoot,
        bytes memory transaction,
        bytes memory stateProof,
        bytes memory executionTrace
    ) internal pure returns (bytes32) {
        // In production, this would:
        // 1. Load necessary state from proof
        // 2. Execute the transaction step by step
        // 3. Compute the resulting state root
        
        // Simplified: hash inputs to get deterministic output
        return keccak256(abi.encodePacked(
            preStateRoot,
            keccak256(transaction),
            keccak256(stateProof),
            keccak256(executionTrace)
        ));
    }

    // ============================================
    // BOND MANAGEMENT
    // ============================================

    function depositBond() external payable onlyRole(PROPOSER_ROLE) {
        proposerBonds[msg.sender] += msg.value;
    }

    function withdrawBond(uint256 amount) external onlyRole(PROPOSER_ROLE) {
        require(proposerBonds[msg.sender] >= amount + PROPOSAL_BOND, "Maintain min bond");
        proposerBonds[msg.sender] -= amount;
        payable(msg.sender).transfer(amount);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getCommitment(uint256 index) external view returns (
        bytes32 stateRoot,
        bytes32 transactionRoot,
        uint64 l2BlockStart,
        uint64 l2BlockEnd,
        CommitmentStatus status
    ) {
        StateCommitment memory c = commitments[index];
        return (c.stateRoot, c.transactionRoot, c.l2BlockStart, c.l2BlockEnd, c.status);
    }

    function getDisputeState(uint256 commitmentIndex) external view returns (
        GamePhase phase,
        address challenger,
        uint64 lowerBound,
        uint64 upperBound,
        bool defenderTurn
    ) {
        DisputeGame memory g = disputes[commitmentIndex];
        return (
            g.phase,
            g.challenger,
            g.bisection.lowerBound,
            g.bisection.upperBound,
            g.bisection.defenderTurn
        );
    }

    function isCommitmentFinalized(uint256 index) external view returns (bool) {
        return commitments[index].status == CommitmentStatus.Finalized;
    }

    receive() external payable {}
}
