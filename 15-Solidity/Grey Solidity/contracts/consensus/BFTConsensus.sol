// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title BFTConsensus
 * @author Grey Protocol
 * @notice Byzantine Fault Tolerant consensus mechanism implementation
 * @dev Implements PBFT-style consensus with pre-prepare, prepare, and commit phases
 * 
 * Features:
 * - Three-phase consensus (pre-prepare, prepare, commit)
 * - View change protocol for leader rotation
 * - Checkpoint mechanism for state pruning
 * - Byzantine fault tolerance (tolerates f < n/3 malicious nodes)
 * - Message aggregation for gas efficiency
 */
contract BFTConsensus is AccessControl, ReentrancyGuard {
    using ECDSA for bytes32;

    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant VALIDATOR_ROLE = keccak256("VALIDATOR_ROLE");
    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");

    /// @notice Message types
    uint8 public constant MSG_PRE_PREPARE = 0;
    uint8 public constant MSG_PREPARE = 1;
    uint8 public constant MSG_COMMIT = 2;
    uint8 public constant MSG_VIEW_CHANGE = 3;
    uint8 public constant MSG_NEW_VIEW = 4;

    /// @notice Consensus phases
    uint8 public constant PHASE_IDLE = 0;
    uint8 public constant PHASE_PRE_PREPARE = 1;
    uint8 public constant PHASE_PREPARE = 2;
    uint8 public constant PHASE_COMMIT = 3;
    uint8 public constant PHASE_FINALIZED = 4;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Consensus message
    struct ConsensusMessage {
        uint8 msgType;
        uint256 viewNumber;
        uint256 sequence;
        bytes32 digest;
        address sender;
        bytes signature;
        uint256 timestamp;
    }

    /// @notice Proposal for consensus
    struct Proposal {
        uint256 sequence;
        uint256 viewNumber;
        bytes32 proposalHash;
        bytes data;
        address proposer;
        uint256 timestamp;
        uint8 phase;
        bool executed;
    }

    /// @notice Vote record
    struct VoteRecord {
        uint256 prepareCount;
        uint256 commitCount;
        mapping(address => bool) hasPrepared;
        mapping(address => bool) hasCommitted;
        bool prepared;
        bool committed;
    }

    /// @notice View change request
    struct ViewChangeRequest {
        uint256 newView;
        uint256 lastSequence;
        bytes32 lastDigest;
        address requester;
        bytes signature;
    }

    /// @notice Checkpoint for state pruning
    struct Checkpoint {
        uint256 sequence;
        bytes32 stateDigest;
        uint256 timestamp;
        uint256 confirmations;
        mapping(address => bool) hasConfirmed;
        bool stable;
    }

    /// @notice Validator info
    struct ValidatorInfo {
        address addr;
        uint256 weight;
        bool active;
        uint256 lastActivity;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Current view number
    uint256 public currentView;

    /// @notice Current sequence number
    uint256 public currentSequence;

    /// @notice Last executed sequence
    uint256 public lastExecutedSequence;

    /// @notice Last stable checkpoint sequence
    uint256 public lastStableCheckpoint;

    /// @notice Checkpoint interval
    uint256 public checkpointInterval;

    /// @notice View change timeout (blocks)
    uint256 public viewChangeTimeout;

    /// @notice Last block with activity
    uint256 public lastActivityBlock;

    /// @notice Total validator weight
    uint256 public totalWeight;

    /// @notice Number of validators
    uint256 public validatorCount;

    /// @notice Is consensus active
    bool public consensusActive;

    // ============================================
    // MAPPINGS
    // ============================================

    /// @notice Sequence => Proposal
    mapping(uint256 => Proposal) public proposals;

    /// @notice Sequence => Vote record
    mapping(uint256 => VoteRecord) private voteRecords;

    /// @notice Sequence => Checkpoint
    mapping(uint256 => Checkpoint) private checkpoints;

    /// @notice Validator address => info
    mapping(address => ValidatorInfo) public validators;

    /// @notice View => view change requests
    mapping(uint256 => mapping(address => ViewChangeRequest)) public viewChangeRequests;

    /// @notice View => count of view change requests
    mapping(uint256 => uint256) public viewChangeRequestCount;

    /// @notice Digest => already processed
    mapping(bytes32 => bool) public processedDigests;

    /// @notice Active validator list
    address[] public validatorList;

    // ============================================
    // EVENTS
    // ============================================

    event ProposalCreated(
        uint256 indexed sequence,
        uint256 indexed viewNumber,
        bytes32 proposalHash,
        address proposer
    );

    event PrepareReceived(
        uint256 indexed sequence,
        address indexed validator,
        uint256 count
    );

    event CommitReceived(
        uint256 indexed sequence,
        address indexed validator,
        uint256 count
    );

    event ProposalPrepared(uint256 indexed sequence, bytes32 proposalHash);

    event ProposalCommitted(uint256 indexed sequence, bytes32 proposalHash);

    event ProposalExecuted(
        uint256 indexed sequence,
        bytes32 proposalHash,
        bool success
    );

    event ViewChanged(uint256 indexed oldView, uint256 indexed newView, address newPrimary);

    event CheckpointCreated(uint256 indexed sequence, bytes32 stateDigest);

    event CheckpointStable(uint256 indexed sequence);

    event ValidatorAdded(address indexed validator, uint256 weight);

    event ValidatorRemoved(address indexed validator);

    // ============================================
    // ERRORS
    // ============================================

    error NotPrimary();
    error InvalidView();
    error InvalidSequence();
    error InvalidPhase();
    error AlreadyVoted();
    error InvalidSignature();
    error ProposalNotPrepared();
    error ProposalNotCommitted();
    error ProposalAlreadyExecuted();
    error ConsensusNotActive();
    error InsufficientValidators();
    error InvalidValidator();
    error ViewChangeInProgress();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(uint256 _checkpointInterval, uint256 _viewChangeTimeout) {
        checkpointInterval = _checkpointInterval;
        viewChangeTimeout = _viewChangeTimeout;
        currentView = 0;
        currentSequence = 0;
        lastActivityBlock = block.number;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(GOVERNANCE_ROLE, msg.sender);
    }

    // ============================================
    // MODIFIERS
    // ============================================

    modifier onlyPrimary() {
        if (getPrimary() != msg.sender) revert NotPrimary();
        _;
    }

    modifier onlyValidator() {
        if (!validators[msg.sender].active) revert InvalidValidator();
        _;
    }

    modifier whenActive() {
        if (!consensusActive) revert ConsensusNotActive();
        _;
    }

    // ============================================
    // CONSENSUS PROTOCOL
    // ============================================

    /**
     * @notice Primary broadcasts pre-prepare message (Phase 1)
     * @param data Proposal data
     */
    function prePrepare(bytes calldata data) 
        external 
        onlyPrimary 
        whenActive 
        returns (uint256 sequence) 
    {
        sequence = ++currentSequence;
        bytes32 proposalHash = keccak256(abi.encodePacked(currentView, sequence, data));

        proposals[sequence] = Proposal({
            sequence: sequence,
            viewNumber: currentView,
            proposalHash: proposalHash,
            data: data,
            proposer: msg.sender,
            timestamp: block.timestamp,
            phase: PHASE_PRE_PREPARE,
            executed: false
        });

        // Primary's pre-prepare counts as a prepare vote
        voteRecords[sequence].hasPrepared[msg.sender] = true;
        voteRecords[sequence].prepareCount = validators[msg.sender].weight;

        lastActivityBlock = block.number;

        emit ProposalCreated(sequence, currentView, proposalHash, msg.sender);
    }

    /**
     * @notice Validator sends prepare message (Phase 2)
     * @param sequence Sequence number
     * @param proposalHash Hash of the proposal to prepare
     * @param signature Validator signature
     */
    function prepare(
        uint256 sequence,
        bytes32 proposalHash,
        bytes calldata signature
    ) external onlyValidator whenActive {
        Proposal storage proposal = proposals[sequence];
        VoteRecord storage votes = voteRecords[sequence];

        // Validate
        if (proposal.proposalHash != proposalHash) revert InvalidSequence();
        if (proposal.phase != PHASE_PRE_PREPARE && proposal.phase != PHASE_PREPARE) 
            revert InvalidPhase();
        if (votes.hasPrepared[msg.sender]) revert AlreadyVoted();

        // Verify signature
        bytes32 messageHash = keccak256(abi.encodePacked(
            MSG_PREPARE, currentView, sequence, proposalHash
        ));
        _verifySignature(messageHash, signature, msg.sender);

        // Record vote
        votes.hasPrepared[msg.sender] = true;
        votes.prepareCount += validators[msg.sender].weight;

        emit PrepareReceived(sequence, msg.sender, votes.prepareCount);

        // Check if prepared (2f + 1 prepares)
        if (!votes.prepared && votes.prepareCount >= _quorumWeight()) {
            votes.prepared = true;
            proposal.phase = PHASE_PREPARE;
            emit ProposalPrepared(sequence, proposalHash);
        }

        lastActivityBlock = block.number;
    }

    /**
     * @notice Validator sends commit message (Phase 3)
     * @param sequence Sequence number
     * @param proposalHash Hash of the proposal to commit
     * @param signature Validator signature
     */
    function commit(
        uint256 sequence,
        bytes32 proposalHash,
        bytes calldata signature
    ) external onlyValidator whenActive {
        Proposal storage proposal = proposals[sequence];
        VoteRecord storage votes = voteRecords[sequence];

        // Validate
        if (proposal.proposalHash != proposalHash) revert InvalidSequence();
        if (!votes.prepared) revert ProposalNotPrepared();
        if (votes.hasCommitted[msg.sender]) revert AlreadyVoted();

        // Verify signature
        bytes32 messageHash = keccak256(abi.encodePacked(
            MSG_COMMIT, currentView, sequence, proposalHash
        ));
        _verifySignature(messageHash, signature, msg.sender);

        // Record vote
        votes.hasCommitted[msg.sender] = true;
        votes.commitCount += validators[msg.sender].weight;

        emit CommitReceived(sequence, msg.sender, votes.commitCount);

        // Check if committed (2f + 1 commits)
        if (!votes.committed && votes.commitCount >= _quorumWeight()) {
            votes.committed = true;
            proposal.phase = PHASE_COMMIT;
            emit ProposalCommitted(sequence, proposalHash);
        }

        lastActivityBlock = block.number;
    }

    /**
     * @notice Execute a committed proposal
     * @param sequence Sequence number
     */
    function execute(uint256 sequence) external nonReentrant whenActive {
        Proposal storage proposal = proposals[sequence];
        VoteRecord storage votes = voteRecords[sequence];

        if (!votes.committed) revert ProposalNotCommitted();
        if (proposal.executed) revert ProposalAlreadyExecuted();
        if (sequence != lastExecutedSequence + 1) revert InvalidSequence();

        proposal.executed = true;
        proposal.phase = PHASE_FINALIZED;
        lastExecutedSequence = sequence;

        // Execute the proposal (call the encoded function)
        // This would be customized based on what proposals represent
        bool success = _executeProposal(proposal.data);

        emit ProposalExecuted(sequence, proposal.proposalHash, success);

        // Create checkpoint if needed
        if (sequence % checkpointInterval == 0) {
            _createCheckpoint(sequence);
        }
    }

    // ============================================
    // VIEW CHANGE PROTOCOL
    // ============================================

    /**
     * @notice Request a view change (leader rotation)
     * @param newView The new view to switch to
     * @param signature Signature of the request
     */
    function requestViewChange(
        uint256 newView,
        bytes calldata signature
    ) external onlyValidator {
        if (newView <= currentView) revert InvalidView();

        bytes32 messageHash = keccak256(abi.encodePacked(
            MSG_VIEW_CHANGE, newView, lastExecutedSequence
        ));
        _verifySignature(messageHash, signature, msg.sender);

        viewChangeRequests[newView][msg.sender] = ViewChangeRequest({
            newView: newView,
            lastSequence: lastExecutedSequence,
            lastDigest: proposals[lastExecutedSequence].proposalHash,
            requester: msg.sender,
            signature: signature
        });

        viewChangeRequestCount[newView] += validators[msg.sender].weight;

        // Check if enough validators want view change
        if (viewChangeRequestCount[newView] >= _quorumWeight()) {
            _performViewChange(newView);
        }
    }

    /**
     * @notice Check if view change is needed due to timeout
     */
    function checkViewChangeTimeout() external {
        if (block.number > lastActivityBlock + viewChangeTimeout) {
            uint256 newView = currentView + 1;
            _performViewChange(newView);
        }
    }

    function _performViewChange(uint256 newView) internal {
        uint256 oldView = currentView;
        currentView = newView;
        lastActivityBlock = block.number;

        address newPrimary = getPrimary();
        
        emit ViewChanged(oldView, newView, newPrimary);
    }

    // ============================================
    // CHECKPOINT MECHANISM
    // ============================================

    function _createCheckpoint(uint256 sequence) internal {
        bytes32 stateDigest = keccak256(abi.encodePacked(
            sequence,
            proposals[sequence].proposalHash,
            block.timestamp
        ));

        Checkpoint storage cp = checkpoints[sequence];
        cp.sequence = sequence;
        cp.stateDigest = stateDigest;
        cp.timestamp = block.timestamp;
        cp.confirmations = validators[msg.sender].weight;
        cp.hasConfirmed[msg.sender] = true;

        emit CheckpointCreated(sequence, stateDigest);
    }

    /**
     * @notice Confirm a checkpoint
     * @param sequence Checkpoint sequence
     * @param stateDigest Expected state digest
     */
    function confirmCheckpoint(
        uint256 sequence,
        bytes32 stateDigest
    ) external onlyValidator {
        Checkpoint storage cp = checkpoints[sequence];
        
        require(cp.stateDigest == stateDigest, "Digest mismatch");
        require(!cp.hasConfirmed[msg.sender], "Already confirmed");
        require(!cp.stable, "Already stable");

        cp.hasConfirmed[msg.sender] = true;
        cp.confirmations += validators[msg.sender].weight;

        if (cp.confirmations >= _quorumWeight()) {
            cp.stable = true;
            lastStableCheckpoint = sequence;
            emit CheckpointStable(sequence);
        }
    }

    // ============================================
    // VALIDATOR MANAGEMENT
    // ============================================

    /**
     * @notice Add a validator
     */
    function addValidator(address validator, uint256 weight) 
        external 
        onlyRole(GOVERNANCE_ROLE) 
    {
        require(!validators[validator].active, "Already validator");
        require(weight > 0, "Invalid weight");

        validators[validator] = ValidatorInfo({
            addr: validator,
            weight: weight,
            active: true,
            lastActivity: block.timestamp
        });

        validatorList.push(validator);
        validatorCount++;
        totalWeight += weight;

        _grantRole(VALIDATOR_ROLE, validator);

        // Activate consensus if enough validators
        if (validatorCount >= 4) {
            consensusActive = true;
        }

        emit ValidatorAdded(validator, weight);
    }

    /**
     * @notice Remove a validator
     */
    function removeValidator(address validator) 
        external 
        onlyRole(GOVERNANCE_ROLE) 
    {
        require(validators[validator].active, "Not a validator");

        totalWeight -= validators[validator].weight;
        validators[validator].active = false;
        validatorCount--;

        _revokeRole(VALIDATOR_ROLE, validator);

        // Deactivate consensus if not enough validators
        if (validatorCount < 4) {
            consensusActive = false;
        }

        emit ValidatorRemoved(validator);
    }

    /**
     * @notice Update validator weight
     */
    function updateValidatorWeight(address validator, uint256 newWeight)
        external
        onlyRole(GOVERNANCE_ROLE)
    {
        require(validators[validator].active, "Not a validator");
        
        totalWeight = totalWeight - validators[validator].weight + newWeight;
        validators[validator].weight = newWeight;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get current primary (leader)
     */
    function getPrimary() public view returns (address) {
        if (validatorCount == 0) return address(0);
        uint256 primaryIndex = currentView % validatorList.length;
        return validatorList[primaryIndex];
    }

    /**
     * @notice Get quorum weight required (2f + 1 for BFT)
     */
    function _quorumWeight() internal view returns (uint256) {
        // For n validators tolerating f faults: need 2f + 1 votes
        // f = (n - 1) / 3, so quorum = 2 * ((n - 1) / 3) + 1
        // Simplify to: 2/3 of total weight + 1
        return (totalWeight * 2) / 3 + 1;
    }

    /**
     * @notice Get fault tolerance threshold
     */
    function faultTolerance() external view returns (uint256) {
        return (validatorCount - 1) / 3;
    }

    /**
     * @notice Check if proposal is prepared
     */
    function isPrepared(uint256 sequence) external view returns (bool) {
        return voteRecords[sequence].prepared;
    }

    /**
     * @notice Check if proposal is committed
     */
    function isCommitted(uint256 sequence) external view returns (bool) {
        return voteRecords[sequence].committed;
    }

    /**
     * @notice Get vote counts for a sequence
     */
    function getVoteCounts(uint256 sequence) 
        external 
        view 
        returns (uint256 prepareCount, uint256 commitCount) 
    {
        return (
            voteRecords[sequence].prepareCount,
            voteRecords[sequence].commitCount
        );
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    function _verifySignature(
        bytes32 messageHash,
        bytes calldata signature,
        address expectedSigner
    ) internal pure {
        bytes32 ethSignedHash = MessageHashUtils.toEthSignedMessageHash(messageHash);
        address recovered = ECDSA.recover(ethSignedHash, signature);
        if (recovered != expectedSigner) revert InvalidSignature();
    }

    function _executeProposal(bytes memory data) internal returns (bool) {
        // Decode and execute the proposal
        // This is a placeholder - actual implementation depends on proposal type
        if (data.length > 0) {
            // Could be a call to another contract, state update, etc.
            return true;
        }
        return true;
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setCheckpointInterval(uint256 interval) 
        external 
        onlyRole(GOVERNANCE_ROLE) 
    {
        checkpointInterval = interval;
    }

    function setViewChangeTimeout(uint256 timeout) 
        external 
        onlyRole(GOVERNANCE_ROLE) 
    {
        viewChangeTimeout = timeout;
    }
}
