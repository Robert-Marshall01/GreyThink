// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title QuadraticVoting
 * @author Grey Protocol
 * @notice Implementation of quadratic voting for democratic governance
 * @dev Vote cost scales quadratically: cost = n² credits for n votes
 * 
 * Features:
 * - Quadratic vote cost calculation
 * - Voice credit management
 * - Proposal lifecycle management
 * - Anti-sybil mechanisms
 * - Bribing resistance through commitment schemes
 * - Partial refunds for unused credits
 */
contract QuadraticVoting is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant PROPOSAL_CREATOR_ROLE = keccak256("PROPOSAL_CREATOR_ROLE");
    bytes32 public constant COORDINATOR_ROLE = keccak256("COORDINATOR_ROLE");

    uint256 public constant MAX_VOTE_OPTIONS = 10;
    uint256 public constant PRECISION = 1e18;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Proposal structure
    struct Proposal {
        uint256 id;
        address proposer;
        string description;
        string[] options;
        uint256 startTime;
        uint256 endTime;
        uint256 totalVoters;
        bool executed;
        bool cancelled;
    }

    /// @notice Vote commitment (for commit-reveal)
    struct VoteCommitment {
        bytes32 commitmentHash;
        uint256 timestamp;
        bool revealed;
    }

    /// @notice User's vote allocation
    struct VoteAllocation {
        uint256 proposalId;
        uint256[] votesPerOption;     // Votes cast per option
        uint256 totalCreditsUsed;
        uint256 timestamp;
    }

    /// @notice Voter info
    struct VoterInfo {
        uint256 voiceCredits;         // Available credits
        uint256 creditsUsed;          // Credits used in current round
        bool registered;
        uint256 registeredAt;
        mapping(uint256 => bool) hasVoted;  // proposalId => voted
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Voice credit token
    IERC20 public creditToken;

    /// @notice Credits per token
    uint256 public creditsPerToken;

    /// @notice Current round
    uint256 public currentRound;

    /// @notice Proposal count
    uint256 public proposalCount;

    /// @notice Proposals
    mapping(uint256 => Proposal) public proposals;

    /// @notice Proposal => option => vote count
    mapping(uint256 => mapping(uint256 => uint256)) public voteCount;

    /// @notice User => voter info
    mapping(address => VoterInfo) private voterInfo;

    /// @notice User => proposal => vote allocation
    mapping(address => mapping(uint256 => VoteAllocation)) public voteAllocations;

    /// @notice User => proposal => vote commitment
    mapping(address => mapping(uint256 => VoteCommitment)) public commitments;

    /// @notice Round => total credits in pool
    mapping(uint256 => uint256) public roundCredits;

    /// @notice Use commit-reveal scheme
    bool public useCommitReveal;

    /// @notice Commit phase duration
    uint256 public commitPhaseDuration;

    /// @notice Minimum proposal duration
    uint256 public minProposalDuration;

    /// @notice Maximum credits per voter
    uint256 public maxCreditsPerVoter;

    // ============================================
    // EVENTS
    // ============================================

    event VoterRegistered(address indexed voter, uint256 credits);

    event ProposalCreated(
        uint256 indexed proposalId,
        address indexed proposer,
        uint256 startTime,
        uint256 endTime
    );

    event VoteCommitted(
        address indexed voter,
        uint256 indexed proposalId,
        bytes32 commitment
    );

    event VotesCast(
        address indexed voter,
        uint256 indexed proposalId,
        uint256[] votesPerOption,
        uint256 creditsUsed
    );

    event ProposalExecuted(uint256 indexed proposalId, uint256 winningOption);

    event ProposalCancelled(uint256 indexed proposalId);

    event CreditsRefunded(address indexed voter, uint256 amount);

    // ============================================
    // ERRORS
    // ============================================

    error NotRegistered();
    error AlreadyRegistered();
    error ProposalNotActive();
    error AlreadyVoted();
    error InsufficientCredits();
    error InvalidVoteCount();
    error CommitmentRequired();
    error InvalidReveal();
    error ProposalNotEnded();
    error ProposalAlreadyExecuted();
    error TooManyOptions();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address _creditToken,
        uint256 _creditsPerToken,
        uint256 _maxCreditsPerVoter
    ) {
        creditToken = IERC20(_creditToken);
        creditsPerToken = _creditsPerToken;
        maxCreditsPerVoter = _maxCreditsPerVoter;
        
        minProposalDuration = 7 days;
        commitPhaseDuration = 2 days;
        useCommitReveal = false;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(PROPOSAL_CREATOR_ROLE, msg.sender);
        _grantRole(COORDINATOR_ROLE, msg.sender);
    }

    // ============================================
    // VOTER REGISTRATION
    // ============================================

    /**
     * @notice Register as a voter by depositing tokens
     * @param tokenAmount Amount of tokens to deposit
     */
    function register(uint256 tokenAmount) external nonReentrant {
        VoterInfo storage voter = voterInfo[msg.sender];
        if (voter.registered) revert AlreadyRegistered();

        creditToken.safeTransferFrom(msg.sender, address(this), tokenAmount);

        uint256 credits = (tokenAmount * creditsPerToken) / PRECISION;
        if (credits > maxCreditsPerVoter) {
            credits = maxCreditsPerVoter;
        }

        voter.voiceCredits = credits;
        voter.registered = true;
        voter.registeredAt = block.timestamp;

        roundCredits[currentRound] += credits;

        emit VoterRegistered(msg.sender, credits);
    }

    /**
     * @notice Top up voice credits
     */
    function topUpCredits(uint256 tokenAmount) external nonReentrant {
        VoterInfo storage voter = voterInfo[msg.sender];
        if (!voter.registered) revert NotRegistered();

        creditToken.safeTransferFrom(msg.sender, address(this), tokenAmount);

        uint256 newCredits = (tokenAmount * creditsPerToken) / PRECISION;
        uint256 totalCredits = voter.voiceCredits + newCredits;
        
        if (totalCredits > maxCreditsPerVoter) {
            voter.voiceCredits = maxCreditsPerVoter;
        } else {
            voter.voiceCredits = totalCredits;
        }

        roundCredits[currentRound] += newCredits;
    }

    // ============================================
    // PROPOSAL MANAGEMENT
    // ============================================

    /**
     * @notice Create a new proposal
     * @param description Proposal description
     * @param options Vote options
     * @param duration Voting duration
     */
    function createProposal(
        string calldata description,
        string[] calldata options,
        uint256 duration
    ) external onlyRole(PROPOSAL_CREATOR_ROLE) returns (uint256 proposalId) {
        if (options.length > MAX_VOTE_OPTIONS) revert TooManyOptions();
        if (duration < minProposalDuration) revert InvalidVoteCount();

        proposalId = ++proposalCount;

        proposals[proposalId] = Proposal({
            id: proposalId,
            proposer: msg.sender,
            description: description,
            options: options,
            startTime: block.timestamp,
            endTime: block.timestamp + duration,
            totalVoters: 0,
            executed: false,
            cancelled: false
        });

        emit ProposalCreated(proposalId, msg.sender, block.timestamp, block.timestamp + duration);
    }

    // ============================================
    // VOTING (STANDARD)
    // ============================================

    /**
     * @notice Cast votes on a proposal
     * @param proposalId Proposal ID
     * @param votesPerOption Number of votes for each option
     */
    function vote(uint256 proposalId, uint256[] calldata votesPerOption) 
        external 
        nonReentrant 
    {
        if (useCommitReveal) revert CommitmentRequired();
        _castVote(proposalId, votesPerOption);
    }

    function _castVote(uint256 proposalId, uint256[] calldata votesPerOption) internal {
        Proposal storage proposal = proposals[proposalId];
        VoterInfo storage voter = voterInfo[msg.sender];

        // Validations
        if (!voter.registered) revert NotRegistered();
        if (block.timestamp < proposal.startTime || block.timestamp > proposal.endTime) {
            revert ProposalNotActive();
        }
        if (voter.hasVoted[proposalId]) revert AlreadyVoted();
        if (votesPerOption.length != proposal.options.length) revert InvalidVoteCount();

        // Calculate credits needed: cost = sum(votes[i]²)
        uint256 creditsNeeded = 0;
        for (uint256 i = 0; i < votesPerOption.length; i++) {
            creditsNeeded += votesPerOption[i] * votesPerOption[i];
        }

        if (creditsNeeded > voter.voiceCredits) revert InsufficientCredits();

        // Apply votes
        for (uint256 i = 0; i < votesPerOption.length; i++) {
            if (votesPerOption[i] > 0) {
                voteCount[proposalId][i] += votesPerOption[i];
            }
        }

        // Update voter state
        voter.voiceCredits -= creditsNeeded;
        voter.creditsUsed += creditsNeeded;
        voter.hasVoted[proposalId] = true;

        // Record allocation
        voteAllocations[msg.sender][proposalId] = VoteAllocation({
            proposalId: proposalId,
            votesPerOption: votesPerOption,
            totalCreditsUsed: creditsNeeded,
            timestamp: block.timestamp
        });

        proposal.totalVoters++;

        emit VotesCast(msg.sender, proposalId, votesPerOption, creditsNeeded);
    }

    // ============================================
    // VOTING (COMMIT-REVEAL)
    // ============================================

    /**
     * @notice Commit a vote hash (phase 1)
     * @param proposalId Proposal ID
     * @param commitmentHash Hash of (votes, salt)
     */
    function commitVote(uint256 proposalId, bytes32 commitmentHash) external {
        if (!useCommitReveal) revert InvalidReveal();
        
        Proposal storage proposal = proposals[proposalId];
        VoterInfo storage voter = voterInfo[msg.sender];

        if (!voter.registered) revert NotRegistered();
        if (block.timestamp > proposal.endTime - commitPhaseDuration) revert ProposalNotActive();
        if (voter.hasVoted[proposalId]) revert AlreadyVoted();

        commitments[msg.sender][proposalId] = VoteCommitment({
            commitmentHash: commitmentHash,
            timestamp: block.timestamp,
            revealed: false
        });

        emit VoteCommitted(msg.sender, proposalId, commitmentHash);
    }

    /**
     * @notice Reveal a committed vote (phase 2)
     * @param proposalId Proposal ID
     * @param votesPerOption Vote allocation
     * @param salt Salt used in commitment
     */
    function revealVote(
        uint256 proposalId,
        uint256[] calldata votesPerOption,
        bytes32 salt
    ) external nonReentrant {
        if (!useCommitReveal) revert InvalidReveal();

        VoteCommitment storage commitment = commitments[msg.sender][proposalId];
        Proposal storage proposal = proposals[proposalId];

        // Must be in reveal phase
        if (block.timestamp < proposal.endTime - commitPhaseDuration) revert ProposalNotActive();
        if (block.timestamp > proposal.endTime) revert ProposalNotActive();
        if (commitment.revealed) revert AlreadyVoted();

        // Verify commitment
        bytes32 expectedHash = keccak256(abi.encodePacked(votesPerOption, salt));
        if (commitment.commitmentHash != expectedHash) revert InvalidReveal();

        commitment.revealed = true;

        _castVote(proposalId, votesPerOption);
    }

    // ============================================
    // EXECUTION
    // ============================================

    /**
     * @notice Execute a proposal after voting ends
     */
    function executeProposal(uint256 proposalId) 
        external 
        onlyRole(COORDINATOR_ROLE) 
        returns (uint256 winningOption) 
    {
        Proposal storage proposal = proposals[proposalId];

        if (block.timestamp <= proposal.endTime) revert ProposalNotEnded();
        if (proposal.executed) revert ProposalAlreadyExecuted();
        if (proposal.cancelled) revert ProposalNotActive();

        // Find winning option
        uint256 maxVotes = 0;
        for (uint256 i = 0; i < proposal.options.length; i++) {
            if (voteCount[proposalId][i] > maxVotes) {
                maxVotes = voteCount[proposalId][i];
                winningOption = i;
            }
        }

        proposal.executed = true;

        emit ProposalExecuted(proposalId, winningOption);
    }

    /**
     * @notice Cancel a proposal
     */
    function cancelProposal(uint256 proposalId) external onlyRole(COORDINATOR_ROLE) {
        Proposal storage proposal = proposals[proposalId];
        require(!proposal.executed, "Already executed");
        
        proposal.cancelled = true;
        emit ProposalCancelled(proposalId);
    }

    // ============================================
    // CREDIT MANAGEMENT
    // ============================================

    /**
     * @notice Refund unused credits at end of round
     */
    function refundUnusedCredits() external nonReentrant {
        VoterInfo storage voter = voterInfo[msg.sender];
        if (!voter.registered) revert NotRegistered();

        uint256 unusedCredits = voter.voiceCredits;
        if (unusedCredits == 0) revert InsufficientCredits();

        voter.voiceCredits = 0;

        uint256 tokenAmount = (unusedCredits * PRECISION) / creditsPerToken;
        creditToken.safeTransfer(msg.sender, tokenAmount);

        emit CreditsRefunded(msg.sender, tokenAmount);
    }

    /**
     * @notice Start a new voting round
     */
    function startNewRound() external onlyRole(COORDINATOR_ROLE) {
        currentRound++;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Calculate cost for votes
     * @param votes Number of votes to cast for an option
     * @return cost Credits required
     */
    function calculateVoteCost(uint256 votes) external pure returns (uint256 cost) {
        return votes * votes;
    }

    /**
     * @notice Calculate total cost for vote allocation
     */
    function calculateTotalCost(uint256[] calldata votesPerOption) 
        external 
        pure 
        returns (uint256 totalCost) 
    {
        for (uint256 i = 0; i < votesPerOption.length; i++) {
            totalCost += votesPerOption[i] * votesPerOption[i];
        }
    }

    /**
     * @notice Get voter credits
     */
    function getVoterCredits(address voter) external view returns (uint256) {
        return voterInfo[voter].voiceCredits;
    }

    /**
     * @notice Check if voter is registered
     */
    function isRegistered(address voter) external view returns (bool) {
        return voterInfo[voter].registered;
    }

    /**
     * @notice Get proposal results
     */
    function getProposalResults(uint256 proposalId) 
        external 
        view 
        returns (uint256[] memory votes) 
    {
        Proposal storage proposal = proposals[proposalId];
        votes = new uint256[](proposal.options.length);
        
        for (uint256 i = 0; i < proposal.options.length; i++) {
            votes[i] = voteCount[proposalId][i];
        }
    }

    /**
     * @notice Get proposal options
     */
    function getProposalOptions(uint256 proposalId) 
        external 
        view 
        returns (string[] memory) 
    {
        return proposals[proposalId].options;
    }

    /**
     * @notice Get winning option for a proposal
     */
    function getWinningOption(uint256 proposalId) 
        external 
        view 
        returns (uint256 winningOption, uint256 votes) 
    {
        Proposal storage proposal = proposals[proposalId];
        
        for (uint256 i = 0; i < proposal.options.length; i++) {
            if (voteCount[proposalId][i] > votes) {
                votes = voteCount[proposalId][i];
                winningOption = i;
            }
        }
    }

    // ============================================
    // ADMIN
    // ============================================

    function setUseCommitReveal(bool enabled) external onlyRole(DEFAULT_ADMIN_ROLE) {
        useCommitReveal = enabled;
    }

    function setCommitPhaseDuration(uint256 duration) external onlyRole(DEFAULT_ADMIN_ROLE) {
        commitPhaseDuration = duration;
    }

    function setMinProposalDuration(uint256 duration) external onlyRole(DEFAULT_ADMIN_ROLE) {
        minProposalDuration = duration;
    }

    function setMaxCreditsPerVoter(uint256 max) external onlyRole(DEFAULT_ADMIN_ROLE) {
        maxCreditsPerVoter = max;
    }
}
