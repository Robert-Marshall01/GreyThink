// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/math/Math.sol";

/**
 * @title ConvictionVoting
 * @author Grey Protocol
 * @notice Time-weighted voting system where conviction builds over time
 * @dev Votes become stronger the longer they remain on a proposal
 * 
 * Features:
 * - Conviction accumulation based on time and stake
 * - Continuous voting (no voting periods)
 * - Signal/abstain/oppose options
 * - Funding proposals with threshold-based execution
 * - Decay when votes are moved
 * - Anti-flash-loan protection
 */
contract ConvictionVoting is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;
    using Math for uint256;

    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant PROPOSAL_CREATOR_ROLE = keccak256("PROPOSAL_CREATOR_ROLE");
    bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");

    uint256 public constant PRECISION = 1e18;
    uint256 public constant MAX_PROPOSALS = 1000;

    // Conviction decay: conviction(t+1) = conviction(t) * D + stake
    // D = decay factor (e.g., 0.9 means 10% decay per period)
    uint256 public constant DECAY_DENOMINATOR = 10000;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Proposal for funding
    struct Proposal {
        uint256 id;
        address proposer;
        address beneficiary;
        uint256 requestedAmount;    // Amount requested from pool
        string title;
        string description;
        string url;
        uint256 createdAt;
        uint256 totalStaked;        // Total tokens staked on this proposal
        uint256 conviction;         // Current accumulated conviction
        uint256 lastCalculated;     // Last block conviction was calculated
        ProposalStatus status;
    }

    enum ProposalStatus {
        Active,
        Executed,
        Cancelled,
        Expired
    }

    /// @notice Stake record
    struct StakeRecord {
        uint256 proposalId;
        uint256 amount;
        uint256 stakedAt;
        uint256 lastConviction;
    }

    /// @notice Voter state
    struct VoterState {
        uint256 totalStaked;
        uint256[] activeProposals;
    }

    /// @notice Threshold parameters
    struct ThresholdParams {
        uint256 alpha;              // Conviction growth rate
        uint256 beta;               // Spending limit coefficient
        uint256 rho;                // Proposal threshold coefficient
        uint256 decay;              // Decay rate (% * 100)
        uint256 minThresholdPct;    // Minimum threshold as % of supply
        uint256 maxRatio;           // Max requested / total pool ratio
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Staking token
    IERC20 public stakingToken;

    /// @notice Funding pool token (can be same as staking)
    IERC20 public fundingToken;

    /// @notice Proposal count
    uint256 public proposalCount;

    /// @notice Proposals
    mapping(uint256 => Proposal) public proposals;

    /// @notice User => proposal => stake
    mapping(address => mapping(uint256 => StakeRecord)) public stakes;

    /// @notice User => voter state
    mapping(address => VoterState) private voterStates;

    /// @notice Total conviction in system
    uint256 public totalConviction;

    /// @notice Total tokens in funding pool
    uint256 public fundingPool;

    /// @notice Threshold parameters
    ThresholdParams public thresholdParams;

    /// @notice Conviction calculation period (blocks)
    uint256 public convictionPeriod;

    /// @notice Proposal expiry duration
    uint256 public proposalExpiry;

    /// @notice Min stake to create proposal
    uint256 public minStakeToPropose;

    /// @notice Total staked across all proposals
    uint256 public totalStakedGlobal;

    /// @notice Block delay before stake is counted (anti-flash-loan)
    uint256 public stakeDelay;

    // ============================================
    // EVENTS
    // ============================================

    event ProposalCreated(
        uint256 indexed proposalId,
        address indexed proposer,
        uint256 requestedAmount,
        address beneficiary
    );

    event Staked(
        address indexed voter,
        uint256 indexed proposalId,
        uint256 amount
    );

    event Unstaked(
        address indexed voter,
        uint256 indexed proposalId,
        uint256 amount
    );

    event ConvictionUpdated(
        uint256 indexed proposalId,
        uint256 oldConviction,
        uint256 newConviction
    );

    event ProposalExecuted(
        uint256 indexed proposalId,
        uint256 conviction,
        uint256 threshold,
        uint256 fundedAmount
    );

    event ProposalCancelled(uint256 indexed proposalId);

    event FundingPoolUpdated(uint256 newBalance);

    // ============================================
    // ERRORS
    // ============================================

    error ProposalNotActive();
    error InsufficientStake();
    error ThresholdNotMet();
    error ProposalExpired();
    error ExcessiveRequest();
    error StakeDelayNotPassed();
    error NoStakeOnProposal();
    error InsufficientFunding();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address _stakingToken,
        address _fundingToken,
        ThresholdParams memory _params
    ) {
        stakingToken = IERC20(_stakingToken);
        fundingToken = IERC20(_fundingToken);
        thresholdParams = _params;

        convictionPeriod = 100;        // ~100 blocks
        proposalExpiry = 90 days;
        minStakeToPropose = 1000e18;   // 1000 tokens
        stakeDelay = 50;               // 50 blocks

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(PROPOSAL_CREATOR_ROLE, msg.sender);
        _grantRole(ADMIN_ROLE, msg.sender);
    }

    // ============================================
    // PROPOSAL MANAGEMENT
    // ============================================

    /**
     * @notice Create a funding proposal
     * @param requestedAmount Amount requested from pool
     * @param beneficiary Address to receive funds
     * @param title Proposal title
     * @param description Proposal description
     * @param url Link to details
     */
    function createProposal(
        uint256 requestedAmount,
        address beneficiary,
        string calldata title,
        string calldata description,
        string calldata url
    ) external returns (uint256 proposalId) {
        // Check creator has minimum stake
        if (voterStates[msg.sender].totalStaked < minStakeToPropose) {
            revert InsufficientStake();
        }

        // Check request is within limits
        uint256 maxRequest = (fundingPool * thresholdParams.maxRatio) / DECAY_DENOMINATOR;
        if (requestedAmount > maxRequest) revert ExcessiveRequest();

        proposalId = ++proposalCount;

        proposals[proposalId] = Proposal({
            id: proposalId,
            proposer: msg.sender,
            beneficiary: beneficiary,
            requestedAmount: requestedAmount,
            title: title,
            description: description,
            url: url,
            createdAt: block.timestamp,
            totalStaked: 0,
            conviction: 0,
            lastCalculated: block.number,
            status: ProposalStatus.Active
        });

        emit ProposalCreated(proposalId, msg.sender, requestedAmount, beneficiary);
    }

    /**
     * @notice Cancel a proposal (proposer only)
     */
    function cancelProposal(uint256 proposalId) external {
        Proposal storage proposal = proposals[proposalId];
        
        require(
            msg.sender == proposal.proposer || hasRole(ADMIN_ROLE, msg.sender),
            "Not authorized"
        );
        require(proposal.status == ProposalStatus.Active, "Not active");

        proposal.status = ProposalStatus.Cancelled;
        emit ProposalCancelled(proposalId);
    }

    // ============================================
    // STAKING
    // ============================================

    /**
     * @notice Stake tokens on a proposal
     * @param proposalId Proposal to support
     * @param amount Amount to stake
     */
    function stake(uint256 proposalId, uint256 amount) external nonReentrant {
        Proposal storage proposal = proposals[proposalId];
        if (proposal.status != ProposalStatus.Active) revert ProposalNotActive();
        if (amount == 0) revert InsufficientStake();

        // Check expiry
        if (block.timestamp > proposal.createdAt + proposalExpiry) {
            proposal.status = ProposalStatus.Expired;
            revert ProposalExpired();
        }

        // Update conviction before staking
        _updateConviction(proposalId);

        // Transfer tokens
        stakingToken.safeTransferFrom(msg.sender, address(this), amount);

        // Record stake
        StakeRecord storage stakeRecord = stakes[msg.sender][proposalId];
        
        if (stakeRecord.amount == 0) {
            // New stake
            stakeRecord.proposalId = proposalId;
            stakeRecord.stakedAt = block.number;
            voterStates[msg.sender].activeProposals.push(proposalId);
        }

        stakeRecord.amount += amount;
        stakeRecord.lastConviction = proposal.conviction;

        proposal.totalStaked += amount;
        voterStates[msg.sender].totalStaked += amount;
        totalStakedGlobal += amount;

        emit Staked(msg.sender, proposalId, amount);
    }

    /**
     * @notice Unstake tokens from a proposal
     * @param proposalId Proposal to unstake from
     * @param amount Amount to unstake
     */
    function unstake(uint256 proposalId, uint256 amount) external nonReentrant {
        StakeRecord storage stakeRecord = stakes[msg.sender][proposalId];
        if (stakeRecord.amount == 0) revert NoStakeOnProposal();
        if (amount > stakeRecord.amount) revert InsufficientStake();

        Proposal storage proposal = proposals[proposalId];

        // Update conviction before unstaking
        if (proposal.status == ProposalStatus.Active) {
            _updateConviction(proposalId);
        }

        // Update records
        stakeRecord.amount -= amount;
        proposal.totalStaked -= amount;
        voterStates[msg.sender].totalStaked -= amount;
        totalStakedGlobal -= amount;

        // Remove from active proposals if fully unstaked
        if (stakeRecord.amount == 0) {
            _removeFromActiveProposals(msg.sender, proposalId);
        }

        // Transfer tokens back
        stakingToken.safeTransfer(msg.sender, amount);

        emit Unstaked(msg.sender, proposalId, amount);
    }

    /**
     * @notice Move stake between proposals
     */
    function moveStake(uint256 fromProposal, uint256 toProposal, uint256 amount) 
        external 
        nonReentrant 
    {
        StakeRecord storage fromStake = stakes[msg.sender][fromProposal];
        if (fromStake.amount < amount) revert InsufficientStake();

        Proposal storage toProp = proposals[toProposal];
        if (toProp.status != ProposalStatus.Active) revert ProposalNotActive();

        // Update convictions
        _updateConviction(fromProposal);
        _updateConviction(toProposal);

        // Move stake
        fromStake.amount -= amount;
        proposals[fromProposal].totalStaked -= amount;

        StakeRecord storage toStake = stakes[msg.sender][toProposal];
        if (toStake.amount == 0) {
            toStake.proposalId = toProposal;
            toStake.stakedAt = block.number;
            voterStates[msg.sender].activeProposals.push(toProposal);
        }
        toStake.amount += amount;
        toProp.totalStaked += amount;

        // Clean up if fully moved
        if (fromStake.amount == 0) {
            _removeFromActiveProposals(msg.sender, fromProposal);
        }

        emit Unstaked(msg.sender, fromProposal, amount);
        emit Staked(msg.sender, toProposal, amount);
    }

    // ============================================
    // CONVICTION CALCULATION
    // ============================================

    /**
     * @notice Update conviction for a proposal
     */
    function _updateConviction(uint256 proposalId) internal {
        Proposal storage proposal = proposals[proposalId];
        
        uint256 blocksPassed = block.number - proposal.lastCalculated;
        if (blocksPassed == 0) return;

        uint256 periods = blocksPassed / convictionPeriod;
        if (periods == 0) return;

        uint256 oldConviction = proposal.conviction;
        uint256 newConviction = oldConviction;
        uint256 decay = thresholdParams.decay;

        // Apply conviction formula: C(t+1) = C(t) * D + stake * alpha
        for (uint256 i = 0; i < periods && i < 100; i++) {
            // Decay existing conviction
            newConviction = (newConviction * decay) / DECAY_DENOMINATOR;
            
            // Add conviction from current stake
            uint256 stakeConviction = (proposal.totalStaked * thresholdParams.alpha) / PRECISION;
            newConviction += stakeConviction;
        }

        proposal.conviction = newConviction;
        proposal.lastCalculated = block.number;

        // Update total conviction
        totalConviction = totalConviction - oldConviction + newConviction;

        emit ConvictionUpdated(proposalId, oldConviction, newConviction);
    }

    /**
     * @notice Calculate threshold for a proposal
     * @param requestedAmount Amount being requested
     */
    function calculateThreshold(uint256 requestedAmount) 
        public 
        view 
        returns (uint256 threshold) 
    {
        if (fundingPool == 0) return type(uint256).max;

        // Threshold formula: T = (rho * supply) / (1 - requestedAmount/pool)^beta
        // Simplified: higher requests need higher conviction

        uint256 requestRatio = (requestedAmount * PRECISION) / fundingPool;
        uint256 base = PRECISION - requestRatio;
        
        if (base == 0) return type(uint256).max;

        // Threshold increases as request ratio increases
        uint256 supply = stakingToken.totalSupply();
        uint256 minThreshold = (supply * thresholdParams.minThresholdPct) / DECAY_DENOMINATOR;

        // Scale by rho and beta
        threshold = (thresholdParams.rho * supply * PRECISION) / base;
        threshold = (threshold * thresholdParams.beta) / PRECISION;

        if (threshold < minThreshold) {
            threshold = minThreshold;
        }
    }

    // ============================================
    // EXECUTION
    // ============================================

    /**
     * @notice Execute a proposal that has met its threshold
     */
    function executeProposal(uint256 proposalId) external nonReentrant {
        Proposal storage proposal = proposals[proposalId];
        
        if (proposal.status != ProposalStatus.Active) revert ProposalNotActive();
        if (proposal.requestedAmount > fundingPool) revert InsufficientFunding();

        // Check expiry
        if (block.timestamp > proposal.createdAt + proposalExpiry) {
            proposal.status = ProposalStatus.Expired;
            revert ProposalExpired();
        }

        // Update conviction
        _updateConviction(proposalId);

        // Calculate threshold
        uint256 threshold = calculateThreshold(proposal.requestedAmount);
        
        if (proposal.conviction < threshold) revert ThresholdNotMet();

        // Execute
        proposal.status = ProposalStatus.Executed;
        fundingPool -= proposal.requestedAmount;

        fundingToken.safeTransfer(proposal.beneficiary, proposal.requestedAmount);

        emit ProposalExecuted(
            proposalId,
            proposal.conviction,
            threshold,
            proposal.requestedAmount
        );
        emit FundingPoolUpdated(fundingPool);
    }

    // ============================================
    // FUNDING POOL
    // ============================================

    /**
     * @notice Add funds to the pool
     */
    function fundPool(uint256 amount) external {
        fundingToken.safeTransferFrom(msg.sender, address(this), amount);
        fundingPool += amount;
        emit FundingPoolUpdated(fundingPool);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Get current conviction for a proposal (updated)
     */
    function getCurrentConviction(uint256 proposalId) 
        external 
        view 
        returns (uint256) 
    {
        Proposal storage proposal = proposals[proposalId];
        
        uint256 blocksPassed = block.number - proposal.lastCalculated;
        uint256 periods = blocksPassed / convictionPeriod;

        uint256 conviction = proposal.conviction;
        uint256 decay = thresholdParams.decay;

        for (uint256 i = 0; i < periods && i < 100; i++) {
            conviction = (conviction * decay) / DECAY_DENOMINATOR;
            uint256 stakeConviction = (proposal.totalStaked * thresholdParams.alpha) / PRECISION;
            conviction += stakeConviction;
        }

        return conviction;
    }

    /**
     * @notice Check if proposal can be executed
     */
    function canExecute(uint256 proposalId) external view returns (bool, uint256 conviction, uint256 threshold) {
        Proposal storage proposal = proposals[proposalId];
        
        if (proposal.status != ProposalStatus.Active) return (false, 0, 0);
        if (proposal.requestedAmount > fundingPool) return (false, 0, 0);

        conviction = this.getCurrentConviction(proposalId);
        threshold = calculateThreshold(proposal.requestedAmount);

        return (conviction >= threshold, conviction, threshold);
    }

    /**
     * @notice Get user's stake on a proposal
     */
    function getUserStake(address user, uint256 proposalId) 
        external 
        view 
        returns (uint256) 
    {
        return stakes[user][proposalId].amount;
    }

    /**
     * @notice Get user's total staked amount
     */
    function getUserTotalStaked(address user) external view returns (uint256) {
        return voterStates[user].totalStaked;
    }

    /**
     * @notice Get user's active proposals
     */
    function getUserActiveProposals(address user) 
        external 
        view 
        returns (uint256[] memory) 
    {
        return voterStates[user].activeProposals;
    }

    /**
     * @notice Estimate time to threshold
     */
    function estimateTimeToThreshold(uint256 proposalId) 
        external 
        view 
        returns (uint256 blocks) 
    {
        Proposal storage proposal = proposals[proposalId];
        uint256 currentConv = this.getCurrentConviction(proposalId);
        uint256 threshold = calculateThreshold(proposal.requestedAmount);

        if (currentConv >= threshold) return 0;
        if (proposal.totalStaked == 0) return type(uint256).max;

        // Estimate periods needed
        uint256 stakeContribution = (proposal.totalStaked * thresholdParams.alpha) / PRECISION;
        uint256 needed = threshold - currentConv;
        uint256 periods = (needed * PRECISION) / (stakeContribution * PRECISION);

        return periods * convictionPeriod;
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    function _removeFromActiveProposals(address user, uint256 proposalId) internal {
        uint256[] storage active = voterStates[user].activeProposals;
        for (uint256 i = 0; i < active.length; i++) {
            if (active[i] == proposalId) {
                active[i] = active[active.length - 1];
                active.pop();
                break;
            }
        }
    }

    // ============================================
    // ADMIN
    // ============================================

    function setThresholdParams(ThresholdParams calldata params) 
        external 
        onlyRole(ADMIN_ROLE) 
    {
        thresholdParams = params;
    }

    function setConvictionPeriod(uint256 period) external onlyRole(ADMIN_ROLE) {
        convictionPeriod = period;
    }

    function setProposalExpiry(uint256 expiry) external onlyRole(ADMIN_ROLE) {
        proposalExpiry = expiry;
    }

    function setMinStakeToPropose(uint256 minStake) external onlyRole(ADMIN_ROLE) {
        minStakeToPropose = minStake;
    }

    function setStakeDelay(uint256 delay) external onlyRole(ADMIN_ROLE) {
        stakeDelay = delay;
    }
}
