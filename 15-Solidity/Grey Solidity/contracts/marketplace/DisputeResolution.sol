// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title DisputeResolution
 * @notice Decentralized dispute resolution system for marketplace transactions
 * @dev Implements a multi-phase dispute process with arbitrators and appeals
 *
 * Features:
 * - Multi-phase dispute lifecycle (Filing → Evidence → Arbitration → Resolution)
 * - Decentralized arbitrator selection
 * - Evidence submission with deadlines
 * - Appeal mechanism
 * - Stake-based incentives for arbitrators
 * - Automatic resolution after timeout
 * - Multiple dispute types (NFT, Payment, Service, etc.)
 */
contract DisputeResolution is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============ Enums ============

    enum DisputeStatus {
        None,
        Filed,           // Initial filing
        EvidencePhase,   // Collecting evidence
        Arbitration,     // Arbitrators reviewing
        Resolved,        // Final decision made
        Appealed,        // Under appeal
        AppealResolved,  // Appeal completed
        Cancelled        // Dispute cancelled
    }

    enum DisputeType {
        NFTTransaction,
        PaymentDispute,
        ServiceDispute,
        ContractBreach,
        Other
    }

    enum Resolution {
        None,
        FavorClaimant,
        FavorRespondent,
        Split,
        Dismissed
    }

    // ============ Structs ============

    struct Dispute {
        uint256 disputeId;
        DisputeType disputeType;
        DisputeStatus status;
        address claimant;
        address respondent;
        address relatedContract;
        uint256 relatedTokenId;
        uint256 disputedAmount;
        uint256 filingFee;
        uint256 filedAt;
        uint256 evidenceDeadline;
        uint256 resolutionDeadline;
        string claimantEvidence;
        string respondentEvidence;
        Resolution resolution;
        uint256 claimantAward;
        uint256 respondentAward;
        bytes32 arbitratorCommit;
        bool appealable;
    }

    struct ArbitratorInfo {
        address arbitratorAddress;
        uint256 stakedAmount;
        uint256 casesResolved;
        uint256 successRate; // basis points
        bool isActive;
        uint256 lastActiveAt;
        uint256 pendingCases;
    }

    struct Appeal {
        uint256 disputeId;
        address appellant;
        string reason;
        uint256 appealFee;
        uint256 filedAt;
        uint256 deadline;
        Resolution originalResolution;
        Resolution appealResolution;
        bool resolved;
    }

    struct ArbitratorVote {
        address arbitrator;
        Resolution vote;
        uint256 claimantPercentage;
        uint256 timestamp;
        bool revealed;
        bytes32 commitHash;
    }

    // ============ Constants ============

    bytes32 public constant ARBITRATOR_ROLE = keccak256("ARBITRATOR_ROLE");
    bytes32 public constant MODERATOR_ROLE = keccak256("MODERATOR_ROLE");

    uint256 public constant MIN_ARBITRATOR_STAKE = 1 ether;
    uint256 public constant EVIDENCE_PERIOD = 3 days;
    uint256 public constant ARBITRATION_PERIOD = 5 days;
    uint256 public constant APPEAL_PERIOD = 2 days;
    uint256 public constant APPEAL_RESOLUTION_PERIOD = 7 days;
    uint256 public constant MIN_ARBITRATORS_PER_CASE = 3;
    uint256 public constant MAX_ARBITRATORS_PER_CASE = 7;
    uint256 public constant BASIS_POINTS = 10000;

    // ============ State Variables ============

    /// @notice Payment token for fees and stakes
    IERC20 public immutable paymentToken;

    /// @notice Treasury for collected fees
    address public treasury;

    /// @notice Filing fee percentage (basis points)
    uint256 public filingFeeRate = 100; // 1%

    /// @notice Appeal fee multiplier
    uint256 public appealFeeMultiplier = 2;

    /// @notice Dispute counter
    uint256 public disputeCount;

    /// @notice All disputes
    mapping(uint256 => Dispute) public disputes;

    /// @notice Dispute arbitrator assignments
    mapping(uint256 => address[]) public disputeArbitrators;

    /// @notice Arbitrator votes per dispute
    mapping(uint256 => mapping(address => ArbitratorVote)) public arbitratorVotes;

    /// @notice Vote counts per dispute
    mapping(uint256 => mapping(Resolution => uint256)) public voteCounts;

    /// @notice Arbitrator information
    mapping(address => ArbitratorInfo) public arbitrators;

    /// @notice All arbitrator addresses
    address[] public arbitratorList;

    /// @notice Appeals
    mapping(uint256 => Appeal) public appeals;

    /// @notice User dispute history
    mapping(address => uint256[]) public userDisputes;

    /// @notice Contract dispute history
    mapping(address => uint256[]) public contractDisputes;

    // ============ Events ============

    event DisputeFiled(
        uint256 indexed disputeId,
        address indexed claimant,
        address indexed respondent,
        DisputeType disputeType,
        uint256 disputedAmount
    );
    event EvidenceSubmitted(uint256 indexed disputeId, address indexed party, string evidence);
    event ArbitratorAssigned(uint256 indexed disputeId, address indexed arbitrator);
    event VoteCommitted(uint256 indexed disputeId, address indexed arbitrator);
    event VoteRevealed(uint256 indexed disputeId, address indexed arbitrator, Resolution vote);
    event DisputeResolved(
        uint256 indexed disputeId,
        Resolution resolution,
        uint256 claimantAward,
        uint256 respondentAward
    );
    event AppealFiled(uint256 indexed disputeId, address indexed appellant, string reason);
    event AppealResolved(uint256 indexed disputeId, Resolution newResolution);
    event ArbitratorRegistered(address indexed arbitrator, uint256 stake);
    event ArbitratorSlashed(address indexed arbitrator, uint256 amount, string reason);
    event ArbitratorRewarded(address indexed arbitrator, uint256 amount);
    event DisputeCancelled(uint256 indexed disputeId, address indexed canceller);

    // ============ Errors ============

    error InvalidDispute();
    error InvalidStatus();
    error UnauthorizedParty();
    error EvidenceDeadlinePassed();
    error EvidenceDeadlineNotPassed();
    error NotArbitrator();
    error AlreadyVoted();
    error VoteNotCommitted();
    error InvalidCommit();
    error ResolutionDeadlineNotPassed();
    error AppealPeriodPassed();
    error AppealNotAllowed();
    error InsufficientFee();
    error InsufficientStake();
    error ArbitratorAlreadyRegistered();
    error ArbitratorNotActive();
    error TooManyPendingCases();
    error CannotArbitrateOwnCase();

    // ============ Constructor ============

    constructor(
        address _paymentToken,
        address _treasury,
        address _admin
    ) {
        paymentToken = IERC20(_paymentToken);
        treasury = _treasury;

        _grantRole(DEFAULT_ADMIN_ROLE, _admin);
        _grantRole(MODERATOR_ROLE, _admin);
    }

    // ============ View Functions ============

    /**
     * @notice Get dispute details
     */
    function getDispute(uint256 disputeId) external view returns (Dispute memory) {
        return disputes[disputeId];
    }

    /**
     * @notice Get assigned arbitrators for a dispute
     */
    function getDisputeArbitrators(uint256 disputeId) external view returns (address[] memory) {
        return disputeArbitrators[disputeId];
    }

    /**
     * @notice Get user's disputes
     */
    function getUserDisputes(address user) external view returns (uint256[] memory) {
        return userDisputes[user];
    }

    /**
     * @notice Get contract's disputes
     */
    function getContractDisputes(address contractAddr) external view returns (uint256[] memory) {
        return contractDisputes[contractAddr];
    }

    /**
     * @notice Check if address is active arbitrator
     */
    function isActiveArbitrator(address addr) public view returns (bool) {
        return arbitrators[addr].isActive && 
               arbitrators[addr].stakedAmount >= MIN_ARBITRATOR_STAKE;
    }

    /**
     * @notice Calculate filing fee
     */
    function calculateFilingFee(uint256 amount) public view returns (uint256) {
        uint256 fee = (amount * filingFeeRate) / BASIS_POINTS;
        return fee > 0 ? fee : 1; // Minimum 1 wei
    }

    // ============ Arbitrator Functions ============

    /**
     * @notice Register as an arbitrator
     */
    function registerArbitrator(uint256 stakeAmount) external nonReentrant {
        if (arbitrators[msg.sender].arbitratorAddress != address(0)) {
            revert ArbitratorAlreadyRegistered();
        }
        if (stakeAmount < MIN_ARBITRATOR_STAKE) revert InsufficientStake();

        paymentToken.safeTransferFrom(msg.sender, address(this), stakeAmount);

        arbitrators[msg.sender] = ArbitratorInfo({
            arbitratorAddress: msg.sender,
            stakedAmount: stakeAmount,
            casesResolved: 0,
            successRate: 5000, // Start at 50%
            isActive: true,
            lastActiveAt: block.timestamp,
            pendingCases: 0
        });
        arbitratorList.push(msg.sender);

        _grantRole(ARBITRATOR_ROLE, msg.sender);

        emit ArbitratorRegistered(msg.sender, stakeAmount);
    }

    /**
     * @notice Add stake as arbitrator
     */
    function addArbitratorStake(uint256 amount) external nonReentrant {
        ArbitratorInfo storage arb = arbitrators[msg.sender];
        if (arb.arbitratorAddress == address(0)) revert NotArbitrator();

        paymentToken.safeTransferFrom(msg.sender, address(this), amount);
        arb.stakedAmount += amount;
    }

    /**
     * @notice Withdraw arbitrator stake (only if no pending cases)
     */
    function withdrawArbitratorStake(uint256 amount) external nonReentrant {
        ArbitratorInfo storage arb = arbitrators[msg.sender];
        if (arb.arbitratorAddress == address(0)) revert NotArbitrator();
        if (arb.pendingCases > 0) revert TooManyPendingCases();
        if (arb.stakedAmount < amount) revert InsufficientStake();

        arb.stakedAmount -= amount;
        if (arb.stakedAmount < MIN_ARBITRATOR_STAKE) {
            arb.isActive = false;
        }

        paymentToken.safeTransfer(msg.sender, amount);
    }

    // ============ Dispute Functions ============

    /**
     * @notice File a new dispute
     */
    function fileDispute(
        DisputeType disputeType,
        address respondent,
        address relatedContract,
        uint256 relatedTokenId,
        uint256 disputedAmount,
        string calldata initialEvidence
    ) external nonReentrant returns (uint256) {
        uint256 filingFee = calculateFilingFee(disputedAmount);
        paymentToken.safeTransferFrom(msg.sender, address(this), filingFee);

        uint256 disputeId = ++disputeCount;

        disputes[disputeId] = Dispute({
            disputeId: disputeId,
            disputeType: disputeType,
            status: DisputeStatus.Filed,
            claimant: msg.sender,
            respondent: respondent,
            relatedContract: relatedContract,
            relatedTokenId: relatedTokenId,
            disputedAmount: disputedAmount,
            filingFee: filingFee,
            filedAt: block.timestamp,
            evidenceDeadline: block.timestamp + EVIDENCE_PERIOD,
            resolutionDeadline: block.timestamp + EVIDENCE_PERIOD + ARBITRATION_PERIOD,
            claimantEvidence: initialEvidence,
            respondentEvidence: "",
            resolution: Resolution.None,
            claimantAward: 0,
            respondentAward: 0,
            arbitratorCommit: bytes32(0),
            appealable: true
        });

        userDisputes[msg.sender].push(disputeId);
        userDisputes[respondent].push(disputeId);
        if (relatedContract != address(0)) {
            contractDisputes[relatedContract].push(disputeId);
        }

        emit DisputeFiled(disputeId, msg.sender, respondent, disputeType, disputedAmount);

        return disputeId;
    }

    /**
     * @notice Submit evidence for a dispute
     */
    function submitEvidence(uint256 disputeId, string calldata evidence) external {
        Dispute storage dispute = disputes[disputeId];
        if (dispute.disputeId == 0) revert InvalidDispute();
        if (dispute.status != DisputeStatus.Filed && 
            dispute.status != DisputeStatus.EvidencePhase) revert InvalidStatus();
        if (block.timestamp > dispute.evidenceDeadline) revert EvidenceDeadlinePassed();

        if (msg.sender == dispute.claimant) {
            dispute.claimantEvidence = evidence;
        } else if (msg.sender == dispute.respondent) {
            dispute.respondentEvidence = evidence;
        } else {
            revert UnauthorizedParty();
        }

        if (dispute.status == DisputeStatus.Filed) {
            dispute.status = DisputeStatus.EvidencePhase;
        }

        emit EvidenceSubmitted(disputeId, msg.sender, evidence);
    }

    /**
     * @notice Move dispute to arbitration phase
     */
    function startArbitration(uint256 disputeId) external {
        Dispute storage dispute = disputes[disputeId];
        if (dispute.disputeId == 0) revert InvalidDispute();
        if (dispute.status != DisputeStatus.Filed && 
            dispute.status != DisputeStatus.EvidencePhase) revert InvalidStatus();
        if (block.timestamp < dispute.evidenceDeadline) revert EvidenceDeadlineNotPassed();

        dispute.status = DisputeStatus.Arbitration;
        _assignArbitrators(disputeId);
    }

    /**
     * @notice Commit a vote (commit-reveal scheme)
     */
    function commitVote(uint256 disputeId, bytes32 commitHash) external {
        Dispute storage dispute = disputes[disputeId];
        if (dispute.status != DisputeStatus.Arbitration) revert InvalidStatus();
        if (!_isAssignedArbitrator(disputeId, msg.sender)) revert NotArbitrator();
        if (arbitratorVotes[disputeId][msg.sender].commitHash != bytes32(0)) revert AlreadyVoted();

        arbitratorVotes[disputeId][msg.sender] = ArbitratorVote({
            arbitrator: msg.sender,
            vote: Resolution.None,
            claimantPercentage: 0,
            timestamp: block.timestamp,
            revealed: false,
            commitHash: commitHash
        });

        emit VoteCommitted(disputeId, msg.sender);
    }

    /**
     * @notice Reveal a vote
     */
    function revealVote(
        uint256 disputeId,
        Resolution vote,
        uint256 claimantPercentage,
        bytes32 salt
    ) external {
        Dispute storage dispute = disputes[disputeId];
        if (dispute.status != DisputeStatus.Arbitration) revert InvalidStatus();
        
        ArbitratorVote storage arbVote = arbitratorVotes[disputeId][msg.sender];
        if (arbVote.commitHash == bytes32(0)) revert VoteNotCommitted();
        if (arbVote.revealed) revert AlreadyVoted();

        // Verify commit
        bytes32 expectedHash = keccak256(abi.encodePacked(vote, claimantPercentage, salt));
        if (expectedHash != arbVote.commitHash) revert InvalidCommit();

        arbVote.vote = vote;
        arbVote.claimantPercentage = claimantPercentage;
        arbVote.revealed = true;

        voteCounts[disputeId][vote]++;

        emit VoteRevealed(disputeId, msg.sender, vote);
    }

    /**
     * @notice Finalize dispute resolution
     */
    function resolveDispute(uint256 disputeId) external nonReentrant {
        Dispute storage dispute = disputes[disputeId];
        if (dispute.status != DisputeStatus.Arbitration) revert InvalidStatus();
        if (block.timestamp < dispute.resolutionDeadline) revert ResolutionDeadlineNotPassed();

        // Tally votes
        Resolution winningResolution = _tallyVotes(disputeId);
        
        // Calculate awards
        (uint256 claimantAward, uint256 respondentAward) = _calculateAwards(
            disputeId,
            winningResolution
        );

        dispute.resolution = winningResolution;
        dispute.claimantAward = claimantAward;
        dispute.respondentAward = respondentAward;
        dispute.status = DisputeStatus.Resolved;

        // Distribute awards
        _distributeAwards(dispute);

        // Reward/slash arbitrators
        _processArbitratorOutcomes(disputeId, winningResolution);

        emit DisputeResolved(disputeId, winningResolution, claimantAward, respondentAward);
    }

    /**
     * @notice File an appeal
     */
    function fileAppeal(uint256 disputeId, string calldata reason) external nonReentrant {
        Dispute storage dispute = disputes[disputeId];
        if (dispute.status != DisputeStatus.Resolved) revert InvalidStatus();
        if (!dispute.appealable) revert AppealNotAllowed();
        if (msg.sender != dispute.claimant && msg.sender != dispute.respondent) {
            revert UnauthorizedParty();
        }
        if (block.timestamp > dispute.resolutionDeadline + APPEAL_PERIOD) {
            revert AppealPeriodPassed();
        }

        uint256 appealFee = dispute.filingFee * appealFeeMultiplier;
        paymentToken.safeTransferFrom(msg.sender, address(this), appealFee);

        appeals[disputeId] = Appeal({
            disputeId: disputeId,
            appellant: msg.sender,
            reason: reason,
            appealFee: appealFee,
            filedAt: block.timestamp,
            deadline: block.timestamp + APPEAL_RESOLUTION_PERIOD,
            originalResolution: dispute.resolution,
            appealResolution: Resolution.None,
            resolved: false
        });

        dispute.status = DisputeStatus.Appealed;
        dispute.appealable = false; // Only one appeal allowed

        // Assign new arbitrators for appeal
        _assignArbitrators(disputeId);

        emit AppealFiled(disputeId, msg.sender, reason);
    }

    /**
     * @notice Cancel dispute (only by claimant before arbitration)
     */
    function cancelDispute(uint256 disputeId) external nonReentrant {
        Dispute storage dispute = disputes[disputeId];
        if (dispute.disputeId == 0) revert InvalidDispute();
        if (msg.sender != dispute.claimant) revert UnauthorizedParty();
        if (dispute.status != DisputeStatus.Filed && 
            dispute.status != DisputeStatus.EvidencePhase) revert InvalidStatus();

        dispute.status = DisputeStatus.Cancelled;

        // Return half of filing fee
        uint256 refund = dispute.filingFee / 2;
        if (refund > 0) {
            paymentToken.safeTransfer(msg.sender, refund);
        }

        emit DisputeCancelled(disputeId, msg.sender);
    }

    // ============ Internal Functions ============

    function _assignArbitrators(uint256 disputeId) internal {
        Dispute storage dispute = disputes[disputeId];
        delete disputeArbitrators[disputeId];

        uint256 needed = MIN_ARBITRATORS_PER_CASE;
        uint256 assigned = 0;

        // Simple round-robin assignment (in production, use randomness)
        for (uint256 i = 0; i < arbitratorList.length && assigned < needed; i++) {
            address arb = arbitratorList[i];
            if (isActiveArbitrator(arb) &&
                arb != dispute.claimant &&
                arb != dispute.respondent &&
                arbitrators[arb].pendingCases < 5) {
                
                disputeArbitrators[disputeId].push(arb);
                arbitrators[arb].pendingCases++;
                assigned++;
                
                emit ArbitratorAssigned(disputeId, arb);
            }
        }
    }

    function _isAssignedArbitrator(uint256 disputeId, address addr) internal view returns (bool) {
        address[] storage assigned = disputeArbitrators[disputeId];
        for (uint256 i = 0; i < assigned.length; i++) {
            if (assigned[i] == addr) return true;
        }
        return false;
    }

    function _tallyVotes(uint256 disputeId) internal view returns (Resolution) {
        uint256 favorClaimant = voteCounts[disputeId][Resolution.FavorClaimant];
        uint256 favorRespondent = voteCounts[disputeId][Resolution.FavorRespondent];
        uint256 split = voteCounts[disputeId][Resolution.Split];
        uint256 dismissed = voteCounts[disputeId][Resolution.Dismissed];

        if (favorClaimant >= favorRespondent && favorClaimant >= split && favorClaimant >= dismissed) {
            return Resolution.FavorClaimant;
        }
        if (favorRespondent >= split && favorRespondent >= dismissed) {
            return Resolution.FavorRespondent;
        }
        if (split >= dismissed) {
            return Resolution.Split;
        }
        return Resolution.Dismissed;
    }

    function _calculateAwards(
        uint256 disputeId,
        Resolution resolution
    ) internal view returns (uint256 claimantAward, uint256 respondentAward) {
        Dispute storage dispute = disputes[disputeId];
        uint256 pool = dispute.disputedAmount + dispute.filingFee;

        if (resolution == Resolution.FavorClaimant) {
            claimantAward = pool;
            respondentAward = 0;
        } else if (resolution == Resolution.FavorRespondent) {
            claimantAward = 0;
            respondentAward = pool;
        } else if (resolution == Resolution.Split) {
            // Calculate average split from arbitrator votes
            uint256 totalPercentage = 0;
            uint256 voteCount = 0;
            address[] storage arbs = disputeArbitrators[disputeId];
            
            for (uint256 i = 0; i < arbs.length; i++) {
                ArbitratorVote storage v = arbitratorVotes[disputeId][arbs[i]];
                if (v.revealed) {
                    totalPercentage += v.claimantPercentage;
                    voteCount++;
                }
            }
            
            uint256 avgPercentage = voteCount > 0 ? totalPercentage / voteCount : 5000;
            claimantAward = (pool * avgPercentage) / BASIS_POINTS;
            respondentAward = pool - claimantAward;
        } else {
            // Dismissed - return to respondent (status quo)
            claimantAward = 0;
            respondentAward = pool;
        }
    }

    function _distributeAwards(Dispute storage dispute) internal {
        if (dispute.claimantAward > 0) {
            paymentToken.safeTransfer(dispute.claimant, dispute.claimantAward);
        }
        if (dispute.respondentAward > 0) {
            paymentToken.safeTransfer(dispute.respondent, dispute.respondentAward);
        }
    }

    function _processArbitratorOutcomes(uint256 disputeId, Resolution winningResolution) internal {
        address[] storage arbs = disputeArbitrators[disputeId];
        uint256 rewardPerArbitrator = disputes[disputeId].filingFee / (arbs.length * 10);

        for (uint256 i = 0; i < arbs.length; i++) {
            address arb = arbs[i];
            ArbitratorVote storage vote = arbitratorVotes[disputeId][arb];
            ArbitratorInfo storage info = arbitrators[arb];

            info.pendingCases--;
            info.casesResolved++;
            info.lastActiveAt = block.timestamp;

            if (vote.revealed && vote.vote == winningResolution) {
                // Reward for correct vote
                info.successRate = (info.successRate * 9 + BASIS_POINTS) / 10;
                if (rewardPerArbitrator > 0) {
                    paymentToken.safeTransfer(arb, rewardPerArbitrator);
                    emit ArbitratorRewarded(arb, rewardPerArbitrator);
                }
            } else if (!vote.revealed) {
                // Slash for not voting
                uint256 slashAmount = info.stakedAmount / 20; // 5% slash
                info.stakedAmount -= slashAmount;
                paymentToken.safeTransfer(treasury, slashAmount);
                emit ArbitratorSlashed(arb, slashAmount, "Failed to vote");
            }
        }
    }

    // ============ Admin Functions ============

    function setFilingFeeRate(uint256 rate) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(rate <= 1000, "Max 10%");
        filingFeeRate = rate;
    }

    function setAppealFeeMultiplier(uint256 multiplier) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(multiplier >= 1 && multiplier <= 10, "Invalid multiplier");
        appealFeeMultiplier = multiplier;
    }

    function setTreasury(address _treasury) external onlyRole(DEFAULT_ADMIN_ROLE) {
        treasury = _treasury;
    }

    /**
     * @notice Emergency arbitrator deactivation
     */
    function deactivateArbitrator(address arb) external onlyRole(MODERATOR_ROLE) {
        arbitrators[arb].isActive = false;
        _revokeRole(ARBITRATOR_ROLE, arb);
    }
}
