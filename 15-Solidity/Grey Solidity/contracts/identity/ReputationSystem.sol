// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title ReputationSystem
 * @notice On-chain reputation tracking with staking and slashing
 * @dev Implements composable reputation scores across protocols
 * 
 * Features:
 * - Multi-dimensional reputation scoring
 * - Stake-weighted reputation
 * - Reputation decay over time
 * - Cross-protocol score aggregation
 * - Reputation-gated access control
 * - Slashing for bad behavior
 * - Appeals and dispute resolution
 */
contract ReputationSystem is AccessControl, ReentrancyGuard {
    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant REPORTER_ROLE = keccak256("REPORTER_ROLE");
    bytes32 public constant ARBITER_ROLE = keccak256("ARBITER_ROLE");
    bytes32 public constant PROTOCOL_ROLE = keccak256("PROTOCOL_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Reputation score structure
    struct ReputationScore {
        int256 baseScore;           // Base reputation score
        int256 weightedScore;       // Stake-weighted score
        uint256 totalPositive;      // Total positive actions
        uint256 totalNegative;      // Total negative actions
        uint256 lastUpdateTime;
        uint256 stakeAmount;        // Stake backing reputation
        uint256 createdAt;
        bool exists;
    }

    /// @notice Dimension-specific scores
    struct DimensionScore {
        int256 score;
        uint256 actionCount;
        uint256 lastUpdate;
    }

    /// @notice Reputation action
    struct ReputationAction {
        bytes32 actionId;
        address subject;
        address reporter;
        bytes32 dimension;
        int256 scoreChange;
        string reason;
        uint256 timestamp;
        ActionStatus status;
        uint256 weight;             // Reporter weight
    }

    /// @notice Dispute for reputation action
    struct Dispute {
        uint256 disputeId;
        bytes32 actionId;
        address disputant;
        string evidence;
        uint256 stakeAmount;
        uint256 createdAt;
        uint256 deadline;
        DisputeStatus status;
        address arbiter;
        string resolution;
    }

    /// @notice Reputation threshold for access
    struct ReputationGate {
        bytes32 gateId;
        int256 minScore;
        bytes32[] requiredDimensions;
        int256[] dimensionMinScores;
        uint256 minStake;
        bool active;
    }

    enum ActionStatus {
        Pending,
        Applied,
        Disputed,
        Reversed,
        Voided
    }

    enum DisputeStatus {
        Open,
        UnderReview,
        ResolvedForDisputer,
        ResolvedAgainstDisputer,
        Expired
    }

    // ============================================
    // CONSTANTS
    // ============================================

    uint256 public constant DECAY_PERIOD = 30 days;
    uint256 public constant DECAY_RATE = 5; // 5% per period
    uint256 public constant MIN_DISPUTE_STAKE = 0.1 ether;
    uint256 public constant DISPUTE_PERIOD = 7 days;
    int256 public constant MAX_SCORE = 10000;
    int256 public constant MIN_SCORE = -10000;
    uint256 public constant STAKE_WEIGHT_MULTIPLIER = 100;

    // ============================================
    // STATE
    // ============================================

    /// @notice Reputation scores by address
    mapping(address => ReputationScore) public reputations;

    /// @notice Dimension scores: address => dimension => score
    mapping(address => mapping(bytes32 => DimensionScore)) public dimensionScores;

    /// @notice User dimensions
    mapping(address => bytes32[]) public userDimensions;

    /// @notice Reputation actions by ID
    mapping(bytes32 => ReputationAction) public actions;

    /// @notice User action history
    mapping(address => bytes32[]) public userActionHistory;

    /// @notice Disputes
    mapping(uint256 => Dispute) public disputes;
    uint256 public disputeCounter;

    /// @notice Action to dispute mapping
    mapping(bytes32 => uint256) public actionDisputes;

    /// @notice Reputation gates
    mapping(bytes32 => ReputationGate) public gates;

    /// @notice Protocol scores (external sources)
    mapping(address => mapping(address => int256)) public protocolScores;

    /// @notice Registered protocols
    mapping(address => bool) public registeredProtocols;

    /// @notice Reporter weights
    mapping(address => uint256) public reporterWeights;

    /// @notice Dimension definitions
    mapping(bytes32 => string) public dimensionNames;
    bytes32[] public dimensions;

    // ============================================
    // EVENTS
    // ============================================

    event ReputationCreated(
        address indexed user,
        uint256 timestamp
    );

    event ReputationUpdated(
        address indexed user,
        int256 oldScore,
        int256 newScore,
        bytes32 dimension
    );

    event ActionRecorded(
        bytes32 indexed actionId,
        address indexed subject,
        address indexed reporter,
        bytes32 dimension,
        int256 scoreChange
    );

    event StakeUpdated(
        address indexed user,
        uint256 oldStake,
        uint256 newStake
    );

    event DisputeCreated(
        uint256 indexed disputeId,
        bytes32 indexed actionId,
        address indexed disputant
    );

    event DisputeResolved(
        uint256 indexed disputeId,
        DisputeStatus status,
        address arbiter
    );

    event ReputationSlashed(
        address indexed user,
        int256 slashAmount,
        string reason
    );

    event GateCreated(
        bytes32 indexed gateId,
        int256 minScore
    );

    event DimensionRegistered(
        bytes32 indexed dimension,
        string name
    );

    // ============================================
    // ERRORS
    // ============================================

    error UserNotRegistered();
    error AlreadyRegistered();
    error ActionNotFound();
    error DisputeNotFound();
    error DisputeAlreadyExists();
    error InsufficientStake();
    error DisputePeriodExpired();
    error DisputeNotOpen();
    error NotArbiter();
    error BelowReputationThreshold();
    error InvalidDimension();
    error InvalidScoreChange();
    error GateNotActive();
    error Unauthorized();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(ARBITER_ROLE, msg.sender);

        // Register default dimensions
        _registerDimension("GENERAL", "General reputation");
        _registerDimension("TRADING", "Trading activity");
        _registerDimension("GOVERNANCE", "Governance participation");
        _registerDimension("LENDING", "Lending/borrowing history");
        _registerDimension("SECURITY", "Security-related behavior");
    }

    // ============================================
    // REGISTRATION
    // ============================================

    /**
     * @notice Register for reputation tracking
     */
    function register() external {
        if (reputations[msg.sender].exists) revert AlreadyRegistered();

        reputations[msg.sender] = ReputationScore({
            baseScore: 0,
            weightedScore: 0,
            totalPositive: 0,
            totalNegative: 0,
            lastUpdateTime: block.timestamp,
            stakeAmount: 0,
            createdAt: block.timestamp,
            exists: true
        });

        emit ReputationCreated(msg.sender, block.timestamp);
    }

    /**
     * @notice Stake ETH to back reputation
     */
    function stake() external payable nonReentrant {
        if (!reputations[msg.sender].exists) revert UserNotRegistered();

        uint256 oldStake = reputations[msg.sender].stakeAmount;
        reputations[msg.sender].stakeAmount += msg.value;

        _recalculateWeightedScore(msg.sender);

        emit StakeUpdated(msg.sender, oldStake, reputations[msg.sender].stakeAmount);
    }

    /**
     * @notice Withdraw stake (affects weighted score)
     */
    function unstake(uint256 amount) external nonReentrant {
        ReputationScore storage rep = reputations[msg.sender];
        
        if (!rep.exists) revert UserNotRegistered();
        if (rep.stakeAmount < amount) revert InsufficientStake();

        uint256 oldStake = rep.stakeAmount;
        rep.stakeAmount -= amount;

        _recalculateWeightedScore(msg.sender);

        payable(msg.sender).transfer(amount);

        emit StakeUpdated(msg.sender, oldStake, rep.stakeAmount);
    }

    // ============================================
    // REPUTATION ACTIONS
    // ============================================

    /**
     * @notice Record a reputation action
     */
    function recordAction(
        address subject,
        bytes32 dimension,
        int256 scoreChange,
        string calldata reason
    ) external onlyRole(REPORTER_ROLE) returns (bytes32 actionId) {
        if (!reputations[subject].exists) revert UserNotRegistered();
        if (bytes(dimensionNames[dimension]).length == 0) revert InvalidDimension();
        if (scoreChange > MAX_SCORE || scoreChange < MIN_SCORE) revert InvalidScoreChange();

        actionId = keccak256(abi.encodePacked(
            subject,
            msg.sender,
            dimension,
            scoreChange,
            block.timestamp
        ));

        uint256 weight = reporterWeights[msg.sender] > 0 ? reporterWeights[msg.sender] : 100;

        actions[actionId] = ReputationAction({
            actionId: actionId,
            subject: subject,
            reporter: msg.sender,
            dimension: dimension,
            scoreChange: scoreChange,
            reason: reason,
            timestamp: block.timestamp,
            status: ActionStatus.Applied,
            weight: weight
        });

        userActionHistory[subject].push(actionId);

        // Apply score change
        _applyScoreChange(subject, dimension, scoreChange, weight);

        emit ActionRecorded(actionId, subject, msg.sender, dimension, scoreChange);
    }

    /**
     * @notice Record action from external protocol
     */
    function recordProtocolAction(
        address subject,
        bytes32 dimension,
        int256 scoreChange,
        string calldata reason
    ) external onlyRole(PROTOCOL_ROLE) returns (bytes32 actionId) {
        if (!reputations[subject].exists) {
            // Auto-register users receiving protocol actions
            reputations[subject] = ReputationScore({
                baseScore: 0,
                weightedScore: 0,
                totalPositive: 0,
                totalNegative: 0,
                lastUpdateTime: block.timestamp,
                stakeAmount: 0,
                createdAt: block.timestamp,
                exists: true
            });
        }

        actionId = keccak256(abi.encodePacked(
            subject,
            msg.sender,
            dimension,
            scoreChange,
            block.timestamp
        ));

        // Protocol actions have higher weight
        uint256 weight = 150;

        actions[actionId] = ReputationAction({
            actionId: actionId,
            subject: subject,
            reporter: msg.sender,
            dimension: dimension,
            scoreChange: scoreChange,
            reason: reason,
            timestamp: block.timestamp,
            status: ActionStatus.Applied,
            weight: weight
        });

        userActionHistory[subject].push(actionId);
        _applyScoreChange(subject, dimension, scoreChange, weight);

        // Store protocol-specific score
        protocolScores[subject][msg.sender] += scoreChange;

        emit ActionRecorded(actionId, subject, msg.sender, dimension, scoreChange);
    }

    /**
     * @notice Apply score change internally
     */
    function _applyScoreChange(
        address subject,
        bytes32 dimension,
        int256 change,
        uint256 weight
    ) internal {
        ReputationScore storage rep = reputations[subject];
        DimensionScore storage dimScore = dimensionScores[subject][dimension];

        // Apply decay first
        _applyDecay(subject);

        int256 oldScore = rep.baseScore;
        int256 weightedChange = (change * int256(weight)) / 100;

        // Update base score
        rep.baseScore += weightedChange;
        
        // Clamp to bounds
        if (rep.baseScore > MAX_SCORE) rep.baseScore = MAX_SCORE;
        if (rep.baseScore < MIN_SCORE) rep.baseScore = MIN_SCORE;

        // Update dimension score
        dimScore.score += change;
        dimScore.actionCount++;
        dimScore.lastUpdate = block.timestamp;

        // Track dimension if new
        if (dimScore.actionCount == 1) {
            userDimensions[subject].push(dimension);
        }

        // Update counters
        if (change > 0) {
            rep.totalPositive++;
        } else if (change < 0) {
            rep.totalNegative++;
        }

        rep.lastUpdateTime = block.timestamp;
        _recalculateWeightedScore(subject);

        emit ReputationUpdated(subject, oldScore, rep.baseScore, dimension);
    }

    /**
     * @notice Recalculate stake-weighted score
     */
    function _recalculateWeightedScore(address user) internal {
        ReputationScore storage rep = reputations[user];
        
        // Weighted score = base score + (stake / 1 ether) * multiplier
        uint256 stakeBonus = (rep.stakeAmount * STAKE_WEIGHT_MULTIPLIER) / 1 ether;
        rep.weightedScore = rep.baseScore + int256(stakeBonus);
    }

    /**
     * @notice Apply time-based decay
     */
    function _applyDecay(address user) internal {
        ReputationScore storage rep = reputations[user];
        
        if (block.timestamp < rep.lastUpdateTime + DECAY_PERIOD) return;

        uint256 periodsElapsed = (block.timestamp - rep.lastUpdateTime) / DECAY_PERIOD;
        
        // Decay towards zero
        for (uint256 i = 0; i < periodsElapsed && i < 10; i++) {
            if (rep.baseScore > 0) {
                rep.baseScore -= (rep.baseScore * int256(DECAY_RATE)) / 100;
            } else if (rep.baseScore < 0) {
                rep.baseScore -= (rep.baseScore * int256(DECAY_RATE)) / 100; // Less negative
            }
        }
    }

    // ============================================
    // DISPUTES
    // ============================================

    /**
     * @notice Dispute a reputation action
     */
    function disputeAction(
        bytes32 actionId,
        string calldata evidence
    ) external payable nonReentrant returns (uint256 disputeId) {
        ReputationAction storage action = actions[actionId];

        if (action.subject == address(0)) revert ActionNotFound();
        if (action.subject != msg.sender) revert Unauthorized();
        if (action.status != ActionStatus.Applied) revert ActionNotFound();
        if (actionDisputes[actionId] != 0) revert DisputeAlreadyExists();
        if (msg.value < MIN_DISPUTE_STAKE) revert InsufficientStake();
        if (block.timestamp > action.timestamp + DISPUTE_PERIOD) revert DisputePeriodExpired();

        disputeId = ++disputeCounter;

        disputes[disputeId] = Dispute({
            disputeId: disputeId,
            actionId: actionId,
            disputant: msg.sender,
            evidence: evidence,
            stakeAmount: msg.value,
            createdAt: block.timestamp,
            deadline: block.timestamp + DISPUTE_PERIOD,
            status: DisputeStatus.Open,
            arbiter: address(0),
            resolution: ""
        });

        actionDisputes[actionId] = disputeId;
        action.status = ActionStatus.Disputed;

        emit DisputeCreated(disputeId, actionId, msg.sender);
    }

    /**
     * @notice Resolve a dispute (arbiter only)
     */
    function resolveDispute(
        uint256 disputeId,
        bool inFavorOfDisputant,
        string calldata resolution
    ) external onlyRole(ARBITER_ROLE) nonReentrant {
        Dispute storage dispute = disputes[disputeId];

        if (dispute.disputeId == 0) revert DisputeNotFound();
        if (dispute.status != DisputeStatus.Open && dispute.status != DisputeStatus.UnderReview) {
            revert DisputeNotOpen();
        }

        dispute.arbiter = msg.sender;
        dispute.resolution = resolution;

        ReputationAction storage action = actions[dispute.actionId];

        if (inFavorOfDisputant) {
            dispute.status = DisputeStatus.ResolvedForDisputer;
            action.status = ActionStatus.Reversed;

            // Reverse the score change
            _applyScoreChange(
                action.subject,
                action.dimension,
                -action.scoreChange,
                action.weight
            );

            // Return stake + bonus
            uint256 bonus = dispute.stakeAmount / 10;
            payable(dispute.disputant).transfer(dispute.stakeAmount + bonus);
        } else {
            dispute.status = DisputeStatus.ResolvedAgainstDisputer;
            action.status = ActionStatus.Applied;

            // Stake is forfeited to protocol
        }

        emit DisputeResolved(disputeId, dispute.status, msg.sender);
    }

    // ============================================
    // SLASHING
    // ============================================

    /**
     * @notice Slash user reputation and stake
     */
    function slash(
        address user,
        int256 scoreSlash,
        uint256 stakeSlash,
        string calldata reason
    ) external onlyRole(ARBITER_ROLE) nonReentrant {
        ReputationScore storage rep = reputations[user];

        if (!rep.exists) revert UserNotRegistered();

        // Slash score
        rep.baseScore -= scoreSlash;
        if (rep.baseScore < MIN_SCORE) rep.baseScore = MIN_SCORE;

        // Slash stake
        if (stakeSlash > 0 && rep.stakeAmount > 0) {
            uint256 slashAmount = stakeSlash > rep.stakeAmount ? rep.stakeAmount : stakeSlash;
            rep.stakeAmount -= slashAmount;
            // Slashed stake goes to protocol treasury
        }

        _recalculateWeightedScore(user);

        emit ReputationSlashed(user, scoreSlash, reason);
    }

    // ============================================
    // REPUTATION GATES
    // ============================================

    /**
     * @notice Create a reputation gate
     */
    function createGate(
        bytes32 gateId,
        int256 minScore,
        bytes32[] calldata requiredDimensions,
        int256[] calldata dimensionMinScores,
        uint256 minStake
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        require(requiredDimensions.length == dimensionMinScores.length, "Length mismatch");

        gates[gateId] = ReputationGate({
            gateId: gateId,
            minScore: minScore,
            requiredDimensions: requiredDimensions,
            dimensionMinScores: dimensionMinScores,
            minStake: minStake,
            active: true
        });

        emit GateCreated(gateId, minScore);
    }

    /**
     * @notice Check if user passes reputation gate
     */
    function checkGate(bytes32 gateId, address user) external view returns (bool) {
        ReputationGate memory gate = gates[gateId];
        if (!gate.active) revert GateNotActive();

        ReputationScore memory rep = reputations[user];
        if (!rep.exists) return false;

        // Check overall score
        if (rep.weightedScore < gate.minScore) return false;

        // Check stake
        if (rep.stakeAmount < gate.minStake) return false;

        // Check dimension requirements
        for (uint256 i = 0; i < gate.requiredDimensions.length; i++) {
            DimensionScore memory dimScore = dimensionScores[user][gate.requiredDimensions[i]];
            if (dimScore.score < gate.dimensionMinScores[i]) return false;
        }

        return true;
    }

    /**
     * @notice Modifier-style gate check
     */
    function requireReputation(bytes32 gateId, address user) external view {
        ReputationGate memory gate = gates[gateId];
        if (!gate.active) revert GateNotActive();

        ReputationScore memory rep = reputations[user];
        if (!rep.exists) revert UserNotRegistered();
        if (rep.weightedScore < gate.minScore) revert BelowReputationThreshold();
        if (rep.stakeAmount < gate.minStake) revert InsufficientStake();

        for (uint256 i = 0; i < gate.requiredDimensions.length; i++) {
            DimensionScore memory dimScore = dimensionScores[user][gate.requiredDimensions[i]];
            if (dimScore.score < gate.dimensionMinScores[i]) {
                revert BelowReputationThreshold();
            }
        }
    }

    // ============================================
    // DIMENSION MANAGEMENT
    // ============================================

    /**
     * @notice Register a new reputation dimension
     */
    function registerDimension(
        string calldata key,
        string calldata name
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        _registerDimension(key, name);
    }

    function _registerDimension(string memory key, string memory name) internal {
        bytes32 dimension = keccak256(abi.encodePacked(key));
        dimensionNames[dimension] = name;
        dimensions.push(dimension);

        emit DimensionRegistered(dimension, name);
    }

    // ============================================
    // ADMIN FUNCTIONS
    // ============================================

    function setReporterWeight(address reporter, uint256 weight) external onlyRole(DEFAULT_ADMIN_ROLE) {
        reporterWeights[reporter] = weight;
    }

    function registerProtocol(address protocol) external onlyRole(DEFAULT_ADMIN_ROLE) {
        registeredProtocols[protocol] = true;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getReputation(address user) external view returns (
        int256 baseScore,
        int256 weightedScore,
        uint256 totalPositive,
        uint256 totalNegative,
        uint256 stakeAmount
    ) {
        ReputationScore memory rep = reputations[user];
        return (
            rep.baseScore,
            rep.weightedScore,
            rep.totalPositive,
            rep.totalNegative,
            rep.stakeAmount
        );
    }

    function getDimensionScore(
        address user,
        bytes32 dimension
    ) external view returns (int256 score, uint256 actionCount) {
        DimensionScore memory dimScore = dimensionScores[user][dimension];
        return (dimScore.score, dimScore.actionCount);
    }

    function getUserDimensions(address user) external view returns (bytes32[] memory) {
        return userDimensions[user];
    }

    function getUserActionHistory(address user) external view returns (bytes32[] memory) {
        return userActionHistory[user];
    }

    function getAllDimensions() external view returns (bytes32[] memory) {
        return dimensions;
    }

    receive() external payable {}
}
