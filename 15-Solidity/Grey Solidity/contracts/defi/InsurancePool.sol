// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

/**
 * @title InsurancePool
 * @author Grey Team
 * @notice Decentralized insurance protocol for DeFi coverage
 * @dev Implements parametric and claim-based insurance with staker capital,
 *      risk assessment, claim adjudication, and automatic payouts
 * 
 * Architecture inspired by Nexus Mutual, InsurAce, and Unslashed:
 * - Capital pools with risk-weighted staking
 * - Multiple coverage types (smart contract, oracle, depeg, slashing)
 * - Claim assessment with stake-weighted voting
 * - Parametric triggers for automatic payouts
 * - Premium calculation based on risk and utilization
 */
contract InsurancePool is AccessControl, ReentrancyGuard, Pausable {
    using SafeERC20 for IERC20;

    // ============ Roles ============
    bytes32 public constant RISK_MANAGER_ROLE = keccak256("RISK_MANAGER_ROLE");
    bytes32 public constant CLAIMS_ASSESSOR_ROLE = keccak256("CLAIMS_ASSESSOR_ROLE");
    bytes32 public constant ORACLE_ROLE = keccak256("ORACLE_ROLE");
    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");

    // ============ Enums ============
    enum CoverageType { SMART_CONTRACT, ORACLE_FAILURE, STABLECOIN_DEPEG, SLASHING, CUSTODIAL }
    enum ClaimStatus { PENDING, ASSESSING, APPROVED, REJECTED, PAID, EXPIRED }
    enum PolicyStatus { ACTIVE, EXPIRED, CLAIMED, CANCELLED }

    // ============ Structs ============
    
    /**
     * @notice Protocol/product that can be insured
     */
    struct CoverableProtocol {
        address protocolAddress;
        string name;
        CoverageType coverageType;
        uint256 riskScore;              // 1-100, higher = riskier
        uint256 baseRateBps;            // Annual rate in basis points
        uint256 maxCoverage;            // Maximum coverage available
        uint256 currentCoverage;        // Coverage currently sold
        uint256 stakedCapital;          // Capital staked to this protocol
        uint256 minCapitalRatio;        // Minimum capital/coverage ratio (basis points)
        bool isActive;
        uint256 claimCount;
        uint256 paidClaims;
    }

    /**
     * @notice Insurance policy purchased by user
     */
    struct Policy {
        uint256 id;
        address holder;
        bytes32 protocolId;
        uint256 coverageAmount;
        uint256 premium;
        uint256 startTime;
        uint256 endTime;
        PolicyStatus status;
        uint256 claimId;                // If claimed
    }

    /**
     * @notice Claim submitted for a policy
     */
    struct Claim {
        uint256 id;
        uint256 policyId;
        address claimant;
        uint256 amount;
        string evidence;
        uint256 submitTime;
        ClaimStatus status;
        uint256 votesFor;
        uint256 votesAgainst;
        uint256 assessmentDeadline;
        bool parametricTriggered;
        mapping(address => bool) hasVoted;
    }

    /**
     * @notice Staker providing capital to the pool
     */
    struct Staker {
        uint256 stakedAmount;
        uint256 stakingTime;
        uint256 lockedUntil;
        uint256 rewardsDebt;
        uint256 claimedRewards;
        mapping(bytes32 => uint256) protocolAllocations;
    }

    /**
     * @notice Parametric trigger configuration
     */
    struct ParametricTrigger {
        bytes32 protocolId;
        address oracle;
        bytes32 feedId;
        int256 threshold;
        bool isAbove;               // True if trigger when value > threshold
        uint256 cooldownPeriod;
        uint256 lastTriggerTime;
        bool isActive;
    }

    // ============ Constants ============
    uint256 public constant PRECISION = 1e18;
    uint256 public constant YEAR = 365 days;
    uint256 public constant MIN_STAKE_PERIOD = 30 days;
    uint256 public constant CLAIM_ASSESSMENT_PERIOD = 7 days;
    uint256 public constant MAX_RISK_SCORE = 100;
    uint256 public constant MIN_CAPITAL_RATIO = 10000; // 100% minimum

    // ============ State Variables ============
    
    // Token for staking and payments
    IERC20 public immutable capitalToken;
    
    // Protocol registry
    mapping(bytes32 => CoverableProtocol) public protocols;
    bytes32[] public protocolIds;
    
    // Policies
    mapping(uint256 => Policy) public policies;
    mapping(address => uint256[]) public userPolicies;
    uint256 public nextPolicyId = 1;
    
    // Claims
    mapping(uint256 => Claim) public claims;
    uint256 public nextClaimId = 1;
    
    // Staking
    mapping(address => Staker) public stakers;
    uint256 public totalStaked;
    uint256 public totalCoverage;
    
    // Rewards
    uint256 public accRewardsPerShare;
    uint256 public lastRewardTime;
    uint256 public premiumsCollected;
    
    // Parametric triggers
    mapping(bytes32 => ParametricTrigger) public parametricTriggers;
    
    // Treasury
    address public treasury;
    uint256 public treasuryFee = 1000; // 10%
    
    // MCR (Minimum Capital Requirement)
    uint256 public globalMCR = 15000; // 150%

    // ============ Events ============
    event ProtocolAdded(bytes32 indexed protocolId, address protocolAddress, string name);
    event ProtocolUpdated(bytes32 indexed protocolId, uint256 riskScore, uint256 baseRateBps);
    event Staked(address indexed staker, uint256 amount);
    event Unstaked(address indexed staker, uint256 amount);
    event StakeAllocated(address indexed staker, bytes32 indexed protocolId, uint256 amount);
    event PolicyPurchased(uint256 indexed policyId, address indexed holder, bytes32 protocolId, uint256 coverage);
    event PolicyExpired(uint256 indexed policyId);
    event ClaimSubmitted(uint256 indexed claimId, uint256 indexed policyId, uint256 amount);
    event ClaimVoted(uint256 indexed claimId, address indexed voter, bool approve, uint256 weight);
    event ClaimResolved(uint256 indexed claimId, ClaimStatus status);
    event ClaimPaid(uint256 indexed claimId, address indexed claimant, uint256 amount);
    event ParametricTriggerSet(bytes32 indexed protocolId, address oracle, int256 threshold);
    event ParametricTriggered(bytes32 indexed protocolId, int256 value);
    event RewardsClaimed(address indexed staker, uint256 amount);

    // ============ Errors ============
    error ProtocolNotActive();
    error InsufficientCapacity();
    error PolicyNotActive();
    error PolicyNotExpired();
    error ClaimAlreadySubmitted();
    error ClaimNotPending();
    error AlreadyVoted();
    error AssessmentNotComplete();
    error InsufficientStake();
    error StakeLocked();
    error InvalidAmount();
    error InvalidPeriod();
    error BelowMCR();
    error TriggerOnCooldown();
    error UnauthorizedOracle();

    // ============ Constructor ============
    constructor(address _capitalToken, address _treasury) {
        require(_capitalToken != address(0), "Invalid token");
        require(_treasury != address(0), "Invalid treasury");
        
        capitalToken = IERC20(_capitalToken);
        treasury = _treasury;
        
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(RISK_MANAGER_ROLE, msg.sender);
        _grantRole(GOVERNANCE_ROLE, msg.sender);
        
        lastRewardTime = block.timestamp;
    }

    // ============ Protocol Management ============
    
    /**
     * @notice Add a new coverable protocol
     */
    function addProtocol(
        address protocolAddress,
        string calldata name,
        CoverageType coverageType,
        uint256 riskScore,
        uint256 baseRateBps,
        uint256 maxCoverage,
        uint256 minCapitalRatio
    ) external onlyRole(RISK_MANAGER_ROLE) {
        require(riskScore > 0 && riskScore <= MAX_RISK_SCORE, "Invalid risk score");
        require(minCapitalRatio >= MIN_CAPITAL_RATIO, "Ratio too low");
        
        bytes32 protocolId = keccak256(abi.encodePacked(protocolAddress, name));
        require(protocols[protocolId].protocolAddress == address(0), "Already exists");
        
        protocols[protocolId] = CoverableProtocol({
            protocolAddress: protocolAddress,
            name: name,
            coverageType: coverageType,
            riskScore: riskScore,
            baseRateBps: baseRateBps,
            maxCoverage: maxCoverage,
            currentCoverage: 0,
            stakedCapital: 0,
            minCapitalRatio: minCapitalRatio,
            isActive: true,
            claimCount: 0,
            paidClaims: 0
        });
        
        protocolIds.push(protocolId);
        
        emit ProtocolAdded(protocolId, protocolAddress, name);
    }

    /**
     * @notice Update protocol risk parameters
     */
    function updateProtocol(
        bytes32 protocolId,
        uint256 riskScore,
        uint256 baseRateBps,
        uint256 maxCoverage,
        bool isActive
    ) external onlyRole(RISK_MANAGER_ROLE) {
        CoverableProtocol storage protocol = protocols[protocolId];
        require(protocol.protocolAddress != address(0), "Protocol not found");
        
        protocol.riskScore = riskScore;
        protocol.baseRateBps = baseRateBps;
        protocol.maxCoverage = maxCoverage;
        protocol.isActive = isActive;
        
        emit ProtocolUpdated(protocolId, riskScore, baseRateBps);
    }

    // ============ Staking ============
    
    /**
     * @notice Stake capital into the insurance pool
     */
    function stake(uint256 amount) external nonReentrant whenNotPaused {
        if (amount == 0) revert InvalidAmount();
        
        _updateRewards();
        
        Staker storage staker = stakers[msg.sender];
        
        // Claim pending rewards first
        if (staker.stakedAmount > 0) {
            uint256 pending = _pendingRewards(msg.sender);
            if (pending > 0) {
                _payRewards(msg.sender, pending);
            }
        }
        
        capitalToken.safeTransferFrom(msg.sender, address(this), amount);
        
        staker.stakedAmount += amount;
        staker.stakingTime = block.timestamp;
        staker.lockedUntil = block.timestamp + MIN_STAKE_PERIOD;
        staker.rewardsDebt = (staker.stakedAmount * accRewardsPerShare) / PRECISION;
        
        totalStaked += amount;
        
        emit Staked(msg.sender, amount);
    }

    /**
     * @notice Unstake capital from the pool
     */
    function unstake(uint256 amount) external nonReentrant {
        Staker storage staker = stakers[msg.sender];
        
        if (amount > staker.stakedAmount) revert InsufficientStake();
        if (block.timestamp < staker.lockedUntil) revert StakeLocked();
        
        // Check MCR after unstake
        uint256 newTotalStaked = totalStaked - amount;
        if (totalCoverage > 0 && (newTotalStaked * 10000) / totalCoverage < globalMCR) {
            revert BelowMCR();
        }
        
        _updateRewards();
        
        // Claim pending rewards
        uint256 pending = _pendingRewards(msg.sender);
        if (pending > 0) {
            _payRewards(msg.sender, pending);
        }
        
        // Reduce allocations proportionally
        _reduceAllocations(msg.sender, amount);
        
        staker.stakedAmount -= amount;
        staker.rewardsDebt = (staker.stakedAmount * accRewardsPerShare) / PRECISION;
        
        totalStaked -= amount;
        
        capitalToken.safeTransfer(msg.sender, amount);
        
        emit Unstaked(msg.sender, amount);
    }

    /**
     * @notice Allocate stake to specific protocol for higher rewards
     */
    function allocateStake(bytes32 protocolId, uint256 amount) external nonReentrant {
        CoverableProtocol storage protocol = protocols[protocolId];
        if (!protocol.isActive) revert ProtocolNotActive();
        
        Staker storage staker = stakers[msg.sender];
        
        // Calculate total currently allocated
        uint256 totalAllocated = _getTotalAllocated(msg.sender);
        if (totalAllocated + amount > staker.stakedAmount) revert InsufficientStake();
        
        staker.protocolAllocations[protocolId] += amount;
        protocol.stakedCapital += amount;
        
        emit StakeAllocated(msg.sender, protocolId, amount);
    }

    /**
     * @notice Deallocate stake from a protocol
     */
    function deallocateStake(bytes32 protocolId, uint256 amount) external nonReentrant {
        Staker storage staker = stakers[msg.sender];
        
        if (staker.protocolAllocations[protocolId] < amount) revert InsufficientStake();
        
        // Check protocol still has capacity
        CoverableProtocol storage protocol = protocols[protocolId];
        uint256 newCapital = protocol.stakedCapital - amount;
        if (protocol.currentCoverage > 0 && 
            (newCapital * 10000) / protocol.currentCoverage < protocol.minCapitalRatio) {
            revert BelowMCR();
        }
        
        staker.protocolAllocations[protocolId] -= amount;
        protocol.stakedCapital -= amount;
        
        emit StakeAllocated(msg.sender, protocolId, staker.protocolAllocations[protocolId]);
    }

    // ============ Policy Purchase ============
    
    /**
     * @notice Purchase insurance coverage
     */
    function purchaseCoverage(
        bytes32 protocolId,
        uint256 coverageAmount,
        uint256 periodDays
    ) external nonReentrant whenNotPaused returns (uint256 policyId) {
        CoverableProtocol storage protocol = protocols[protocolId];
        
        if (!protocol.isActive) revert ProtocolNotActive();
        if (periodDays < 7 || periodDays > 365) revert InvalidPeriod();
        
        // Check capacity
        uint256 availableCoverage = _getAvailableCoverage(protocolId);
        if (coverageAmount > availableCoverage) revert InsufficientCapacity();
        
        // Calculate premium
        uint256 premium = calculatePremium(protocolId, coverageAmount, periodDays);
        
        // Collect premium
        capitalToken.safeTransferFrom(msg.sender, address(this), premium);
        
        // Treasury fee
        uint256 treasuryAmount = (premium * treasuryFee) / 10000;
        capitalToken.safeTransfer(treasury, treasuryAmount);
        
        premiumsCollected += premium - treasuryAmount;
        
        // Create policy
        policyId = nextPolicyId++;
        policies[policyId] = Policy({
            id: policyId,
            holder: msg.sender,
            protocolId: protocolId,
            coverageAmount: coverageAmount,
            premium: premium,
            startTime: block.timestamp,
            endTime: block.timestamp + (periodDays * 1 days),
            status: PolicyStatus.ACTIVE,
            claimId: 0
        });
        
        userPolicies[msg.sender].push(policyId);
        
        protocol.currentCoverage += coverageAmount;
        totalCoverage += coverageAmount;
        
        emit PolicyPurchased(policyId, msg.sender, protocolId, coverageAmount);
        
        return policyId;
    }

    /**
     * @notice Calculate premium for coverage
     */
    function calculatePremium(
        bytes32 protocolId,
        uint256 coverageAmount,
        uint256 periodDays
    ) public view returns (uint256) {
        CoverableProtocol storage protocol = protocols[protocolId];
        
        // Base rate adjusted by risk score
        uint256 riskMultiplier = 100 + protocol.riskScore; // 101-200%
        uint256 adjustedRate = (protocol.baseRateBps * riskMultiplier) / 100;
        
        // Utilization adjustment
        uint256 utilization = protocol.stakedCapital > 0 
            ? (protocol.currentCoverage * 10000) / protocol.stakedCapital 
            : 0;
        
        if (utilization > 5000) { // > 50%
            adjustedRate = adjustedRate * (100 + (utilization - 5000) / 100) / 100;
        }
        
        // Calculate annual premium, pro-rate for period
        uint256 annualPremium = (coverageAmount * adjustedRate) / 10000;
        uint256 premium = (annualPremium * periodDays) / 365;
        
        return premium;
    }

    // ============ Claims ============
    
    /**
     * @notice Submit a claim for a policy
     */
    function submitClaim(
        uint256 policyId,
        uint256 claimAmount,
        string calldata evidence
    ) external nonReentrant returns (uint256 claimId) {
        Policy storage policy = policies[policyId];
        
        if (policy.holder != msg.sender) revert PolicyNotActive();
        if (policy.status != PolicyStatus.ACTIVE) revert PolicyNotActive();
        if (block.timestamp > policy.endTime) revert PolicyNotActive();
        if (claimAmount > policy.coverageAmount) revert InvalidAmount();
        if (policy.claimId != 0) revert ClaimAlreadySubmitted();
        
        claimId = nextClaimId++;
        
        Claim storage claim = claims[claimId];
        claim.id = claimId;
        claim.policyId = policyId;
        claim.claimant = msg.sender;
        claim.amount = claimAmount;
        claim.evidence = evidence;
        claim.submitTime = block.timestamp;
        claim.status = ClaimStatus.ASSESSING;
        claim.assessmentDeadline = block.timestamp + CLAIM_ASSESSMENT_PERIOD;
        
        policy.claimId = claimId;
        policy.status = PolicyStatus.CLAIMED;
        
        CoverableProtocol storage protocol = protocols[policy.protocolId];
        protocol.claimCount++;
        
        emit ClaimSubmitted(claimId, policyId, claimAmount);
        
        return claimId;
    }

    /**
     * @notice Vote on a claim (stake-weighted voting)
     */
    function voteClaim(uint256 claimId, bool approve) external nonReentrant {
        Claim storage claim = claims[claimId];
        
        if (claim.status != ClaimStatus.ASSESSING) revert ClaimNotPending();
        if (block.timestamp > claim.assessmentDeadline) revert AssessmentNotComplete();
        if (claim.hasVoted[msg.sender]) revert AlreadyVoted();
        
        Staker storage staker = stakers[msg.sender];
        uint256 weight = staker.stakedAmount;
        if (weight == 0) revert InsufficientStake();
        
        claim.hasVoted[msg.sender] = true;
        
        if (approve) {
            claim.votesFor += weight;
        } else {
            claim.votesAgainst += weight;
        }
        
        emit ClaimVoted(claimId, msg.sender, approve, weight);
    }

    /**
     * @notice Resolve a claim after assessment period
     */
    function resolveClaim(uint256 claimId) external nonReentrant {
        Claim storage claim = claims[claimId];
        
        if (claim.status != ClaimStatus.ASSESSING) revert ClaimNotPending();
        if (block.timestamp < claim.assessmentDeadline && !claim.parametricTriggered) {
            revert AssessmentNotComplete();
        }
        
        Policy storage policy = policies[claim.policyId];
        
        bool approved = claim.parametricTriggered || claim.votesFor > claim.votesAgainst;
        
        if (approved) {
            claim.status = ClaimStatus.APPROVED;
            _payClaim(claimId);
        } else {
            claim.status = ClaimStatus.REJECTED;
            policy.status = PolicyStatus.ACTIVE; // Restore policy
            policy.claimId = 0;
        }
        
        emit ClaimResolved(claimId, claim.status);
    }

    /**
     * @notice Pay out an approved claim
     */
    function _payClaim(uint256 claimId) internal {
        Claim storage claim = claims[claimId];
        Policy storage policy = policies[claim.policyId];
        CoverableProtocol storage protocol = protocols[policy.protocolId];
        
        uint256 payoutAmount = claim.amount;
        
        // Reduce from staked capital (proportionally from stakers)
        // For simplicity, reduce from pool
        if (payoutAmount > totalStaked) {
            payoutAmount = totalStaked;
        }
        
        totalStaked -= payoutAmount;
        protocol.stakedCapital -= payoutAmount > protocol.stakedCapital 
            ? protocol.stakedCapital 
            : payoutAmount;
        protocol.paidClaims += payoutAmount;
        
        protocol.currentCoverage -= policy.coverageAmount;
        totalCoverage -= policy.coverageAmount;
        
        claim.status = ClaimStatus.PAID;
        
        capitalToken.safeTransfer(claim.claimant, payoutAmount);
        
        emit ClaimPaid(claimId, claim.claimant, payoutAmount);
    }

    // ============ Parametric Triggers ============
    
    /**
     * @notice Set up a parametric trigger for automatic payouts
     */
    function setParametricTrigger(
        bytes32 protocolId,
        address oracle,
        bytes32 feedId,
        int256 threshold,
        bool isAbove,
        uint256 cooldownPeriod
    ) external onlyRole(RISK_MANAGER_ROLE) {
        require(protocols[protocolId].protocolAddress != address(0), "Protocol not found");
        
        parametricTriggers[protocolId] = ParametricTrigger({
            protocolId: protocolId,
            oracle: oracle,
            feedId: feedId,
            threshold: threshold,
            isAbove: isAbove,
            cooldownPeriod: cooldownPeriod,
            lastTriggerTime: 0,
            isActive: true
        });
        
        emit ParametricTriggerSet(protocolId, oracle, threshold);
    }

    /**
     * @notice Trigger parametric payout (called by oracle)
     */
    function triggerParametric(bytes32 protocolId, int256 currentValue) external {
        ParametricTrigger storage trigger = parametricTriggers[protocolId];
        
        if (!trigger.isActive) revert ProtocolNotActive();
        if (msg.sender != trigger.oracle && !hasRole(ORACLE_ROLE, msg.sender)) {
            revert UnauthorizedOracle();
        }
        if (block.timestamp < trigger.lastTriggerTime + trigger.cooldownPeriod) {
            revert TriggerOnCooldown();
        }
        
        bool triggered = trigger.isAbove 
            ? currentValue > trigger.threshold 
            : currentValue < trigger.threshold;
        
        if (!triggered) return;
        
        trigger.lastTriggerTime = block.timestamp;
        
        // Auto-approve all pending claims for this protocol
        for (uint256 i = 1; i < nextClaimId; i++) {
            Claim storage claim = claims[i];
            if (claim.status != ClaimStatus.ASSESSING) continue;
            
            Policy storage policy = policies[claim.policyId];
            if (policy.protocolId != protocolId) continue;
            
            claim.parametricTriggered = true;
        }
        
        emit ParametricTriggered(protocolId, currentValue);
    }

    // ============ Rewards ============
    
    /**
     * @notice Claim staking rewards
     */
    function claimRewards() external nonReentrant {
        _updateRewards();
        
        uint256 pending = _pendingRewards(msg.sender);
        if (pending > 0) {
            _payRewards(msg.sender, pending);
        }
    }

    function _updateRewards() internal {
        if (totalStaked == 0) {
            lastRewardTime = block.timestamp;
            return;
        }
        
        // Distribute premiums collected since last update
        if (premiumsCollected > 0) {
            accRewardsPerShare += (premiumsCollected * PRECISION) / totalStaked;
            premiumsCollected = 0;
        }
        
        lastRewardTime = block.timestamp;
    }

    function _pendingRewards(address stakerAddr) internal view returns (uint256) {
        Staker storage staker = stakers[stakerAddr];
        uint256 accumulated = (staker.stakedAmount * accRewardsPerShare) / PRECISION;
        return accumulated > staker.rewardsDebt ? accumulated - staker.rewardsDebt : 0;
    }

    function _payRewards(address stakerAddr, uint256 amount) internal {
        Staker storage staker = stakers[stakerAddr];
        staker.rewardsDebt = (staker.stakedAmount * accRewardsPerShare) / PRECISION;
        staker.claimedRewards += amount;
        
        capitalToken.safeTransfer(stakerAddr, amount);
        
        emit RewardsClaimed(stakerAddr, amount);
    }

    // ============ Internal Helpers ============
    
    function _getAvailableCoverage(bytes32 protocolId) internal view returns (uint256) {
        CoverableProtocol storage protocol = protocols[protocolId];
        
        // Capacity based on staked capital
        uint256 capacityFromStake = (protocol.stakedCapital * 10000) / protocol.minCapitalRatio;
        
        // Capacity based on max
        uint256 remainingMax = protocol.maxCoverage > protocol.currentCoverage 
            ? protocol.maxCoverage - protocol.currentCoverage 
            : 0;
        
        return capacityFromStake < remainingMax ? capacityFromStake : remainingMax;
    }

    function _getTotalAllocated(address stakerAddr) internal view returns (uint256 total) {
        for (uint256 i = 0; i < protocolIds.length; i++) {
            total += stakers[stakerAddr].protocolAllocations[protocolIds[i]];
        }
    }

    function _reduceAllocations(address stakerAddr, uint256 amount) internal {
        Staker storage staker = stakers[stakerAddr];
        uint256 totalAllocated = _getTotalAllocated(stakerAddr);
        
        if (totalAllocated == 0) return;
        
        // Proportional reduction
        for (uint256 i = 0; i < protocolIds.length; i++) {
            bytes32 protocolId = protocolIds[i];
            uint256 allocation = staker.protocolAllocations[protocolId];
            if (allocation == 0) continue;
            
            uint256 reduction = (allocation * amount) / totalAllocated;
            staker.protocolAllocations[protocolId] -= reduction;
            protocols[protocolId].stakedCapital -= reduction;
        }
    }

    // ============ View Functions ============
    
    /**
     * @notice Get pool statistics
     */
    function getPoolStats() external view returns (
        uint256 _totalStaked,
        uint256 _totalCoverage,
        uint256 capitalRatio,
        uint256 protocolCount
    ) {
        _totalStaked = totalStaked;
        _totalCoverage = totalCoverage;
        capitalRatio = totalCoverage > 0 ? (totalStaked * 10000) / totalCoverage : 0;
        protocolCount = protocolIds.length;
    }

    /**
     * @notice Get staker info
     */
    function getStakerInfo(address stakerAddr) external view returns (
        uint256 stakedAmount,
        uint256 lockedUntil,
        uint256 pendingRewards,
        uint256 claimedRewards
    ) {
        Staker storage staker = stakers[stakerAddr];
        stakedAmount = staker.stakedAmount;
        lockedUntil = staker.lockedUntil;
        pendingRewards = _pendingRewards(stakerAddr);
        claimedRewards = staker.claimedRewards;
    }

    /**
     * @notice Get user policies
     */
    function getUserPolicies(address user) external view returns (uint256[] memory) {
        return userPolicies[user];
    }

    /**
     * @notice Check if MCR is satisfied
     */
    function isMCRSatisfied() external view returns (bool) {
        if (totalCoverage == 0) return true;
        return (totalStaked * 10000) / totalCoverage >= globalMCR;
    }

    // ============ Admin Functions ============
    
    function setTreasury(address _treasury) external onlyRole(GOVERNANCE_ROLE) {
        require(_treasury != address(0), "Invalid treasury");
        treasury = _treasury;
    }

    function setTreasuryFee(uint256 _fee) external onlyRole(GOVERNANCE_ROLE) {
        require(_fee <= 2000, "Fee too high"); // Max 20%
        treasuryFee = _fee;
    }

    function setGlobalMCR(uint256 _mcr) external onlyRole(GOVERNANCE_ROLE) {
        require(_mcr >= MIN_CAPITAL_RATIO, "MCR too low");
        globalMCR = _mcr;
    }

    function pause() external onlyRole(GOVERNANCE_ROLE) {
        _pause();
    }

    function unpause() external onlyRole(GOVERNANCE_ROLE) {
        _unpause();
    }
}
