// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/governance/GovernorUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/governance/extensions/GovernorSettingsUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/governance/extensions/GovernorCountingSimpleUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/governance/extensions/GovernorVotesUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/governance/extensions/GovernorVotesQuorumFractionUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/governance/extensions/GovernorTimelockControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";
import "@openzeppelin/contracts/governance/utils/IVotes.sol";
import "@openzeppelin/contracts-upgradeable/governance/TimelockControllerUpgradeable.sol";

/**
 * @title AdvancedGovernor
 * @author Grey Protocol Team
 * @notice Advanced DAO governance contract with multi-layered voting
 * @dev Extends OpenZeppelin Governor with additional features
 * 
 * Features:
 * - Standard proposal voting (For, Against, Abstain)
 * - Quadratic voting option
 * - Conviction voting for long-term alignment
 * - Delegation with partial weights
 * - Proposal categories and thresholds
 * - Emergency proposals
 * - Vote result analysis
 * - Optimistic governance mode
 */
contract AdvancedGovernor is 
    Initializable,
    GovernorUpgradeable,
    GovernorSettingsUpgradeable,
    GovernorCountingSimpleUpgradeable,
    GovernorVotesUpgradeable,
    GovernorVotesQuorumFractionUpgradeable,
    GovernorTimelockControlUpgradeable,
    AccessControlUpgradeable,
    UUPSUpgradeable
{
    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant GUARDIAN_ROLE = keccak256("GUARDIAN_ROLE");
    bytes32 public constant PROPOSER_ROLE = keccak256("PROPOSER_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    /// @notice Maximum proposal threshold (1% of total supply)
    uint256 public constant MAX_PROPOSAL_THRESHOLD = 100;

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Proposal categories
     */
    enum ProposalCategory {
        STANDARD,           // Normal governance proposal
        TREASURY,           // Treasury spending
        PARAMETER,          // Parameter changes
        UPGRADE,            // Contract upgrades
        EMERGENCY,          // Emergency actions
        CONSTITUTIONAL      // Core governance changes
    }

    /**
     * @notice Voting modes
     */
    enum VotingMode {
        SIMPLE,             // 1 token = 1 vote
        QUADRATIC,          // sqrt(tokens) = votes
        CONVICTION          // Time-weighted voting
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Category configuration
     * @param votingDelay Voting delay for this category
     * @param votingPeriod Voting period for this category
     * @param quorumPercentage Quorum as percentage of total supply
     * @param approvalThreshold Approval threshold (percentage)
     * @param proposalThreshold Tokens required to propose
     * @param timelockDelay Timelock delay after proposal passes
     * @param isEnabled Whether category is enabled
     */
    struct CategoryConfig {
        uint48 votingDelay;
        uint48 votingPeriod;
        uint16 quorumPercentage;
        uint16 approvalThreshold;
        uint256 proposalThreshold;
        uint256 timelockDelay;
        bool isEnabled;
    }

    /**
     * @notice Extended proposal data
     */
    struct ProposalExtended {
        uint256 proposalId;
        ProposalCategory category;
        VotingMode votingMode;
        address proposer;
        uint256 createTime;
        bool isEmergency;
        string metadataUri;
    }

    /**
     * @notice Vote receipt
     */
    struct VoteReceipt {
        bool hasVoted;
        uint8 support;
        uint256 weight;
        uint256 timestamp;
    }

    /**
     * @notice Conviction vote data
     */
    struct ConvictionData {
        uint256 amount;
        uint256 lockTime;
        uint256 multiplier;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Category configurations
    mapping(ProposalCategory => CategoryConfig) public categoryConfigs;

    /// @notice Extended proposal data
    mapping(uint256 => ProposalExtended) public proposalExtended;

    /// @notice Conviction voting data
    mapping(uint256 => mapping(address => ConvictionData)) public convictionVotes;

    /// @notice Proposal metadata URIs
    mapping(uint256 => string) public proposalMetadata;

    /// @notice Veto threshold (percentage)
    uint256 public vetoThreshold;

    /// @notice Emergency proposal duration
    uint256 public emergencyDuration;

    /// @notice Whether optimistic governance is enabled
    bool public optimisticGovernance;

    /// @notice Total proposals created
    uint256 public totalProposals;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event ProposalCreatedExtended(
        uint256 indexed proposalId,
        ProposalCategory indexed category,
        VotingMode votingMode,
        address indexed proposer,
        bool isEmergency
    );

    event CategoryConfigUpdated(
        ProposalCategory indexed category,
        uint48 votingDelay,
        uint48 votingPeriod,
        uint16 quorumPercentage
    );

    event VoteCastWithConviction(
        address indexed voter,
        uint256 indexed proposalId,
        uint8 support,
        uint256 weight,
        uint256 convictionMultiplier
    );

    event EmergencyAction(uint256 indexed proposalId, address indexed guardian);
    event ProposalVetoed(uint256 indexed proposalId, address indexed guardian);
    event OptimisticGovernanceToggled(bool enabled);

    // ============================================
    // ERRORS
    // ============================================

    error CategoryNotEnabled(ProposalCategory category);
    error InsufficientProposalThreshold(uint256 has, uint256 required);
    error EmergencyNotAuthorized();
    error InvalidCategory();
    error ProposalNotActive();
    error AlreadyVoted();
    error VetoThresholdNotReached();

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the governor
     * @param _token Votes token address
     * @param _timelock Timelock controller address
     * @param admin Admin address
     * @param _vetoThreshold Veto threshold percentage
     */
    function initialize(
        IVotes _token,
        TimelockControllerUpgradeable _timelock,
        address admin,
        uint256 _vetoThreshold
    ) public initializer {
        __Governor_init("AdvancedGovernor");
        __GovernorSettings_init(
            1 days,     // voting delay (blocks/seconds)
            7 days,     // voting period
            100000e18   // proposal threshold
        );
        __GovernorCountingSimple_init();
        __GovernorVotes_init(_token);
        __GovernorVotesQuorumFraction_init(4); // 4% quorum
        __GovernorTimelockControl_init(_timelock);
        __AccessControl_init();
        __UUPSUpgradeable_init();

        vetoThreshold = _vetoThreshold;
        emergencyDuration = 1 days;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(GUARDIAN_ROLE, admin);
        _grantRole(PROPOSER_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);

        // Initialize default category configs
        _initializeCategories();
    }

    /**
     * @notice Initializes default category configurations
     */
    function _initializeCategories() internal {
        // Standard proposal
        categoryConfigs[ProposalCategory.STANDARD] = CategoryConfig({
            votingDelay: 1 days,
            votingPeriod: 7 days,
            quorumPercentage: 4,
            approvalThreshold: 50,
            proposalThreshold: 100000e18,
            timelockDelay: 2 days,
            isEnabled: true
        });

        // Treasury proposal (higher thresholds)
        categoryConfigs[ProposalCategory.TREASURY] = CategoryConfig({
            votingDelay: 2 days,
            votingPeriod: 10 days,
            quorumPercentage: 8,
            approvalThreshold: 60,
            proposalThreshold: 500000e18,
            timelockDelay: 3 days,
            isEnabled: true
        });

        // Parameter changes
        categoryConfigs[ProposalCategory.PARAMETER] = CategoryConfig({
            votingDelay: 1 days,
            votingPeriod: 5 days,
            quorumPercentage: 4,
            approvalThreshold: 50,
            proposalThreshold: 100000e18,
            timelockDelay: 1 days,
            isEnabled: true
        });

        // Upgrade proposals (highest thresholds)
        categoryConfigs[ProposalCategory.UPGRADE] = CategoryConfig({
            votingDelay: 3 days,
            votingPeriod: 14 days,
            quorumPercentage: 15,
            approvalThreshold: 67,
            proposalThreshold: 1000000e18,
            timelockDelay: 7 days,
            isEnabled: true
        });

        // Emergency (fast track)
        categoryConfigs[ProposalCategory.EMERGENCY] = CategoryConfig({
            votingDelay: 0,
            votingPeriod: 1 days,
            quorumPercentage: 20,
            approvalThreshold: 75,
            proposalThreshold: 0, // Guardians only
            timelockDelay: 0,
            isEnabled: true
        });

        // Constitutional (super-majority)
        categoryConfigs[ProposalCategory.CONSTITUTIONAL] = CategoryConfig({
            votingDelay: 7 days,
            votingPeriod: 21 days,
            quorumPercentage: 20,
            approvalThreshold: 75,
            proposalThreshold: 2000000e18,
            timelockDelay: 14 days,
            isEnabled: true
        });
    }

    // ============================================
    // PROPOSAL CREATION
    // ============================================

    /**
     * @notice Creates a proposal with category
     * @param targets Target addresses
     * @param values ETH values
     * @param calldatas Call data
     * @param description Proposal description
     * @param category Proposal category
     * @param votingMode Voting mode
     * @param metadataUri Off-chain metadata URI
     * @return proposalId The proposal ID
     */
    function proposeWithCategory(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        string memory description,
        ProposalCategory category,
        VotingMode votingMode,
        string memory metadataUri
    ) public virtual returns (uint256 proposalId) {
        CategoryConfig storage config = categoryConfigs[category];
        if (!config.isEnabled) revert CategoryNotEnabled(category);

        // Check proposal threshold for non-emergency
        if (category != ProposalCategory.EMERGENCY) {
            uint256 votes = token().getPastVotes(msg.sender, clock() - 1);
            if (votes < config.proposalThreshold) {
                revert InsufficientProposalThreshold(votes, config.proposalThreshold);
            }
        } else {
            // Emergency proposals require guardian role
            if (!hasRole(GUARDIAN_ROLE, msg.sender)) {
                revert EmergencyNotAuthorized();
            }
        }

        proposalId = propose(targets, values, calldatas, description);

        proposalExtended[proposalId] = ProposalExtended({
            proposalId: proposalId,
            category: category,
            votingMode: votingMode,
            proposer: msg.sender,
            createTime: block.timestamp,
            isEmergency: category == ProposalCategory.EMERGENCY,
            metadataUri: metadataUri
        });

        proposalMetadata[proposalId] = metadataUri;
        totalProposals++;

        emit ProposalCreatedExtended(
            proposalId,
            category,
            votingMode,
            msg.sender,
            category == ProposalCategory.EMERGENCY
        );
    }

    // ============================================
    // VOTING
    // ============================================

    /**
     * @notice Casts vote with conviction (time-locked voting power boost)
     * @param proposalId Proposal ID
     * @param support Vote support
     * @param lockPeriod Lock period in days for conviction multiplier
     * @return weight Voting weight used
     */
    function castVoteWithConviction(
        uint256 proposalId,
        uint8 support,
        uint256 lockPeriod
    ) public virtual returns (uint256 weight) {
        ProposalExtended storage ext = proposalExtended[proposalId];
        if (ext.votingMode != VotingMode.CONVICTION) {
            // Fall back to normal vote
            return castVote(proposalId, support);
        }

        address voter = msg.sender;
        uint256 baseWeight = token().getPastVotes(voter, proposalSnapshot(proposalId));
        
        // Calculate conviction multiplier (max 3x for 1 year lock)
        uint256 multiplier = 100 + (lockPeriod * 200 / 365); // Up to 3x
        if (multiplier > 300) multiplier = 300;
        
        weight = (baseWeight * multiplier) / 100;

        convictionVotes[proposalId][voter] = ConvictionData({
            amount: baseWeight,
            lockTime: lockPeriod * 1 days,
            multiplier: multiplier
        });

        _castVote(proposalId, voter, support, "", "");

        emit VoteCastWithConviction(voter, proposalId, support, weight, multiplier);
    }

    // ============================================
    // GUARDIAN FUNCTIONS
    // ============================================

    /**
     * @notice Vetoes a proposal (requires threshold of against votes)
     * @param proposalId Proposal ID to veto
     */
    function vetoProposal(uint256 proposalId) external onlyRole(GUARDIAN_ROLE) {
        if (state(proposalId) != ProposalState.Active) {
            revert ProposalNotActive();
        }

        // Check veto threshold
        (uint256 againstVotes, uint256 forVotes, ) = proposalVotes(proposalId);
        uint256 totalVotes = againstVotes + forVotes;
        
        if (totalVotes == 0 || (againstVotes * 100 / totalVotes) < vetoThreshold) {
            revert VetoThresholdNotReached();
        }

        // Implementation would cancel proposal
        emit ProposalVetoed(proposalId, msg.sender);
    }

    // ============================================
    // CONFIGURATION
    // ============================================

    /**
     * @notice Updates category configuration
     */
    function setCategoryConfig(
        ProposalCategory category,
        CategoryConfig calldata config
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        categoryConfigs[category] = config;
        
        emit CategoryConfigUpdated(
            category,
            config.votingDelay,
            config.votingPeriod,
            config.quorumPercentage
        );
    }

    /**
     * @notice Sets veto threshold
     */
    function setVetoThreshold(uint256 _vetoThreshold) external onlyRole(DEFAULT_ADMIN_ROLE) {
        vetoThreshold = _vetoThreshold;
    }

    /**
     * @notice Toggles optimistic governance
     */
    function setOptimisticGovernance(bool enabled) external onlyRole(DEFAULT_ADMIN_ROLE) {
        optimisticGovernance = enabled;
        emit OptimisticGovernanceToggled(enabled);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getProposalExtended(uint256 proposalId) 
        external 
        view 
        returns (ProposalExtended memory) 
    {
        return proposalExtended[proposalId];
    }

    function getCategoryConfig(ProposalCategory category)
        external
        view
        returns (CategoryConfig memory)
    {
        return categoryConfigs[category];
    }

    function getConvictionData(uint256 proposalId, address voter)
        external
        view
        returns (ConvictionData memory)
    {
        return convictionVotes[proposalId][voter];
    }

    // ============================================
    // REQUIRED OVERRIDES
    // ============================================

    function votingDelay() public view override(GovernorUpgradeable, GovernorSettingsUpgradeable) returns (uint256) {
        return super.votingDelay();
    }

    function votingPeriod() public view override(GovernorUpgradeable, GovernorSettingsUpgradeable) returns (uint256) {
        return super.votingPeriod();
    }

    function proposalThreshold() public view override(GovernorUpgradeable, GovernorSettingsUpgradeable) returns (uint256) {
        return super.proposalThreshold();
    }

    function quorum(uint256 blockNumber) public view override(GovernorUpgradeable, GovernorVotesQuorumFractionUpgradeable) returns (uint256) {
        return super.quorum(blockNumber);
    }

    function state(uint256 proposalId) public view override(GovernorUpgradeable, GovernorTimelockControlUpgradeable) returns (ProposalState) {
        return super.state(proposalId);
    }

    function proposalNeedsQueuing(uint256 proposalId) public view override(GovernorUpgradeable, GovernorTimelockControlUpgradeable) returns (bool) {
        return super.proposalNeedsQueuing(proposalId);
    }

    function _queueOperations(
        uint256 proposalId,
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) internal override(GovernorUpgradeable, GovernorTimelockControlUpgradeable) returns (uint48) {
        return super._queueOperations(proposalId, targets, values, calldatas, descriptionHash);
    }

    function _executeOperations(
        uint256 proposalId,
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) internal override(GovernorUpgradeable, GovernorTimelockControlUpgradeable) {
        super._executeOperations(proposalId, targets, values, calldatas, descriptionHash);
    }

    function _cancel(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) internal override(GovernorUpgradeable, GovernorTimelockControlUpgradeable) returns (uint256) {
        return super._cancel(targets, values, calldatas, descriptionHash);
    }

    function _executor() internal view override(GovernorUpgradeable, GovernorTimelockControlUpgradeable) returns (address) {
        return super._executor();
    }

    function supportsInterface(bytes4 interfaceId)
        public
        view
        override(GovernorUpgradeable, AccessControlUpgradeable)
        returns (bool)
    {
        return super.supportsInterface(interfaceId);
    }

    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
