// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/governance/Governor.sol";
import "@openzeppelin/contracts/governance/extensions/GovernorSettings.sol";
import "@openzeppelin/contracts/governance/extensions/GovernorCountingSimple.sol";
import "@openzeppelin/contracts/governance/extensions/GovernorVotes.sol";
import "@openzeppelin/contracts/governance/extensions/GovernorVotesQuorumFraction.sol";
import "@openzeppelin/contracts/governance/extensions/GovernorTimelockControl.sol";

/**
 * @title GreyGovernor
 * @author Grey Solidity Project
 * @notice Full-featured DAO governor with voting, quorum, and timelock
 * @dev Extends OpenZeppelin Governor with settings and timelock integration
 */
contract GreyGovernor is
    Governor,
    GovernorSettings,
    GovernorCountingSimple,
    GovernorVotes,
    GovernorVotesQuorumFraction,
    GovernorTimelockControl
{
    /// @notice Proposal threshold updated
    event ProposalThresholdUpdated(uint256 oldThreshold, uint256 newThreshold);

    /// @notice Voting delay updated
    event VotingDelayUpdated(uint256 oldDelay, uint256 newDelay);

    /// @notice Voting period updated
    event VotingPeriodUpdated(uint256 oldPeriod, uint256 newPeriod);

    /// @notice Additional proposal metadata
    struct ProposalMetadata {
        string title;
        string description;
        string category;
        address proposer;
        uint256 createdAt;
    }

    /// @notice Mapping from proposal ID to metadata
    mapping(uint256 => ProposalMetadata) private _proposalMetadata;

    /// @notice Mapping of proposal categories
    mapping(string => uint256[]) private _categoryProposals;

    /// @notice Emitted when proposal metadata is set
    event ProposalMetadataSet(
        uint256 indexed proposalId,
        string title,
        string category
    );

    /**
     * @notice Initializes the governor
     * @param token_ The voting token (must implement IVotes)
     * @param timelock_ The timelock controller
     * @param votingDelay_ The delay before voting starts (in blocks)
     * @param votingPeriod_ The voting period duration (in blocks)
     * @param proposalThreshold_ The minimum votes to create a proposal
     * @param quorumPercentage_ The quorum percentage (4 = 4%)
     */
    constructor(
        IVotes token_,
        TimelockController timelock_,
        uint48 votingDelay_,
        uint32 votingPeriod_,
        uint256 proposalThreshold_,
        uint256 quorumPercentage_
    )
        Governor("Grey Governor")
        GovernorSettings(votingDelay_, votingPeriod_, proposalThreshold_)
        GovernorVotes(token_)
        GovernorVotesQuorumFraction(quorumPercentage_)
        GovernorTimelockControl(timelock_)
    {}

    // ============ Proposal Functions ============

    /**
     * @notice Creates a proposal with metadata
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param calldatas Array of calldata
     * @param description The proposal description
     * @param title The proposal title
     * @param category The proposal category
     * @return proposalId The proposal ID
     */
    function proposeWithMetadata(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        string memory description,
        string memory title,
        string memory category
    ) public returns (uint256 proposalId) {
        proposalId = propose(targets, values, calldatas, description);
        
        _proposalMetadata[proposalId] = ProposalMetadata({
            title: title,
            description: description,
            category: category,
            proposer: msg.sender,
            createdAt: block.timestamp
        });

        _categoryProposals[category].push(proposalId);

        emit ProposalMetadataSet(proposalId, title, category);
    }

    /**
     * @notice Gets proposal metadata
     * @param proposalId The proposal ID
     * @return The metadata struct
     */
    function getProposalMetadata(uint256 proposalId) external view returns (ProposalMetadata memory) {
        return _proposalMetadata[proposalId];
    }

    /**
     * @notice Gets proposals by category
     * @param category The category name
     * @return Array of proposal IDs
     */
    function getProposalsByCategory(string calldata category) external view returns (uint256[] memory) {
        return _categoryProposals[category];
    }

    /**
     * @notice Returns detailed proposal info
     * @param proposalId The proposal ID
     * @return proposalState The proposal state
     * @return forVotes Votes in favor
     * @return againstVotes Votes against
     * @return abstainVotes Abstain votes
     * @return deadline The voting deadline
     */
    function getProposalDetails(uint256 proposalId) external view returns (
        ProposalState proposalState,
        uint256 forVotes,
        uint256 againstVotes,
        uint256 abstainVotes,
        uint256 deadline
    ) {
        proposalState = state(proposalId);
        (againstVotes, forVotes, abstainVotes) = proposalVotes(proposalId);
        deadline = proposalDeadline(proposalId);
    }

    // ============ Required Overrides ============

    function votingDelay()
        public
        view
        override(Governor, GovernorSettings)
        returns (uint256)
    {
        return super.votingDelay();
    }

    function votingPeriod()
        public
        view
        override(Governor, GovernorSettings)
        returns (uint256)
    {
        return super.votingPeriod();
    }

    function quorum(uint256 blockNumber)
        public
        view
        override(Governor, GovernorVotesQuorumFraction)
        returns (uint256)
    {
        return super.quorum(blockNumber);
    }

    function state(uint256 proposalId)
        public
        view
        override(Governor, GovernorTimelockControl)
        returns (ProposalState)
    {
        return super.state(proposalId);
    }

    function proposalNeedsQueuing(uint256 proposalId)
        public
        view
        override(Governor, GovernorTimelockControl)
        returns (bool)
    {
        return super.proposalNeedsQueuing(proposalId);
    }

    function proposalThreshold()
        public
        view
        override(Governor, GovernorSettings)
        returns (uint256)
    {
        return super.proposalThreshold();
    }

    function _queueOperations(
        uint256 proposalId,
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) internal override(Governor, GovernorTimelockControl) returns (uint48) {
        return super._queueOperations(proposalId, targets, values, calldatas, descriptionHash);
    }

    function _executeOperations(
        uint256 proposalId,
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) internal override(Governor, GovernorTimelockControl) {
        super._executeOperations(proposalId, targets, values, calldatas, descriptionHash);
    }

    function _cancel(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) internal override(Governor, GovernorTimelockControl) returns (uint256) {
        return super._cancel(targets, values, calldatas, descriptionHash);
    }

    function _executor()
        internal
        view
        override(Governor, GovernorTimelockControl)
        returns (address)
    {
        return super._executor();
    }
}
