// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IGovernor
 * @notice Interface for governance contracts
 * @dev Supports proposals, voting, queueing, and execution
 */
interface IGovernor {
    /// @notice Proposal state enumeration
    enum ProposalState {
        Pending,
        Active,
        Canceled,
        Defeated,
        Succeeded,
        Queued,
        Expired,
        Executed
    }

    /// @notice Vote type enumeration
    enum VoteType {
        Against,
        For,
        Abstain
    }

    /**
     * @notice Proposal structure
     */
    struct Proposal {
        uint256 id;
        address proposer;
        uint256 eta;
        uint256 startBlock;
        uint256 endBlock;
        uint256 forVotes;
        uint256 againstVotes;
        uint256 abstainVotes;
        bool canceled;
        bool executed;
    }

    /**
     * @notice Returns the name of the governor
     * @return The governor name
     */
    function name() external view returns (string memory);

    /**
     * @notice Returns the version of the governor
     * @return The version string
     */
    function version() external pure returns (string memory);

    /**
     * @notice Returns the voting delay in blocks
     * @return The voting delay
     */
    function votingDelay() external view returns (uint256);

    /**
     * @notice Returns the voting period in blocks
     * @return The voting period
     */
    function votingPeriod() external view returns (uint256);

    /**
     * @notice Returns the proposal threshold
     * @return The minimum votes required to create a proposal
     */
    function proposalThreshold() external view returns (uint256);

    /**
     * @notice Returns the quorum required for a proposal
     * @param blockNumber The block number
     * @return The quorum amount
     */
    function quorum(uint256 blockNumber) external view returns (uint256);

    /**
     * @notice Creates a new proposal
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param calldatas Array of encoded function calls
     * @param description The proposal description
     * @return The proposal ID
     */
    function propose(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        string memory description
    ) external returns (uint256);

    /**
     * @notice Queues a successful proposal
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param calldatas Array of encoded function calls
     * @param descriptionHash The hash of the description
     * @return The proposal ID
     */
    function queue(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) external returns (uint256);

    /**
     * @notice Executes a queued proposal
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param calldatas Array of encoded function calls
     * @param descriptionHash The hash of the description
     * @return The proposal ID
     */
    function execute(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) external payable returns (uint256);

    /**
     * @notice Cancels a proposal
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param calldatas Array of encoded function calls
     * @param descriptionHash The hash of the description
     * @return The proposal ID
     */
    function cancel(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) external returns (uint256);

    /**
     * @notice Casts a vote on a proposal
     * @param proposalId The proposal ID
     * @param support The vote type (0=Against, 1=For, 2=Abstain)
     * @return The voting weight
     */
    function castVote(uint256 proposalId, uint8 support) external returns (uint256);

    /**
     * @notice Casts a vote with reason
     * @param proposalId The proposal ID
     * @param support The vote type
     * @param reason The vote reason
     * @return The voting weight
     */
    function castVoteWithReason(
        uint256 proposalId,
        uint8 support,
        string calldata reason
    ) external returns (uint256);

    /**
     * @notice Casts a vote using signature
     * @param proposalId The proposal ID
     * @param support The vote type
     * @param v Signature v
     * @param r Signature r
     * @param s Signature s
     * @return The voting weight
     */
    function castVoteBySig(
        uint256 proposalId,
        uint8 support,
        uint8 v,
        bytes32 r,
        bytes32 s
    ) external returns (uint256);

    /**
     * @notice Returns the state of a proposal
     * @param proposalId The proposal ID
     * @return The proposal state
     */
    function state(uint256 proposalId) external view returns (ProposalState);

    /**
     * @notice Returns the deadline for a proposal
     * @param proposalId The proposal ID
     * @return The deadline block number
     */
    function proposalDeadline(uint256 proposalId) external view returns (uint256);

    /**
     * @notice Returns the snapshot block for a proposal
     * @param proposalId The proposal ID
     * @return The snapshot block number
     */
    function proposalSnapshot(uint256 proposalId) external view returns (uint256);

    /**
     * @notice Returns the proposer of a proposal
     * @param proposalId The proposal ID
     * @return The proposer address
     */
    function proposalProposer(uint256 proposalId) external view returns (address);

    /**
     * @notice Returns voting power for an account at a block
     * @param account The account address
     * @param blockNumber The block number
     * @return The voting power
     */
    function getVotes(address account, uint256 blockNumber) external view returns (uint256);

    /**
     * @notice Checks if an account has voted on a proposal
     * @param proposalId The proposal ID
     * @param account The account address
     * @return True if voted
     */
    function hasVoted(uint256 proposalId, address account) external view returns (bool);

    /**
     * @notice Hashes a proposal
     * @param targets Array of target addresses
     * @param values Array of ETH values
     * @param calldatas Array of encoded function calls
     * @param descriptionHash The hash of the description
     * @return The proposal ID
     */
    function hashProposal(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) external pure returns (uint256);

    // Events
    event ProposalCreated(
        uint256 proposalId,
        address proposer,
        address[] targets,
        uint256[] values,
        string[] signatures,
        bytes[] calldatas,
        uint256 startBlock,
        uint256 endBlock,
        string description
    );
    event ProposalQueued(uint256 proposalId, uint256 eta);
    event ProposalExecuted(uint256 proposalId);
    event ProposalCanceled(uint256 proposalId);
    event VoteCast(address indexed voter, uint256 proposalId, uint8 support, uint256 weight, string reason);
}
