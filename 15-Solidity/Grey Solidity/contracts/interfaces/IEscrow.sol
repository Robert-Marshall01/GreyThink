// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IEscrow
 * @notice Interface for escrow contracts
 * @dev Manages escrowed assets with release conditions
 */
interface IEscrow {
    /// @notice Escrow state enumeration
    enum EscrowState {
        Created,
        Funded,
        Released,
        Refunded,
        Disputed,
        Resolved
    }

    /// @notice Asset type enumeration
    enum AssetType {
        ETH,
        ERC20,
        ERC721,
        ERC1155
    }

    /**
     * @notice Escrow structure
     */
    struct EscrowInfo {
        uint256 escrowId;
        address depositor;
        address beneficiary;
        address arbiter;
        AssetType assetType;
        address assetAddress;
        uint256 assetId; // for NFTs
        uint256 amount;
        uint256 createdAt;
        uint256 releaseTime;
        uint256 expiryTime;
        EscrowState state;
        string description;
    }

    /**
     * @notice Dispute structure
     */
    struct Dispute {
        uint256 escrowId;
        address initiator;
        string reason;
        uint256 timestamp;
        bool resolved;
        address winner;
    }

    /**
     * @notice Creates an ETH escrow
     * @param beneficiary The beneficiary address
     * @param arbiter The arbiter address
     * @param releaseTime The release time
     * @param expiryTime The expiry time
     * @param description The escrow description
     * @return The escrow ID
     */
    function createETHEscrow(
        address beneficiary,
        address arbiter,
        uint256 releaseTime,
        uint256 expiryTime,
        string calldata description
    ) external payable returns (uint256);

    /**
     * @notice Creates an ERC20 escrow
     * @param beneficiary The beneficiary address
     * @param arbiter The arbiter address
     * @param token The token address
     * @param amount The amount to escrow
     * @param releaseTime The release time
     * @param expiryTime The expiry time
     * @param description The escrow description
     * @return The escrow ID
     */
    function createERC20Escrow(
        address beneficiary,
        address arbiter,
        address token,
        uint256 amount,
        uint256 releaseTime,
        uint256 expiryTime,
        string calldata description
    ) external returns (uint256);

    /**
     * @notice Creates an ERC721 escrow
     * @param beneficiary The beneficiary address
     * @param arbiter The arbiter address
     * @param nftContract The NFT contract address
     * @param tokenId The token ID
     * @param releaseTime The release time
     * @param expiryTime The expiry time
     * @param description The escrow description
     * @return The escrow ID
     */
    function createERC721Escrow(
        address beneficiary,
        address arbiter,
        address nftContract,
        uint256 tokenId,
        uint256 releaseTime,
        uint256 expiryTime,
        string calldata description
    ) external returns (uint256);

    /**
     * @notice Releases escrow to beneficiary
     * @param escrowId The escrow ID
     */
    function release(uint256 escrowId) external;

    /**
     * @notice Refunds escrow to depositor
     * @param escrowId The escrow ID
     */
    function refund(uint256 escrowId) external;

    /**
     * @notice Initiates a dispute
     * @param escrowId The escrow ID
     * @param reason The dispute reason
     */
    function dispute(uint256 escrowId, string calldata reason) external;

    /**
     * @notice Resolves a dispute
     * @param escrowId The escrow ID
     * @param releaseTobeneficiary True to release to beneficiary
     */
    function resolveDispute(uint256 escrowId, bool releaseTobeneficiary) external;

    /**
     * @notice Claims expired escrow
     * @param escrowId The escrow ID
     */
    function claimExpired(uint256 escrowId) external;

    /**
     * @notice Gets escrow info
     * @param escrowId The escrow ID
     * @return The escrow info
     */
    function getEscrow(uint256 escrowId) external view returns (EscrowInfo memory);

    /**
     * @notice Gets dispute info
     * @param escrowId The escrow ID
     * @return The dispute info
     */
    function getDispute(uint256 escrowId) external view returns (Dispute memory);

    /**
     * @notice Gets escrows by depositor
     * @param depositor The depositor address
     * @return Array of escrow IDs
     */
    function getEscrowsByDepositor(address depositor) external view returns (uint256[] memory);

    /**
     * @notice Gets escrows by beneficiary
     * @param beneficiary The beneficiary address
     * @return Array of escrow IDs
     */
    function getEscrowsByBeneficiary(address beneficiary) external view returns (uint256[] memory);

    /**
     * @notice Gets escrows by arbiter
     * @param arbiter The arbiter address
     * @return Array of escrow IDs
     */
    function getEscrowsByArbiter(address arbiter) external view returns (uint256[] memory);

    /**
     * @notice Returns the escrow count
     * @return The count
     */
    function getEscrowCount() external view returns (uint256);

    /**
     * @notice Returns the escrow fee
     * @return The fee in basis points
     */
    function escrowFee() external view returns (uint256);

    /**
     * @notice Sets the escrow fee
     * @param fee The fee in basis points
     */
    function setEscrowFee(uint256 fee) external;

    // Events
    event EscrowCreated(
        uint256 indexed escrowId,
        address indexed depositor,
        address indexed beneficiary,
        AssetType assetType,
        uint256 amount
    );
    event EscrowReleased(uint256 indexed escrowId, address indexed beneficiary, uint256 amount);
    event EscrowRefunded(uint256 indexed escrowId, address indexed depositor, uint256 amount);
    event DisputeInitiated(uint256 indexed escrowId, address indexed initiator, string reason);
    event DisputeResolved(uint256 indexed escrowId, address indexed winner);
    event EscrowExpiryClaimed(uint256 indexed escrowId, address indexed claimer);
}
