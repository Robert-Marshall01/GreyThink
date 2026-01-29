// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title IFlashLoan
 * @notice Interface for flash loan providers
 */
interface IFlashLoan {
    /**
     * @notice Execute a flash loan
     * @param receiver Address receiving the flash loan
     * @param token Token to borrow
     * @param amount Amount to borrow
     * @param data Arbitrary data to pass to receiver
     */
    function flashLoan(
        address receiver,
        address token,
        uint256 amount,
        bytes calldata data
    ) external returns (bool);

    /**
     * @notice Get the flash loan fee
     * @param token The token to check
     * @param amount The amount to borrow
     * @return The fee amount
     */
    function flashFee(address token, uint256 amount) external view returns (uint256);

    /**
     * @notice Get max flash loan amount
     * @param token The token to check
     * @return The maximum amount available
     */
    function maxFlashLoan(address token) external view returns (uint256);

    event FlashLoan(
        address indexed receiver,
        address indexed token,
        uint256 amount,
        uint256 fee
    );
}

/**
 * @title IFlashLoanReceiver
 * @notice Interface for flash loan receivers
 */
interface IFlashLoanReceiver {
    /**
     * @notice Callback for flash loan execution
     * @param initiator The address that initiated the loan
     * @param token The token borrowed
     * @param amount The amount borrowed
     * @param fee The fee to repay
     * @param data Arbitrary data
     * @return keccak256("ERC3156FlashBorrower.onFlashLoan") on success
     */
    function onFlashLoan(
        address initiator,
        address token,
        uint256 amount,
        uint256 fee,
        bytes calldata data
    ) external returns (bytes32);
}

/**
 * @title ILending
 * @notice Interface for lending protocols
 */
interface ILending {
    struct Market {
        address underlying;
        uint256 totalSupply;
        uint256 totalBorrows;
        uint256 borrowRate;
        uint256 supplyRate;
        uint256 collateralFactor;
        bool isListed;
    }

    function supply(address asset, uint256 amount) external returns (uint256 shares);
    function withdraw(address asset, uint256 shares) external returns (uint256 amount);
    function borrow(address asset, uint256 amount) external;
    function repay(address asset, uint256 amount) external returns (uint256 repaid);
    function liquidate(address borrower, address collateral, address debt, uint256 amount) external;
    
    function getAccountLiquidity(address account) external view returns (uint256 collateral, uint256 debt, uint256 shortfall);
    function getMarketInfo(address asset) external view returns (Market memory);
    function getBorrowRate(address asset) external view returns (uint256);
    function getSupplyRate(address asset) external view returns (uint256);

    event Supply(address indexed account, address indexed asset, uint256 amount, uint256 shares);
    event Withdraw(address indexed account, address indexed asset, uint256 amount, uint256 shares);
    event Borrow(address indexed account, address indexed asset, uint256 amount);
    event Repay(address indexed account, address indexed asset, uint256 amount);
    event Liquidation(address indexed liquidator, address indexed borrower, address collateral, address debt, uint256 amount);
}

/**
 * @title IStreamPayments
 * @notice Interface for streaming payment protocols
 */
interface IStreamPayments {
    struct Stream {
        address sender;
        address recipient;
        address token;
        uint256 depositAmount;
        uint256 withdrawnAmount;
        uint256 startTime;
        uint256 stopTime;
        uint256 ratePerSecond;
        bool active;
    }

    function createStream(
        address recipient,
        address token,
        uint256 depositAmount,
        uint256 startTime,
        uint256 stopTime
    ) external returns (uint256 streamId);

    function withdraw(uint256 streamId, uint256 amount) external returns (uint256 withdrawn);
    function cancelStream(uint256 streamId) external returns (uint256 senderBalance, uint256 recipientBalance);
    function balanceOf(uint256 streamId) external view returns (uint256 available);
    function getStream(uint256 streamId) external view returns (Stream memory);

    event StreamCreated(uint256 indexed streamId, address indexed sender, address indexed recipient, uint256 amount);
    event Withdrawal(uint256 indexed streamId, address indexed recipient, uint256 amount);
    event StreamCancelled(uint256 indexed streamId, uint256 senderBalance, uint256 recipientBalance);
}

/**
 * @title IAuction
 * @notice Interface for auction systems
 */
interface IAuction {
    enum AuctionType { English, Dutch, Sealed }
    enum AuctionStatus { Active, Completed, Cancelled }

    struct AuctionInfo {
        address seller;
        address asset;
        uint256 tokenId;
        address paymentToken;
        uint256 startPrice;
        uint256 endPrice;
        uint256 startTime;
        uint256 endTime;
        AuctionType auctionType;
        AuctionStatus status;
    }

    function createAuction(
        address asset,
        uint256 tokenId,
        address paymentToken,
        uint256 startPrice,
        uint256 endPrice,
        uint256 duration,
        AuctionType auctionType
    ) external returns (uint256 auctionId);

    function bid(uint256 auctionId, uint256 amount) external payable returns (uint256 bidId);
    function cancelAuction(uint256 auctionId) external;
    function finalizeAuction(uint256 auctionId) external;
    function getCurrentPrice(uint256 auctionId) external view returns (uint256);
    function getAuction(uint256 auctionId) external view returns (AuctionInfo memory);

    event AuctionCreated(uint256 indexed auctionId, address indexed seller, address asset, uint256 tokenId);
    event BidPlaced(uint256 indexed auctionId, address indexed bidder, uint256 amount);
    event AuctionFinalized(uint256 indexed auctionId, address indexed winner, uint256 amount);
}

/**
 * @title IIdentity
 * @notice Interface for identity registries
 */
interface IIdentity {
    struct Identity {
        address owner;
        uint256 created;
        bool active;
        address recoveryAddress;
    }

    function createIdentity(address recoveryAddress) external;
    function transferOwnership(address identity, address newOwner) external;
    function addDelegate(address identity, address delegate, bytes32 delegateType, uint256 validity) external;
    function removeDelegate(address identity, address delegate, bytes32 delegateType) external;
    function isValidDelegate(address identity, address delegate, bytes32 delegateType) external view returns (bool);
    function getIdentity(address identity) external view returns (Identity memory);

    event IdentityCreated(address indexed identity, address indexed owner);
    event OwnerChanged(address indexed identity, address indexed previousOwner, address indexed newOwner);
    event DelegateAdded(address indexed identity, address indexed delegate, bytes32 delegateType, uint256 validUntil);
    event DelegateRemoved(address indexed identity, address indexed delegate, bytes32 delegateType);
}

/**
 * @title IAttestation
 * @notice Interface for attestation systems
 */
interface IAttestation {
    struct Attestation {
        bytes32 attestationId;
        address issuer;
        address subject;
        bytes32 schemaHash;
        bytes32 dataHash;
        uint256 issuedAt;
        uint256 expiresAt;
        bool revoked;
    }

    function issueAttestation(
        address subject,
        bytes32 schemaHash,
        bytes32 dataHash,
        uint256 expiresIn
    ) external returns (bytes32 attestationId);

    function revokeAttestation(bytes32 attestationId) external;
    function verifyAttestation(bytes32 attestationId) external view returns (bool isValid, Attestation memory);
    function getAttestations(address subject) external view returns (bytes32[] memory);

    event AttestationIssued(bytes32 indexed attestationId, address indexed issuer, address indexed subject);
    event AttestationRevoked(bytes32 indexed attestationId, address indexed issuer);
}

/**
 * @title IConsensus
 * @notice Interface for consensus mechanisms
 */
interface IConsensus {
    struct Validator {
        address validator;
        uint256 stake;
        uint256 commission;
        bool active;
        bool jailed;
    }

    struct Block {
        bytes32 hash;
        bytes32 parentHash;
        uint256 number;
        uint256 timestamp;
        address proposer;
        bytes32 stateRoot;
        bool finalized;
    }

    function registerValidator(uint256 stake, uint256 commission) external;
    function unregisterValidator() external;
    function proposeBlock(bytes32 parentHash, bytes32 stateRoot, bytes calldata data) external returns (bytes32 blockHash);
    function vote(bytes32 blockHash, bool approve) external;
    function finalizeBlock(bytes32 blockHash) external;
    function slash(address validator, uint256 amount, bytes32 reason) external;
    
    function getValidator(address validator) external view returns (Validator memory);
    function getValidatorSet() external view returns (address[] memory);
    function getBlock(bytes32 blockHash) external view returns (Block memory);

    event ValidatorRegistered(address indexed validator, uint256 stake);
    event ValidatorUnregistered(address indexed validator);
    event BlockProposed(bytes32 indexed blockHash, address indexed proposer);
    event BlockFinalized(bytes32 indexed blockHash);
    event ValidatorSlashed(address indexed validator, uint256 amount, bytes32 reason);
}

/**
 * @title IBridge
 * @notice Interface for cross-chain bridges
 */
interface IBridge {
    struct Message {
        bytes32 messageId;
        uint256 sourceChainId;
        uint256 destChainId;
        address sender;
        address recipient;
        bytes data;
        uint256 timestamp;
        bool executed;
    }

    function sendMessage(
        uint256 destChainId,
        address recipient,
        bytes calldata data
    ) external payable returns (bytes32 messageId);

    function receiveMessage(
        bytes32 messageId,
        uint256 sourceChainId,
        address sender,
        bytes calldata data,
        bytes calldata proof
    ) external;

    function depositToken(
        address token,
        uint256 amount,
        uint256 destChainId,
        address recipient
    ) external;

    function withdrawToken(
        address token,
        uint256 amount,
        bytes32 depositHash,
        bytes calldata proof
    ) external;

    function getMessage(bytes32 messageId) external view returns (Message memory);
    function isMessageExecuted(bytes32 messageId) external view returns (bool);

    event MessageSent(bytes32 indexed messageId, uint256 indexed destChainId, address sender, address recipient);
    event MessageReceived(bytes32 indexed messageId, uint256 indexed sourceChainId, address sender, address recipient);
    event TokenDeposited(address indexed token, address indexed depositor, uint256 amount, uint256 destChainId);
    event TokenWithdrawn(address indexed token, address indexed recipient, uint256 amount);
}

/**
 * @title IUpgradeable
 * @notice Interface for upgradeable contracts
 */
interface IUpgradeable {
    function upgradeToAndCall(address newImplementation, bytes calldata data) external;
    function implementation() external view returns (address);
    function version() external view returns (uint256);

    event Upgraded(address indexed implementation, uint256 version);
}
