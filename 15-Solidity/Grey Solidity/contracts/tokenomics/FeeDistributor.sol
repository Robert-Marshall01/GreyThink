// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

/**
 * @title FeeDistributor
 * @author Grey Protocol
 * @notice Advanced fee distribution and revenue sharing system
 * @dev Implements multiple distribution strategies and stakeholder management
 * 
 * Features:
 * - Multi-token fee collection
 * - Weighted distribution to stakeholders
 * - Merkle-based claim system for gas efficiency
 * - Revenue streaming with vesting
 * - Retroactive rewards
 * - Cross-protocol revenue aggregation
 */
contract FeeDistributor is AccessControl, ReentrancyGuard {
    using SafeERC20 for IERC20;

    // ============================================
    // CONSTANTS
    // ============================================

    bytes32 public constant DISTRIBUTOR_ROLE = keccak256("DISTRIBUTOR_ROLE");
    bytes32 public constant FEE_COLLECTOR_ROLE = keccak256("FEE_COLLECTOR_ROLE");

    uint256 public constant BPS_DENOMINATOR = 10000;
    uint256 public constant MAX_RECIPIENTS = 20;
    uint256 public constant CLAIM_PERIOD = 90 days;

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Distribution recipient
    struct Recipient {
        address addr;
        uint256 share;           // Share in BPS
        bool isContract;         // Is recipient a contract
        bool isActive;
        uint256 totalReceived;
        uint256 lastClaimTime;
    }

    /// @notice Distribution epoch
    struct Epoch {
        uint256 epochNumber;
        uint256 startTime;
        uint256 endTime;
        uint256 totalCollected;
        uint256 totalDistributed;
        bool finalized;
        bytes32 merkleRoot;      // For Merkle claims
    }

    /// @notice Token accumulation
    struct TokenAccumulation {
        address token;
        uint256 amount;
        uint256 lastUpdated;
    }

    /// @notice Revenue stream (for vesting)
    struct RevenueStream {
        address recipient;
        address token;
        uint256 totalAmount;
        uint256 startTime;
        uint256 duration;
        uint256 claimedAmount;
    }

    /// @notice Claim request
    struct ClaimRequest {
        address claimer;
        address token;
        uint256 amount;
        uint256 epochNumber;
        bytes32[] merkleProof;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Supported fee tokens
    address[] public feeTokens;

    /// @notice Token => is supported
    mapping(address => bool) public isSupportedToken;

    /// @notice Token => accumulated amount
    mapping(address => uint256) public accumulatedFees;

    /// @notice Recipients
    Recipient[] public recipients;

    /// @notice Address => recipient index + 1 (0 means not a recipient)
    mapping(address => uint256) public recipientIndex;

    /// @notice Current epoch number
    uint256 public currentEpoch;

    /// @notice Epoch data
    mapping(uint256 => Epoch) public epochs;

    /// @notice User => epoch => token => claimed
    mapping(address => mapping(uint256 => mapping(address => bool))) public hasClaimed;

    /// @notice User => token => claimable amount
    mapping(address => mapping(address => uint256)) public claimable;

    /// @notice Revenue streams
    mapping(bytes32 => RevenueStream) public revenueStreams;

    /// @notice Stream count per recipient
    mapping(address => uint256) public streamCount;

    /// @notice Epoch duration
    uint256 public epochDuration;

    /// @notice Minimum distribution amount
    uint256 public minDistributionAmount;

    /// @notice Total share allocated
    uint256 public totalShareAllocated;

    /// @notice Distribution paused
    bool public paused;

    // ============================================
    // EVENTS
    // ============================================

    event FeesCollected(
        address indexed token,
        address indexed from,
        uint256 amount
    );

    event FeesDistributed(
        uint256 indexed epoch,
        address indexed token,
        uint256 totalAmount
    );

    event RecipientShareUpdated(
        address indexed recipient,
        uint256 oldShare,
        uint256 newShare
    );

    event Claimed(
        address indexed claimer,
        address indexed token,
        uint256 amount,
        uint256 epoch
    );

    event RevenueStreamCreated(
        bytes32 indexed streamId,
        address indexed recipient,
        address token,
        uint256 amount
    );

    event RevenueStreamClaimed(
        bytes32 indexed streamId,
        uint256 amount
    );

    event EpochFinalized(uint256 indexed epoch, bytes32 merkleRoot);

    // ============================================
    // ERRORS
    // ============================================

    error InvalidRecipient();
    error ShareExceedsLimit();
    error AlreadyClaimed();
    error InvalidProof();
    error NothingToClaim();
    error TooManyRecipients();
    error DistributionPaused();
    error TokenNotSupported();
    error StreamNotClaimable();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(uint256 _epochDuration) {
        epochDuration = _epochDuration;
        minDistributionAmount = 1e15;

        // Initialize first epoch
        epochs[0] = Epoch({
            epochNumber: 0,
            startTime: block.timestamp,
            endTime: block.timestamp + _epochDuration,
            totalCollected: 0,
            totalDistributed: 0,
            finalized: false,
            merkleRoot: bytes32(0)
        });

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(DISTRIBUTOR_ROLE, msg.sender);
        _grantRole(FEE_COLLECTOR_ROLE, msg.sender);
    }

    // ============================================
    // FEE COLLECTION
    // ============================================

    /**
     * @notice Collect fees from a source
     * @param token Token address
     * @param amount Amount to collect
     */
    function collectFees(address token, uint256 amount) 
        external 
        onlyRole(FEE_COLLECTOR_ROLE) 
    {
        if (!isSupportedToken[token]) revert TokenNotSupported();

        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);
        
        accumulatedFees[token] += amount;
        epochs[currentEpoch].totalCollected += amount;

        emit FeesCollected(token, msg.sender, amount);
    }

    /**
     * @notice Receive ETH fees
     */
    receive() external payable {
        accumulatedFees[address(0)] += msg.value;
        epochs[currentEpoch].totalCollected += msg.value;
        emit FeesCollected(address(0), msg.sender, msg.value);
    }

    /**
     * @notice Collect fees from multiple sources
     */
    function batchCollectFees(
        address[] calldata tokens,
        uint256[] calldata amounts
    ) external onlyRole(FEE_COLLECTOR_ROLE) {
        require(tokens.length == amounts.length, "Length mismatch");

        for (uint256 i = 0; i < tokens.length; i++) {
            if (!isSupportedToken[tokens[i]]) continue;
            
            IERC20(tokens[i]).safeTransferFrom(msg.sender, address(this), amounts[i]);
            accumulatedFees[tokens[i]] += amounts[i];
            epochs[currentEpoch].totalCollected += amounts[i];
            
            emit FeesCollected(tokens[i], msg.sender, amounts[i]);
        }
    }

    // ============================================
    // DISTRIBUTION
    // ============================================

    /**
     * @notice Distribute accumulated fees to recipients
     * @param token Token to distribute
     */
    function distribute(address token) external nonReentrant onlyRole(DISTRIBUTOR_ROLE) {
        if (paused) revert DistributionPaused();
        if (!isSupportedToken[token] && token != address(0)) revert TokenNotSupported();

        uint256 amount = accumulatedFees[token];
        if (amount < minDistributionAmount) return;

        accumulatedFees[token] = 0;

        for (uint256 i = 0; i < recipients.length; i++) {
            Recipient storage recipient = recipients[i];
            if (!recipient.isActive) continue;

            uint256 recipientAmount = (amount * recipient.share) / BPS_DENOMINATOR;
            if (recipientAmount == 0) continue;

            // Add to claimable
            claimable[recipient.addr][token] += recipientAmount;
            recipient.totalReceived += recipientAmount;
        }

        epochs[currentEpoch].totalDistributed += amount;

        emit FeesDistributed(currentEpoch, token, amount);
    }

    /**
     * @notice Distribute all supported tokens
     */
    function distributeAll() external nonReentrant onlyRole(DISTRIBUTOR_ROLE) {
        for (uint256 i = 0; i < feeTokens.length; i++) {
            address token = feeTokens[i];
            uint256 amount = accumulatedFees[token];
            
            if (amount < minDistributionAmount) continue;
            
            accumulatedFees[token] = 0;

            for (uint256 j = 0; j < recipients.length; j++) {
                Recipient storage recipient = recipients[j];
                if (!recipient.isActive) continue;

                uint256 recipientAmount = (amount * recipient.share) / BPS_DENOMINATOR;
                if (recipientAmount > 0) {
                    claimable[recipient.addr][token] += recipientAmount;
                    recipient.totalReceived += recipientAmount;
                }
            }

            epochs[currentEpoch].totalDistributed += amount;
            emit FeesDistributed(currentEpoch, token, amount);
        }
    }

    // ============================================
    // CLAIMING
    // ============================================

    /**
     * @notice Claim accumulated rewards
     * @param token Token to claim
     */
    function claim(address token) external nonReentrant {
        uint256 amount = claimable[msg.sender][token];
        if (amount == 0) revert NothingToClaim();

        claimable[msg.sender][token] = 0;
        
        uint256 recipientIdx = recipientIndex[msg.sender];
        if (recipientIdx > 0) {
            recipients[recipientIdx - 1].lastClaimTime = block.timestamp;
        }

        if (token == address(0)) {
            (bool success, ) = msg.sender.call{value: amount}("");
            require(success, "ETH transfer failed");
        } else {
            IERC20(token).safeTransfer(msg.sender, amount);
        }

        emit Claimed(msg.sender, token, amount, currentEpoch);
    }

    /**
     * @notice Claim all tokens
     */
    function claimAll() external nonReentrant {
        for (uint256 i = 0; i < feeTokens.length; i++) {
            address token = feeTokens[i];
            uint256 amount = claimable[msg.sender][token];
            
            if (amount == 0) continue;

            claimable[msg.sender][token] = 0;
            IERC20(token).safeTransfer(msg.sender, amount);
            emit Claimed(msg.sender, token, amount, currentEpoch);
        }

        // Check ETH
        uint256 ethAmount = claimable[msg.sender][address(0)];
        if (ethAmount > 0) {
            claimable[msg.sender][address(0)] = 0;
            (bool success, ) = msg.sender.call{value: ethAmount}("");
            require(success, "ETH transfer failed");
            emit Claimed(msg.sender, address(0), ethAmount, currentEpoch);
        }
    }

    /**
     * @notice Claim with Merkle proof (for retroactive distributions)
     */
    function claimWithProof(ClaimRequest calldata request) external nonReentrant {
        Epoch storage epoch = epochs[request.epochNumber];
        
        if (hasClaimed[request.claimer][request.epochNumber][request.token]) {
            revert AlreadyClaimed();
        }

        // Verify Merkle proof
        bytes32 leaf = keccak256(abi.encodePacked(
            request.claimer,
            request.token,
            request.amount
        ));

        if (!_verifyProof(request.merkleProof, epoch.merkleRoot, leaf)) {
            revert InvalidProof();
        }

        hasClaimed[request.claimer][request.epochNumber][request.token] = true;

        if (request.token == address(0)) {
            (bool success, ) = request.claimer.call{value: request.amount}("");
            require(success, "ETH transfer failed");
        } else {
            IERC20(request.token).safeTransfer(request.claimer, request.amount);
        }

        emit Claimed(request.claimer, request.token, request.amount, request.epochNumber);
    }

    // ============================================
    // REVENUE STREAMS
    // ============================================

    /**
     * @notice Create a vesting revenue stream
     * @param recipient Recipient address
     * @param token Token address
     * @param amount Total amount
     * @param duration Vesting duration
     */
    function createRevenueStream(
        address recipient,
        address token,
        uint256 amount,
        uint256 duration
    ) external onlyRole(DISTRIBUTOR_ROLE) returns (bytes32 streamId) {
        streamId = keccak256(abi.encodePacked(
            recipient,
            token,
            amount,
            block.timestamp,
            streamCount[recipient]
        ));

        revenueStreams[streamId] = RevenueStream({
            recipient: recipient,
            token: token,
            totalAmount: amount,
            startTime: block.timestamp,
            duration: duration,
            claimedAmount: 0
        });

        streamCount[recipient]++;

        // Transfer tokens to this contract
        IERC20(token).safeTransferFrom(msg.sender, address(this), amount);

        emit RevenueStreamCreated(streamId, recipient, token, amount);
    }

    /**
     * @notice Claim from revenue stream
     */
    function claimFromStream(bytes32 streamId) external nonReentrant {
        RevenueStream storage stream = revenueStreams[streamId];
        
        if (stream.recipient != msg.sender) revert InvalidRecipient();

        uint256 vested = _vestedAmount(stream);
        uint256 claimable_ = vested - stream.claimedAmount;
        
        if (claimable_ == 0) revert NothingToClaim();

        stream.claimedAmount += claimable_;

        if (stream.token == address(0)) {
            (bool success, ) = msg.sender.call{value: claimable_}("");
            require(success, "ETH transfer failed");
        } else {
            IERC20(stream.token).safeTransfer(msg.sender, claimable_);
        }

        emit RevenueStreamClaimed(streamId, claimable_);
    }

    function _vestedAmount(RevenueStream storage stream) internal view returns (uint256) {
        if (block.timestamp < stream.startTime) return 0;
        if (block.timestamp >= stream.startTime + stream.duration) {
            return stream.totalAmount;
        }
        
        uint256 elapsed = block.timestamp - stream.startTime;
        return (stream.totalAmount * elapsed) / stream.duration;
    }

    // ============================================
    // RECIPIENT MANAGEMENT
    // ============================================

    /**
     * @notice Add a recipient
     */
    function addRecipient(address addr, uint256 share, bool isContract) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        if (addr == address(0)) revert InvalidRecipient();
        if (recipients.length >= MAX_RECIPIENTS) revert TooManyRecipients();
        if (totalShareAllocated + share > BPS_DENOMINATOR) revert ShareExceedsLimit();
        if (recipientIndex[addr] != 0) revert InvalidRecipient();

        recipients.push(Recipient({
            addr: addr,
            share: share,
            isContract: isContract,
            isActive: true,
            totalReceived: 0,
            lastClaimTime: 0
        }));

        recipientIndex[addr] = recipients.length;
        totalShareAllocated += share;

        emit RecipientShareUpdated(addr, 0, share);
    }

    /**
     * @notice Update recipient share
     */
    function updateRecipientShare(address addr, uint256 newShare) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        uint256 idx = recipientIndex[addr];
        if (idx == 0) revert InvalidRecipient();

        Recipient storage recipient = recipients[idx - 1];
        uint256 oldShare = recipient.share;

        uint256 newTotal = totalShareAllocated - oldShare + newShare;
        if (newTotal > BPS_DENOMINATOR) revert ShareExceedsLimit();

        recipient.share = newShare;
        totalShareAllocated = newTotal;

        emit RecipientShareUpdated(addr, oldShare, newShare);
    }

    /**
     * @notice Deactivate recipient
     */
    function deactivateRecipient(address addr) external onlyRole(DEFAULT_ADMIN_ROLE) {
        uint256 idx = recipientIndex[addr];
        if (idx == 0) revert InvalidRecipient();

        recipients[idx - 1].isActive = false;
        totalShareAllocated -= recipients[idx - 1].share;
    }

    // ============================================
    // EPOCH MANAGEMENT
    // ============================================

    /**
     * @notice Finalize current epoch and start new one
     */
    function finalizeEpoch(bytes32 merkleRoot) 
        external 
        onlyRole(DISTRIBUTOR_ROLE) 
    {
        Epoch storage epoch = epochs[currentEpoch];
        require(block.timestamp >= epoch.endTime, "Epoch not ended");
        require(!epoch.finalized, "Already finalized");

        epoch.finalized = true;
        epoch.merkleRoot = merkleRoot;

        emit EpochFinalized(currentEpoch, merkleRoot);

        // Start new epoch
        currentEpoch++;
        epochs[currentEpoch] = Epoch({
            epochNumber: currentEpoch,
            startTime: block.timestamp,
            endTime: block.timestamp + epochDuration,
            totalCollected: 0,
            totalDistributed: 0,
            finalized: false,
            merkleRoot: bytes32(0)
        });
    }

    // ============================================
    // TOKEN MANAGEMENT
    // ============================================

    function addSupportedToken(address token) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (!isSupportedToken[token]) {
            isSupportedToken[token] = true;
            feeTokens.push(token);
        }
    }

    function removeSupportedToken(address token) external onlyRole(DEFAULT_ADMIN_ROLE) {
        isSupportedToken[token] = false;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getRecipient(address addr) external view returns (Recipient memory) {
        uint256 idx = recipientIndex[addr];
        if (idx == 0) revert InvalidRecipient();
        return recipients[idx - 1];
    }

    function getAllRecipients() external view returns (Recipient[] memory) {
        return recipients;
    }

    function getFeeTokens() external view returns (address[] memory) {
        return feeTokens;
    }

    function getClaimable(address account, address token) 
        external 
        view 
        returns (uint256) 
    {
        return claimable[account][token];
    }

    function getStreamVested(bytes32 streamId) external view returns (uint256) {
        RevenueStream storage stream = revenueStreams[streamId];
        return _vestedAmount(stream);
    }

    function getStreamClaimable(bytes32 streamId) external view returns (uint256) {
        RevenueStream storage stream = revenueStreams[streamId];
        return _vestedAmount(stream) - stream.claimedAmount;
    }

    // ============================================
    // INTERNAL FUNCTIONS
    // ============================================

    function _verifyProof(
        bytes32[] calldata proof,
        bytes32 root,
        bytes32 leaf
    ) internal pure returns (bool) {
        bytes32 computedHash = leaf;

        for (uint256 i = 0; i < proof.length; i++) {
            bytes32 proofElement = proof[i];

            if (computedHash <= proofElement) {
                computedHash = keccak256(abi.encodePacked(computedHash, proofElement));
            } else {
                computedHash = keccak256(abi.encodePacked(proofElement, computedHash));
            }
        }

        return computedHash == root;
    }

    // ============================================
    // ADMIN
    // ============================================

    function setPaused(bool _paused) external onlyRole(DEFAULT_ADMIN_ROLE) {
        paused = _paused;
    }

    function setEpochDuration(uint256 duration) external onlyRole(DEFAULT_ADMIN_ROLE) {
        epochDuration = duration;
    }

    function setMinDistributionAmount(uint256 amount) external onlyRole(DEFAULT_ADMIN_ROLE) {
        minDistributionAmount = amount;
    }

    function emergencyWithdraw(address token, uint256 amount, address to) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        if (token == address(0)) {
            (bool success, ) = to.call{value: amount}("");
            require(success, "ETH transfer failed");
        } else {
            IERC20(token).safeTransfer(to, amount);
        }
    }
}
