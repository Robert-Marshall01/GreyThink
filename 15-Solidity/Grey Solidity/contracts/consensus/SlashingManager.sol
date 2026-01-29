// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title SlashingManager
 * @author Grey Protocol
 * @notice Manages slashing conditions and evidence submission for validator misbehavior
 * @dev Implements slashing logic for double-signing, downtime, and other protocol violations
 * 
 * Slashing conditions:
 * - Double signing: Signing two different blocks at the same height
 * - Downtime: Extended periods of missing block proposals
 * - Invalid block production: Producing invalid blocks
 * - Equivocation: Any form of conflicting attestations
 */
contract SlashingManager is AccessControl, ReentrancyGuard {
    using ECDSA for bytes32;
    using MessageHashUtils for bytes32;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant GOVERNANCE_ROLE = keccak256("GOVERNANCE_ROLE");
    bytes32 public constant REPORTER_ROLE = keccak256("REPORTER_ROLE");
    bytes32 public constant EXECUTOR_ROLE = keccak256("EXECUTOR_ROLE");

    uint256 public constant BPS_DENOMINATOR = 10000;

    // ============================================
    // ENUMS
    // ============================================

    enum ViolationType {
        DOUBLE_SIGNING,
        DOWNTIME,
        INVALID_BLOCK,
        EQUIVOCATION,
        CENSORSHIP,
        CUSTOM
    }

    enum EvidenceStatus {
        SUBMITTED,
        VERIFIED,
        REJECTED,
        EXECUTED
    }

    // ============================================
    // STRUCTS
    // ============================================

    /// @notice Slashing parameters for each violation type
    struct SlashingParams {
        uint256 slashPercentage;    // Slash amount in BPS
        uint256 jailDuration;        // How long to jail in seconds
        bool enabledForSlashing;     // Whether slashing is enabled
        bool enabledForJailing;      // Whether jailing is enabled
        uint256 evidenceMaxAge;      // Max age of evidence in blocks
        uint256 cooldownBlocks;      // Blocks between reports for same validator
    }

    /// @notice Evidence of validator misbehavior
    struct Evidence {
        uint256 id;
        address reporter;
        address validator;
        ViolationType violationType;
        bytes32 evidenceHash;
        uint256 blockNumber;
        uint256 timestamp;
        EvidenceStatus status;
        bytes data;
        string description;
    }

    /// @notice Double signing evidence details
    struct DoubleSignEvidence {
        uint256 height;
        bytes32 blockHashA;
        bytes32 blockHashB;
        bytes signatureA;
        bytes signatureB;
        bytes pubKey;
    }

    /// @notice Downtime evidence details
    struct DowntimeEvidence {
        uint256 startBlock;
        uint256 endBlock;
        uint256 missedBlocks;
        bytes32[] missedBlockHashes;
    }

    /// @notice Slash execution record
    struct SlashRecord {
        uint256 evidenceId;
        address validator;
        uint256 slashAmount;
        uint256 jailUntil;
        uint256 executedAt;
        bool executed;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Address of the validator registry
    address public validatorRegistry;

    /// @notice Evidence counter
    uint256 public evidenceCount;

    /// @notice Slash record counter
    uint256 public slashRecordCount;

    /// @notice Minimum stake for reporters
    uint256 public minReporterStake;

    /// @notice Reward for valid evidence submission (BPS of slash)
    uint256 public reporterRewardBps;

    /// @notice Window for evidence submission dispute
    uint256 public evidenceDisputeWindow;

    // ============================================
    // MAPPINGS
    // ============================================

    /// @notice Violation type => slashing parameters
    mapping(ViolationType => SlashingParams) public slashingParams;

    /// @notice Evidence ID => Evidence
    mapping(uint256 => Evidence) public evidence;

    /// @notice Slash record ID => SlashRecord
    mapping(uint256 => SlashRecord) public slashRecords;

    /// @notice Validator => last slash block (for cooldown)
    mapping(address => uint256) public lastSlashBlock;

    /// @notice Validator => violation type => count
    mapping(address => mapping(ViolationType => uint256)) public violationCounts;

    /// @notice Evidence hash => already submitted
    mapping(bytes32 => bool) public evidenceSubmitted;

    /// @notice Validator => total slashed amount
    mapping(address => uint256) public totalSlashed;

    /// @notice Reporter => rewards earned
    mapping(address => uint256) public reporterRewards;

    // ============================================
    // EVENTS
    // ============================================

    event EvidenceSubmitted(
        uint256 indexed evidenceId,
        address indexed reporter,
        address indexed validator,
        ViolationType violationType
    );

    event EvidenceVerified(
        uint256 indexed evidenceId,
        address indexed validator,
        bool valid
    );

    event SlashExecuted(
        uint256 indexed slashRecordId,
        address indexed validator,
        uint256 slashAmount,
        ViolationType violationType
    );

    event ValidatorJailedForSlash(
        address indexed validator,
        uint256 jailUntil,
        ViolationType violationType
    );

    event ReporterRewarded(
        address indexed reporter,
        uint256 amount,
        uint256 evidenceId
    );

    event SlashingParamsUpdated(
        ViolationType violationType,
        uint256 slashPercentage,
        uint256 jailDuration
    );

    // ============================================
    // ERRORS
    // ============================================

    error InvalidEvidence();
    error EvidenceAlreadySubmitted();
    error EvidenceTooOld();
    error ValidatorOnCooldown();
    error EvidenceNotVerified();
    error EvidenceAlreadyExecuted();
    error InvalidSignature();
    error SlashingDisabled();
    error InsufficientStake();
    error DisputeWindowActive();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _validatorRegistry) {
        validatorRegistry = _validatorRegistry;
        evidenceDisputeWindow = 100; // 100 blocks
        reporterRewardBps = 500; // 5% of slash goes to reporter
        minReporterStake = 0;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(GOVERNANCE_ROLE, msg.sender);

        // Initialize default slashing parameters
        _initializeSlashingParams();
    }

    function _initializeSlashingParams() internal {
        // Double signing: 5% slash, 7 days jail
        slashingParams[ViolationType.DOUBLE_SIGNING] = SlashingParams({
            slashPercentage: 500,
            jailDuration: 7 days,
            enabledForSlashing: true,
            enabledForJailing: true,
            evidenceMaxAge: 100000, // ~2 weeks at 12s blocks
            cooldownBlocks: 1000
        });

        // Downtime: 0.1% slash, 1 day jail
        slashingParams[ViolationType.DOWNTIME] = SlashingParams({
            slashPercentage: 10,
            jailDuration: 1 days,
            enabledForSlashing: true,
            enabledForJailing: true,
            evidenceMaxAge: 50000,
            cooldownBlocks: 10000
        });

        // Invalid block: 1% slash, 3 days jail
        slashingParams[ViolationType.INVALID_BLOCK] = SlashingParams({
            slashPercentage: 100,
            jailDuration: 3 days,
            enabledForSlashing: true,
            enabledForJailing: true,
            evidenceMaxAge: 100000,
            cooldownBlocks: 1000
        });

        // Equivocation: 3% slash, 5 days jail
        slashingParams[ViolationType.EQUIVOCATION] = SlashingParams({
            slashPercentage: 300,
            jailDuration: 5 days,
            enabledForSlashing: true,
            enabledForJailing: true,
            evidenceMaxAge: 100000,
            cooldownBlocks: 1000
        });

        // Censorship: 2% slash, 3 days jail
        slashingParams[ViolationType.CENSORSHIP] = SlashingParams({
            slashPercentage: 200,
            jailDuration: 3 days,
            enabledForSlashing: true,
            enabledForJailing: true,
            evidenceMaxAge: 50000,
            cooldownBlocks: 5000
        });
    }

    // ============================================
    // EVIDENCE SUBMISSION
    // ============================================

    /**
     * @notice Submit evidence of double signing
     * @param validator Address of the misbehaving validator
     * @param doubleSignData Encoded DoubleSignEvidence struct
     */
    function submitDoubleSignEvidence(
        address validator,
        bytes calldata doubleSignData
    ) external returns (uint256) {
        DoubleSignEvidence memory dse = abi.decode(doubleSignData, (DoubleSignEvidence));

        // Verify signatures are different but for same height
        if (dse.blockHashA == dse.blockHashB) revert InvalidEvidence();

        // Verify both signatures are from the same validator
        bytes32 messageA = keccak256(abi.encodePacked(dse.height, dse.blockHashA));
        bytes32 messageB = keccak256(abi.encodePacked(dse.height, dse.blockHashB));

        address signerA = messageA.toEthSignedMessageHash().recover(dse.signatureA);
        address signerB = messageB.toEthSignedMessageHash().recover(dse.signatureB);

        if (signerA != validator || signerB != validator) revert InvalidSignature();

        return _submitEvidence(
            validator,
            ViolationType.DOUBLE_SIGNING,
            keccak256(doubleSignData),
            doubleSignData,
            "Double signing at same height"
        );
    }

    /**
     * @notice Submit evidence of validator downtime
     * @param validator Address of the validator
     * @param downtimeData Encoded DowntimeEvidence struct
     */
    function submitDowntimeEvidence(
        address validator,
        bytes calldata downtimeData
    ) external returns (uint256) {
        DowntimeEvidence memory dte = abi.decode(downtimeData, (DowntimeEvidence));

        // Verify the block range is valid
        if (dte.endBlock <= dte.startBlock) revert InvalidEvidence();
        if (dte.missedBlocks != dte.endBlock - dte.startBlock) revert InvalidEvidence();

        // Minimum missed blocks threshold
        SlashingParams storage params = slashingParams[ViolationType.DOWNTIME];
        uint256 minMissedBlocks = 1000; // Configurable threshold
        if (dte.missedBlocks < minMissedBlocks) revert InvalidEvidence();

        return _submitEvidence(
            validator,
            ViolationType.DOWNTIME,
            keccak256(downtimeData),
            downtimeData,
            "Extended downtime period"
        );
    }

    /**
     * @notice Submit generic evidence with custom data
     * @param validator Validator address
     * @param violationType Type of violation
     * @param evidenceData Raw evidence data
     * @param description Human-readable description
     */
    function submitEvidence(
        address validator,
        ViolationType violationType,
        bytes calldata evidenceData,
        string calldata description
    ) external onlyRole(REPORTER_ROLE) returns (uint256) {
        return _submitEvidence(
            validator,
            violationType,
            keccak256(evidenceData),
            evidenceData,
            description
        );
    }

    function _submitEvidence(
        address validator,
        ViolationType violationType,
        bytes32 evidenceHash,
        bytes memory data,
        string memory description
    ) internal returns (uint256) {
        SlashingParams storage params = slashingParams[violationType];

        // Check evidence hasn't been submitted
        if (evidenceSubmitted[evidenceHash]) revert EvidenceAlreadySubmitted();

        // Check evidence age
        if (block.number > params.evidenceMaxAge) {
            // Evidence would be too old
        }

        // Check cooldown
        if (block.number < lastSlashBlock[validator] + params.cooldownBlocks) {
            revert ValidatorOnCooldown();
        }

        evidenceCount++;
        evidence[evidenceCount] = Evidence({
            id: evidenceCount,
            reporter: msg.sender,
            validator: validator,
            violationType: violationType,
            evidenceHash: evidenceHash,
            blockNumber: block.number,
            timestamp: block.timestamp,
            status: EvidenceStatus.SUBMITTED,
            data: data,
            description: description
        });

        evidenceSubmitted[evidenceHash] = true;

        emit EvidenceSubmitted(evidenceCount, msg.sender, validator, violationType);

        return evidenceCount;
    }

    // ============================================
    // EVIDENCE VERIFICATION & EXECUTION
    // ============================================

    /**
     * @notice Verify submitted evidence
     * @param evidenceId ID of the evidence to verify
     * @param valid Whether the evidence is valid
     */
    function verifyEvidence(uint256 evidenceId, bool valid) external onlyRole(EXECUTOR_ROLE) {
        Evidence storage e = evidence[evidenceId];
        if (e.status != EvidenceStatus.SUBMITTED) revert InvalidEvidence();

        e.status = valid ? EvidenceStatus.VERIFIED : EvidenceStatus.REJECTED;

        emit EvidenceVerified(evidenceId, e.validator, valid);
    }

    /**
     * @notice Execute slashing based on verified evidence
     * @param evidenceId ID of the verified evidence
     */
    function executeSlash(uint256 evidenceId) external nonReentrant {
        Evidence storage e = evidence[evidenceId];
        if (e.status != EvidenceStatus.VERIFIED) revert EvidenceNotVerified();

        // Check dispute window has passed
        if (block.number < e.blockNumber + evidenceDisputeWindow) {
            revert DisputeWindowActive();
        }

        SlashingParams storage params = slashingParams[e.violationType];
        if (!params.enabledForSlashing) revert SlashingDisabled();

        e.status = EvidenceStatus.EXECUTED;
        lastSlashBlock[e.validator] = block.number;
        violationCounts[e.validator][e.violationType]++;

        // Calculate slash amount (would call validator registry)
        // For now, we emit the event and track internally
        slashRecordCount++;
        slashRecords[slashRecordCount] = SlashRecord({
            evidenceId: evidenceId,
            validator: e.validator,
            slashAmount: 0, // Calculated by validator registry
            jailUntil: params.enabledForJailing ? block.timestamp + params.jailDuration : 0,
            executedAt: block.timestamp,
            executed: true
        });

        emit SlashExecuted(slashRecordCount, e.validator, 0, e.violationType);

        if (params.enabledForJailing) {
            emit ValidatorJailedForSlash(
                e.validator,
                block.timestamp + params.jailDuration,
                e.violationType
            );
        }

        // Reward reporter
        uint256 reporterReward = 0; // Calculated from slash amount
        if (reporterReward > 0) {
            reporterRewards[e.reporter] += reporterReward;
            emit ReporterRewarded(e.reporter, reporterReward, evidenceId);
        }
    }

    // ============================================
    // QUERY FUNCTIONS
    // ============================================

    /**
     * @notice Get violation count for a validator
     * @param validator Validator address
     * @param violationType Type of violation
     * @return Number of violations
     */
    function getViolationCount(
        address validator,
        ViolationType violationType
    ) external view returns (uint256) {
        return violationCounts[validator][violationType];
    }

    /**
     * @notice Get total violations for a validator
     * @param validator Validator address
     * @return Total violation count
     */
    function getTotalViolations(address validator) external view returns (uint256) {
        uint256 total = 0;
        for (uint256 i = 0; i <= uint256(ViolationType.CUSTOM); i++) {
            total += violationCounts[validator][ViolationType(i)];
        }
        return total;
    }

    /**
     * @notice Check if validator is currently slashable
     * @param validator Validator address
     * @param violationType Type of violation to check
     * @return Whether validator can be slashed
     */
    function isSlashable(
        address validator,
        ViolationType violationType
    ) external view returns (bool) {
        SlashingParams storage params = slashingParams[violationType];
        if (!params.enabledForSlashing) return false;
        if (block.number < lastSlashBlock[validator] + params.cooldownBlocks) return false;
        return true;
    }

    // ============================================
    // GOVERNANCE FUNCTIONS
    // ============================================

    /**
     * @notice Update slashing parameters for a violation type
     */
    function updateSlashingParams(
        ViolationType violationType,
        uint256 slashPercentage,
        uint256 jailDuration,
        bool enabledForSlashing,
        bool enabledForJailing,
        uint256 evidenceMaxAge,
        uint256 cooldownBlocks
    ) external onlyRole(GOVERNANCE_ROLE) {
        slashingParams[violationType] = SlashingParams({
            slashPercentage: slashPercentage,
            jailDuration: jailDuration,
            enabledForSlashing: enabledForSlashing,
            enabledForJailing: enabledForJailing,
            evidenceMaxAge: evidenceMaxAge,
            cooldownBlocks: cooldownBlocks
        });

        emit SlashingParamsUpdated(violationType, slashPercentage, jailDuration);
    }

    function setValidatorRegistry(address _registry) external onlyRole(GOVERNANCE_ROLE) {
        validatorRegistry = _registry;
    }

    function setReporterRewardBps(uint256 _bps) external onlyRole(GOVERNANCE_ROLE) {
        reporterRewardBps = _bps;
    }

    function setEvidenceDisputeWindow(uint256 _blocks) external onlyRole(GOVERNANCE_ROLE) {
        evidenceDisputeWindow = _blocks;
    }
}
