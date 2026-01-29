// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title VRFConsumer
 * @author Grey Protocol Team
 * @notice Verifiable Random Function (VRF) consumer and mock implementation
 * @dev Provides interfaces and utilities for VRF-based randomness
 * 
 * Features:
 * - VRF request/response pattern
 * - Chainlink VRF v2 compatible interface
 * - Mock VRF for testing
 * - Request tracking
 * - Callback handling
 * - Multiple random words support
 */

/**
 * @notice VRF Coordinator interface (Chainlink VRF v2 compatible)
 */
interface IVRFCoordinator {
    /**
     * @notice Request random words
     * @param keyHash Gas lane key hash
     * @param subId Subscription ID
     * @param minConfirmations Minimum confirmations
     * @param callbackGasLimit Gas limit for callback
     * @param numWords Number of random words
     * @return requestId Request identifier
     */
    function requestRandomWords(
        bytes32 keyHash,
        uint64 subId,
        uint16 minConfirmations,
        uint32 callbackGasLimit,
        uint32 numWords
    ) external returns (uint256 requestId);
}

/**
 * @notice VRF Consumer base interface
 */
interface IVRFConsumer {
    function rawFulfillRandomWords(uint256 requestId, uint256[] memory randomWords) external;
}

/**
 * @title VRFConsumerBase
 * @notice Base contract for VRF consumers
 */
abstract contract VRFConsumerBase is IVRFConsumer {
    // ============================================
    // ERRORS
    // ============================================

    error OnlyCoordinatorCanFulfill(address have, address want);
    error RequestNotFound(uint256 requestId);
    error RequestAlreadyFulfilled(uint256 requestId);

    // ============================================
    // STATE
    // ============================================

    /// @notice VRF Coordinator address
    address private immutable vrfCoordinator;

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(address _vrfCoordinator) {
        vrfCoordinator = _vrfCoordinator;
    }

    // ============================================
    // FULFILLMENT
    // ============================================

    /**
     * @notice Raw fulfillment called by coordinator
     */
    function rawFulfillRandomWords(
        uint256 requestId,
        uint256[] memory randomWords
    ) external override {
        if (msg.sender != vrfCoordinator) {
            revert OnlyCoordinatorCanFulfill(msg.sender, vrfCoordinator);
        }
        fulfillRandomWords(requestId, randomWords);
    }

    /**
     * @notice Override to handle random words
     * @param requestId Request ID
     * @param randomWords Array of random words
     */
    function fulfillRandomWords(
        uint256 requestId,
        uint256[] memory randomWords
    ) internal virtual;
}

/**
 * @title MockVRFCoordinator
 * @notice Mock VRF coordinator for testing
 */
contract MockVRFCoordinator is IVRFCoordinator {
    // ============================================
    // STRUCTS
    // ============================================

    struct Request {
        address consumer;
        uint32 numWords;
        bool fulfilled;
        uint256[] randomWords;
    }

    // ============================================
    // STATE
    // ============================================

    /// @notice Request counter
    uint256 public requestCounter;

    /// @notice Requests by ID
    mapping(uint256 => Request) public requests;

    /// @notice Base fee
    uint256 public baseFee = 0.1 ether;

    /// @notice Gas price link
    uint256 public gasPriceLink = 1e9;

    // ============================================
    // EVENTS
    // ============================================

    event RandomWordsRequested(
        uint256 indexed requestId,
        address indexed consumer,
        uint32 numWords
    );

    event RandomWordsFulfilled(
        uint256 indexed requestId,
        uint256[] randomWords
    );

    // ============================================
    // FUNCTIONS
    // ============================================

    /**
     * @notice Request random words (mock)
     */
    function requestRandomWords(
        bytes32, // keyHash
        uint64,  // subId
        uint16,  // minConfirmations
        uint32,  // callbackGasLimit
        uint32 numWords
    ) external override returns (uint256 requestId) {
        requestId = ++requestCounter;
        
        requests[requestId] = Request({
            consumer: msg.sender,
            numWords: numWords,
            fulfilled: false,
            randomWords: new uint256[](0)
        });

        emit RandomWordsRequested(requestId, msg.sender, numWords);
    }

    /**
     * @notice Fulfill request with mock random words
     * @param requestId Request to fulfill
     * @param seed Seed for generating random words
     */
    function fulfillRandomWords(
        uint256 requestId,
        uint256 seed
    ) external {
        Request storage request = requests[requestId];
        require(request.consumer != address(0), "Request not found");
        require(!request.fulfilled, "Already fulfilled");

        uint256[] memory randomWords = new uint256[](request.numWords);
        for (uint256 i = 0; i < request.numWords; i++) {
            randomWords[i] = uint256(keccak256(abi.encodePacked(seed, i, block.timestamp, requestId)));
        }

        request.fulfilled = true;
        request.randomWords = randomWords;

        IVRFConsumer(request.consumer).rawFulfillRandomWords(requestId, randomWords);

        emit RandomWordsFulfilled(requestId, randomWords);
    }

    /**
     * @notice Fulfill with specific values (for testing)
     */
    function fulfillRandomWordsWithValues(
        uint256 requestId,
        uint256[] calldata randomWords
    ) external {
        Request storage request = requests[requestId];
        require(request.consumer != address(0), "Request not found");
        require(!request.fulfilled, "Already fulfilled");
        require(randomWords.length == request.numWords, "Wrong word count");

        request.fulfilled = true;
        request.randomWords = randomWords;

        IVRFConsumer(request.consumer).rawFulfillRandomWords(requestId, randomWords);

        emit RandomWordsFulfilled(requestId, randomWords);
    }
}

/**
 * @title VRFRandomUtils
 * @notice Utility functions for working with VRF random words
 */
library VRFRandomUtils {
    /**
     * @notice Generates a random number in range [0, max)
     * @param randomWord Random word from VRF
     * @param max Maximum value (exclusive)
     * @return result Random number in range
     */
    function randomInRange(
        uint256 randomWord,
        uint256 max
    ) internal pure returns (uint256 result) {
        return randomWord % max;
    }

    /**
     * @notice Generates a random number in range [min, max)
     */
    function randomInRange(
        uint256 randomWord,
        uint256 min,
        uint256 max
    ) internal pure returns (uint256 result) {
        require(max > min, "Invalid range");
        return min + (randomWord % (max - min));
    }

    /**
     * @notice Generates a random boolean
     */
    function randomBool(uint256 randomWord) internal pure returns (bool) {
        return randomWord % 2 == 1;
    }

    /**
     * @notice Shuffles an array using Fisher-Yates
     * @param array Array to shuffle
     * @param randomWord Seed for shuffling
     */
    function shuffle(
        uint256[] memory array,
        uint256 randomWord
    ) internal pure {
        uint256 n = array.length;
        for (uint256 i = n - 1; i > 0; i--) {
            uint256 j = uint256(keccak256(abi.encodePacked(randomWord, i))) % (i + 1);
            (array[i], array[j]) = (array[j], array[i]);
        }
    }

    /**
     * @notice Selects random indices from array
     * @param arrayLength Length of source array
     * @param count Number of indices to select
     * @param randomWord Random seed
     * @return indices Selected indices
     */
    function selectRandom(
        uint256 arrayLength,
        uint256 count,
        uint256 randomWord
    ) internal pure returns (uint256[] memory indices) {
        require(count <= arrayLength, "Count exceeds array length");
        
        indices = new uint256[](count);
        bool[] memory selected = new bool[](arrayLength);
        
        uint256 selectedCount = 0;
        uint256 nonce = 0;
        
        while (selectedCount < count) {
            uint256 index = uint256(keccak256(abi.encodePacked(randomWord, nonce))) % arrayLength;
            if (!selected[index]) {
                selected[index] = true;
                indices[selectedCount] = index;
                selectedCount++;
            }
            nonce++;
        }
    }

    /**
     * @notice Generates multiple random numbers from one word
     * @param randomWord Source random word
     * @param count Number of random numbers to generate
     * @return numbers Array of derived random numbers
     */
    function expand(
        uint256 randomWord,
        uint256 count
    ) internal pure returns (uint256[] memory numbers) {
        numbers = new uint256[](count);
        for (uint256 i = 0; i < count; i++) {
            numbers[i] = uint256(keccak256(abi.encodePacked(randomWord, i)));
        }
    }

    /**
     * @notice Generates weighted random selection
     * @param randomWord Random word
     * @param weights Array of weights
     * @return index Selected index
     */
    function weightedRandom(
        uint256 randomWord,
        uint256[] memory weights
    ) internal pure returns (uint256 index) {
        uint256 totalWeight = 0;
        for (uint256 i = 0; i < weights.length; i++) {
            totalWeight += weights[i];
        }

        uint256 random = randomWord % totalWeight;
        uint256 cumulative = 0;

        for (uint256 i = 0; i < weights.length; i++) {
            cumulative += weights[i];
            if (random < cumulative) {
                return i;
            }
        }

        return weights.length - 1; // Fallback
    }

    /**
     * @notice Checks if random word passes a probability check
     * @param randomWord Random word
     * @param probability Probability in basis points (0-10000)
     * @return passed Whether the check passed
     */
    function probabilityCheck(
        uint256 randomWord,
        uint256 probability
    ) internal pure returns (bool passed) {
        return (randomWord % 10000) < probability;
    }
}

/**
 * @title VRFExample
 * @notice Example VRF consumer implementation
 */
contract VRFExample is VRFConsumerBase {
    using VRFRandomUtils for uint256;

    // ============================================
    // STATE
    // ============================================

    /// @notice Key hash for VRF
    bytes32 public keyHash;

    /// @notice Subscription ID
    uint64 public subscriptionId;

    /// @notice Callback gas limit
    uint32 public callbackGasLimit = 100000;

    /// @notice Request confirmations
    uint16 public requestConfirmations = 3;

    /// @notice Number of random words to request
    uint32 public numWords = 2;

    /// @notice Request ID tracking
    mapping(uint256 => address) public requestToSender;

    /// @notice Results storage
    mapping(address => uint256[]) public randomResults;

    /// @notice Last request ID
    uint256 public lastRequestId;

    // ============================================
    // EVENTS
    // ============================================

    event RandomnessRequested(uint256 indexed requestId, address indexed requester);
    event RandomnessFulfilled(uint256 indexed requestId, uint256[] randomWords);

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor(
        address vrfCoordinator,
        bytes32 _keyHash,
        uint64 _subscriptionId
    ) VRFConsumerBase(vrfCoordinator) {
        keyHash = _keyHash;
        subscriptionId = _subscriptionId;
    }

    // ============================================
    // FUNCTIONS
    // ============================================

    /**
     * @notice Requests random words
     */
    function requestRandomWords() external returns (uint256 requestId) {
        requestId = IVRFCoordinator(address(this)).requestRandomWords(
            keyHash,
            subscriptionId,
            requestConfirmations,
            callbackGasLimit,
            numWords
        );

        requestToSender[requestId] = msg.sender;
        lastRequestId = requestId;

        emit RandomnessRequested(requestId, msg.sender);
    }

    /**
     * @notice Handles VRF callback
     */
    function fulfillRandomWords(
        uint256 requestId,
        uint256[] memory randomWords
    ) internal override {
        address requester = requestToSender[requestId];
        randomResults[requester] = randomWords;
        
        emit RandomnessFulfilled(requestId, randomWords);
    }

    /**
     * @notice Gets latest random results for a user
     */
    function getRandomResults(address user) external view returns (uint256[] memory) {
        return randomResults[user];
    }
}
