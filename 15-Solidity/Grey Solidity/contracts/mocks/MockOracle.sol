// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

/**
 * @title MockOracle
 * @notice Mock price oracle for testing DeFi protocols
 * @dev Simulates Chainlink-style aggregator with test utilities
 */
contract MockOracle {
    struct RoundData {
        uint80 roundId;
        int256 answer;
        uint256 startedAt;
        uint256 updatedAt;
        uint80 answeredInRound;
    }
    
    string public description;
    uint8 public decimals;
    uint256 public version = 1;
    
    int256 private _price;
    uint80 private _roundId;
    uint256 private _timestamp;
    bool private _stale;
    bool private _invalid;
    
    mapping(uint80 => RoundData) private _rounds;
    
    event PriceUpdated(int256 indexed price, uint80 indexed roundId);
    
    constructor(string memory _description, uint8 _decimals, int256 initialPrice) {
        description = _description;
        decimals = _decimals;
        _price = initialPrice;
        _roundId = 1;
        _timestamp = block.timestamp;
        
        _rounds[_roundId] = RoundData({
            roundId: _roundId,
            answer: initialPrice,
            startedAt: block.timestamp,
            updatedAt: block.timestamp,
            answeredInRound: _roundId
        });
    }
    
    // ============================================
    // CHAINLINK AGGREGATOR INTERFACE
    // ============================================
    
    function latestRoundData() external view returns (
        uint80 roundId,
        int256 answer,
        uint256 startedAt,
        uint256 updatedAt,
        uint80 answeredInRound
    ) {
        require(!_invalid, "Invalid oracle");
        
        return (
            _roundId,
            _price,
            _timestamp,
            _stale ? _timestamp - 1 hours : block.timestamp,
            _roundId
        );
    }
    
    function getRoundData(uint80 roundId_) external view returns (
        uint80 roundId,
        int256 answer,
        uint256 startedAt,
        uint256 updatedAt,
        uint80 answeredInRound
    ) {
        RoundData memory data = _rounds[roundId_];
        require(data.roundId != 0, "Round not found");
        
        return (
            data.roundId,
            data.answer,
            data.startedAt,
            data.updatedAt,
            data.answeredInRound
        );
    }
    
    function latestAnswer() external view returns (int256) {
        return _price;
    }
    
    function latestTimestamp() external view returns (uint256) {
        return _timestamp;
    }
    
    function latestRound() external view returns (uint256) {
        return _roundId;
    }
    
    // ============================================
    // TEST UTILITIES
    // ============================================
    
    /**
     * @notice Set a new price
     */
    function setPrice(int256 newPrice) external {
        _price = newPrice;
        _roundId++;
        _timestamp = block.timestamp;
        
        _rounds[_roundId] = RoundData({
            roundId: _roundId,
            answer: newPrice,
            startedAt: block.timestamp,
            updatedAt: block.timestamp,
            answeredInRound: _roundId
        });
        
        emit PriceUpdated(newPrice, _roundId);
    }
    
    /**
     * @notice Set price with specific timestamp
     */
    function setPriceWithTimestamp(int256 newPrice, uint256 timestamp) external {
        _price = newPrice;
        _roundId++;
        _timestamp = timestamp;
        
        _rounds[_roundId] = RoundData({
            roundId: _roundId,
            answer: newPrice,
            startedAt: timestamp,
            updatedAt: timestamp,
            answeredInRound: _roundId
        });
        
        emit PriceUpdated(newPrice, _roundId);
    }
    
    /**
     * @notice Make oracle return stale data
     */
    function setStale(bool stale) external {
        _stale = stale;
    }
    
    /**
     * @notice Make oracle revert on read
     */
    function setInvalid(bool invalid) external {
        _invalid = invalid;
    }
    
    /**
     * @notice Simulate price spike (for testing manipulation resistance)
     */
    function simulatePriceSpike(int256 spikePrice, uint256 duration) external {
        int256 originalPrice = _price;
        
        _price = spikePrice;
        _roundId++;
        _timestamp = block.timestamp;
        
        _rounds[_roundId] = RoundData({
            roundId: _roundId,
            answer: spikePrice,
            startedAt: block.timestamp,
            updatedAt: block.timestamp,
            answeredInRound: _roundId
        });
        
        // Schedule return to normal (in real tests, this would be done manually)
        // For now, just emit the event
        emit PriceUpdated(spikePrice, _roundId);
    }
    
    /**
     * @notice Simulate gradual price change
     */
    function simulateGradualChange(
        int256 targetPrice,
        uint256 steps
    ) external returns (int256[] memory prices) {
        prices = new int256[](steps);
        int256 startPrice = _price;
        int256 priceStep = (targetPrice - startPrice) / int256(steps);
        
        for (uint256 i = 0; i < steps; i++) {
            _price = startPrice + (priceStep * int256(i + 1));
            _roundId++;
            _timestamp = block.timestamp + i;
            
            _rounds[_roundId] = RoundData({
                roundId: _roundId,
                answer: _price,
                startedAt: _timestamp,
                updatedAt: _timestamp,
                answeredInRound: _roundId
            });
            
            prices[i] = _price;
        }
    }
}

/**
 * @title MockVRF
 * @notice Mock VRF for testing randomness-dependent contracts
 * @dev Allows deterministic testing of random number consumers
 */
contract MockVRF {
    struct Request {
        address requester;
        uint256 requestId;
        uint256 randomness;
        bool fulfilled;
    }
    
    uint256 private _requestId;
    uint256 private _nextRandomness;
    bool private _autoFulfill;
    
    mapping(uint256 => Request) public requests;
    mapping(address => uint256[]) public userRequests;
    
    event RandomnessRequested(uint256 indexed requestId, address indexed requester);
    event RandomnessFulfilled(uint256 indexed requestId, uint256 randomness);
    
    constructor() {
        _autoFulfill = true;
        _nextRandomness = uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao)));
    }
    
    // ============================================
    // VRF COORDINATOR INTERFACE
    // ============================================
    
    /**
     * @notice Request randomness
     */
    function requestRandomness() external returns (uint256 requestId) {
        requestId = ++_requestId;
        
        requests[requestId] = Request({
            requester: msg.sender,
            requestId: requestId,
            randomness: 0,
            fulfilled: false
        });
        
        userRequests[msg.sender].push(requestId);
        
        emit RandomnessRequested(requestId, msg.sender);
        
        if (_autoFulfill) {
            fulfillRandomness(requestId, _generateRandomness());
        }
    }
    
    /**
     * @notice Request multiple random words
     */
    function requestRandomWords(uint32 numWords) external returns (uint256 requestId) {
        requestId = ++_requestId;
        
        requests[requestId] = Request({
            requester: msg.sender,
            requestId: requestId,
            randomness: 0,
            fulfilled: false
        });
        
        userRequests[msg.sender].push(requestId);
        
        emit RandomnessRequested(requestId, msg.sender);
        
        if (_autoFulfill) {
            fulfillRandomness(requestId, _generateRandomness());
        }
    }
    
    /**
     * @notice Fulfill randomness (callback to consumer)
     */
    function fulfillRandomness(uint256 requestId, uint256 randomness) public {
        Request storage request = requests[requestId];
        require(!request.fulfilled, "Already fulfilled");
        
        request.randomness = randomness;
        request.fulfilled = true;
        
        // Call the consumer's callback (if it implements the interface)
        try IVRFConsumer(request.requester).fulfillRandomness(requestId, randomness) {
        } catch {
            // Callback failed, but we still mark as fulfilled
        }
        
        emit RandomnessFulfilled(requestId, randomness);
    }
    
    // ============================================
    // TEST UTILITIES
    // ============================================
    
    /**
     * @notice Set auto-fulfill mode
     */
    function setAutoFulfill(bool auto_) external {
        _autoFulfill = auto_;
    }
    
    /**
     * @notice Set next randomness value (deterministic testing)
     */
    function setNextRandomness(uint256 randomness) external {
        _nextRandomness = randomness;
    }
    
    /**
     * @notice Manually fulfill with specific randomness
     */
    function fulfillWithRandomness(uint256 requestId, uint256 randomness) external {
        fulfillRandomness(requestId, randomness);
    }
    
    /**
     * @notice Get random words from seed
     */
    function getRandomWords(uint256 seed, uint32 numWords) external pure returns (uint256[] memory) {
        uint256[] memory words = new uint256[](numWords);
        for (uint32 i = 0; i < numWords; i++) {
            words[i] = uint256(keccak256(abi.encodePacked(seed, i)));
        }
        return words;
    }
    
    /**
     * @notice Generate pseudo-random number
     */
    function _generateRandomness() internal returns (uint256) {
        _nextRandomness = uint256(keccak256(abi.encodePacked(
            _nextRandomness,
            block.timestamp,
            block.prevrandao,
            msg.sender
        )));
        return _nextRandomness;
    }
}

interface IVRFConsumer {
    function fulfillRandomness(uint256 requestId, uint256 randomness) external;
}

/**
 * @title MockFlashLoanReceiver
 * @notice Mock flash loan receiver for testing flash loan attacks
 */
contract MockFlashLoanReceiver {
    address public lastLender;
    address public lastToken;
    uint256 public lastAmount;
    bytes public lastData;
    bool public shouldRepay = true;
    bool public shouldReenter;
    address public reenterTarget;
    bytes public reenterData;
    
    event FlashLoanReceived(address lender, address token, uint256 amount);
    
    /**
     * @notice Flash loan callback
     */
    function executeOperation(
        address[] calldata assets,
        uint256[] calldata amounts,
        uint256[] calldata premiums,
        address initiator,
        bytes calldata params
    ) external returns (bool) {
        lastLender = msg.sender;
        lastToken = assets[0];
        lastAmount = amounts[0];
        lastData = params;
        
        emit FlashLoanReceived(msg.sender, assets[0], amounts[0]);
        
        if (shouldReenter && reenterTarget != address(0)) {
            // Attempt reentrancy attack
            (bool success,) = reenterTarget.call(reenterData);
            // Ignore success - this is for testing
        }
        
        return shouldRepay;
    }
    
    /**
     * @notice Alternative flash loan callback (AAVE style)
     */
    function onFlashLoan(
        address initiator,
        address token,
        uint256 amount,
        uint256 fee,
        bytes calldata data
    ) external returns (bytes32) {
        lastLender = msg.sender;
        lastToken = token;
        lastAmount = amount;
        lastData = data;
        
        emit FlashLoanReceived(msg.sender, token, amount);
        
        if (shouldReenter && reenterTarget != address(0)) {
            (bool success,) = reenterTarget.call(reenterData);
        }
        
        return shouldRepay ? keccak256("ERC3156FlashBorrower.onFlashLoan") : bytes32(0);
    }
    
    // ============================================
    // TEST UTILITIES
    // ============================================
    
    function setShouldRepay(bool repay) external {
        shouldRepay = repay;
    }
    
    function setReentrancy(bool reenter, address target, bytes calldata data) external {
        shouldReenter = reenter;
        reenterTarget = target;
        reenterData = data;
    }
    
    function reset() external {
        lastLender = address(0);
        lastToken = address(0);
        lastAmount = 0;
        lastData = "";
        shouldRepay = true;
        shouldReenter = false;
        reenterTarget = address(0);
        reenterData = "";
    }
}

/**
 * @title MockReentrant
 * @notice Mock contract for testing reentrancy vulnerabilities
 */
contract MockReentrant {
    address public target;
    bytes public attackData;
    uint256 public attackCount;
    uint256 public maxAttacks = 3;
    bool public attacking;
    
    event ReentrancyAttempt(uint256 count);
    
    receive() external payable {
        if (attacking && attackCount < maxAttacks && target != address(0)) {
            attackCount++;
            emit ReentrancyAttempt(attackCount);
            (bool success,) = target.call(attackData);
        }
    }
    
    function setAttack(address _target, bytes calldata _data, uint256 _maxAttacks) external {
        target = _target;
        attackData = _data;
        maxAttacks = _maxAttacks;
        attackCount = 0;
    }
    
    function startAttack() external {
        attacking = true;
        attackCount = 0;
    }
    
    function stopAttack() external {
        attacking = false;
    }
    
    function executeAttack(address _target, bytes calldata _data) external payable {
        attacking = true;
        attackCount = 0;
        target = _target;
        attackData = _data;
        
        (bool success,) = _target.call{value: msg.value}(_data);
        require(success, "Initial attack failed");
    }
    
    function reset() external {
        target = address(0);
        attackData = "";
        attackCount = 0;
        attacking = false;
    }
    
    function withdraw() external {
        payable(msg.sender).transfer(address(this).balance);
    }
}

/**
 * @title MockGovernanceAttacker
 * @notice Mock contract for testing governance attack vectors
 */
contract MockGovernanceAttacker {
    address public governor;
    address public token;
    
    event AttackAttempted(string attackType);
    
    constructor(address _governor, address _token) {
        governor = _governor;
        token = _token;
    }
    
    /**
     * @notice Attempt flash loan governance attack
     */
    function attemptFlashLoanAttack(
        address flashLender,
        uint256 borrowAmount,
        bytes calldata proposalData
    ) external {
        emit AttackAttempted("flash_loan_governance");
        // In a real attack:
        // 1. Borrow tokens via flash loan
        // 2. Delegate/stake to gain voting power
        // 3. Create malicious proposal
        // 4. Vote on proposal (should fail due to checkpoints)
        // 5. Repay flash loan
    }
    
    /**
     * @notice Attempt vote buying attack
     */
    function attemptVoteBuying(
        address[] calldata voters,
        uint256[] calldata payments
    ) external {
        emit AttackAttempted("vote_buying");
        // Simulate vote buying - should be detectable/prevented
    }
    
    /**
     * @notice Attempt proposal sniping
     */
    function attemptProposalSniping(uint256 proposalId) external {
        emit AttackAttempted("proposal_sniping");
        // Attempt to execute proposal at the last moment
    }
}
