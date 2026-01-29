// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts-upgradeable/token/ERC20/ERC20Upgradeable.sol";
import "@openzeppelin/contracts-upgradeable/token/ERC20/extensions/ERC20BurnableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/access/AccessControlUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/UUPSUpgradeable.sol";

/**
 * @title WrappedToken
 * @author Grey Protocol Team
 * @notice Cross-chain wrapped token representation
 * @dev ERC20 token that represents a wrapped version of a native token from another chain
 * 
 * Features:
 * - Mintable by bridge contracts
 * - Burnable for cross-chain transfers
 * - Original chain and token tracking
 * - Pausable for emergencies
 * - Rate limiting for mints
 * - Balance snapshots
 * - Multi-bridge support
 */
contract WrappedToken is
    Initializable,
    ERC20Upgradeable,
    ERC20BurnableUpgradeable,
    AccessControlUpgradeable,
    PausableUpgradeable,
    UUPSUpgradeable
{
    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");
    bytes32 public constant RATE_LIMITER_ROLE = keccak256("RATE_LIMITER_ROLE");

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Bridge configuration
     * @param bridge Bridge address
     * @param isActive Whether bridge is active
     * @param mintLimit Per-transaction mint limit
     * @param dailyLimit Daily mint limit
     * @param dailyMinted Amount minted today
     * @param totalMinted Total amount minted by this bridge
     * @param lastDayReset Last daily reset timestamp
     */
    struct BridgeConfig {
        address bridge;
        bool isActive;
        uint256 mintLimit;
        uint256 dailyLimit;
        uint256 dailyMinted;
        uint256 totalMinted;
        uint256 lastDayReset;
    }

    /**
     * @notice Rate limit configuration
     */
    struct RateLimitConfig {
        uint256 windowDuration;
        uint256 maxPerWindow;
        uint256 currentWindowStart;
        uint256 currentWindowMinted;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Original token address on native chain
    address public originalToken;

    /// @notice Original chain ID
    uint256 public originalChainId;

    /// @notice Token decimals (may differ from original)
    uint8 private _decimals;

    /// @notice Bridge configurations
    mapping(address => BridgeConfig) public bridges;

    /// @notice List of authorized bridges
    address[] public authorizedBridges;

    /// @notice Global rate limit
    RateLimitConfig public globalRateLimit;

    /// @notice Whether rate limiting is enabled
    bool public rateLimitEnabled;

    /// @notice Total amount ever minted
    uint256 public totalMinted;

    /// @notice Total amount ever burned
    uint256 public totalBurned;

    /// @notice Blacklisted addresses
    mapping(address => bool) public blacklisted;

    /// @notice Storage gap
    uint256[40] private __gap;

    // ============================================
    // EVENTS
    // ============================================

    event TokenMinted(address indexed bridge, address indexed to, uint256 amount);
    event TokenBurned(address indexed from, uint256 amount);
    event BridgeAdded(address indexed bridge, uint256 mintLimit, uint256 dailyLimit);
    event BridgeRemoved(address indexed bridge);
    event BridgeUpdated(address indexed bridge, uint256 mintLimit, uint256 dailyLimit);
    event RateLimitUpdated(uint256 windowDuration, uint256 maxPerWindow);
    event AddressBlacklisted(address indexed account, bool blacklisted);

    // ============================================
    // ERRORS
    // ============================================

    error UnauthorizedBridge(address bridge);
    error BridgeNotActive(address bridge);
    error ExceedsMintLimit(uint256 amount, uint256 limit);
    error ExceedsDailyLimit(uint256 amount, uint256 remaining);
    error RateLimitExceeded(uint256 amount, uint256 remaining);
    error AddressBlacklistedError(address account);
    error ZeroAddress();
    error ZeroAmount();
    error AlreadyBridge(address bridge);
    error NotBridge(address bridge);

    // ============================================
    // MODIFIERS
    // ============================================

    modifier onlyBridge() {
        if (!bridges[msg.sender].isActive) {
            revert UnauthorizedBridge(msg.sender);
        }
        _;
    }

    modifier notBlacklisted(address account) {
        if (blacklisted[account]) {
            revert AddressBlacklistedError(account);
        }
        _;
    }

    // ============================================
    // INITIALIZER
    // ============================================

    /**
     * @notice Initializes the wrapped token
     * @param name Token name
     * @param symbol Token symbol
     * @param decimals_ Token decimals
     * @param _originalToken Original token address
     * @param _originalChainId Original chain ID
     * @param admin Admin address
     */
    function initialize(
        string memory name,
        string memory symbol,
        uint8 decimals_,
        address _originalToken,
        uint256 _originalChainId,
        address admin
    ) public initializer {
        if (admin == address(0)) revert ZeroAddress();
        if (_originalToken == address(0)) revert ZeroAddress();

        __ERC20_init(name, symbol);
        __ERC20Burnable_init();
        __AccessControl_init();
        __Pausable_init();
        __UUPSUpgradeable_init();

        _decimals = decimals_;
        originalToken = _originalToken;
        originalChainId = _originalChainId;

        // Default rate limit: 1M tokens per hour
        globalRateLimit = RateLimitConfig({
            windowDuration: 1 hours,
            maxPerWindow: 1_000_000 * 10 ** decimals_,
            currentWindowStart: block.timestamp,
            currentWindowMinted: 0
        });
        rateLimitEnabled = true;

        _grantRole(DEFAULT_ADMIN_ROLE, admin);
        _grantRole(PAUSER_ROLE, admin);
        _grantRole(UPGRADER_ROLE, admin);
        _grantRole(RATE_LIMITER_ROLE, admin);
    }

    // ============================================
    // MINTING
    // ============================================

    /**
     * @notice Mints wrapped tokens
     * @param to Recipient address
     * @param amount Amount to mint
     */
    function mint(address to, uint256 amount)
        external
        onlyBridge
        whenNotPaused
        notBlacklisted(to)
    {
        if (to == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();

        BridgeConfig storage config = bridges[msg.sender];

        // Check mint limit
        if (amount > config.mintLimit) {
            revert ExceedsMintLimit(amount, config.mintLimit);
        }

        // Check daily limit
        _checkAndUpdateDailyLimit(config, amount);

        // Check global rate limit
        if (rateLimitEnabled) {
            _checkAndUpdateRateLimit(amount);
        }

        // Update stats
        config.totalMinted += amount;
        totalMinted += amount;

        _mint(to, amount);

        emit TokenMinted(msg.sender, to, amount);
    }

    /**
     * @notice Batch mints to multiple recipients
     * @param recipients Array of recipients
     * @param amounts Array of amounts
     */
    function batchMint(address[] calldata recipients, uint256[] calldata amounts)
        external
        onlyBridge
        whenNotPaused
    {
        require(recipients.length == amounts.length, "Array length mismatch");

        BridgeConfig storage config = bridges[msg.sender];
        uint256 totalAmount = 0;

        for (uint256 i = 0; i < amounts.length; i++) {
            totalAmount += amounts[i];
        }

        // Check limits on total
        if (totalAmount > config.mintLimit * recipients.length) {
            revert ExceedsMintLimit(totalAmount, config.mintLimit * recipients.length);
        }

        _checkAndUpdateDailyLimit(config, totalAmount);

        if (rateLimitEnabled) {
            _checkAndUpdateRateLimit(totalAmount);
        }

        config.totalMinted += totalAmount;
        totalMinted += totalAmount;

        for (uint256 i = 0; i < recipients.length; i++) {
            if (!blacklisted[recipients[i]]) {
                _mint(recipients[i], amounts[i]);
                emit TokenMinted(msg.sender, recipients[i], amounts[i]);
            }
        }
    }

    // ============================================
    // BURNING
    // ============================================

    /**
     * @notice Burns tokens for cross-chain transfer
     * @param amount Amount to burn
     */
    function burn(uint256 amount) public override whenNotPaused notBlacklisted(msg.sender) {
        totalBurned += amount;
        super.burn(amount);
        emit TokenBurned(msg.sender, amount);
    }

    /**
     * @notice Burns tokens from another account
     * @param account Account to burn from
     * @param amount Amount to burn
     */
    function burnFrom(address account, uint256 amount)
        public
        override
        whenNotPaused
        notBlacklisted(account)
    {
        totalBurned += amount;
        super.burnFrom(account, amount);
        emit TokenBurned(account, amount);
    }

    // ============================================
    // RATE LIMITING
    // ============================================

    /**
     * @notice Checks and updates daily limit
     */
    function _checkAndUpdateDailyLimit(BridgeConfig storage config, uint256 amount) internal {
        // Reset if new day
        if (block.timestamp >= config.lastDayReset + 1 days) {
            config.dailyMinted = 0;
            config.lastDayReset = block.timestamp - (block.timestamp % 1 days);
        }

        uint256 remaining = config.dailyLimit > config.dailyMinted
            ? config.dailyLimit - config.dailyMinted
            : 0;

        if (amount > remaining) {
            revert ExceedsDailyLimit(amount, remaining);
        }

        config.dailyMinted += amount;
    }

    /**
     * @notice Checks and updates global rate limit
     */
    function _checkAndUpdateRateLimit(uint256 amount) internal {
        // Reset window if expired
        if (block.timestamp >= globalRateLimit.currentWindowStart + globalRateLimit.windowDuration) {
            globalRateLimit.currentWindowStart = block.timestamp;
            globalRateLimit.currentWindowMinted = 0;
        }

        uint256 remaining = globalRateLimit.maxPerWindow > globalRateLimit.currentWindowMinted
            ? globalRateLimit.maxPerWindow - globalRateLimit.currentWindowMinted
            : 0;

        if (amount > remaining) {
            revert RateLimitExceeded(amount, remaining);
        }

        globalRateLimit.currentWindowMinted += amount;
    }

    // ============================================
    // BRIDGE MANAGEMENT
    // ============================================

    /**
     * @notice Adds a new bridge
     */
    function addBridge(
        address bridge,
        uint256 mintLimit,
        uint256 dailyLimit
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (bridge == address(0)) revert ZeroAddress();
        if (bridges[bridge].bridge != address(0)) revert AlreadyBridge(bridge);

        bridges[bridge] = BridgeConfig({
            bridge: bridge,
            isActive: true,
            mintLimit: mintLimit,
            dailyLimit: dailyLimit,
            dailyMinted: 0,
            totalMinted: 0,
            lastDayReset: block.timestamp
        });

        authorizedBridges.push(bridge);
        _grantRole(MINTER_ROLE, bridge);

        emit BridgeAdded(bridge, mintLimit, dailyLimit);
    }

    /**
     * @notice Removes a bridge
     */
    function removeBridge(address bridge) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (bridges[bridge].bridge == address(0)) revert NotBridge(bridge);

        bridges[bridge].isActive = false;
        _revokeRole(MINTER_ROLE, bridge);

        emit BridgeRemoved(bridge);
    }

    /**
     * @notice Updates bridge limits
     */
    function updateBridgeLimits(
        address bridge,
        uint256 mintLimit,
        uint256 dailyLimit
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (bridges[bridge].bridge == address(0)) revert NotBridge(bridge);

        bridges[bridge].mintLimit = mintLimit;
        bridges[bridge].dailyLimit = dailyLimit;

        emit BridgeUpdated(bridge, mintLimit, dailyLimit);
    }

    /**
     * @notice Sets bridge active status
     */
    function setBridgeActive(address bridge, bool active) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (bridges[bridge].bridge == address(0)) revert NotBridge(bridge);
        bridges[bridge].isActive = active;
    }

    // ============================================
    // RATE LIMIT CONFIGURATION
    // ============================================

    /**
     * @notice Updates global rate limit
     */
    function setRateLimit(
        uint256 windowDuration,
        uint256 maxPerWindow
    ) external onlyRole(RATE_LIMITER_ROLE) {
        globalRateLimit.windowDuration = windowDuration;
        globalRateLimit.maxPerWindow = maxPerWindow;
        
        emit RateLimitUpdated(windowDuration, maxPerWindow);
    }

    /**
     * @notice Enables or disables rate limiting
     */
    function setRateLimitEnabled(bool enabled) external onlyRole(RATE_LIMITER_ROLE) {
        rateLimitEnabled = enabled;
    }

    // ============================================
    // BLACKLIST
    // ============================================

    /**
     * @notice Blacklists or unblacklists an address
     */
    function setBlacklisted(address account, bool _blacklisted) 
        external 
        onlyRole(DEFAULT_ADMIN_ROLE) 
    {
        blacklisted[account] = _blacklisted;
        emit AddressBlacklisted(account, _blacklisted);
    }

    /**
     * @notice Batch blacklist
     */
    function batchBlacklist(address[] calldata accounts, bool _blacklisted)
        external
        onlyRole(DEFAULT_ADMIN_ROLE)
    {
        for (uint256 i = 0; i < accounts.length; i++) {
            blacklisted[accounts[i]] = _blacklisted;
            emit AddressBlacklisted(accounts[i], _blacklisted);
        }
    }

    // ============================================
    // OVERRIDES
    // ============================================

    function decimals() public view virtual override returns (uint8) {
        return _decimals;
    }

    function _update(
        address from,
        address to,
        uint256 amount
    ) internal virtual override whenNotPaused {
        if (blacklisted[from]) revert AddressBlacklistedError(from);
        if (blacklisted[to]) revert AddressBlacklistedError(to);
        super._update(from, to, amount);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getAuthorizedBridges() external view returns (address[] memory) {
        return authorizedBridges;
    }

    function getBridgeConfig(address bridge) external view returns (BridgeConfig memory) {
        return bridges[bridge];
    }

    function getRemainingDailyLimit(address bridge) external view returns (uint256) {
        BridgeConfig storage config = bridges[bridge];
        
        // Check if new day
        if (block.timestamp >= config.lastDayReset + 1 days) {
            return config.dailyLimit;
        }
        
        return config.dailyLimit > config.dailyMinted
            ? config.dailyLimit - config.dailyMinted
            : 0;
    }

    function getRemainingRateLimit() external view returns (uint256) {
        // Check if window reset
        if (block.timestamp >= globalRateLimit.currentWindowStart + globalRateLimit.windowDuration) {
            return globalRateLimit.maxPerWindow;
        }
        
        return globalRateLimit.maxPerWindow > globalRateLimit.currentWindowMinted
            ? globalRateLimit.maxPerWindow - globalRateLimit.currentWindowMinted
            : 0;
    }

    function getTokenInfo() external view returns (
        string memory name_,
        string memory symbol_,
        uint8 decimals_,
        address original,
        uint256 originalChain,
        uint256 supply,
        uint256 minted,
        uint256 burned
    ) {
        return (
            name(),
            symbol(),
            _decimals,
            originalToken,
            originalChainId,
            totalSupply(),
            totalMinted,
            totalBurned
        );
    }

    // ============================================
    // ADMIN
    // ============================================

    function pause() external onlyRole(PAUSER_ROLE) { _pause(); }
    function unpause() external onlyRole(PAUSER_ROLE) { _unpause(); }
    function _authorizeUpgrade(address) internal override onlyRole(UPGRADER_ROLE) {}
}
