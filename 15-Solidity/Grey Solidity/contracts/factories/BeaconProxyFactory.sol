// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/proxy/beacon/BeaconProxy.sol";
import "@openzeppelin/contracts/proxy/beacon/UpgradeableBeacon.sol";

/**
 * @title BeaconProxyFactory
 * @notice Factory for deploying beacon proxies with deterministic addresses
 * @dev Implements beacon proxy pattern for efficient upgrades of multiple instances
 * 
 * Architecture:
 * - Multiple beacons for different implementation types
 * - CREATE2 deployment for deterministic addresses
 * - Version tracking for each beacon
 * - Batch deployment support
 * - Migration utilities for upgrading from other proxy patterns
 * 
 * Use Cases:
 * - Deploying multiple vaults with shared implementation
 * - Token factories where all tokens share upgrade path
 * - User-deployed contracts that can be upgraded together
 */
contract BeaconProxyFactory is AccessControl {
    // ============================================
    // ROLES
    // ============================================

    bytes32 public constant DEPLOYER_ROLE = keccak256("DEPLOYER_ROLE");
    bytes32 public constant UPGRADER_ROLE = keccak256("UPGRADER_ROLE");

    // ============================================
    // TYPES
    // ============================================

    /// @notice Beacon registration data
    struct BeaconData {
        address beacon;
        address currentImpl;
        uint256 version;
        uint256 proxyCount;
        string name;
        bool active;
    }

    /// @notice Deployed proxy data
    struct ProxyData {
        address proxy;
        bytes32 beaconId;
        address deployer;
        uint256 deployedAt;
        uint256 deploymentVersion;
        bytes32 salt;
    }

    /// @notice Upgrade history entry
    struct UpgradeRecord {
        address oldImpl;
        address newImpl;
        uint256 timestamp;
        uint256 fromVersion;
        uint256 toVersion;
    }

    // ============================================
    // STATE
    // ============================================

    /// @notice Registered beacons
    mapping(bytes32 => BeaconData) public beacons;
    bytes32[] public beaconIds;

    /// @notice Deployed proxies
    mapping(address => ProxyData) public proxies;
    address[] public allProxies;

    /// @notice Proxies by beacon
    mapping(bytes32 => address[]) public beaconProxies;

    /// @notice Upgrade history by beacon
    mapping(bytes32 => UpgradeRecord[]) public upgradeHistory;

    /// @notice Proxy by deployer and salt (for lookup)
    mapping(address => mapping(bytes32 => address)) public deployerProxies;

    /// @notice Nonce for deterministic deployment
    mapping(address => uint256) public deployerNonces;

    // ============================================
    // EVENTS
    // ============================================

    event BeaconRegistered(
        bytes32 indexed beaconId,
        address beacon,
        address implementation,
        string name
    );

    event BeaconUpgraded(
        bytes32 indexed beaconId,
        address indexed oldImpl,
        address indexed newImpl,
        uint256 version
    );

    event BeaconDeactivated(bytes32 indexed beaconId);

    event ProxyDeployed(
        address indexed proxy,
        bytes32 indexed beaconId,
        address indexed deployer,
        bytes32 salt
    );

    event BatchDeployment(
        bytes32 indexed beaconId,
        uint256 count,
        address[] proxies
    );

    // ============================================
    // ERRORS
    // ============================================

    error BeaconNotFound();
    error BeaconNotActive();
    error BeaconAlreadyExists();
    error InvalidImplementation();
    error DeploymentFailed();
    error ProxyAlreadyExists();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    constructor() {
        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(DEPLOYER_ROLE, msg.sender);
        _grantRole(UPGRADER_ROLE, msg.sender);
    }

    // ============================================
    // BEACON MANAGEMENT
    // ============================================

    /**
     * @notice Register a new beacon with implementation
     * @param beaconId Unique identifier for the beacon
     * @param implementation Initial implementation address
     * @param name Human-readable name
     */
    function registerBeacon(
        bytes32 beaconId,
        address implementation,
        string calldata name
    ) external onlyRole(DEFAULT_ADMIN_ROLE) returns (address beacon) {
        if (beacons[beaconId].beacon != address(0)) revert BeaconAlreadyExists();
        if (implementation == address(0)) revert InvalidImplementation();
        if (implementation.code.length == 0) revert InvalidImplementation();

        // Deploy new UpgradeableBeacon
        beacon = address(new UpgradeableBeacon(implementation, address(this)));

        beacons[beaconId] = BeaconData({
            beacon: beacon,
            currentImpl: implementation,
            version: 1,
            proxyCount: 0,
            name: name,
            active: true
        });

        beaconIds.push(beaconId);

        emit BeaconRegistered(beaconId, beacon, implementation, name);
    }

    /**
     * @notice Register an existing beacon
     */
    function registerExistingBeacon(
        bytes32 beaconId,
        address beacon,
        string calldata name
    ) external onlyRole(DEFAULT_ADMIN_ROLE) {
        if (beacons[beaconId].beacon != address(0)) revert BeaconAlreadyExists();

        address implementation = UpgradeableBeacon(beacon).implementation();

        beacons[beaconId] = BeaconData({
            beacon: beacon,
            currentImpl: implementation,
            version: 1,
            proxyCount: 0,
            name: name,
            active: true
        });

        beaconIds.push(beaconId);

        emit BeaconRegistered(beaconId, beacon, implementation, name);
    }

    /**
     * @notice Upgrade a beacon's implementation
     * @param beaconId The beacon to upgrade
     * @param newImplementation New implementation address
     */
    function upgradeBeacon(
        bytes32 beaconId,
        address newImplementation
    ) external onlyRole(UPGRADER_ROLE) {
        BeaconData storage data = beacons[beaconId];

        if (data.beacon == address(0)) revert BeaconNotFound();
        if (!data.active) revert BeaconNotActive();
        if (newImplementation == address(0)) revert InvalidImplementation();
        if (newImplementation.code.length == 0) revert InvalidImplementation();

        address oldImpl = data.currentImpl;
        uint256 oldVersion = data.version;

        // Upgrade the beacon
        UpgradeableBeacon(data.beacon).upgradeTo(newImplementation);

        data.currentImpl = newImplementation;
        data.version++;

        // Record upgrade history
        upgradeHistory[beaconId].push(UpgradeRecord({
            oldImpl: oldImpl,
            newImpl: newImplementation,
            timestamp: block.timestamp,
            fromVersion: oldVersion,
            toVersion: data.version
        }));

        emit BeaconUpgraded(beaconId, oldImpl, newImplementation, data.version);
    }

    /**
     * @notice Deactivate a beacon (prevents new deployments)
     */
    function deactivateBeacon(bytes32 beaconId) external onlyRole(DEFAULT_ADMIN_ROLE) {
        BeaconData storage data = beacons[beaconId];

        if (data.beacon == address(0)) revert BeaconNotFound();

        data.active = false;

        emit BeaconDeactivated(beaconId);
    }

    // ============================================
    // PROXY DEPLOYMENT
    // ============================================

    /**
     * @notice Deploy a new proxy with initialization
     * @param beaconId The beacon to use
     * @param initData Initialization calldata
     * @param salt Salt for CREATE2 (0 for regular CREATE)
     */
    function deployProxy(
        bytes32 beaconId,
        bytes calldata initData,
        bytes32 salt
    ) external onlyRole(DEPLOYER_ROLE) returns (address proxy) {
        BeaconData storage data = beacons[beaconId];

        if (data.beacon == address(0)) revert BeaconNotFound();
        if (!data.active) revert BeaconNotActive();

        // Use deployer nonce if no salt provided
        bytes32 actualSalt = salt == bytes32(0) 
            ? keccak256(abi.encodePacked(msg.sender, deployerNonces[msg.sender]++))
            : salt;

        // Check for collision
        if (deployerProxies[msg.sender][actualSalt] != address(0)) {
            revert ProxyAlreadyExists();
        }

        // Deploy with CREATE2
        proxy = address(new BeaconProxy{salt: actualSalt}(
            data.beacon,
            initData
        ));

        if (proxy == address(0)) revert DeploymentFailed();

        // Record deployment
        _recordDeployment(proxy, beaconId, actualSalt);

        emit ProxyDeployed(proxy, beaconId, msg.sender, actualSalt);
    }

    /**
     * @notice Deploy multiple proxies in one transaction
     */
    function batchDeployProxies(
        bytes32 beaconId,
        bytes[] calldata initDatas
    ) external onlyRole(DEPLOYER_ROLE) returns (address[] memory deployedProxies) {
        BeaconData storage data = beacons[beaconId];

        if (data.beacon == address(0)) revert BeaconNotFound();
        if (!data.active) revert BeaconNotActive();

        uint256 count = initDatas.length;
        deployedProxies = new address[](count);

        for (uint256 i = 0; i < count; i++) {
            bytes32 salt = keccak256(abi.encodePacked(
                msg.sender,
                deployerNonces[msg.sender]++,
                i
            ));

            address proxy = address(new BeaconProxy{salt: salt}(
                data.beacon,
                initDatas[i]
            ));

            if (proxy == address(0)) revert DeploymentFailed();

            _recordDeployment(proxy, beaconId, salt);
            deployedProxies[i] = proxy;
        }

        emit BatchDeployment(beaconId, count, deployedProxies);
    }

    /**
     * @notice Compute proxy address before deployment
     */
    function computeProxyAddress(
        bytes32 beaconId,
        bytes calldata initData,
        bytes32 salt
    ) external view returns (address) {
        BeaconData memory data = beacons[beaconId];
        if (data.beacon == address(0)) revert BeaconNotFound();

        bytes memory bytecode = abi.encodePacked(
            type(BeaconProxy).creationCode,
            abi.encode(data.beacon, initData)
        );

        return address(uint160(uint256(keccak256(abi.encodePacked(
            bytes1(0xff),
            address(this),
            salt,
            keccak256(bytecode)
        )))));
    }

    /**
     * @notice Record deployment in storage
     */
    function _recordDeployment(
        address proxy,
        bytes32 beaconId,
        bytes32 salt
    ) internal {
        BeaconData storage data = beacons[beaconId];

        proxies[proxy] = ProxyData({
            proxy: proxy,
            beaconId: beaconId,
            deployer: msg.sender,
            deployedAt: block.timestamp,
            deploymentVersion: data.version,
            salt: salt
        });

        allProxies.push(proxy);
        beaconProxies[beaconId].push(proxy);
        deployerProxies[msg.sender][salt] = proxy;
        data.proxyCount++;
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    function getBeacon(bytes32 beaconId) external view returns (
        address beacon,
        address currentImpl,
        uint256 version,
        uint256 proxyCount,
        string memory name,
        bool active
    ) {
        BeaconData memory data = beacons[beaconId];
        return (
            data.beacon,
            data.currentImpl,
            data.version,
            data.proxyCount,
            data.name,
            data.active
        );
    }

    function getBeaconCount() external view returns (uint256) {
        return beaconIds.length;
    }

    function getAllBeaconIds() external view returns (bytes32[] memory) {
        return beaconIds;
    }

    function getBeaconProxies(bytes32 beaconId) external view returns (address[] memory) {
        return beaconProxies[beaconId];
    }

    function getUpgradeHistory(bytes32 beaconId) external view returns (UpgradeRecord[] memory) {
        return upgradeHistory[beaconId];
    }

    function getProxyInfo(address proxy) external view returns (
        bytes32 beaconId,
        address deployer,
        uint256 deployedAt,
        uint256 deploymentVersion,
        uint256 currentVersion
    ) {
        ProxyData memory data = proxies[proxy];
        return (
            data.beaconId,
            data.deployer,
            data.deployedAt,
            data.deploymentVersion,
            beacons[data.beaconId].version
        );
    }

    function isProxyUpToDate(address proxy) external view returns (bool) {
        ProxyData memory data = proxies[proxy];
        return data.deploymentVersion == beacons[data.beaconId].version;
    }

    function getOutdatedProxies(bytes32 beaconId) external view returns (address[] memory) {
        address[] memory allBeaconProxies = beaconProxies[beaconId];
        uint256 currentVersion = beacons[beaconId].version;

        // Count outdated
        uint256 outdatedCount = 0;
        for (uint256 i = 0; i < allBeaconProxies.length; i++) {
            if (proxies[allBeaconProxies[i]].deploymentVersion < currentVersion) {
                outdatedCount++;
            }
        }

        // Collect outdated
        address[] memory outdated = new address[](outdatedCount);
        uint256 index = 0;
        for (uint256 i = 0; i < allBeaconProxies.length; i++) {
            if (proxies[allBeaconProxies[i]].deploymentVersion < currentVersion) {
                outdated[index++] = allBeaconProxies[i];
            }
        }

        return outdated;
    }
}
