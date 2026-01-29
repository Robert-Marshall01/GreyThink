// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/proxy/Clones.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Pausable.sol";
import "@openzeppelin/contracts/proxy/ERC1967/ERC1967Proxy.sol";
import "@openzeppelin/contracts/proxy/beacon/BeaconProxy.sol";
import "@openzeppelin/contracts/proxy/beacon/UpgradeableBeacon.sol";
import "@openzeppelin/contracts/proxy/transparent/TransparentUpgradeableProxy.sol";

/**
 * @title ProxyFactory
 * @author Grey Protocol Team
 * @notice Factory for deploying various types of proxy contracts
 * @dev Supports UUPS, Transparent, and Beacon proxy patterns
 * 
 * Features:
 * - Deploy ERC1967 (UUPS) proxies
 * - Deploy Transparent proxies
 * - Deploy Beacon proxies with shared implementation
 * - Manage upgrade beacons
 * - Deterministic deployment support
 * - Batch proxy deployments
 */
contract ProxyFactory is AccessControl, ReentrancyGuard, Pausable {
    using Clones for address;

    // ============================================
    // CONSTANTS & ROLES
    // ============================================

    /// @notice Role for managing beacons
    bytes32 public constant BEACON_ADMIN_ROLE = keccak256("BEACON_ADMIN_ROLE");
    
    /// @notice Role for pausin the factory
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    
    /// @notice Role for fee management
    bytes32 public constant FEE_MANAGER_ROLE = keccak256("FEE_MANAGER_ROLE");

    // ============================================
    // ENUMS
    // ============================================

    /**
     * @notice Types of proxies supported
     */
    enum ProxyType {
        ERC1967_UUPS,
        TRANSPARENT,
        BEACON
    }

    // ============================================
    // STRUCTS
    // ============================================

    /**
     * @notice Beacon information
     */
    struct BeaconInfo {
        address beaconAddress;
        address currentImplementation;
        uint256 totalProxies;
        uint256 createdAt;
        uint256 lastUpgradedAt;
        string name;
        bool isActive;
    }

    /**
     * @notice Proxy deployment record
     */
    struct ProxyRecord {
        address proxyAddress;
        ProxyType proxyType;
        address implementation;
        address beaconAddress;
        address deployer;
        address admin;
        uint256 deployedAt;
        bytes32 initDataHash;
        uint256 feePaid;
    }

    /**
     * @notice Batch deployment request
     */
    struct BatchDeployRequest {
        ProxyType proxyType;
        address implementation;
        bytes initData;
        address admin;
        bytes32 salt;
    }

    // ============================================
    // STATE VARIABLES
    // ============================================

    /// @notice Counter for beacon IDs
    uint256 public beaconCounter;
    
    /// @notice Mapping from beacon ID to beacon info
    mapping(uint256 => BeaconInfo) public beacons;
    
    /// @notice Mapping from beacon address to beacon ID
    mapping(address => uint256) public beaconToId;
    
    /// @notice All deployed proxies
    address[] public allProxies;
    
    /// @notice Mapping from proxy address to record
    mapping(address => ProxyRecord) public proxyRecords;
    
    /// @notice Mapping from deployer to their proxies
    mapping(address => address[]) public deployerProxies;
    
    /// @notice Mapping from beacon to its proxies
    mapping(address => address[]) public beaconProxies;
    
    /// @notice Total fees collected
    uint256 public totalFeesCollected;
    
    /// @notice Fee recipient
    address public feeRecipient;
    
    /// @notice Deployment fees by proxy type
    mapping(ProxyType => uint256) public deploymentFees;
    
    /// @notice Nonce for deterministic deployments
    uint256 public deploymentNonce;

    // ============================================
    // EVENTS
    // ============================================

    event ProxyDeployed(
        address indexed proxy,
        ProxyType indexed proxyType,
        address indexed implementation,
        address deployer,
        address admin
    );

    event BeaconCreated(
        uint256 indexed beaconId,
        address indexed beaconAddress,
        address indexed implementation,
        string name
    );

    event BeaconUpgraded(
        uint256 indexed beaconId,
        address indexed oldImplementation,
        address indexed newImplementation
    );

    event FeesWithdrawn(address indexed recipient, uint256 amount);
    
    event DeploymentFeeUpdated(ProxyType indexed proxyType, uint256 newFee);

    // ============================================
    // ERRORS
    // ============================================

    error InvalidImplementation();
    error InvalidAdmin();
    error BeaconNotFound(uint256 beaconId);
    error BeaconNotActive(uint256 beaconId);
    error InsufficientFee(uint256 required, uint256 provided);
    error DeploymentFailed();
    error InitializationFailed();
    error InvalidRecipient();
    error NoFeesToWithdraw();
    error NotBeaconOwner();
    error ArrayLengthMismatch();

    // ============================================
    // CONSTRUCTOR
    // ============================================

    /**
     * @notice Initializes the ProxyFactory
     * @param _feeRecipient Address to receive deployment fees
     */
    constructor(address _feeRecipient) {
        if (_feeRecipient == address(0)) revert InvalidRecipient();
        
        feeRecipient = _feeRecipient;
        
        // Set default deployment fees
        deploymentFees[ProxyType.ERC1967_UUPS] = 0.005 ether;
        deploymentFees[ProxyType.TRANSPARENT] = 0.01 ether;
        deploymentFees[ProxyType.BEACON] = 0.003 ether;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(BEACON_ADMIN_ROLE, msg.sender);
        _grantRole(PAUSER_ROLE, msg.sender);
        _grantRole(FEE_MANAGER_ROLE, msg.sender);
    }

    // ============================================
    // UUPS (ERC1967) PROXY DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys a new ERC1967 (UUPS) proxy
     * @param implementation Address of the implementation contract
     * @param initData Initialization calldata
     * @return proxy Address of the deployed proxy
     */
    function deployUUPSProxy(
        address implementation,
        bytes calldata initData
    )
        external
        payable
        nonReentrant
        whenNotPaused
        returns (address proxy)
    {
        if (implementation == address(0)) revert InvalidImplementation();
        
        // Collect fee
        uint256 fee = _collectFee(ProxyType.ERC1967_UUPS);

        // Deploy proxy
        proxy = address(new ERC1967Proxy(implementation, initData));
        if (proxy == address(0)) revert DeploymentFailed();

        // Record deployment
        _recordProxy(proxy, ProxyType.ERC1967_UUPS, implementation, address(0), msg.sender, fee, initData);

        emit ProxyDeployed(proxy, ProxyType.ERC1967_UUPS, implementation, msg.sender, msg.sender);
    }

    /**
     * @notice Deploys a deterministic UUPS proxy using CREATE2
     * @param implementation Address of the implementation
     * @param initData Initialization calldata
     * @param salt Salt for deterministic deployment
     * @return proxy Address of the deployed proxy
     */
    function deployUUPSProxyDeterministic(
        address implementation,
        bytes calldata initData,
        bytes32 salt
    )
        external
        payable
        nonReentrant
        whenNotPaused
        returns (address proxy)
    {
        if (implementation == address(0)) revert InvalidImplementation();
        
        // Collect fee
        uint256 fee = _collectFee(ProxyType.ERC1967_UUPS);

        // Compute final salt
        bytes32 finalSalt = keccak256(abi.encodePacked(msg.sender, salt, deploymentNonce++));

        // Deploy using CREATE2
        bytes memory bytecode = abi.encodePacked(
            type(ERC1967Proxy).creationCode,
            abi.encode(implementation, initData)
        );
        
        assembly {
            proxy := create2(0, add(bytecode, 0x20), mload(bytecode), finalSalt)
        }
        if (proxy == address(0)) revert DeploymentFailed();

        // Record deployment
        _recordProxy(proxy, ProxyType.ERC1967_UUPS, implementation, address(0), msg.sender, fee, initData);

        emit ProxyDeployed(proxy, ProxyType.ERC1967_UUPS, implementation, msg.sender, msg.sender);
    }

    // ============================================
    // TRANSPARENT PROXY DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys a new Transparent proxy
     * @param implementation Address of the implementation
     * @param admin Address of the proxy admin
     * @param initData Initialization calldata
     * @return proxy Address of the deployed proxy
     */
    function deployTransparentProxy(
        address implementation,
        address admin,
        bytes calldata initData
    )
        external
        payable
        nonReentrant
        whenNotPaused
        returns (address proxy)
    {
        if (implementation == address(0)) revert InvalidImplementation();
        if (admin == address(0)) revert InvalidAdmin();
        
        // Collect fee
        uint256 fee = _collectFee(ProxyType.TRANSPARENT);

        // Deploy proxy
        proxy = address(new TransparentUpgradeableProxy(implementation, admin, initData));
        if (proxy == address(0)) revert DeploymentFailed();

        // Record deployment
        _recordProxy(proxy, ProxyType.TRANSPARENT, implementation, address(0), admin, fee, initData);

        emit ProxyDeployed(proxy, ProxyType.TRANSPARENT, implementation, msg.sender, admin);
    }

    // ============================================
    // BEACON MANAGEMENT
    // ============================================

    /**
     * @notice Creates a new upgrade beacon
     * @param implementation Initial implementation address
     * @param name Human-readable name for the beacon
     * @return beaconId ID of the created beacon
     * @return beaconAddress Address of the beacon
     */
    function createBeacon(
        address implementation,
        string calldata name
    )
        external
        onlyRole(BEACON_ADMIN_ROLE)
        returns (uint256 beaconId, address beaconAddress)
    {
        if (implementation == address(0)) revert InvalidImplementation();

        // Deploy beacon
        UpgradeableBeacon beacon = new UpgradeableBeacon(implementation, address(this));
        beaconAddress = address(beacon);

        // Assign ID
        beaconId = beaconCounter++;

        // Record beacon
        beacons[beaconId] = BeaconInfo({
            beaconAddress: beaconAddress,
            currentImplementation: implementation,
            totalProxies: 0,
            createdAt: block.timestamp,
            lastUpgradedAt: block.timestamp,
            name: name,
            isActive: true
        });

        beaconToId[beaconAddress] = beaconId;

        emit BeaconCreated(beaconId, beaconAddress, implementation, name);
    }

    /**
     * @notice Upgrades a beacon's implementation
     * @param beaconId ID of the beacon to upgrade
     * @param newImplementation New implementation address
     */
    function upgradeBeacon(
        uint256 beaconId,
        address newImplementation
    ) external onlyRole(BEACON_ADMIN_ROLE) {
        if (beaconId >= beaconCounter) revert BeaconNotFound(beaconId);
        if (newImplementation == address(0)) revert InvalidImplementation();

        BeaconInfo storage beacon = beacons[beaconId];
        if (!beacon.isActive) revert BeaconNotActive(beaconId);

        address oldImplementation = beacon.currentImplementation;
        
        // Upgrade beacon
        UpgradeableBeacon(beacon.beaconAddress).upgradeTo(newImplementation);
        
        // Update record
        beacon.currentImplementation = newImplementation;
        beacon.lastUpgradedAt = block.timestamp;

        emit BeaconUpgraded(beaconId, oldImplementation, newImplementation);
    }

    /**
     * @notice Deactivates a beacon (prevents new proxy deployments)
     * @param beaconId ID of the beacon
     */
    function deactivateBeacon(uint256 beaconId) external onlyRole(BEACON_ADMIN_ROLE) {
        if (beaconId >= beaconCounter) revert BeaconNotFound(beaconId);
        beacons[beaconId].isActive = false;
    }

    /**
     * @notice Reactivates a beacon
     * @param beaconId ID of the beacon
     */
    function reactivateBeacon(uint256 beaconId) external onlyRole(BEACON_ADMIN_ROLE) {
        if (beaconId >= beaconCounter) revert BeaconNotFound(beaconId);
        beacons[beaconId].isActive = true;
    }

    // ============================================
    // BEACON PROXY DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys a new beacon proxy
     * @param beaconId ID of the beacon to use
     * @param initData Initialization calldata
     * @return proxy Address of the deployed proxy
     */
    function deployBeaconProxy(
        uint256 beaconId,
        bytes calldata initData
    )
        external
        payable
        nonReentrant
        whenNotPaused
        returns (address proxy)
    {
        if (beaconId >= beaconCounter) revert BeaconNotFound(beaconId);
        
        BeaconInfo storage beacon = beacons[beaconId];
        if (!beacon.isActive) revert BeaconNotActive(beaconId);
        
        // Collect fee
        uint256 fee = _collectFee(ProxyType.BEACON);

        // Deploy proxy
        proxy = address(new BeaconProxy(beacon.beaconAddress, initData));
        if (proxy == address(0)) revert DeploymentFailed();

        // Update beacon stats
        beacon.totalProxies++;

        // Record deployment
        _recordProxy(
            proxy,
            ProxyType.BEACON,
            beacon.currentImplementation,
            beacon.beaconAddress,
            msg.sender,
            fee,
            initData
        );

        // Track beacon's proxies
        beaconProxies[beacon.beaconAddress].push(proxy);

        emit ProxyDeployed(proxy, ProxyType.BEACON, beacon.currentImplementation, msg.sender, address(this));
    }

    /**
     * @notice Deploys multiple beacon proxies in a batch
     * @param beaconId ID of the beacon to use
     * @param initDataArray Array of initialization calldata
     * @return proxies Array of deployed proxy addresses
     */
    function batchDeployBeaconProxies(
        uint256 beaconId,
        bytes[] calldata initDataArray
    )
        external
        payable
        nonReentrant
        whenNotPaused
        returns (address[] memory proxies)
    {
        if (beaconId >= beaconCounter) revert BeaconNotFound(beaconId);
        
        BeaconInfo storage beacon = beacons[beaconId];
        if (!beacon.isActive) revert BeaconNotActive(beaconId);

        uint256 count = initDataArray.length;
        uint256 totalFee = deploymentFees[ProxyType.BEACON] * count;
        if (msg.value < totalFee) revert InsufficientFee(totalFee, msg.value);

        totalFeesCollected += totalFee;
        proxies = new address[](count);

        for (uint256 i = 0; i < count; i++) {
            address proxy = address(new BeaconProxy(beacon.beaconAddress, initDataArray[i]));
            if (proxy == address(0)) revert DeploymentFailed();

            proxies[i] = proxy;
            beacon.totalProxies++;

            _recordProxy(
                proxy,
                ProxyType.BEACON,
                beacon.currentImplementation,
                beacon.beaconAddress,
                msg.sender,
                deploymentFees[ProxyType.BEACON],
                initDataArray[i]
            );

            beaconProxies[beacon.beaconAddress].push(proxy);
            
            emit ProxyDeployed(
                proxy,
                ProxyType.BEACON,
                beacon.currentImplementation,
                msg.sender,
                address(this)
            );
        }

        // Refund excess
        if (msg.value > totalFee) {
            (bool success,) = msg.sender.call{value: msg.value - totalFee}("");
            require(success, "Refund failed");
        }
    }

    // ============================================
    // BATCH DEPLOYMENT
    // ============================================

    /**
     * @notice Deploys multiple different proxies in a batch
     * @param requests Array of deployment requests
     * @return proxies Array of deployed proxy addresses
     */
    function batchDeploy(
        BatchDeployRequest[] calldata requests
    )
        external
        payable
        nonReentrant
        whenNotPaused
        returns (address[] memory proxies)
    {
        uint256 count = requests.length;
        proxies = new address[](count);
        
        uint256 totalFee = 0;
        for (uint256 i = 0; i < count; i++) {
            totalFee += deploymentFees[requests[i].proxyType];
        }
        
        if (msg.value < totalFee) revert InsufficientFee(totalFee, msg.value);
        totalFeesCollected += totalFee;

        for (uint256 i = 0; i < count; i++) {
            BatchDeployRequest calldata req = requests[i];
            address proxy;

            if (req.proxyType == ProxyType.ERC1967_UUPS) {
                proxy = address(new ERC1967Proxy(req.implementation, req.initData));
            } else if (req.proxyType == ProxyType.TRANSPARENT) {
                proxy = address(new TransparentUpgradeableProxy(
                    req.implementation,
                    req.admin,
                    req.initData
                ));
            } else {
                revert("Use deployBeaconProxy for beacon proxies");
            }

            if (proxy == address(0)) revert DeploymentFailed();

            proxies[i] = proxy;
            _recordProxy(
                proxy,
                req.proxyType,
                req.implementation,
                address(0),
                req.admin != address(0) ? req.admin : msg.sender,
                deploymentFees[req.proxyType],
                req.initData
            );

            emit ProxyDeployed(
                proxy,
                req.proxyType,
                req.implementation,
                msg.sender,
                req.admin != address(0) ? req.admin : msg.sender
            );
        }

        // Refund excess
        if (msg.value > totalFee) {
            (bool success,) = msg.sender.call{value: msg.value - totalFee}("");
            require(success, "Refund failed");
        }
    }

    // ============================================
    // FEE MANAGEMENT
    // ============================================

    /**
     * @notice Collects deployment fee
     * @param proxyType Type of proxy being deployed
     * @return fee Amount collected
     */
    function _collectFee(ProxyType proxyType) internal returns (uint256 fee) {
        fee = deploymentFees[proxyType];
        if (msg.value < fee) revert InsufficientFee(fee, msg.value);
        
        totalFeesCollected += fee;

        // Refund excess
        if (msg.value > fee) {
            (bool success,) = msg.sender.call{value: msg.value - fee}("");
            require(success, "Refund failed");
        }
    }

    /**
     * @notice Withdraws collected fees
     */
    function withdrawFees() external onlyRole(FEE_MANAGER_ROLE) nonReentrant {
        uint256 balance = address(this).balance;
        if (balance == 0) revert NoFeesToWithdraw();

        (bool success,) = feeRecipient.call{value: balance}("");
        require(success, "Transfer failed");

        emit FeesWithdrawn(feeRecipient, balance);
    }

    /**
     * @notice Updates deployment fee for a proxy type
     * @param proxyType Type of proxy
     * @param newFee New fee amount
     */
    function setDeploymentFee(
        ProxyType proxyType,
        uint256 newFee
    ) external onlyRole(FEE_MANAGER_ROLE) {
        deploymentFees[proxyType] = newFee;
        emit DeploymentFeeUpdated(proxyType, newFee);
    }

    /**
     * @notice Updates fee recipient
     * @param newRecipient New recipient address
     */
    function setFeeRecipient(address newRecipient) external onlyRole(FEE_MANAGER_ROLE) {
        if (newRecipient == address(0)) revert InvalidRecipient();
        feeRecipient = newRecipient;
    }

    // ============================================
    // INTERNAL HELPERS
    // ============================================

    /**
     * @notice Records proxy deployment
     */
    function _recordProxy(
        address proxy,
        ProxyType proxyType,
        address implementation,
        address beaconAddress,
        address admin,
        uint256 feePaid,
        bytes calldata initData
    ) internal {
        proxyRecords[proxy] = ProxyRecord({
            proxyAddress: proxy,
            proxyType: proxyType,
            implementation: implementation,
            beaconAddress: beaconAddress,
            deployer: msg.sender,
            admin: admin,
            deployedAt: block.timestamp,
            initDataHash: keccak256(initData),
            feePaid: feePaid
        });

        allProxies.push(proxy);
        deployerProxies[msg.sender].push(proxy);
    }

    // ============================================
    // VIEW FUNCTIONS
    // ============================================

    /**
     * @notice Returns total number of deployed proxies
     */
    function totalProxies() external view returns (uint256) {
        return allProxies.length;
    }

    /**
     * @notice Returns proxies deployed by an address
     */
    function getDeployerProxies(address deployer) external view returns (address[] memory) {
        return deployerProxies[deployer];
    }

    /**
     * @notice Returns proxies using a specific beacon
     */
    function getBeaconProxies(address beacon) external view returns (address[] memory) {
        return beaconProxies[beacon];
    }

    /**
     * @notice Returns beacon information
     */
    function getBeacon(uint256 beaconId) external view returns (BeaconInfo memory) {
        if (beaconId >= beaconCounter) revert BeaconNotFound(beaconId);
        return beacons[beaconId];
    }

    /**
     * @notice Returns all beacon IDs
     */
    function getAllBeaconIds() external view returns (uint256[] memory) {
        uint256[] memory ids = new uint256[](beaconCounter);
        for (uint256 i = 0; i < beaconCounter; i++) {
            ids[i] = i;
        }
        return ids;
    }

    /**
     * @notice Predicts UUPS proxy address for deterministic deployment
     */
    function predictUUPSProxyAddress(
        address implementation,
        bytes calldata initData,
        bytes32 salt
    ) external view returns (address) {
        bytes32 finalSalt = keccak256(abi.encodePacked(msg.sender, salt, deploymentNonce));
        bytes memory bytecode = abi.encodePacked(
            type(ERC1967Proxy).creationCode,
            abi.encode(implementation, initData)
        );
        bytes32 bytecodeHash = keccak256(bytecode);
        return address(uint160(uint256(keccak256(abi.encodePacked(
            bytes1(0xff),
            address(this),
            finalSalt,
            bytecodeHash
        )))));
    }

    // ============================================
    // PAUSE
    // ============================================

    function pause() external onlyRole(PAUSER_ROLE) {
        _pause();
    }

    function unpause() external onlyRole(PAUSER_ROLE) {
        _unpause();
    }

    receive() external payable {}
}
