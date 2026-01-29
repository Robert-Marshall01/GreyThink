/**
 * @title Comprehensive Ecosystem Deployment Script
 * @notice Deploys the complete Grey Solidity ecosystem with all modules
 * @dev Run with: npx hardhat run scripts/deploy-full-ecosystem.js --network <network>
 */

const { ethers, upgrades } = require("hardhat");
const fs = require("fs");
const path = require("path");

// ============================================
// CONFIGURATION
// ============================================

const CONFIG = {
  // Token Configuration
  token: {
    name: "Grey Token",
    symbol: "GREY",
    initialSupply: ethers.parseEther("100000000"),  // 100M tokens
  },
  
  // NFT Configuration
  nft: {
    name: "Grey NFT Collection",
    symbol: "GNFT",
    baseURI: "https://api.grey.io/nft/",
    maxSupply: 10000,
    royaltyBps: 500,  // 5%
  },
  
  // Staking Configuration
  staking: {
    rewardRate: ethers.parseEther("0.1"),  // 10% APR
    lockPeriod: 7 * 24 * 60 * 60,          // 7 days
    minStake: ethers.parseEther("100"),
    maxStake: ethers.parseEther("1000000"),
  },
  
  // Liquidity Pool Configuration
  liquidityPool: {
    fee: 30,  // 0.3%
  },
  
  // Governance Configuration
  governance: {
    votingDelay: 7200,           // ~1 day (12s blocks)
    votingPeriod: 50400,         // ~1 week
    proposalThreshold: ethers.parseEther("100000"),
    quorumNumerator: 4,          // 4%
  },
  
  // Timelock Configuration
  timelock: {
    minDelay: 86400,  // 1 day
  },
  
  // Marketplace Configuration
  marketplace: {
    fee: 250,  // 2.5%
  },
  
  // Escrow Configuration
  escrow: {
    disputePeriod: 86400,  // 1 day
    fee: 100,  // 1%
  },
  
  // Bridge Configuration
  bridge: {
    fee: ethers.parseEther("0.001"),
    maxTransfer: ethers.parseEther("100000"),
    validatorThreshold: 2,
  },
  
  // Oracle Configuration
  oracle: {
    minReporters: 2,
    priceTTL: 3600,  // 1 hour
  },
  
  // Security Configuration
  security: {
    circuitBreaker: {
      cooldown: 3600,
      threshold: 5,
      lockDuration: 86400,
    },
    rateLimiter: {
      callLimit: 100,
      window: 3600,
      volumeLimit: ethers.parseEther("10000"),
    },
    multiSig: {
      required: 3,
    },
    emergency: {
      delay: 7 * 24 * 60 * 60,  // 7 days
    },
  },
  
  // Vault Configuration
  vault: {
    performanceFee: 1000,  // 10%
    managementFee: 200,    // 2%
  },
};

// ============================================
// DEPLOYMENT STATE
// ============================================

const deployments = {
  network: "",
  timestamp: "",
  deployer: "",
  contracts: {},
};

// ============================================
// UTILITY FUNCTIONS
// ============================================

async function logDeployment(name, address, ...args) {
  console.log(`  ✓ ${name} deployed at: ${address}`);
  deployments.contracts[name] = {
    address,
    args,
    verified: false,
  };
}

async function deploy(name, factory, ...args) {
  const contract = await factory.deploy(...args);
  await contract.waitForDeployment();
  const address = await contract.getAddress();
  await logDeployment(name, address, ...args);
  return contract;
}

async function deployProxy(name, factory, initArgs) {
  const contract = await upgrades.deployProxy(factory, initArgs);
  await contract.waitForDeployment();
  const address = await contract.getAddress();
  await logDeployment(`${name} (Proxy)`, address, initArgs);
  return contract;
}

function saveDeployments() {
  const deploymentsDir = path.join(__dirname, "../deployments");
  if (!fs.existsSync(deploymentsDir)) {
    fs.mkdirSync(deploymentsDir, { recursive: true });
  }
  
  const filename = `${deployments.network}-${deployments.timestamp}.json`;
  const filepath = path.join(deploymentsDir, filename);
  
  fs.writeFileSync(filepath, JSON.stringify(deployments, null, 2));
  console.log(`\n📄 Deployments saved to: ${filepath}`);
  
  // Also save latest
  const latestPath = path.join(deploymentsDir, `${deployments.network}-latest.json`);
  fs.writeFileSync(latestPath, JSON.stringify(deployments, null, 2));
}

// ============================================
// CORE DEPLOYMENT FUNCTIONS
// ============================================

async function deployTokens(deployer, feeRecipient) {
  console.log("\n📦 Deploying Token Contracts...");
  
  // GreyToken (ERC20)
  const GreyToken = await ethers.getContractFactory("GreyToken");
  const greyToken = await deploy(
    "GreyToken",
    GreyToken,
    CONFIG.token.name,
    CONFIG.token.symbol,
    CONFIG.token.initialSupply,
    deployer
  );
  
  // GreyNFT (ERC721)
  const GreyNFT = await ethers.getContractFactory("GreyNFT");
  const greyNFT = await deploy(
    "GreyNFT",
    GreyNFT,
    CONFIG.nft.name,
    CONFIG.nft.symbol,
    CONFIG.nft.baseURI,
    CONFIG.nft.maxSupply,
    CONFIG.nft.royaltyBps,
    feeRecipient
  );
  
  // GreyMultiToken (ERC1155)
  const GreyMultiToken = await ethers.getContractFactory("GreyMultiToken");
  const greyMultiToken = await deploy(
    "GreyMultiToken",
    GreyMultiToken,
    "https://api.grey.io/multi/",
    feeRecipient,
    CONFIG.nft.royaltyBps
  );
  
  // MockERC20 for testing
  const MockERC20 = await ethers.getContractFactory("MockERC20");
  const rewardToken = await deploy(
    "RewardToken",
    MockERC20,
    "Grey Reward",
    "GREYR",
    18
  );
  
  return { greyToken, greyNFT, greyMultiToken, rewardToken };
}

async function deployGovernance(deployer, greyToken) {
  console.log("\n🏛️  Deploying Governance Contracts...");
  
  // Timelock
  const GreyTimelock = await ethers.getContractFactory("GreyTimelock");
  const timelock = await deploy(
    "GreyTimelock",
    GreyTimelock,
    CONFIG.timelock.minDelay,
    [deployer],  // proposers
    [deployer],  // executors
    deployer     // admin
  );
  
  // Governor
  const GreyGovernor = await ethers.getContractFactory("GreyGovernor");
  const governor = await deploy(
    "GreyGovernor",
    GreyGovernor,
    await greyToken.getAddress(),
    await timelock.getAddress(),
    CONFIG.governance.votingDelay,
    CONFIG.governance.votingPeriod,
    CONFIG.governance.proposalThreshold,
    CONFIG.governance.quorumNumerator
  );
  
  // Treasury
  const Treasury = await ethers.getContractFactory("Treasury");
  const treasury = await deploy(
    "Treasury",
    Treasury,
    await timelock.getAddress()
  );
  
  // Grant roles to governor
  const PROPOSER_ROLE = await timelock.PROPOSER_ROLE();
  const EXECUTOR_ROLE = await timelock.EXECUTOR_ROLE();
  await timelock.grantRole(PROPOSER_ROLE, await governor.getAddress());
  await timelock.grantRole(EXECUTOR_ROLE, await governor.getAddress());
  console.log("  ✓ Governance roles configured");
  
  return { timelock, governor, treasury };
}

async function deployDeFi(deployer, greyToken, rewardToken, feeRecipient) {
  console.log("\n💰 Deploying DeFi Contracts...");
  
  // Staking Pool
  const StakingPool = await ethers.getContractFactory("StakingPool");
  const stakingPool = await deploy(
    "StakingPool",
    StakingPool,
    await greyToken.getAddress(),
    await rewardToken.getAddress(),
    CONFIG.staking.rewardRate,
    CONFIG.staking.lockPeriod,
    CONFIG.staking.minStake,
    CONFIG.staking.maxStake
  );
  
  // Liquidity Pool (using reward token as pair)
  const LiquidityPool = await ethers.getContractFactory("LiquidityPool");
  const liquidityPool = await deploy(
    "LiquidityPool",
    LiquidityPool,
    await greyToken.getAddress(),
    await rewardToken.getAddress(),
    CONFIG.liquidityPool.fee,
    feeRecipient
  );
  
  // Vault
  const Vault = await ethers.getContractFactory("Vault");
  const vault = await deploy(
    "Vault",
    Vault,
    await greyToken.getAddress(),
    "Grey Vault Shares",
    "gvGREY",
    CONFIG.vault.performanceFee,
    CONFIG.vault.managementFee
  );
  
  // Price Oracle
  const PriceOracle = await ethers.getContractFactory("PriceOracle");
  const priceOracle = await deploy(
    "PriceOracle",
    PriceOracle,
    [deployer],
    CONFIG.oracle.priceTTL
  );
  
  return { stakingPool, liquidityPool, vault, priceOracle };
}

async function deployMarketplace(deployer, feeRecipient) {
  console.log("\n🛒 Deploying Marketplace Contracts...");
  
  // NFT Marketplace
  const NFTMarketplace = await ethers.getContractFactory("NFTMarketplace");
  const marketplace = await deploy(
    "NFTMarketplace",
    NFTMarketplace,
    CONFIG.marketplace.fee,
    feeRecipient
  );
  
  // Escrow
  const Escrow = await ethers.getContractFactory("Escrow");
  const escrow = await deploy(
    "Escrow",
    Escrow,
    CONFIG.escrow.disputePeriod,
    feeRecipient,
    CONFIG.escrow.fee
  );
  
  return { marketplace, escrow };
}

async function deploySecurity(deployer, guardians) {
  console.log("\n🔒 Deploying Security Contracts...");
  
  // Circuit Breaker
  const CircuitBreaker = await ethers.getContractFactory("CircuitBreaker");
  const circuitBreaker = await deploy(
    "CircuitBreaker",
    CircuitBreaker,
    CONFIG.security.circuitBreaker.cooldown,
    CONFIG.security.circuitBreaker.threshold,
    CONFIG.security.circuitBreaker.lockDuration
  );
  
  // Rate Limiter
  const RateLimiter = await ethers.getContractFactory("RateLimiter");
  const rateLimiter = await deploy(
    "RateLimiter",
    RateLimiter,
    CONFIG.security.rateLimiter.callLimit,
    CONFIG.security.rateLimiter.window,
    CONFIG.security.rateLimiter.volumeLimit
  );
  
  // MultiSig Wallet
  const MultiSigWallet = await ethers.getContractFactory("MultiSigWallet");
  const multiSig = await deploy(
    "MultiSigWallet",
    MultiSigWallet,
    guardians,
    CONFIG.security.multiSig.required
  );
  
  // Emergency Withdraw
  const EmergencyWithdraw = await ethers.getContractFactory("EmergencyWithdraw");
  const emergency = await deploy(
    "EmergencyWithdraw",
    EmergencyWithdraw,
    CONFIG.security.emergency.delay
  );
  
  // Grant guardian role to circuit breaker
  for (const guardian of guardians) {
    await circuitBreaker.grantRole(await circuitBreaker.GUARDIAN_ROLE(), guardian);
    await emergency.grantRole(await emergency.GUARDIAN_ROLE(), guardian);
  }
  console.log("  ✓ Guardian roles configured");
  
  return { circuitBreaker, rateLimiter, multiSig, emergency };
}

async function deployCrossChain(deployer, validators, relayers) {
  console.log("\n🌉 Deploying Cross-Chain Contracts...");
  
  const chainId = (await ethers.provider.getNetwork()).chainId;
  
  // Cross-Chain Bridge
  const CrossChainBridge = await ethers.getContractFactory("CrossChainBridge");
  const bridge = await deploy(
    "CrossChainBridge",
    CrossChainBridge,
    chainId,
    validators,
    CONFIG.bridge.validatorThreshold,
    CONFIG.bridge.fee,
    CONFIG.bridge.maxTransfer
  );
  
  // Message Router
  const MessageRouter = await ethers.getContractFactory("MessageRouter");
  const router = await deploy(
    "MessageRouter",
    MessageRouter,
    relayers,
    300000,  // gas limit
    ethers.parseEther("0.0001")
  );
  
  return { bridge, router };
}

async function deployOracles(deployer, reporters) {
  console.log("\n📊 Deploying Oracle Contracts...");
  
  // Median Oracle
  const MedianOracle = await ethers.getContractFactory("MedianOracle");
  const medianOracle = await deploy(
    "MedianOracle",
    MedianOracle,
    reporters,
    CONFIG.oracle.minReporters,
    CONFIG.oracle.priceTTL,
    "ETH/USD"
  );
  
  return { medianOracle };
}

async function deployFactories(deployer) {
  console.log("\n🏭 Deploying Factory Contracts...");
  
  // Token Factory
  const TokenFactory = await ethers.getContractFactory("TokenFactory");
  const tokenFactory = await deploy(
    "TokenFactory",
    TokenFactory,
    deployer,
    ethers.parseEther("0.01")  // deployment fee
  );
  
  return { tokenFactory };
}

async function deployUpgradeable(deployer) {
  console.log("\n⬆️  Deploying Upgradeable Contracts...");
  
  // Transparent Registry (UUPS)
  const TransparentRegistry = await ethers.getContractFactory("TransparentRegistry");
  const registry = await deployProxy(
    "TransparentRegistry",
    TransparentRegistry,
    [deployer]
  );
  
  return { registry };
}

async function deployAccessControl(deployer) {
  console.log("\n🔐 Deploying Access Control...");
  
  // Access Control Manager
  const AccessControlManager = await ethers.getContractFactory("AccessControlManager");
  const accessControl = await deploy(
    "AccessControlManager",
    AccessControlManager,
    deployer
  );
  
  // Access Control Registry (security)
  const AccessControlRegistry = await ethers.getContractFactory("AccessControlRegistry");
  const accessRegistry = await deploy(
    "AccessControlRegistry",
    AccessControlRegistry,
    deployer
  );
  
  return { accessControl, accessRegistry };
}

// ============================================
// MAIN DEPLOYMENT
// ============================================

async function main() {
  console.log("═══════════════════════════════════════════════════════════════");
  console.log("                GREY SOLIDITY ECOSYSTEM DEPLOYMENT              ");
  console.log("═══════════════════════════════════════════════════════════════");
  
  const [deployer, guardian1, guardian2, guardian3, validator1, validator2, feeRecipient] = 
    await ethers.getSigners();
  
  const network = await ethers.provider.getNetwork();
  
  deployments.network = network.name || `chain-${network.chainId}`;
  deployments.timestamp = new Date().toISOString().replace(/[:.]/g, "-");
  deployments.deployer = deployer.address;
  
  console.log(`\n📍 Network: ${deployments.network}`);
  console.log(`👤 Deployer: ${deployer.address}`);
  console.log(`💰 Balance: ${ethers.formatEther(await ethers.provider.getBalance(deployer.address))} ETH`);
  
  const guardians = [deployer.address, guardian1?.address, guardian2?.address].filter(Boolean);
  const validators = [deployer.address, validator1?.address, validator2?.address].filter(Boolean);
  const reporters = validators;
  const relayers = [deployer.address];
  const feeAddress = feeRecipient?.address || deployer.address;
  
  try {
    // Deploy all modules
    const { greyToken, greyNFT, greyMultiToken, rewardToken } = 
      await deployTokens(deployer.address, feeAddress);
    
    const { timelock, governor, treasury } = 
      await deployGovernance(deployer.address, greyToken);
    
    const { stakingPool, liquidityPool, vault, priceOracle } = 
      await deployDeFi(deployer.address, greyToken, rewardToken, feeAddress);
    
    const { marketplace, escrow } = 
      await deployMarketplace(deployer.address, feeAddress);
    
    const { circuitBreaker, rateLimiter, multiSig, emergency } = 
      await deploySecurity(deployer.address, guardians);
    
    const { bridge, router } = 
      await deployCrossChain(deployer.address, validators, relayers);
    
    const { medianOracle } = 
      await deployOracles(deployer.address, reporters);
    
    const { tokenFactory } = 
      await deployFactories(deployer.address);
    
    const { registry } = 
      await deployUpgradeable(deployer.address);
    
    const { accessControl, accessRegistry } = 
      await deployAccessControl(deployer.address);
    
    // Save deployments
    saveDeployments();
    
    console.log("\n═══════════════════════════════════════════════════════════════");
    console.log("                    DEPLOYMENT COMPLETE                          ");
    console.log("═══════════════════════════════════════════════════════════════");
    console.log(`\n✅ Deployed ${Object.keys(deployments.contracts).length} contracts`);
    console.log("\n📋 Contract Summary:");
    
    for (const [name, info] of Object.entries(deployments.contracts)) {
      console.log(`   ${name}: ${info.address}`);
    }
    
    console.log("\n🔑 Key Addresses:");
    console.log(`   Token:       ${await greyToken.getAddress()}`);
    console.log(`   Governor:    ${await governor.getAddress()}`);
    console.log(`   Treasury:    ${await treasury.getAddress()}`);
    console.log(`   Staking:     ${await stakingPool.getAddress()}`);
    console.log(`   Marketplace: ${await marketplace.getAddress()}`);
    console.log(`   Bridge:      ${await bridge.getAddress()}`);
    
  } catch (error) {
    console.error("\n❌ Deployment failed:", error);
    process.exit(1);
  }
}

// Export for programmatic use
module.exports = { main, CONFIG };

// Run if called directly
if (require.main === module) {
  main()
    .then(() => process.exit(0))
    .catch((error) => {
      console.error(error);
      process.exit(1);
    });
}
