const { ethers, upgrades, network } = require("hardhat");
const fs = require("fs");

/**
 * @title Grey Solidity Ecosystem Deployment Script
 * @notice Deploys the complete Grey smart contract ecosystem
 * @dev Handles dependency ordering and contract linking
 */

async function main() {
  console.log("🚀 Starting Grey Solidity Ecosystem Deployment...\n");

  const [deployer] = await ethers.getSigners();
  console.log("Deploying contracts with account:", deployer.address);
  console.log("Account balance:", ethers.formatEther(await ethers.provider.getBalance(deployer.address)), "ETH\n");

  const deployedContracts = {};
  const deploymentLog = [];

  // Helper function to log deployments
  const logDeployment = async (name, contract) => {
    const address = await contract.getAddress();
    console.log(`✅ ${name} deployed to: ${address}`);
    deploymentLog.push({ name, address });
    deployedContracts[name] = address;
    return address;
  };

  try {
    // ============================================
    // PHASE 1: Core Infrastructure
    // ============================================
    console.log("\n📦 Phase 1: Deploying Core Infrastructure\n");

    // Deploy AccessControlExtended
    const AccessControlExtended = await ethers.getContractFactory("AccessControlExtended");
    const accessControl = await AccessControlExtended.deploy();
    await accessControl.waitForDeployment();
    await logDeployment("AccessControlExtended", accessControl);

    // ============================================
    // PHASE 2: Token Contracts
    // ============================================
    console.log("\n📦 Phase 2: Deploying Token Contracts\n");

    // Deploy GreyToken (ERC20)
    const GreyToken = await ethers.getContractFactory("GreyToken");
    const greyToken = await GreyToken.deploy(
      "Grey Token",
      "GREY",
      ethers.parseEther("1000000000"), // 1 billion max supply
      deployer.address
    );
    await greyToken.waitForDeployment();
    await logDeployment("GreyToken", greyToken);

    // Deploy GreyNFT (ERC721)
    const GreyNFT = await ethers.getContractFactory("GreyNFT");
    const greyNFT = await GreyNFT.deploy(
      "Grey NFT Collection",
      "GNFT",
      "https://api.grey.io/nft/",
      deployer.address, // Royalty recipient
      500 // 5% royalty
    );
    await greyNFT.waitForDeployment();
    await logDeployment("GreyNFT", greyNFT);

    // Deploy GreyMultiToken (ERC1155)
    const GreyMultiToken = await ethers.getContractFactory("GreyMultiToken");
    const greyMultiToken = await GreyMultiToken.deploy(
      "https://api.grey.io/multi/{id}.json"
    );
    await greyMultiToken.waitForDeployment();
    await logDeployment("GreyMultiToken", greyMultiToken);

    // Deploy a mock USDC for pairing
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    const mockUSDC = await MockERC20.deploy("USD Coin", "USDC", 6);
    await mockUSDC.waitForDeployment();
    await logDeployment("MockUSDC", mockUSDC);

    // ============================================
    // PHASE 3: Governance Contracts
    // ============================================
    console.log("\n📦 Phase 3: Deploying Governance Contracts\n");

    // Deploy GreyTimelock
    const minDelay = 2 * 24 * 60 * 60; // 2 days
    const GreyTimelock = await ethers.getContractFactory("GreyTimelock");
    const timelock = await GreyTimelock.deploy(
      minDelay,
      [deployer.address], // Proposers
      [deployer.address], // Executors
      deployer.address // Admin
    );
    await timelock.waitForDeployment();
    await logDeployment("GreyTimelock", timelock);

    // Deploy GreyGovernor
    const GreyGovernor = await ethers.getContractFactory("GreyGovernor");
    const governor = await GreyGovernor.deploy(
      await greyToken.getAddress(),
      await timelock.getAddress()
    );
    await governor.waitForDeployment();
    await logDeployment("GreyGovernor", governor);

    // Deploy Treasury
    const Treasury = await ethers.getContractFactory("Treasury");
    const treasury = await Treasury.deploy();
    await treasury.waitForDeployment();
    await logDeployment("Treasury", treasury);

    // ============================================
    // PHASE 4: DeFi Contracts
    // ============================================
    console.log("\n📦 Phase 4: Deploying DeFi Contracts\n");

    // Deploy StakingPool
    const StakingPool = await ethers.getContractFactory("StakingPool");
    const stakingPool = await StakingPool.deploy(
      await greyToken.getAddress(), // Staking token
      await greyToken.getAddress(), // Reward token
      ethers.parseEther("0.1"), // 10% APR
      7 * 24 * 60 * 60, // 7 day minimum lock
      ethers.parseEther("100"), // 100 token minimum stake
      ethers.parseEther("1000000") // 1M max stake
    );
    await stakingPool.waitForDeployment();
    await logDeployment("StakingPool", stakingPool);

    // Deploy LiquidityPool (AMM)
    const LiquidityPool = await ethers.getContractFactory("LiquidityPool");
    const liquidityPool = await LiquidityPool.deploy(
      await greyToken.getAddress(),
      await mockUSDC.getAddress(),
      30, // 0.3% fee
      deployer.address // Fee recipient
    );
    await liquidityPool.waitForDeployment();
    await logDeployment("LiquidityPool", liquidityPool);

    // Deploy PriceOracle
    const PriceOracle = await ethers.getContractFactory("PriceOracle");
    const priceOracle = await PriceOracle.deploy();
    await priceOracle.waitForDeployment();
    await logDeployment("PriceOracle", priceOracle);

    // Deploy Vault
    const Vault = await ethers.getContractFactory("Vault");
    const vault = await Vault.deploy(
      await greyToken.getAddress(),
      "Grey Vault Shares",
      "gGREY",
      100, // 1% performance fee
      50 // 0.5% management fee
    );
    await vault.waitForDeployment();
    await logDeployment("Vault", vault);

    // ============================================
    // PHASE 5: Vesting Contracts
    // ============================================
    console.log("\n📦 Phase 5: Deploying Vesting Contracts\n");

    // Deploy TokenVesting
    const TokenVesting = await ethers.getContractFactory("TokenVesting");
    const tokenVesting = await TokenVesting.deploy(await greyToken.getAddress());
    await tokenVesting.waitForDeployment();
    await logDeployment("TokenVesting", tokenVesting);

    // ============================================
    // PHASE 6: Marketplace Contracts
    // ============================================
    console.log("\n📦 Phase 6: Deploying Marketplace Contracts\n");

    // Deploy NFTMarketplace
    const NFTMarketplace = await ethers.getContractFactory("NFTMarketplace");
    const nftMarketplace = await NFTMarketplace.deploy(
      250, // 2.5% platform fee
      await treasury.getAddress() // Fees go to treasury
    );
    await nftMarketplace.waitForDeployment();
    await logDeployment("NFTMarketplace", nftMarketplace);

    // Deploy Escrow
    const Escrow = await ethers.getContractFactory("Escrow");
    const escrow = await Escrow.deploy();
    await escrow.waitForDeployment();
    await logDeployment("Escrow", escrow);

    // ============================================
    // PHASE 7: Upgradeable Contracts
    // ============================================
    console.log("\n📦 Phase 7: Deploying Upgradeable Contracts\n");

    // Deploy GreyVaultV1 (UUPS Proxy)
    const GreyVaultV1 = await ethers.getContractFactory("GreyVaultV1");
    const greyVaultProxy = await upgrades.deployProxy(
      GreyVaultV1,
      [await greyToken.getAddress()],
      { kind: "uups" }
    );
    await greyVaultProxy.waitForDeployment();
    await logDeployment("GreyVaultV1 (Proxy)", greyVaultProxy);

    // Deploy TransparentRegistry
    const TransparentRegistry = await ethers.getContractFactory("TransparentRegistry");
    const transparentRegistry = await upgrades.deployProxy(
      TransparentRegistry,
      [deployer.address],
      { kind: "transparent" }
    );
    await transparentRegistry.waitForDeployment();
    await logDeployment("TransparentRegistry (Proxy)", transparentRegistry);

    // ============================================
    // PHASE 8: Post-Deployment Configuration
    // ============================================
    console.log("\n⚙️  Phase 8: Post-Deployment Configuration\n");

    // Configure NFT Marketplace
    console.log("Configuring NFT Marketplace...");
    await nftMarketplace.setCollection(await greyNFT.getAddress(), true);
    await nftMarketplace.setPaymentToken(await greyToken.getAddress(), true);
    console.log("✅ Marketplace configured with GreyNFT and GREY token\n");

    // Grant MINTER_ROLE to contracts that need it
    console.log("Configuring access control...");
    const MINTER_ROLE = await greyToken.MINTER_ROLE();
    await greyToken.grantRole(MINTER_ROLE, await stakingPool.getAddress());
    await greyToken.grantRole(MINTER_ROLE, await tokenVesting.getAddress());
    console.log("✅ MINTER_ROLE granted to StakingPool and TokenVesting\n");

    // Set up initial price feed
    console.log("Configuring Price Oracle...");
    const greyTokenAddress = await greyToken.getAddress();
    await priceOracle.updatePrice(greyTokenAddress, ethers.parseUnits("1", 8)); // $1.00
    console.log("✅ GREY price set to $1.00\n");

    // ============================================
    // DEPLOYMENT SUMMARY
    // ============================================
    console.log("\n" + "=".repeat(60));
    console.log("📋 DEPLOYMENT SUMMARY");
    console.log("=".repeat(60) + "\n");

    console.log("Network:", network.name);
    console.log("Deployer:", deployer.address);
    console.log("Timestamp:", new Date().toISOString());
    console.log("\nDeployed Contracts:\n");

    for (const entry of deploymentLog) {
      console.log(`  ${entry.name}: ${entry.address}`);
    }

    // Save deployment addresses to file
    const deploymentData = {
      network: network.name,
      chainId: network.config.chainId || 31337,
      deployer: deployer.address,
      timestamp: new Date().toISOString(),
      contracts: deployedContracts
    };

    const deploymentsDir = "./deployments";
    if (!fs.existsSync(deploymentsDir)) {
      fs.mkdirSync(deploymentsDir, { recursive: true });
    }

    const filename = `${deploymentsDir}/${network.name}-${Date.now()}.json`;
    fs.writeFileSync(filename, JSON.stringify(deploymentData, null, 2));
    console.log(`\n💾 Deployment data saved to: ${filename}`);

    console.log("\n✨ Deployment complete!");
    console.log("=".repeat(60) + "\n");

    return deployedContracts;

  } catch (error) {
    console.error("\n❌ Deployment failed:", error);
    throw error;
  }
}

// Execute deployment
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });

module.exports = { main };
