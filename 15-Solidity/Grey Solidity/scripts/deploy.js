const hre = require("hardhat");

async function main() {
  console.log("Starting deployment...\n");

  const [deployer] = await hre.ethers.getSigners();
  console.log("Deploying contracts with account:", deployer.address);
  console.log("Account balance:", (await hre.ethers.provider.getBalance(deployer.address)).toString());
  console.log("");

  // Deploy AccessControlManager
  console.log("1. Deploying AccessControlManager...");
  const AccessControlManager = await hre.ethers.getContractFactory("AccessControlManager");
  const accessControl = await AccessControlManager.deploy();
  await accessControl.waitForDeployment();
  const accessControlAddress = await accessControl.getAddress();
  console.log("   AccessControlManager deployed to:", accessControlAddress);

  // Deploy Registry
  console.log("\n2. Deploying Registry...");
  const Registry = await hre.ethers.getContractFactory("Registry");
  const registry = await Registry.deploy(accessControlAddress);
  await registry.waitForDeployment();
  const registryAddress = await registry.getAddress();
  console.log("   Registry deployed to:", registryAddress);

  // Deploy TokenVault
  console.log("\n3. Deploying TokenVault...");
  const TokenVault = await hre.ethers.getContractFactory("TokenVault");
  const tokenVault = await TokenVault.deploy(accessControlAddress);
  await tokenVault.waitForDeployment();
  const tokenVaultAddress = await tokenVault.getAddress();
  console.log("   TokenVault deployed to:", tokenVaultAddress);

  // Verification summary
  console.log("\n========================================");
  console.log("Deployment Summary");
  console.log("========================================");
  console.log("AccessControlManager:", accessControlAddress);
  console.log("Registry:            ", registryAddress);
  console.log("TokenVault:          ", tokenVaultAddress);
  console.log("========================================\n");

  // Log role information
  const ADMIN_ROLE = await accessControl.ADMIN_ROLE();
  const USER_ROLE = await accessControl.USER_ROLE();
  console.log("Role Identifiers:");
  console.log("ADMIN_ROLE:", ADMIN_ROLE);
  console.log("USER_ROLE: ", USER_ROLE);
  console.log("");

  // Verify deployer has admin role
  const hasAdminRole = await accessControl.hasRole(ADMIN_ROLE, deployer.address);
  console.log("Deployer has ADMIN_ROLE:", hasAdminRole);

  return {
    accessControl: accessControlAddress,
    registry: registryAddress,
    tokenVault: tokenVaultAddress,
  };
}

main()
  .then((addresses) => {
    console.log("\nDeployment completed successfully!");
    process.exit(0);
  })
  .catch((error) => {
    console.error("Deployment failed:", error);
    process.exit(1);
  });
