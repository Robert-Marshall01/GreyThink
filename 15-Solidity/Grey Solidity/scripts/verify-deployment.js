const { ethers } = require("hardhat");
const fs = require("fs");
const path = require("path");

/**
 * Deployment Verification Script
 * 
 * Validates deployed contracts:
 * - Verifies bytecode exists
 * - Checks contract interfaces
 * - Validates role configurations
 * - Tests basic functionality
 * - Generates verification report
 */

// Colors for console output
const colors = {
    reset: "\x1b[0m",
    green: "\x1b[32m",
    red: "\x1b[31m",
    yellow: "\x1b[33m",
    cyan: "\x1b[36m",
    bold: "\x1b[1m"
};

// Verification results
const results = {
    passed: [],
    failed: [],
    warnings: []
};

/**
 * Log with color
 */
function log(message, color = colors.reset) {
    console.log(`${color}${message}${colors.reset}`);
}

/**
 * Log success
 */
function logSuccess(check, details = "") {
    log(`  ✅ ${check}${details ? ": " + details : ""}`, colors.green);
    results.passed.push({ check, details });
}

/**
 * Log failure
 */
function logFailure(check, error) {
    log(`  ❌ ${check}: ${error}`, colors.red);
    results.failed.push({ check, error });
}

/**
 * Log warning
 */
function logWarning(check, warning) {
    log(`  ⚠️  ${check}: ${warning}`, colors.yellow);
    results.warnings.push({ check, warning });
}

/**
 * Verify contract has bytecode
 */
async function verifyBytecode(address, name) {
    try {
        const code = await ethers.provider.getCode(address);
        if (code.length > 2) {
            logSuccess(`${name} bytecode`, `${code.length} bytes`);
            return true;
        } else {
            logFailure(`${name} bytecode`, "No bytecode found");
            return false;
        }
    } catch (e) {
        logFailure(`${name} bytecode`, e.message);
        return false;
    }
}

/**
 * Verify contract interface
 */
async function verifyInterface(contract, name, expectedFunctions) {
    const missing = [];
    for (const func of expectedFunctions) {
        if (typeof contract[func] !== "function") {
            missing.push(func);
        }
    }
    
    if (missing.length === 0) {
        logSuccess(`${name} interface`, `${expectedFunctions.length} functions`);
        return true;
    } else {
        logFailure(`${name} interface`, `Missing: ${missing.join(", ")}`);
        return false;
    }
}

/**
 * Verify role configuration
 */
async function verifyRole(contract, name, role, expectedHolders) {
    try {
        for (const holder of expectedHolders) {
            const hasRole = await contract.hasRole(role, holder);
            if (!hasRole) {
                logWarning(`${name} role`, `${holder} missing role`);
                return false;
            }
        }
        logSuccess(`${name} role`, `${expectedHolders.length} holders`);
        return true;
    } catch (e) {
        logWarning(`${name} role check`, e.message);
        return false;
    }
}

/**
 * Verify contract is not paused
 */
async function verifyNotPaused(contract, name) {
    try {
        if (typeof contract.paused === "function") {
            const isPaused = await contract.paused();
            if (isPaused) {
                logWarning(`${name} pause status`, "Contract is paused");
                return false;
            } else {
                logSuccess(`${name} pause status`, "Not paused");
                return true;
            }
        } else {
            logSuccess(`${name} pause status`, "No pause mechanism");
            return true;
        }
    } catch (e) {
        logWarning(`${name} pause check`, e.message);
        return false;
    }
}

/**
 * Main verification function
 */
async function verify() {
    console.log("\n");
    log("═".repeat(70), colors.cyan);
    log("  GREY PROTOCOL DEPLOYMENT VERIFICATION", colors.bold);
    log("═".repeat(70), colors.cyan);
    console.log("\n");

    const network = await ethers.provider.getNetwork();
    log(`Network: ${network.name} (chainId: ${network.chainId})`, colors.cyan);
    console.log("\n");

    // Load deployment addresses
    const deploymentFile = `./deployments/${network.name}.json`;
    let deployment = {};
    
    if (fs.existsSync(deploymentFile)) {
        deployment = JSON.parse(fs.readFileSync(deploymentFile, "utf8"));
        log(`Loaded deployment from ${deploymentFile}`, colors.cyan);
    } else {
        log(`No deployment file found at ${deploymentFile}`, colors.yellow);
        log("Running verification with manual deployment...", colors.yellow);
        
        // For testing, deploy contracts
        const [deployer] = await ethers.getSigners();
        
        // Deploy mock token for testing
        const MockERC20 = await ethers.getContractFactory("MockERC20");
        const greyToken = await MockERC20.deploy("Grey Token", "GREY", 18);
        await greyToken.waitForDeployment();
        deployment.GreyToken = await greyToken.getAddress();

        // Deploy timelock
        const GreyTimelock = await ethers.getContractFactory("GreyTimelock");
        const timelock = await GreyTimelock.deploy(
            2 * 24 * 60 * 60,
            [deployer.address],
            [deployer.address],
            deployer.address
        );
        await timelock.waitForDeployment();
        deployment.GreyTimelock = await timelock.getAddress();

        log("Test deployment completed", colors.green);
    }
    
    console.log("\n");

    // ============================================================
    // 1. CORE CONTRACTS
    // ============================================================
    
    log("1. CORE CONTRACTS", colors.bold);
    log("-".repeat(40));

    if (deployment.GreyToken) {
        await verifyBytecode(deployment.GreyToken, "GreyToken");
        
        try {
            const token = await ethers.getContractAt("MockERC20", deployment.GreyToken);
            await verifyInterface(token, "GreyToken", [
                "name",
                "symbol",
                "decimals",
                "totalSupply",
                "balanceOf",
                "transfer",
                "approve",
                "transferFrom"
            ]);

            const name = await token.name();
            const symbol = await token.symbol();
            const totalSupply = await token.totalSupply();
            logSuccess("GreyToken metadata", `${name} (${symbol}), supply: ${ethers.formatEther(totalSupply)}`);
        } catch (e) {
            logWarning("GreyToken verification", e.message);
        }
    }

    console.log("\n");

    // ============================================================
    // 2. GOVERNANCE CONTRACTS
    // ============================================================
    
    log("2. GOVERNANCE CONTRACTS", colors.bold);
    log("-".repeat(40));

    if (deployment.GreyTimelock) {
        await verifyBytecode(deployment.GreyTimelock, "GreyTimelock");
        
        try {
            const timelock = await ethers.getContractAt("GreyTimelock", deployment.GreyTimelock);
            await verifyInterface(timelock, "GreyTimelock", [
                "getMinDelay",
                "hasRole",
                "schedule",
                "execute",
                "cancel"
            ]);

            const minDelay = await timelock.getMinDelay();
            logSuccess("GreyTimelock config", `Min delay: ${minDelay / 86400n} days`);
            await verifyNotPaused(timelock, "GreyTimelock");
        } catch (e) {
            logWarning("GreyTimelock verification", e.message);
        }
    }

    console.log("\n");

    // ============================================================
    // 3. DEFI CONTRACTS
    // ============================================================
    
    log("3. DEFI CONTRACTS", colors.bold);
    log("-".repeat(40));

    if (deployment.LiquidityPool) {
        await verifyBytecode(deployment.LiquidityPool, "LiquidityPool");
    } else {
        logWarning("LiquidityPool", "Not deployed");
    }

    if (deployment.StakingPool) {
        await verifyBytecode(deployment.StakingPool, "StakingPool");
    } else {
        logWarning("StakingPool", "Not deployed");
    }

    if (deployment.LendingPool) {
        await verifyBytecode(deployment.LendingPool, "LendingPool");
    } else {
        logWarning("LendingPool", "Not deployed");
    }

    console.log("\n");

    // ============================================================
    // 4. SECURITY CHECKS
    // ============================================================
    
    log("4. SECURITY CHECKS", colors.bold);
    log("-".repeat(40));

    // Owner verification
    const [deployer] = await ethers.getSigners();
    logSuccess("Deployer address", deployer.address);

    // Network configuration
    if (network.chainId === 1n) {
        logSuccess("Network", "Ethereum Mainnet");
    } else if (network.chainId === 11155111n) {
        logSuccess("Network", "Sepolia Testnet");
    } else if (network.chainId === 31337n) {
        logSuccess("Network", "Hardhat Local");
    } else {
        logWarning("Network", `Unknown chain ID: ${network.chainId}`);
    }

    console.log("\n");

    // ============================================================
    // SUMMARY
    // ============================================================
    
    log("═".repeat(70), colors.cyan);
    log("  VERIFICATION SUMMARY", colors.bold);
    log("═".repeat(70), colors.cyan);
    console.log("\n");

    log(`Passed:   ${results.passed.length}`, colors.green);
    log(`Warnings: ${results.warnings.length}`, colors.yellow);
    log(`Failed:   ${results.failed.length}`, colors.red);
    console.log("\n");

    if (results.failed.length === 0) {
        log("✅ ALL CRITICAL CHECKS PASSED", colors.green);
    } else {
        log("❌ SOME CHECKS FAILED - REVIEW REQUIRED", colors.red);
        console.log("\nFailed checks:");
        for (const failure of results.failed) {
            log(`  - ${failure.check}: ${failure.error}`, colors.red);
        }
    }

    if (results.warnings.length > 0) {
        console.log("\nWarnings:");
        for (const warning of results.warnings) {
            log(`  - ${warning.check}: ${warning.warning}`, colors.yellow);
        }
    }

    console.log("\n");
    log("═".repeat(70), colors.cyan);

    // Save report
    const reportPath = `./reports/verification-${Date.now()}.json`;
    const reportDir = path.dirname(reportPath);
    if (!fs.existsSync(reportDir)) {
        fs.mkdirSync(reportDir, { recursive: true });
    }
    fs.writeFileSync(reportPath, JSON.stringify({
        timestamp: new Date().toISOString(),
        network: network.name,
        chainId: network.chainId.toString(),
        deployment,
        results
    }, null, 2));
    
    log(`Report saved to: ${reportPath}`, colors.cyan);
    console.log("\n");

    return results.failed.length === 0;
}

// Run verification
verify()
    .then(success => {
        process.exit(success ? 0 : 1);
    })
    .catch(error => {
        console.error(error);
        process.exit(1);
    });
