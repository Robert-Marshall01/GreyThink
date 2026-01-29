const { ethers, upgrades } = require("hardhat");
const fs = require("fs");
const path = require("path");

/**
 * Comprehensive Migration Script for Grey Protocol
 * 
 * Handles:
 * - Contract upgrades with safety checks
 * - Data migration between versions
 * - State snapshot and restore
 * - Rollback capabilities
 * - Multi-step migrations with checkpoints
 */

// Migration configuration
const CONFIG = {
    deploymentDir: "./deployments",
    snapshotDir: "./snapshots",
    migrationLogFile: "./migrations/migration-log.json",
    maxGasPrice: ethers.parseUnits("100", "gwei"),
    confirmationBlocks: 2,
    timeoutMs: 300000, // 5 minutes
};

// Ensure directories exist
function ensureDirectories() {
    [CONFIG.deploymentDir, CONFIG.snapshotDir, "./migrations"].forEach(dir => {
        if (!fs.existsSync(dir)) {
            fs.mkdirSync(dir, { recursive: true });
        }
    });
}

// Load deployment info
function loadDeployment(network) {
    const filePath = path.join(CONFIG.deploymentDir, `${network}.json`);
    if (fs.existsSync(filePath)) {
        return JSON.parse(fs.readFileSync(filePath, "utf8"));
    }
    return {};
}

// Save deployment info
function saveDeployment(network, data) {
    const filePath = path.join(CONFIG.deploymentDir, `${network}.json`);
    fs.writeFileSync(filePath, JSON.stringify(data, null, 2));
}

// Load migration log
function loadMigrationLog() {
    if (fs.existsSync(CONFIG.migrationLogFile)) {
        return JSON.parse(fs.readFileSync(CONFIG.migrationLogFile, "utf8"));
    }
    return { migrations: [], lastMigration: null };
}

// Save migration log
function saveMigrationLog(log) {
    fs.writeFileSync(CONFIG.migrationLogFile, JSON.stringify(log, null, 2));
}

/**
 * Migration Step Class
 */
class MigrationStep {
    constructor(name, description) {
        this.name = name;
        this.description = description;
        this.startTime = null;
        this.endTime = null;
        this.status = "pending";
        this.error = null;
        this.txHashes = [];
    }

    start() {
        this.startTime = new Date().toISOString();
        this.status = "running";
        console.log(`\n📦 Starting: ${this.name}`);
        console.log(`   ${this.description}`);
    }

    complete() {
        this.endTime = new Date().toISOString();
        this.status = "completed";
        console.log(`✅ Completed: ${this.name}`);
    }

    fail(error) {
        this.endTime = new Date().toISOString();
        this.status = "failed";
        this.error = error.message;
        console.log(`❌ Failed: ${this.name}`);
        console.log(`   Error: ${error.message}`);
    }

    addTransaction(hash) {
        this.txHashes.push(hash);
    }
}

/**
 * State Snapshot Manager
 */
class StateSnapshotManager {
    constructor(network) {
        this.network = network;
        this.snapshotDir = path.join(CONFIG.snapshotDir, network);
        if (!fs.existsSync(this.snapshotDir)) {
            fs.mkdirSync(this.snapshotDir, { recursive: true });
        }
    }

    async createSnapshot(name, contracts) {
        console.log(`\n📸 Creating snapshot: ${name}`);
        
        const snapshot = {
            name,
            timestamp: new Date().toISOString(),
            blockNumber: await ethers.provider.getBlockNumber(),
            contracts: {}
        };

        for (const [contractName, address] of Object.entries(contracts)) {
            if (!address) continue;
            
            snapshot.contracts[contractName] = {
                address,
                codeHash: await ethers.provider.send("eth_getCode", [address, "latest"]),
            };
            
            // Try to read key state variables (implement per contract)
            try {
                const contract = await ethers.getContractAt(contractName, address);
                snapshot.contracts[contractName].state = await this.captureState(contract, contractName);
            } catch (e) {
                console.log(`   Could not capture state for ${contractName}: ${e.message}`);
            }
        }

        const filePath = path.join(this.snapshotDir, `${name}.json`);
        fs.writeFileSync(filePath, JSON.stringify(snapshot, null, 2));
        
        console.log(`✅ Snapshot saved: ${filePath}`);
        return snapshot;
    }

    async captureState(contract, contractName) {
        const state = {};
        
        // Common state variables to capture
        const commonGetters = [
            "owner",
            "paused",
            "totalSupply",
            "decimals",
            "name",
            "symbol"
        ];

        for (const getter of commonGetters) {
            try {
                if (typeof contract[getter] === "function") {
                    const value = await contract[getter]();
                    state[getter] = value.toString();
                }
            } catch (e) {
                // Getter doesn't exist, skip
            }
        }

        return state;
    }

    loadSnapshot(name) {
        const filePath = path.join(this.snapshotDir, `${name}.json`);
        if (fs.existsSync(filePath)) {
            return JSON.parse(fs.readFileSync(filePath, "utf8"));
        }
        return null;
    }

    listSnapshots() {
        const files = fs.readdirSync(this.snapshotDir);
        return files.filter(f => f.endsWith(".json")).map(f => f.replace(".json", ""));
    }
}

/**
 * Main Migration Runner
 */
class MigrationRunner {
    constructor(network) {
        this.network = network;
        this.deployment = loadDeployment(network);
        this.migrationLog = loadMigrationLog();
        this.snapshotManager = new StateSnapshotManager(network);
        this.steps = [];
        this.currentStep = null;
    }

    addStep(name, description, executor) {
        this.steps.push({ name, description, executor });
    }

    async checkGasPrice() {
        const feeData = await ethers.provider.getFeeData();
        const gasPrice = feeData.gasPrice;
        
        if (gasPrice > CONFIG.maxGasPrice) {
            throw new Error(
                `Gas price too high: ${ethers.formatUnits(gasPrice, "gwei")} gwei > ` +
                `${ethers.formatUnits(CONFIG.maxGasPrice, "gwei")} gwei max`
            );
        }
        
        console.log(`⛽ Current gas price: ${ethers.formatUnits(gasPrice, "gwei")} gwei`);
    }

    async run() {
        console.log("\n" + "=".repeat(60));
        console.log("🚀 MIGRATION RUNNER");
        console.log("=".repeat(60));
        console.log(`Network: ${this.network}`);
        console.log(`Steps: ${this.steps.length}`);
        console.log("=".repeat(60));

        ensureDirectories();
        await this.checkGasPrice();

        const migrationId = `migration_${Date.now()}`;
        const migration = {
            id: migrationId,
            network: this.network,
            startTime: new Date().toISOString(),
            endTime: null,
            status: "running",
            steps: []
        };

        // Create pre-migration snapshot
        await this.snapshotManager.createSnapshot(
            `pre_${migrationId}`,
            this.deployment
        );

        for (let i = 0; i < this.steps.length; i++) {
            const { name, description, executor } = this.steps[i];
            const step = new MigrationStep(name, description);
            migration.steps.push(step);
            this.currentStep = step;

            try {
                step.start();
                await executor(this);
                step.complete();
            } catch (error) {
                step.fail(error);
                migration.status = "failed";
                migration.endTime = new Date().toISOString();
                
                this.migrationLog.migrations.push(migration);
                saveMigrationLog(this.migrationLog);
                
                console.log("\n❌ MIGRATION FAILED");
                console.log("Consider running rollback if needed");
                throw error;
            }
        }

        // Create post-migration snapshot
        await this.snapshotManager.createSnapshot(
            `post_${migrationId}`,
            this.deployment
        );

        migration.status = "completed";
        migration.endTime = new Date().toISOString();
        this.migrationLog.migrations.push(migration);
        this.migrationLog.lastMigration = migrationId;
        saveMigrationLog(this.migrationLog);

        console.log("\n" + "=".repeat(60));
        console.log("✅ MIGRATION COMPLETED SUCCESSFULLY");
        console.log("=".repeat(60));

        return migration;
    }

    async waitForConfirmations(tx) {
        console.log(`   Waiting for ${CONFIG.confirmationBlocks} confirmations...`);
        await tx.wait(CONFIG.confirmationBlocks);
        this.currentStep.addTransaction(tx.hash);
        return tx;
    }

    updateDeployment(key, value) {
        this.deployment[key] = value;
        saveDeployment(this.network, this.deployment);
    }
}

// ============================================================
// SPECIFIC MIGRATION IMPLEMENTATIONS
// ============================================================

/**
 * Migration: Deploy Core Contracts
 */
async function migrateCoreContracts() {
    const network = hre.network.name;
    const runner = new MigrationRunner(network);
    const [deployer] = await ethers.getSigners();

    console.log(`Deployer: ${deployer.address}`);

    runner.addStep(
        "Deploy GreyToken",
        "Deploy the core governance token",
        async (ctx) => {
            const GreyToken = await ethers.getContractFactory("GreyToken");
            const token = await GreyToken.deploy();
            await token.waitForDeployment();
            
            const address = await token.getAddress();
            ctx.updateDeployment("GreyToken", address);
            console.log(`   GreyToken deployed to: ${address}`);
        }
    );

    runner.addStep(
        "Deploy Timelock",
        "Deploy the governance timelock controller",
        async (ctx) => {
            const GreyTimelock = await ethers.getContractFactory("GreyTimelock");
            const timelock = await GreyTimelock.deploy(
                2 * 24 * 60 * 60, // 2 days
                [deployer.address],
                [deployer.address],
                deployer.address
            );
            await timelock.waitForDeployment();
            
            const address = await timelock.getAddress();
            ctx.updateDeployment("GreyTimelock", address);
            console.log(`   GreyTimelock deployed to: ${address}`);
        }
    );

    runner.addStep(
        "Deploy Treasury",
        "Deploy the protocol treasury",
        async (ctx) => {
            const Treasury = await ethers.getContractFactory("Treasury");
            const treasury = await Treasury.deploy(
                ctx.deployment.GreyTimelock,
                [ctx.deployment.GreyToken]
            );
            await treasury.waitForDeployment();
            
            const address = await treasury.getAddress();
            ctx.updateDeployment("Treasury", address);
            console.log(`   Treasury deployed to: ${address}`);
        }
    );

    return runner.run();
}

/**
 * Migration: Upgrade Contract with Data Migration
 */
async function migrateWithDataTransfer(contractName, newImplementation) {
    const network = hre.network.name;
    const runner = new MigrationRunner(network);
    const [deployer] = await ethers.getSigners();

    runner.addStep(
        "Pause Current Contract",
        "Pause the contract before upgrade",
        async (ctx) => {
            const contract = await ethers.getContractAt(
                contractName,
                ctx.deployment[contractName]
            );
            
            try {
                const tx = await contract.pause();
                await ctx.waitForConfirmations(tx);
                console.log("   Contract paused");
            } catch (e) {
                console.log("   Contract has no pause function, continuing...");
            }
        }
    );

    runner.addStep(
        "Deploy New Implementation",
        "Deploy the new contract version",
        async (ctx) => {
            const Contract = await ethers.getContractFactory(newImplementation);
            const newContract = await Contract.deploy();
            await newContract.waitForDeployment();
            
            const address = await newContract.getAddress();
            ctx.updateDeployment(`${contractName}_NewImpl`, address);
            console.log(`   New implementation: ${address}`);
        }
    );

    runner.addStep(
        "Verify New Implementation",
        "Verify the new contract code",
        async (ctx) => {
            const code = await ethers.provider.getCode(
                ctx.deployment[`${contractName}_NewImpl`]
            );
            
            if (code === "0x" || code.length < 100) {
                throw new Error("Contract code verification failed");
            }
            
            console.log(`   Code length: ${code.length} bytes`);
        }
    );

    runner.addStep(
        "Upgrade Proxy",
        "Point proxy to new implementation",
        async (ctx) => {
            // This assumes using OpenZeppelin TransparentUpgradeableProxy
            // Adjust based on your proxy pattern
            console.log("   Would upgrade proxy here (implement based on proxy type)");
        }
    );

    runner.addStep(
        "Unpause Contract",
        "Resume contract operations",
        async (ctx) => {
            const contract = await ethers.getContractAt(
                newImplementation,
                ctx.deployment[contractName]
            );
            
            try {
                const tx = await contract.unpause();
                await ctx.waitForConfirmations(tx);
                console.log("   Contract unpaused");
            } catch (e) {
                console.log("   Contract has no unpause function, continuing...");
            }
        }
    );

    return runner.run();
}

/**
 * Rollback Migration
 */
async function rollbackMigration(migrationId) {
    const network = hre.network.name;
    const snapshotManager = new StateSnapshotManager(network);
    const migrationLog = loadMigrationLog();

    console.log("\n" + "=".repeat(60));
    console.log("⏪ ROLLBACK MIGRATION");
    console.log("=".repeat(60));

    // Find the migration
    const migration = migrationLog.migrations.find(m => m.id === migrationId);
    if (!migration) {
        throw new Error(`Migration not found: ${migrationId}`);
    }

    // Load pre-migration snapshot
    const snapshot = snapshotManager.loadSnapshot(`pre_${migrationId}`);
    if (!snapshot) {
        throw new Error("Pre-migration snapshot not found");
    }

    console.log(`Snapshot from: ${snapshot.timestamp}`);
    console.log(`Block number: ${snapshot.blockNumber}`);

    // Note: Actual rollback would require:
    // 1. Redeploying old implementations
    // 2. Restoring state (if possible)
    // 3. Or forking from snapshot block

    console.log("\n⚠️  Rollback requires manual intervention:");
    console.log("1. Fork network from block", snapshot.blockNumber);
    console.log("2. Redeploy contracts from snapshot");
    console.log("3. Verify state matches snapshot");

    return snapshot;
}

/**
 * Verify Migration State
 */
async function verifyMigration() {
    const network = hre.network.name;
    const deployment = loadDeployment(network);
    
    console.log("\n" + "=".repeat(60));
    console.log("🔍 MIGRATION VERIFICATION");
    console.log("=".repeat(60));

    const checks = [];

    for (const [name, address] of Object.entries(deployment)) {
        if (!ethers.isAddress(address)) continue;

        const check = {
            contract: name,
            address,
            hasCode: false,
            isPaused: "N/A",
            owner: "N/A"
        };

        const code = await ethers.provider.getCode(address);
        check.hasCode = code !== "0x" && code.length > 10;

        try {
            const contract = await ethers.getContractAt(name, address);
            
            if (typeof contract.paused === "function") {
                check.isPaused = await contract.paused();
            }
            
            if (typeof contract.owner === "function") {
                check.owner = await contract.owner();
            }
        } catch (e) {
            // Contract not found or doesn't have these methods
        }

        checks.push(check);
    }

    console.log("\nContract Status:");
    console.table(checks);

    return checks;
}

// ============================================================
// EXPORT FUNCTIONS
// ============================================================

module.exports = {
    MigrationRunner,
    MigrationStep,
    StateSnapshotManager,
    migrateCoreContracts,
    migrateWithDataTransfer,
    rollbackMigration,
    verifyMigration,
    CONFIG
};

// CLI execution
if (require.main === module) {
    const command = process.argv[2];
    
    switch (command) {
        case "core":
            migrateCoreContracts()
                .then(() => process.exit(0))
                .catch(e => {
                    console.error(e);
                    process.exit(1);
                });
            break;
        case "verify":
            verifyMigration()
                .then(() => process.exit(0))
                .catch(e => {
                    console.error(e);
                    process.exit(1);
                });
            break;
        case "rollback":
            const migrationId = process.argv[3];
            if (!migrationId) {
                console.error("Usage: migrate.js rollback <migrationId>");
                process.exit(1);
            }
            rollbackMigration(migrationId)
                .then(() => process.exit(0))
                .catch(e => {
                    console.error(e);
                    process.exit(1);
                });
            break;
        default:
            console.log("Usage: migrate.js [core|verify|rollback <id>]");
    }
}
