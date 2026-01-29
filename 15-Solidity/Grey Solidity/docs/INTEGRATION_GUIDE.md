# Grey Protocol Developer Integration Guide

## Table of Contents

1. [Getting Started](#getting-started)
2. [Installation](#installation)
3. [Core Concepts](#core-concepts)
4. [Contract Integration](#contract-integration)
5. [DeFi Integration](#defi-integration)
6. [Governance Integration](#governance-integration)
7. [Cross-Chain Integration](#cross-chain-integration)
8. [Security Best Practices](#security-best-practices)
9. [Testing Guide](#testing-guide)
10. [Deployment Guide](#deployment-guide)
11. [Troubleshooting](#troubleshooting)

---

## Getting Started

### Prerequisites

- Node.js >= 18.0.0
- npm >= 9.0.0 or yarn >= 1.22.0
- Git
- Hardhat knowledge
- Solidity >= 0.8.28

### Quick Start

```bash
# Clone the repository
git clone https://github.com/grey-protocol/grey-solidity.git
cd grey-solidity

# Install dependencies
npm install

# Compile contracts
npx hardhat compile

# Run tests
npx hardhat test

# Deploy to local network
npx hardhat node
npx hardhat run scripts/deploy.js --network localhost
```

---

## Installation

### As a Dependency

```bash
npm install @grey-protocol/contracts
```

### Import in Solidity

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@grey-protocol/contracts/defi/LiquidityPool.sol";
import "@grey-protocol/contracts/governance/GreyGovernor.sol";
import "@grey-protocol/contracts/security/CircuitBreaker.sol";
```

### Project Structure

```
grey-solidity/
├── contracts/
│   ├── access/           # Access control extensions
│   ├── consensus/        # Validator & BFT consensus
│   ├── crosschain/       # Bridge & message routing
│   ├── cryptography/     # ZK proofs, BLS, Merkle trees
│   ├── defi/            # AMMs, lending, vaults
│   ├── factories/       # Contract factories
│   ├── finance/         # Token vesting
│   ├── governance/      # DAO & voting
│   ├── identity/        # DID & reputation
│   ├── interfaces/      # All interfaces
│   ├── libraries/       # Shared libraries
│   ├── marketplace/     # NFT marketplace
│   ├── mocks/          # Test mocks
│   ├── oracles/        # Price feeds
│   ├── security/       # Guards & protection
│   ├── tokenomics/     # Token economics
│   ├── tokens/         # Token implementations
│   └── upgradeable/    # Proxy patterns
├── scripts/             # Deployment scripts
├── test/               # Test suite
└── docs/               # Documentation
```

---

## Core Concepts

### Role-Based Access Control

Grey Protocol uses OpenZeppelin's AccessControl for fine-grained permissions:

```solidity
// Common roles
bytes32 public constant ADMIN_ROLE = keccak256("ADMIN_ROLE");
bytes32 public constant OPERATOR_ROLE = keccak256("OPERATOR_ROLE");
bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");

// Grant role
accessControl.grantRole(OPERATOR_ROLE, operatorAddress);

// Check role
require(hasRole(OPERATOR_ROLE, msg.sender), "Not operator");
```

### Pausable Contracts

Most contracts support emergency pause:

```solidity
import "@grey-protocol/contracts/security/CircuitBreaker.sol";

contract MyContract is CircuitBreaker {
    function sensitiveOperation() external whenNotPaused {
        // Protected logic
    }
}
```

### Reentrancy Protection

All state-changing external functions use ReentrancyGuard:

```solidity
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

contract MyContract is ReentrancyGuard {
    function withdraw() external nonReentrant {
        // Safe from reentrancy
    }
}
```

---

## Contract Integration

### Integrating with LiquidityPool

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@grey-protocol/contracts/interfaces/ILiquidityPool.sol";

contract MyDApp {
    ILiquidityPool public pool;

    constructor(address _pool) {
        pool = ILiquidityPool(_pool);
    }

    function addLiq(uint256 amount0, uint256 amount1) external {
        // Approve tokens first
        IERC20(pool.token0()).approve(address(pool), amount0);
        IERC20(pool.token1()).approve(address(pool), amount1);

        // Add liquidity
        pool.addLiquidity(amount0, amount1, msg.sender);
    }

    function swap(address tokenIn, uint256 amountIn, uint256 minOut) external {
        IERC20(tokenIn).approve(address(pool), amountIn);
        pool.swap(tokenIn, amountIn, minOut, msg.sender);
    }
}
```

### Integrating with ConstantProductAMM

```solidity
import "@grey-protocol/contracts/defi/ConstantProductAMM.sol";

contract AMMIntegration {
    ConstantProductAMM public amm;

    function getQuote(
        address tokenIn,
        uint256 amountIn
    ) external view returns (uint256 amountOut) {
        (uint112 reserve0, uint112 reserve1,) = amm.getReserves();

        bool isToken0 = tokenIn == amm.token0();
        (uint256 reserveIn, uint256 reserveOut) = isToken0 
            ? (uint256(reserve0), uint256(reserve1))
            : (uint256(reserve1), uint256(reserve0));

        amountOut = amm.getAmountOut(amountIn, reserveIn, reserveOut);
    }

    function swapExact(
        address tokenIn,
        uint256 amountIn,
        uint256 minAmountOut
    ) external returns (uint256) {
        IERC20(tokenIn).transferFrom(msg.sender, address(this), amountIn);
        IERC20(tokenIn).approve(address(amm), amountIn);

        return amm.swapExactIn(tokenIn, amountIn, minAmountOut, msg.sender);
    }
}
```

### Integrating with Vault

```solidity
import "@grey-protocol/contracts/defi/Vault.sol";

contract VaultIntegration {
    Vault public vault;

    function deposit(uint256 assets) external returns (uint256 shares) {
        IERC20(vault.asset()).transferFrom(msg.sender, address(this), assets);
        IERC20(vault.asset()).approve(address(vault), assets);

        shares = vault.deposit(assets, msg.sender);
    }

    function withdraw(uint256 shares) external returns (uint256 assets) {
        // User must have approved this contract for vault shares
        assets = vault.redeem(shares, msg.sender, msg.sender);
    }
}
```

---

## DeFi Integration

### Flash Loan Usage

```solidity
import "@grey-protocol/contracts/interfaces/IFlashLoanReceiver.sol";
import "@grey-protocol/contracts/defi/FlashLoanProvider.sol";

contract FlashLoanArbitrage is IFlashLoanReceiver {
    FlashLoanProvider public provider;

    function executeArbitrage(
        address token,
        uint256 amount
    ) external {
        bytes memory data = abi.encode(msg.sender);
        provider.flashLoan(token, amount, data);
    }

    function onFlashLoan(
        address initiator,
        address token,
        uint256 amount,
        uint256 fee,
        bytes calldata data
    ) external override returns (bytes32) {
        // 1. Perform arbitrage
        // 2. Ensure you have amount + fee to repay

        // Approve repayment
        IERC20(token).approve(msg.sender, amount + fee);

        return keccak256("FlashLoanReceiver.onFlashLoan");
    }
}
```

### Lending Integration

```solidity
import "@grey-protocol/contracts/defi/LendingPool.sol";

contract LendingIntegration {
    LendingPool public lendingPool;

    function supplyCollateral(
        address asset,
        uint256 amount
    ) external {
        IERC20(asset).transferFrom(msg.sender, address(this), amount);
        IERC20(asset).approve(address(lendingPool), amount);

        lendingPool.deposit(asset, amount, msg.sender);
    }

    function borrow(
        address asset,
        uint256 amount
    ) external {
        lendingPool.borrow(asset, amount, msg.sender);
    }

    function repay(
        address asset,
        uint256 amount
    ) external {
        IERC20(asset).transferFrom(msg.sender, address(this), amount);
        IERC20(asset).approve(address(lendingPool), amount);

        lendingPool.repay(asset, amount, msg.sender);
    }
}
```

### Oracle Integration

```solidity
import "@grey-protocol/contracts/oracles/ChainlinkAdapter.sol";
import "@grey-protocol/contracts/oracles/TWAPOracle.sol";

contract PriceConsumer {
    ChainlinkAdapter public chainlink;
    TWAPOracle public twap;

    function getChainlinkPrice(
        address token
    ) external view returns (uint256) {
        return chainlink.getPrice(token);
    }

    function getTWAPPrice(
        address pair,
        uint32 period
    ) external view returns (uint256) {
        return twap.consult(pair, period);
    }

    function getMedianPrice(address token) external view returns (uint256) {
        uint256 chainlinkPrice = chainlink.getPrice(token);
        uint256 twapPrice = twap.consult(token, 30 minutes);

        // Use median of both
        return (chainlinkPrice + twapPrice) / 2;
    }
}
```

---

## Governance Integration

### Creating Proposals

```solidity
import "@grey-protocol/contracts/governance/GreyGovernor.sol";

contract GovernanceIntegration {
    GreyGovernor public governor;

    function createProposal(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        string memory description
    ) external returns (uint256 proposalId) {
        proposalId = governor.propose(
            targets,
            values,
            calldatas,
            description
        );
    }

    function vote(
        uint256 proposalId,
        uint8 support // 0=Against, 1=For, 2=Abstain
    ) external {
        governor.castVote(proposalId, support);
    }

    function voteWithReason(
        uint256 proposalId,
        uint8 support,
        string memory reason
    ) external {
        governor.castVoteWithReason(proposalId, support, reason);
    }

    function execute(
        address[] memory targets,
        uint256[] memory values,
        bytes[] memory calldatas,
        bytes32 descriptionHash
    ) external {
        governor.execute(targets, values, calldatas, descriptionHash);
    }
}
```

### Vote Escrow Integration

```solidity
import "@grey-protocol/contracts/governance/VotingEscrow.sol";

contract VeTokenIntegration {
    VotingEscrow public ve;

    function lock(uint256 amount, uint256 duration) external {
        IERC20(ve.token()).transferFrom(msg.sender, address(this), amount);
        IERC20(ve.token()).approve(address(ve), amount);

        ve.createLock(amount, block.timestamp + duration);
    }

    function getVotingPower(address account) external view returns (uint256) {
        return ve.balanceOf(account);
    }

    function increaseLockAmount(uint256 amount) external {
        IERC20(ve.token()).transferFrom(msg.sender, address(this), amount);
        IERC20(ve.token()).approve(address(ve), amount);

        ve.increaseAmount(amount);
    }

    function extendLock(uint256 newUnlockTime) external {
        ve.increaseUnlockTime(newUnlockTime);
    }
}
```

---

## Cross-Chain Integration

### Sending Cross-Chain Messages

```solidity
import "@grey-protocol/contracts/crosschain/CrossChainBridge.sol";

contract CrossChainSender {
    CrossChainBridge public bridge;

    function sendToken(
        uint256 destChainId,
        address token,
        uint256 amount,
        address recipient
    ) external payable {
        IERC20(token).transferFrom(msg.sender, address(this), amount);
        IERC20(token).approve(address(bridge), amount);

        bridge.bridgeTokens{value: msg.value}(
            destChainId,
            token,
            amount,
            recipient
        );
    }

    function sendMessage(
        uint256 destChainId,
        address target,
        bytes calldata data
    ) external payable {
        bridge.sendMessage{value: msg.value}(
            destChainId,
            target,
            data
        );
    }
}
```

### Receiving Cross-Chain Messages

```solidity
import "@grey-protocol/contracts/interfaces/ICrossChainReceiver.sol";

contract CrossChainReceiver is ICrossChainReceiver {
    address public bridge;

    modifier onlyBridge() {
        require(msg.sender == bridge, "Only bridge");
        _;
    }

    function receiveMessage(
        uint256 sourceChainId,
        address sender,
        bytes calldata data
    ) external override onlyBridge {
        // Process cross-chain message
        (address recipient, uint256 amount) = abi.decode(
            data,
            (address, uint256)
        );
        // Execute logic
    }
}
```

---

## Security Best Practices

### 1. Always Use SafeERC20

```solidity
import "@openzeppelin/contracts/token/ERC20/utils/SafeERC20.sol";

using SafeERC20 for IERC20;

// Good
token.safeTransfer(recipient, amount);
token.safeTransferFrom(sender, recipient, amount);

// Avoid
token.transfer(recipient, amount); // May not revert on failure
```

### 2. Check Return Values

```solidity
// For external calls
(bool success, bytes memory data) = target.call(callData);
require(success, "Call failed");

// For low-level calls
assembly {
    if iszero(call(gas(), target, value, 0, 0, 0, 0)) {
        revert(0, 0)
    }
}
```

### 3. Validate Inputs

```solidity
function deposit(uint256 amount, address recipient) external {
    require(amount > 0, "Amount must be positive");
    require(recipient != address(0), "Invalid recipient");
    require(recipient != address(this), "Cannot deposit to self");
    // Proceed with validated inputs
}
```

### 4. Use Timelock for Admin Functions

```solidity
import "@grey-protocol/contracts/governance/GreyTimelock.sol";

contract AdminOperations {
    GreyTimelock public timelock;

    function scheduleUpgrade(
        address newImplementation
    ) external onlyAdmin {
        bytes memory data = abi.encodeWithSignature(
            "upgrade(address)",
            newImplementation
        );

        timelock.schedule(
            address(this),
            0,
            data,
            bytes32(0),
            keccak256(abi.encodePacked(block.timestamp)),
            timelock.getMinDelay()
        );
    }
}
```

### 5. Implement Circuit Breakers

```solidity
import "@grey-protocol/contracts/security/CircuitBreaker.sol";

contract ProtectedContract is CircuitBreaker {
    uint256 public constant DAILY_LIMIT = 1000 ether;
    uint256 public dailyWithdrawn;
    uint256 public lastResetDay;

    function withdraw(uint256 amount) external whenNotPaused {
        // Reset daily counter
        if (block.timestamp / 1 days > lastResetDay) {
            dailyWithdrawn = 0;
            lastResetDay = block.timestamp / 1 days;
        }

        require(dailyWithdrawn + amount <= DAILY_LIMIT, "Daily limit exceeded");
        dailyWithdrawn += amount;

        // Proceed with withdrawal
    }
}
```

---

## Testing Guide

### Unit Tests

```javascript
const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("LiquidityPool", function () {
    let pool, token0, token1, owner, user;

    beforeEach(async function () {
        [owner, user] = await ethers.getSigners();

        // Deploy mocks
        const MockERC20 = await ethers.getContractFactory("MockERC20");
        token0 = await MockERC20.deploy("Token0", "TK0", 18);
        token1 = await MockERC20.deploy("Token1", "TK1", 18);

        // Deploy pool
        const LiquidityPool = await ethers.getContractFactory("LiquidityPool");
        pool = await LiquidityPool.deploy(
            token0.target,
            token1.target,
            owner.address
        );

        // Mint tokens
        await token0.mint(user.address, ethers.parseEther("1000"));
        await token1.mint(user.address, ethers.parseEther("1000"));
    });

    it("Should add liquidity", async function () {
        await token0.connect(user).approve(pool.target, ethers.parseEther("100"));
        await token1.connect(user).approve(pool.target, ethers.parseEther("100"));

        await pool.connect(user).addLiquidity(
            ethers.parseEther("100"),
            ethers.parseEther("100"),
            user.address
        );

        expect(await pool.balanceOf(user.address)).to.be.gt(0);
    });

    it("Should swap tokens", async function () {
        // Add initial liquidity
        // ...

        const amountIn = ethers.parseEther("1");
        await token0.connect(user).approve(pool.target, amountIn);

        const balanceBefore = await token1.balanceOf(user.address);
        await pool.connect(user).swap(token0.target, amountIn, 0, user.address);
        const balanceAfter = await token1.balanceOf(user.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
    });
});
```

### Integration Tests

```javascript
describe("Full DeFi Flow", function () {
    it("Should complete deposit → borrow → repay cycle", async function () {
        // 1. Deposit collateral
        await lendingPool.connect(user).deposit(
            collateral.target,
            ethers.parseEther("100"),
            user.address
        );

        // 2. Borrow
        await lendingPool.connect(user).borrow(
            borrowAsset.target,
            ethers.parseEther("50"),
            user.address
        );

        // 3. Accrue interest
        await time.increase(30 * 24 * 60 * 60);

        // 4. Repay
        const debt = await lendingPool.getUserDebt(user.address);
        await borrowAsset.connect(user).approve(lendingPool.target, debt);
        await lendingPool.connect(user).repay(
            borrowAsset.target,
            debt,
            user.address
        );

        // 5. Withdraw collateral
        await lendingPool.connect(user).withdraw(
            collateral.target,
            ethers.parseEther("100"),
            user.address
        );
    });
});
```

### Invariant Tests (Foundry)

See `contracts/test/InvariantTestHarness.sol` for the testing framework.

```bash
# Run invariant tests
forge test --match-test invariant -vvv
```

---

## Deployment Guide

### Environment Setup

```bash
# .env file
PRIVATE_KEY=your_private_key
ALCHEMY_API_KEY=your_alchemy_key
ETHERSCAN_API_KEY=your_etherscan_key
```

### Deploy Script

```javascript
// scripts/deploy.js
const { ethers, upgrades } = require("hardhat");

async function main() {
    const [deployer] = await ethers.getSigners();
    console.log("Deploying with:", deployer.address);

    // 1. Deploy token
    const GreyToken = await ethers.getContractFactory("GreyToken");
    const token = await GreyToken.deploy(deployer.address);
    console.log("Token:", token.target);

    // 2. Deploy Timelock
    const GreyTimelock = await ethers.getContractFactory("GreyTimelock");
    const timelock = await GreyTimelock.deploy(
        2 * 24 * 60 * 60, // 2 day delay
        [deployer.address], // proposers
        [deployer.address], // executors
        deployer.address    // admin
    );
    console.log("Timelock:", timelock.target);

    // 3. Deploy Governor
    const GreyGovernor = await ethers.getContractFactory("GreyGovernor");
    const governor = await GreyGovernor.deploy(
        token.target,
        timelock.target
    );
    console.log("Governor:", governor.target);

    // 4. Deploy DeFi contracts...
}

main().catch(console.error);
```

### Verification

```bash
npx hardhat verify --network mainnet CONTRACT_ADDRESS "arg1" "arg2"
```

---

## Troubleshooting

### Common Errors

**"Insufficient allowance"**
```javascript
// Solution: Approve before transfer
await token.approve(spender, amount);
```

**"Paused"**
```javascript
// Solution: Check pause state
const paused = await contract.paused();
// Or wait for unpause
```

**"Access denied"**
```javascript
// Solution: Check role
const role = await contract.DEFAULT_ADMIN_ROLE();
const hasRole = await contract.hasRole(role, signer.address);
```

**"Stack too deep"**
```solidity
// Solution: Use structs or break into helper functions
struct Params {
    uint256 a;
    uint256 b;
    // ...
}
```

### Gas Optimization Tips

1. Use `calldata` instead of `memory` for read-only arrays
2. Pack structs efficiently
3. Use `unchecked` for safe arithmetic
4. Batch operations where possible
5. Use events instead of storage for historical data

---

## Support

- **Documentation:** `/docs`
- **Issues:** GitHub Issues
- **Discord:** [Grey Protocol Discord]
- **Email:** dev@greyprotocol.io

---

*Last Updated: [Current Date]*
*Version: 1.0*
