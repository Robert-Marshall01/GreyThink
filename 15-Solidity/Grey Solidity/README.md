# ⚠️ Disclaimer: Experimental Solidity Codebase ⚠️

This repository contains an **experimental, large-scale Solidity ecosystem**.  
Due to Ethereum’s pay-as-you-go gas model, the contracts have **not been deployed or tested on-chain**.  
As a result, functionality, security, and economic behavior remain **unverified**.

**Use at your own risk.**  
If you choose to integrate or build upon this codebase, you should:
- Conduct independent audits and security reviews.  
- Test thoroughly on local environments and public testnets.  
- Validate gas costs, upgradeability, and adversarial scenarios before mainnet deployment.  

This project is intended as an **architectural demonstration** and should not be considered production-ready without further validation.

# Grey Solidity Ecosystem

A **production-grade Solidity smart contract ecosystem** demonstrating senior-level blockchain development mastery across 14 major domains.

![Solidity](https://img.shields.io/badge/Solidity-0.8.28-363636?logo=solidity)
![License](https://img.shields.io/badge/License-MIT-green)
![Tests](https://img.shields.io/badge/Tests-21_Suites-blue)
![Coverage](https://img.shields.io/badge/Contracts-110-orange)
![Lines](https://img.shields.io/badge/Lines-61k+-purple)

## Overview

This enterprise-scale project implements a comprehensive blockchain ecosystem with **110 contracts**, **61,000+ lines of Solidity**, **21 test suites**, and extensive documentation covering:

| Domain | Contracts | Description |
|--------|-----------|-------------|
| Token Standards | 4 | ERC20, ERC721, ERC1155, Soulbound (ERC-5192) |
| Governance & DAO | 8 | Governor, Timelock, Treasury, VotingEscrow, Quadratic, Conviction |
| DeFi Primitives | 21 | AMMs, Lending, Vaults, Liquidation, Synthetics, Perpetuals |
| Consensus | 4 | BFT, Validators, Slashing, Light Client |
| Marketplace | 5 | NFT Trading, Auctions, Escrow, Disputes |
| Cross-Chain | 5 | Bridge, MessageRouter, Rollup, StateCommitment |
| Oracles | 3 | Chainlink, Median, TWAP |
| Identity | 3 | DID Registry, Reputation, Soulbound Credentials |
| Security | 8 | MultiSig, CircuitBreaker, RateLimiter, MEV Protection |
| Upgradeability | 4 | UUPS, Transparent, Beacon, Rollback Manager |
| Cryptography | 4 | ZK Proofs, BLS Signatures, Merkle Trees, EIP-712 |
| Libraries | 9 | Math, Arrays, Strings, Merkle, Signatures, Gas Optimizer |
| Tokenomics | 4 | Bonding Curves, Fee Distribution, Inflation, Rewards |
| Factories | 3 | Token, Proxy, Beacon Factories |
| Interfaces | 16 | Comprehensive interface definitions |

## Quick Start

```bash
# Install dependencies
npm install

# Compile contracts
npx hardhat compile

# Run full test suite
npx hardhat test

# Run specific test category
npx hardhat test test/Security.comprehensive.test.js

# Gas benchmarks
REPORT_GAS=true npx hardhat test test/GasBenchmarks.test.js
```

## Key Contracts

### Core Token
- **GreyToken** - ERC20 with governance, permit, snapshots, pausable, burnable

### DeFi Suite
- **ConstantProductAMM** - Uniswap V2-style AMM
- **StableSwapAMM** - Curve-style stable asset swaps
- **LendingMarket** - Aave-style lending with risk parameters
- **LiquidationEngine** - Dutch auction liquidations
- **YieldAggregator** - Multi-strategy yield optimization

### Governance
- **AdvancedGovernor** - Full-featured DAO with timelock
- **VotingEscrow** - veToken mechanics for long-term alignment
- **EmergencyGovernance** - Multi-guardian emergency controls

### Security
- **MultiSigWallet** - M-of-N signature wallet
- **CircuitBreaker** - Emergency pause mechanisms
- **FlashLoanGuard** - Flash loan attack prevention

### Cross-Chain
- **OptimisticRollupBridge** - L2 bridging with fraud proofs
- **MessageRouter** - Cross-chain message passing

## Documentation

| Document | Description |
|----------|-------------|
| [ARCHITECTURE.md](docs/ARCHITECTURE.md) | System design and module deep-dives |
| [THREAT_MODEL.md](docs/THREAT_MODEL.md) | Security analysis and mitigations |
| [ECONOMIC_DESIGN.md](docs/ECONOMIC_DESIGN.md) | Tokenomics and incentive mechanisms |
| [INTEGRATION_GUIDE.md](docs/INTEGRATION_GUIDE.md) | Developer integration guide |
| [AUDIT_CHECKLIST.md](docs/AUDIT_CHECKLIST.md) | Pre-audit verification status |
| [API.md](docs/API.md) | Contract API reference |

## Deployment

```bash
# Local development
npx hardhat node
npx hardhat run scripts/deploy.js --network localhost

# Testnet deployment
npx hardhat run scripts/deploy-full-ecosystem.js --network sepolia

# Upgrade with migration
npx hardhat run scripts/migrate.js --network sepolia
```

## Docker & Kubernetes

### Docker

```bash
# Build the Docker image
docker build -t grey-solidity:2.0.0 .

# Run Hardhat node in container
docker run -p 8545:8545 grey-solidity:2.0.0

# Build specific stages
docker build --target development -t grey-solidity:dev .
docker build --target test -t grey-solidity:test .

# Run tests in container
docker run grey-solidity:test
```

### Docker Compose

```bash
# Start Hardhat development node
docker-compose up -d hardhat-node

# Run tests (with node dependency)
docker-compose --profile test up test-runner

# Compile contracts
docker-compose --profile tools up compiler

# Run coverage
docker-compose --profile coverage up coverage

# Deploy contracts
NETWORK=localhost docker-compose --profile deploy up deployer

# Stop all services
docker-compose down

# Clean up volumes
docker-compose down -v
```

### Kubernetes

```bash
# Apply all resources using Kustomize
kubectl apply -k k8s/

# Or apply individually
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/secret.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml

# Run compilation job
kubectl apply -f k8s/jobs/compile-job.yaml

# Run tests
kubectl apply -f k8s/jobs/test-job.yaml

# Deploy contracts
kubectl apply -f k8s/jobs/deploy-job.yaml

# Check deployment status
kubectl -n grey-solidity get pods
kubectl -n grey-solidity logs -f deployment/grey-hardhat-node

# Port forward for local access
kubectl -n grey-solidity port-forward svc/grey-hardhat-node 8545:8545

# Delete all resources
kubectl delete -k k8s/
```

### Container Features

| Feature | Description |
|---------|-------------|
| Multi-stage builds | Optimized production images |
| Non-root user | Security hardening |
| Health checks | Automatic container health monitoring |
| Volume mounts | Persistent artifacts and cache |
| Network policies | Traffic isolation in Kubernetes |
| HPA ready | Horizontal Pod Autoscaler support |
| Kustomize | Environment-specific configurations |

## Security Features

| Feature | Implementation |
|---------|----------------|
| Access Control | OpenZeppelin RBAC with extended roles |
| Reentrancy | ReentrancyGuard on all state-changing functions |
| Flash Loans | FlashLoanGuard with state locks |
| Rate Limiting | Per-address and global limits |
| Circuit Breaker | Threshold-based emergency pause |
| Multi-sig | M-of-N signature requirements |
| Timelocks | 24-48h delays on sensitive operations |
| MEV Protection | Commit-reveal and slippage controls |

## Testing

```bash
# Full test suite
npx hardhat test

# With gas reporting
REPORT_GAS=true npx hardhat test

# Coverage report
npx hardhat coverage

# Specific attack simulations
npx hardhat test test/GovernanceAttacks.test.js
npx hardhat test test/ValidatorAdversarial.test.js
npx hardhat test test/OracleAttacks.test.js
```

### Test Coverage

| Category | Test File |
|----------|-----------|
| Governance Attacks | `GovernanceAttacks.test.js` |
| Validator Adversarial | `ValidatorAdversarial.test.js` |
| Oracle Manipulation | `OracleAttacks.test.js` |
| DeFi Comprehensive | `DeFi.comprehensive.test.js` |
| Security Suite | `Security.comprehensive.test.js` |
| Cross-Chain | `CrossChain.comprehensive.test.js` |
| Gas Benchmarks | `GasBenchmarks.test.js` |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    GREY ECOSYSTEM                           │
├─────────────────────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │Governance│  │   DeFi   │  │Cross-Chain│  │ Identity │   │
│  │  8 Ctrs  │  │  21 Ctrs │  │  5 Ctrs   │  │  3 Ctrs  │   │
│  └────┬─────┘  └────┬─────┘  └────┬──────┘  └────┬─────┘   │
│       │             │             │              │          │
│  ┌────┴─────────────┴─────────────┴──────────────┴─────┐   │
│  │              Core Infrastructure                     │   │
│  │  Security (8) │ Libraries (9) │ Oracles (3)         │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## License

MIT
