# Grey Solidity Ecosystem

A comprehensive, production-grade smart contract ecosystem demonstrating mid-to-senior level Solidity development practices. This project showcases advanced patterns in token standards, governance, DeFi primitives, NFT marketplaces, and contract upgradeability.

## 📋 Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Contracts](#contracts)
- [Installation](#installation)
- [Testing](#testing)
- [Deployment](#deployment)
- [Security](#security)
- [License](#license)

## 🎯 Overview

The Grey Solidity Ecosystem is a modular smart contract system featuring:

- **Token Systems**: ERC20 (with governance), ERC721 (with royalties), ERC1155
- **Governance**: Governor + Timelock + Treasury with on-chain voting
- **DeFi Primitives**: AMM pools, staking, vesting, vaults
- **Marketplace**: NFT auctions and fixed-price listings
- **Upgradeability**: UUPS and Transparent proxy patterns
- **Security**: Role-based access control, pausability, reentrancy guards

### Technology Stack

- **Solidity**: 0.8.24
- **Framework**: Hardhat
- **Libraries**: OpenZeppelin Contracts v5.0.0
- **Testing**: Mocha/Chai with Hardhat Network Helpers

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Grey Ecosystem                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │  GreyToken  │  │   GreyNFT   │  │GreyMulti   │  TOKENS       │
│  │   (ERC20)   │  │  (ERC721)   │  │Token(1155) │              │
│  └──────┬──────┘  └──────┬──────┘  └──────┬─────┘              │
│         │                │                │                     │
│  ┌──────┴────────────────┴────────────────┴─────┐              │
│  │              Governance Layer                 │              │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐   │              │
│  │  │ Governor │──│ Timelock │──│ Treasury │   │              │
│  │  └──────────┘  └──────────┘  └──────────┘   │              │
│  └───────────────────────────────────────────────┘              │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                    DeFi Layer                            │   │
│  │  ┌──────────┐  ┌──────────┐  ┌───────┐  ┌───────┐       │   │
│  │  │ Staking  │  │Liquidity │  │ Vault │  │Oracle │       │   │
│  │  │  Pool    │  │   Pool   │  │       │  │       │       │   │
│  │  └──────────┘  └──────────┘  └───────┘  └───────┘       │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                 Marketplace Layer                        │   │
│  │  ┌──────────────────┐      ┌──────────────────┐         │   │
│  │  │  NFTMarketplace  │      │     Escrow       │         │   │
│  │  └──────────────────┘      └──────────────────┘         │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │                Access Control & Security                  │   │
│  │        AccessControlExtended + Pausable + Guards         │   │
│  └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## 📁 Contracts

### Interfaces (`/contracts/interfaces/`)

| Contract | Description |
|----------|-------------|
| IERC20Extended | Extended ERC20 with permit, mint, burn, snapshots |
| IERC721Extended | Extended ERC721 with royalties (EIP-2981) |
| IERC1155Extended | Extended ERC1155 with supply tracking |
| IAccessControlExtended | Role-based access with identity registry |
| IGovernor | Governance proposal and voting |
| IOracle | Price feed interface |
| IMarketplace | NFT marketplace operations |
| IStaking | Staking pool operations |
| IVesting | Token vesting schedules |
| ITimelock | Time-delayed operations |
| ILiquidityPool | AMM swap operations |
| IEscrow | Multi-asset escrow with disputes |

### Libraries (`/contracts/libraries/`)

| Library | Description |
|---------|-------------|
| MathUtils | Min, max, sqrt, percentage, compound interest |
| StringUtils | String manipulation and conversion |
| ArrayUtils | Array operations (contains, sort, unique) |
| AddressUtils | Address validation and low-level calls |
| TimeUtils | Time constants and deadline helpers |

### Core Contracts

#### Tokens (`/contracts/tokens/`)

| Contract | Standard | Features |
|----------|----------|----------|
| GreyToken | ERC20 | Permit (EIP-2612), Votes, Snapshots, Pausable |
| GreyNFT | ERC721 | Royalties (EIP-2981), Enumerable, URI Storage |
| GreyMultiToken | ERC1155 | Supply tracking, Token types, Batch operations |

#### Governance (`/contracts/governance/`)

| Contract | Description |
|----------|-------------|
| GreyTimelock | Time-delayed execution with emergency blocking |
| GreyGovernor | OpenZeppelin Governor with quorum and settings |
| Treasury | Multi-asset treasury with spending limits |

#### DeFi (`/contracts/defi/`)

| Contract | Description |
|----------|-------------|
| StakingPool | Configurable staking with rewards and penalties |
| LiquidityPool | Constant product AMM (x*y=k) with fees |
| PriceOracle | Chainlink-compatible price feed mock |
| Vault | ERC-4626 style vault with performance fees |

#### Finance (`/contracts/finance/`)

| Contract | Description |
|----------|-------------|
| TokenVesting | Linear, cliff, and hybrid vesting schedules |

#### Marketplace (`/contracts/marketplace/`)

| Contract | Description |
|----------|-------------|
| NFTMarketplace | Fixed-price and auction listings with royalties |
| Escrow | Multi-asset escrow with dispute resolution |

#### Upgradeable (`/contracts/upgradeable/`)

| Contract | Pattern | Description |
|----------|---------|-------------|
| GreyVaultV1 | UUPS | Base vault with deposits/withdrawals |
| GreyVaultV2 | UUPS | Upgraded vault with interest and fees |
| TransparentRegistry | Transparent | Record registry with registration fees |

## 🚀 Installation

```bash
# Clone the repository
git clone <repository-url>
cd "Grey Solidity"

# Install dependencies
npm install

# Compile contracts
npx hardhat compile
```

## 🧪 Testing

```bash
# Run all tests
npm test

# Run with coverage
npm run test:coverage

# Run specific test file
npx hardhat test test/GreyToken.test.js
```

### Test Coverage

The test suite covers:
- Unit tests for each contract
- Integration tests for contract interactions
- Edge cases and error conditions
- Access control verification
- Event emission validation

## 📦 Deployment

### Local Deployment

```bash
# Start local node
npx hardhat node

# Deploy to local network
npm run deploy:local
```

### Testnet Deployment

```bash
# Deploy to Sepolia
npm run deploy:sepolia

# Verify contracts
npm run verify
```

### Deployment Script

The `scripts/deploy-ecosystem.js` script handles:
1. Core infrastructure deployment
2. Token contract deployment
3. Governance setup with timelock
4. DeFi primitives deployment
5. Marketplace deployment
6. Upgradeable contract deployment
7. Post-deployment configuration
8. Deployment data persistence

## 🔒 Security

### Security Features

- **Role-Based Access Control**: Granular permissions with multiple role levels
- **Pausability**: Emergency pause functionality on critical contracts
- **Reentrancy Guards**: Protection against reentrancy attacks
- **Safe Math**: Solidity 0.8.x built-in overflow protection
- **Input Validation**: Comprehensive parameter validation
- **Access Modifiers**: Function-level access restrictions

### Security Patterns Used

- Check-Effects-Interactions pattern
- Pull over push for payments
- Storage gaps for upgradeable contracts
- Two-step ownership transfers
- Timelock for governance actions

### Recommended Audits

Before mainnet deployment, consider:
1. Professional security audit
2. Formal verification of critical functions
3. Bug bounty program
4. Testnet stress testing

## 📄 License

MIT License - see [LICENSE](LICENSE) for details.

---

## 📚 Additional Resources

- [Solidity Documentation](https://docs.soliditylang.org/)
- [OpenZeppelin Contracts](https://docs.openzeppelin.com/contracts/)
- [Hardhat Documentation](https://hardhat.org/docs)
- [EIP Standards](https://eips.ethereum.org/)

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Open a Pull Request

---

*Built with ❤️ for the Ethereum ecosystem*
