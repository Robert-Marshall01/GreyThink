# Grey Solidity API Reference

Complete API documentation for all contracts in the Grey Solidity Ecosystem.

---

## Table of Contents

1. [GreyToken (ERC20)](#greytoken-erc20)
2. [GreyNFT (ERC721)](#greynft-erc721)
3. [GreyMultiToken (ERC1155)](#greymultitoken-erc1155)
4. [StakingPool](#stakingpool)
5. [LiquidityPool](#liquiditypool)
6. [TokenVesting](#tokenvesting)
7. [NFTMarketplace](#nftmarketplace)
8. [Escrow](#escrow)
9. [Treasury](#treasury)
10. [Vault](#vault)

---

## GreyToken (ERC20)

Advanced ERC20 token with governance capabilities.

### State Variables

```solidity
uint256 public immutable maxSupply;    // Maximum token supply
mapping(address => bool) public blacklisted;  // Blacklisted addresses
```

### Functions

#### `mint(address to, uint256 amount)`
Mints new tokens to an address.

| Parameter | Type | Description |
|-----------|------|-------------|
| to | address | Recipient address |
| amount | uint256 | Amount to mint |

**Access**: MINTER_ROLE required

#### `burn(uint256 amount)`
Burns tokens from caller's balance.

| Parameter | Type | Description |
|-----------|------|-------------|
| amount | uint256 | Amount to burn |

#### `burnFrom(address account, uint256 amount)`
Burns tokens from specified account (requires allowance).

| Parameter | Type | Description |
|-----------|------|-------------|
| account | address | Account to burn from |
| amount | uint256 | Amount to burn |

#### `snapshot()`
Creates a new balance snapshot.

**Returns**: `uint256` - The snapshot ID

**Access**: SNAPSHOT_ROLE required

#### `pause()` / `unpause()`
Pauses/unpauses all token transfers.

**Access**: PAUSER_ROLE required

#### `permit(address owner, address spender, uint256 value, uint256 deadline, uint8 v, bytes32 r, bytes32 s)`
EIP-2612 gasless approval.

### Events

```solidity
event AddressBlacklisted(address indexed account, bool status);
```

---

## GreyNFT (ERC721)

NFT collection with royalties and enumeration.

### Constructor

```solidity
constructor(
    string memory name,
    string memory symbol,
    string memory baseTokenURI,
    address royaltyReceiver,
    uint96 royaltyFeeNumerator  // Basis points (e.g., 500 = 5%)
)
```

### Functions

#### `mint(address to, string memory tokenURI)`
Mints a new NFT with custom URI.

| Parameter | Type | Description |
|-----------|------|-------------|
| to | address | Recipient address |
| tokenURI | string | Token metadata URI |

**Returns**: `uint256` - The minted token ID

**Access**: MINTER_ROLE required

#### `batchMint(address to, string[] memory tokenURIs)`
Mints multiple NFTs in one transaction.

| Parameter | Type | Description |
|-----------|------|-------------|
| to | address | Recipient address |
| tokenURIs | string[] | Array of token URIs |

**Returns**: `uint256[]` - Array of minted token IDs

#### `setDefaultRoyalty(address receiver, uint96 feeNumerator)`
Sets default royalty info (EIP-2981).

| Parameter | Type | Description |
|-----------|------|-------------|
| receiver | address | Royalty recipient |
| feeNumerator | uint96 | Royalty in basis points |

**Access**: DEFAULT_ADMIN_ROLE required

#### `royaltyInfo(uint256 tokenId, uint256 salePrice)`
Returns royalty payment info.

**Returns**: `(address receiver, uint256 royaltyAmount)`

### Events

```solidity
event BatchMinted(address indexed to, uint256[] tokenIds);
```

---

## GreyMultiToken (ERC1155)

Multi-token standard with token types and supply tracking.

### Structs

```solidity
struct TokenType {
    string name;
    uint256 maxSupply;
    uint256 totalMinted;
    bool transferable;
    bool active;
}
```

### Functions

#### `createTokenType(string memory name, uint256 maxSupply, bool transferable)`
Creates a new token type.

| Parameter | Type | Description |
|-----------|------|-------------|
| name | string | Token type name |
| maxSupply | uint256 | Maximum supply (0 = unlimited) |
| transferable | bool | Whether tokens can be transferred |

**Returns**: `uint256` - The created token type ID

**Access**: DEFAULT_ADMIN_ROLE required

#### `mint(address to, uint256 typeId, uint256 amount, bytes memory data)`
Mints tokens of a specific type.

| Parameter | Type | Description |
|-----------|------|-------------|
| to | address | Recipient address |
| typeId | uint256 | Token type ID |
| amount | uint256 | Amount to mint |
| data | bytes | Additional data |

**Access**: MINTER_ROLE required

#### `totalSupply(uint256 id)`
Returns total supply of a token type.

**Returns**: `uint256` - Current supply

---

## StakingPool

Token staking with configurable rewards and lock periods.

### Constructor

```solidity
constructor(
    address _stakingToken,
    address _rewardToken,
    uint256 _rewardRate,       // Annual reward rate (18 decimals)
    uint256 _minLockDuration,  // Minimum lock in seconds
    uint256 _minStake,         // Minimum stake amount
    uint256 _maxStake          // Maximum stake amount
)
```

### Functions

#### `stake(uint256 amount)`
Stakes tokens with minimum lock period.

| Parameter | Type | Description |
|-----------|------|-------------|
| amount | uint256 | Amount to stake |

**Emits**: `Staked(address indexed user, uint256 amount)`

#### `unstake(uint256 amount)`
Withdraws staked tokens (applies penalty if early).

| Parameter | Type | Description |
|-----------|------|-------------|
| amount | uint256 | Amount to unstake |

**Note**: Early unstake incurs `earlyUnstakePenalty` (default 10%)

#### `claimRewards()`
Claims accumulated rewards.

**Emits**: `RewardClaimed(address indexed user, uint256 amount)`

#### `pendingRewards(address user)`
Returns unclaimed rewards for a user.

**Returns**: `uint256` - Pending reward amount

#### `getStakeInfo(address user)`
Returns staking information for a user.

**Returns**: 
```solidity
(
    uint256 amount,
    uint256 timestamp,
    uint256 unlockTime,
    uint256 rewards
)
```

### Admin Functions

#### `setRewardRate(uint256 newRate)`
Updates the annual reward rate.

#### `setBoostMultiplier(address user, uint256 multiplier)`
Sets reward boost for specific user (basis points).

---

## LiquidityPool

Constant product AMM (x*y=k).

### Constructor

```solidity
constructor(
    address _tokenA,
    address _tokenB,
    uint256 _fee,           // Fee in basis points (e.g., 30 = 0.3%)
    address _feeRecipient
)
```

### Functions

#### `addLiquidity(uint256 amountA, uint256 amountB, uint256 minLiquidity)`
Adds liquidity to the pool.

| Parameter | Type | Description |
|-----------|------|-------------|
| amountA | uint256 | Amount of token A |
| amountB | uint256 | Amount of token B |
| minLiquidity | uint256 | Minimum LP tokens to receive |

**Returns**: `uint256` - LP tokens minted

**Emits**: `LiquidityAdded(address indexed provider, uint256 amountA, uint256 amountB, uint256 liquidity)`

#### `removeLiquidity(uint256 liquidity, uint256 minAmountA, uint256 minAmountB)`
Removes liquidity from the pool.

| Parameter | Type | Description |
|-----------|------|-------------|
| liquidity | uint256 | LP tokens to burn |
| minAmountA | uint256 | Minimum token A to receive |
| minAmountB | uint256 | Minimum token B to receive |

**Returns**: `(uint256 amountA, uint256 amountB)`

#### `swapAForB(uint256 amountIn, uint256 minAmountOut)`
Swaps token A for token B.

| Parameter | Type | Description |
|-----------|------|-------------|
| amountIn | uint256 | Amount of token A to swap |
| minAmountOut | uint256 | Minimum token B to receive |

**Returns**: `uint256` - Amount of token B received

#### `swapBForA(uint256 amountIn, uint256 minAmountOut)`
Swaps token B for token A.

#### `getAmountOut(uint256 amountIn, bool aToB)`
Calculates output amount for a swap.

**Returns**: `uint256` - Expected output amount

#### `getPrice(bool aToB)`
Returns current price (with 18 decimal precision).

**Returns**: `uint256` - Price ratio

---

## TokenVesting

Token vesting with multiple schedule types.

### Enums

```solidity
enum VestingType { Linear, Cliff, LinearCliff }
```

### Structs

```solidity
struct VestingSchedule {
    address beneficiary;
    uint256 totalAmount;
    uint256 releasedAmount;
    uint256 startTime;
    uint256 cliffDuration;
    uint256 vestingDuration;
    VestingType vestingType;
    bool revocable;
    bool revoked;
}
```

### Functions

#### `createVestingSchedule(...)`
Creates a new vesting schedule.

```solidity
function createVestingSchedule(
    address beneficiary,
    uint256 amount,
    uint256 startTime,
    uint256 cliffDuration,
    uint256 vestingDuration,
    VestingType vestingType,
    bool revocable
) external returns (bytes32 scheduleId)
```

**Access**: DEFAULT_ADMIN_ROLE required

#### `release(bytes32 scheduleId)`
Releases vested tokens to beneficiary.

| Parameter | Type | Description |
|-----------|------|-------------|
| scheduleId | bytes32 | Schedule identifier |

**Returns**: `uint256` - Amount released

#### `revoke(bytes32 scheduleId)`
Revokes a vesting schedule (if revocable).

**Access**: DEFAULT_ADMIN_ROLE required

#### `computeReleasableAmount(bytes32 scheduleId)`
Calculates currently releasable tokens.

**Returns**: `uint256` - Releasable amount

#### `vestedAmount(bytes32 scheduleId)`
Calculates total vested amount.

**Returns**: `uint256` - Vested amount

---

## NFTMarketplace

NFT marketplace with fixed-price and auction listings.

### Enums

```solidity
enum ListingType { FixedPrice, Auction }
enum ListingStatus { Active, Sold, Expired, Cancelled }
```

### Functions

#### `createListing(...)`
Creates fixed-price listing.

```solidity
function createListing(
    address nftContract,
    uint256 tokenId,
    address paymentToken,  // address(0) for ETH
    uint256 price,
    uint256 duration
) external returns (uint256 listingId)
```

#### `createAuction(...)`
Creates auction listing.

```solidity
function createAuction(
    address nftContract,
    uint256 tokenId,
    address paymentToken,
    uint256 reservePrice,
    uint256 minBidIncrement,
    uint256 duration
) external returns (uint256 listingId)
```

#### `buyWithETH(uint256 listingId)`
Purchases fixed-price listing with ETH.

**Payable**: Yes (must send exact or excess ETH)

#### `buyWithToken(uint256 listingId)`
Purchases fixed-price listing with ERC20.

#### `placeBid(uint256 listingId, uint256 bidAmount)`
Places bid on auction (for ERC20 auctions).

#### `settleAuction(uint256 listingId)`
Settles ended auction to highest bidder.

#### `cancelListing(uint256 listingId)`
Cancels listing (seller only, no bids for auctions).

### Events

```solidity
event Listed(uint256 indexed listingId, address indexed seller, address nftContract, uint256 tokenId, uint256 price);
event Sale(uint256 indexed listingId, address indexed buyer, uint256 price);
event BidPlaced(uint256 indexed listingId, address indexed bidder, uint256 amount);
event AuctionSettled(uint256 indexed listingId, address indexed winner, uint256 winningBid);
```

---

## Escrow

Multi-asset escrow with dispute resolution.

### Enums

```solidity
enum AssetType { ETH, ERC20, ERC721, ERC1155 }
enum DealStatus { Pending, Deposited, Released, Disputed, Resolved, Cancelled }
```

### Functions

#### `createDeal(...)`
Creates a new escrow deal.

```solidity
function createDeal(
    address counterparty,
    address assetContract,
    AssetType assetType,
    uint256 tokenId,       // For NFTs
    uint256 amount,
    uint256 deadline,
    string memory description
) external returns (uint256 dealId)
```

#### `deposit(uint256 dealId)`
Deposits assets into escrow.

**Payable**: Yes (for ETH deals)

#### `release(uint256 dealId)`
Releases assets to counterparty.

**Access**: Depositor only

#### `openDispute(uint256 dealId, string memory reason)`
Opens a dispute on the deal.

**Access**: Either party

#### `resolveDispute(uint256 dealId, address winner, string memory resolution)`
Resolves dispute in favor of a party.

**Access**: Arbiter only

#### `requestRefund(uint256 dealId)`
Requests refund after deadline.

**Access**: Depositor only, after deadline

### Events

```solidity
event DealCreated(uint256 indexed dealId, address indexed initiator, address indexed counterparty);
event Deposited(uint256 indexed dealId, address indexed depositor, uint256 amount);
event Released(uint256 indexed dealId, address indexed recipient, uint256 amount);
event DisputeOpened(uint256 indexed dealId, address indexed initiator, string reason);
event DisputeResolved(uint256 indexed dealId, address indexed winner, string resolution);
```

---

## Treasury

Multi-asset treasury with spending limits.

### Functions

#### `depositETH()`
Deposits ETH to treasury.

**Payable**: Yes

#### `depositERC20(address token, uint256 amount)`
Deposits ERC20 tokens.

| Parameter | Type | Description |
|-----------|------|-------------|
| token | address | Token contract address |
| amount | uint256 | Amount to deposit |

#### `withdrawETH(address payable to, uint256 amount)`
Withdraws ETH from treasury.

**Access**: Owner only

#### `withdrawERC20(address token, address to, uint256 amount)`
Withdraws ERC20 tokens.

**Access**: Owner only

#### `setSpendingLimit(address token, uint256 dailyLimit)`
Sets daily spending limit for a token.

| Parameter | Type | Description |
|-----------|------|-------------|
| token | address | Token address (address(0) for ETH) |
| dailyLimit | uint256 | Maximum daily spend |

**Access**: Owner only

### Events

```solidity
event ETHDeposited(address indexed from, uint256 amount);
event ETHWithdrawn(address indexed to, uint256 amount);
event TokenDeposited(address indexed token, address indexed from, uint256 amount);
event TokenWithdrawn(address indexed token, address indexed to, uint256 amount);
event SpendingLimitSet(address indexed token, uint256 limit);
```

---

## Vault

ERC-4626 style vault with fees.

### Constructor

```solidity
constructor(
    address _asset,
    string memory _name,
    string memory _symbol,
    uint256 _performanceFee,   // Basis points
    uint256 _managementFee     // Basis points
)
```

### Functions

#### `deposit(uint256 assets)`
Deposits assets and mints shares.

| Parameter | Type | Description |
|-----------|------|-------------|
| assets | uint256 | Amount of assets to deposit |

**Returns**: `uint256` - Shares minted

#### `depositWithPermit(...)`
Deposits using EIP-2612 permit.

#### `withdraw(uint256 shares)`
Burns shares and withdraws assets.

| Parameter | Type | Description |
|-----------|------|-------------|
| shares | uint256 | Shares to burn |

**Returns**: `uint256` - Assets received

#### `previewDeposit(uint256 assets)`
Preview shares for deposit amount.

**Returns**: `uint256` - Expected shares

#### `previewWithdraw(uint256 shares)`
Preview assets for withdrawal.

**Returns**: `uint256` - Expected assets

#### `convertToShares(uint256 assets)`
Convert assets to shares.

#### `convertToAssets(uint256 shares)`
Convert shares to assets.

### Events

```solidity
event Deposit(address indexed caller, address indexed owner, uint256 assets, uint256 shares);
event Withdraw(address indexed caller, address indexed receiver, address indexed owner, uint256 assets, uint256 shares);
```

---

## Error Reference

Common custom errors across contracts:

```solidity
error Unauthorized();
error ZeroAddress();
error ZeroAmount();
error InsufficientBalance();
error InsufficientAllowance();
error DeadlinePassed();
error InvalidSignature();
error TransferFailed();
error Paused();
error NotPaused();
error AlreadyInitialized();
error InvalidState();
error SlippageExceeded();
```

---

*For more details, see the contract source code and NatSpec comments.*
