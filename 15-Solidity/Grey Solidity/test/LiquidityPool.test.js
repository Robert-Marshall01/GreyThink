const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("LiquidityPool", function () {
  let pool;
  let tokenA;
  let tokenB;
  let owner, user1, user2;

  const SWAP_FEE = 30; // 0.3%
  const INITIAL_LIQUIDITY_A = ethers.parseEther("10000");
  const INITIAL_LIQUIDITY_B = ethers.parseEther("10000");

  beforeEach(async function () {
    [owner, user1, user2] = await ethers.getSigners();

    // Deploy mock tokens
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    tokenA = await MockERC20.deploy("Token A", "TKNA", 18);
    tokenB = await MockERC20.deploy("Token B", "TKNB", 18);

    // Deploy liquidity pool
    const LiquidityPool = await ethers.getContractFactory("LiquidityPool");
    pool = await LiquidityPool.deploy(
      await tokenA.getAddress(),
      await tokenB.getAddress(),
      "LP Token",
      "LP",
      SWAP_FEE
    );

    // Mint tokens
    await tokenA.mint(owner.address, ethers.parseEther("1000000"));
    await tokenB.mint(owner.address, ethers.parseEther("1000000"));
    await tokenA.mint(user1.address, ethers.parseEther("100000"));
    await tokenB.mint(user1.address, ethers.parseEther("100000"));
    await tokenA.mint(user2.address, ethers.parseEther("100000"));
    await tokenB.mint(user2.address, ethers.parseEther("100000"));

    // Approve pool
    await tokenA.approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenB.approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenA.connect(user1).approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenB.connect(user1).approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenA.connect(user2).approve(await pool.getAddress(), ethers.MaxUint256);
    await tokenB.connect(user2).approve(await pool.getAddress(), ethers.MaxUint256);
  });

  describe("Deployment", function () {
    it("Should set correct token addresses", async function () {
      expect(await pool.tokenA()).to.equal(await tokenA.getAddress());
      expect(await pool.tokenB()).to.equal(await tokenB.getAddress());
    });

    it("Should set correct swap fee", async function () {
      expect(await pool.swapFee()).to.equal(SWAP_FEE);
    });

    it("Should have zero reserves initially", async function () {
      expect(await pool.reserveA()).to.equal(0);
      expect(await pool.reserveB()).to.equal(0);
    });
  });

  describe("Adding Liquidity", function () {
    it("Should add initial liquidity", async function () {
      const tx = await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );

      await expect(tx).to.emit(pool, "LiquidityAdded");

      expect(await pool.reserveA()).to.equal(INITIAL_LIQUIDITY_A);
      expect(await pool.reserveB()).to.equal(INITIAL_LIQUIDITY_B);
    });

    it("Should mint LP tokens to provider", async function () {
      await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );

      const lpBalance = await pool.balanceOf(owner.address);
      expect(lpBalance).to.be.gt(0);
    });

    it("Should maintain price ratio for subsequent adds", async function () {
      await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );

      // Add more liquidity at same ratio
      await pool.connect(user1).addLiquidity(
        ethers.parseEther("1000"),
        ethers.parseEther("1000"),
        0,
        0,
        user1.address
      );

      const reserveA = await pool.reserveA();
      const reserveB = await pool.reserveB();

      // Ratio should be maintained
      expect(reserveA).to.equal(reserveB);
    });

    it("Should reject slippage-violating adds", async function () {
      await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );

      // Try to add with unreasonable minimum
      await expect(
        pool.connect(user1).addLiquidity(
          ethers.parseEther("1000"),
          ethers.parseEther("1000"),
          ethers.parseEther("2000"), // Impossible minimum
          0,
          user1.address
        )
      ).to.be.revertedWithCustomError(pool, "SlippageExceeded");
    });
  });

  describe("Removing Liquidity", function () {
    beforeEach(async function () {
      await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );
    });

    it("Should remove liquidity", async function () {
      const lpBalance = await pool.balanceOf(owner.address);
      const halfLiquidity = lpBalance / 2n;

      await pool.removeLiquidity(
        halfLiquidity,
        0,
        0,
        owner.address
      );

      expect(await pool.balanceOf(owner.address)).to.equal(halfLiquidity);
    });

    it("Should return both tokens", async function () {
      const lpBalance = await pool.balanceOf(owner.address);
      const initialA = await tokenA.balanceOf(owner.address);
      const initialB = await tokenB.balanceOf(owner.address);

      await pool.removeLiquidity(
        lpBalance,
        0,
        0,
        owner.address
      );

      const finalA = await tokenA.balanceOf(owner.address);
      const finalB = await tokenB.balanceOf(owner.address);

      expect(finalA).to.be.gt(initialA);
      expect(finalB).to.be.gt(initialB);
    });

    it("Should respect minimum amounts", async function () {
      const lpBalance = await pool.balanceOf(owner.address);

      await expect(
        pool.removeLiquidity(
          lpBalance,
          ethers.parseEther("20000"), // Impossible minimum
          0,
          owner.address
        )
      ).to.be.revertedWithCustomError(pool, "SlippageExceeded");
    });
  });

  describe("Swapping", function () {
    beforeEach(async function () {
      await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );
    });

    it("Should swap token A for token B", async function () {
      const swapAmount = ethers.parseEther("100");
      const initialB = await tokenB.balanceOf(user1.address);

      await pool.connect(user1).swap(
        await tokenA.getAddress(),
        swapAmount,
        0,
        user1.address
      );

      const finalB = await tokenB.balanceOf(user1.address);
      expect(finalB).to.be.gt(initialB);
    });

    it("Should swap token B for token A", async function () {
      const swapAmount = ethers.parseEther("100");
      const initialA = await tokenA.balanceOf(user1.address);

      await pool.connect(user1).swap(
        await tokenB.getAddress(),
        swapAmount,
        0,
        user1.address
      );

      const finalA = await tokenA.balanceOf(user1.address);
      expect(finalA).to.be.gt(initialA);
    });

    it("Should apply swap fee", async function () {
      const swapAmount = ethers.parseEther("100");
      
      const expectedOut = await pool.getAmountOut(
        await tokenA.getAddress(),
        swapAmount
      );

      // Expected output should be less than input (due to fee and price impact)
      expect(expectedOut).to.be.lt(swapAmount);
    });

    it("Should accumulate protocol fees", async function () {
      const swapAmount = ethers.parseEther("1000");
      
      await pool.connect(user1).swap(
        await tokenA.getAddress(),
        swapAmount,
        0,
        user1.address
      );

      const protocolFeesA = await pool.protocolFeesA();
      expect(protocolFeesA).to.be.gt(0);
    });

    it("Should respect minimum output", async function () {
      const swapAmount = ethers.parseEther("100");

      await expect(
        pool.connect(user1).swap(
          await tokenA.getAddress(),
          swapAmount,
          ethers.parseEther("200"), // Impossible output
          user1.address
        )
      ).to.be.revertedWithCustomError(pool, "SlippageExceeded");
    });

    it("Should update reserves after swap", async function () {
      const initialReserveA = await pool.reserveA();
      const initialReserveB = await pool.reserveB();
      const swapAmount = ethers.parseEther("100");

      await pool.connect(user1).swap(
        await tokenA.getAddress(),
        swapAmount,
        0,
        user1.address
      );

      const finalReserveA = await pool.reserveA();
      const finalReserveB = await pool.reserveB();

      expect(finalReserveA).to.be.gt(initialReserveA);
      expect(finalReserveB).to.be.lt(initialReserveB);
    });
  });

  describe("Price Queries", function () {
    beforeEach(async function () {
      await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );
    });

    it("Should return correct price", async function () {
      const price = await pool.getPrice();
      // Equal reserves = 1:1 ratio
      expect(price).to.equal(ethers.parseEther("1"));
    });

    it("Should calculate output amount", async function () {
      const amountIn = ethers.parseEther("100");
      const amountOut = await pool.getAmountOut(
        await tokenA.getAddress(),
        amountIn
      );

      expect(amountOut).to.be.gt(0);
      expect(amountOut).to.be.lt(amountIn); // Less due to fee
    });

    it("Should calculate input amount", async function () {
      const amountOut = ethers.parseEther("100");
      const amountIn = await pool.getAmountIn(
        await tokenB.getAddress(),
        amountOut
      );

      expect(amountIn).to.be.gt(0);
      expect(amountIn).to.be.gt(amountOut); // More due to fee
    });
  });

  describe("Admin Functions", function () {
    it("Should allow fee manager to update swap fee", async function () {
      await pool.setSwapFee(50); // 0.5%
      expect(await pool.swapFee()).to.equal(50);
    });

    it("Should reject fee above maximum", async function () {
      await expect(pool.setSwapFee(2000)) // 20%
        .to.be.revertedWith("LiquidityPool: fee too high");
    });

    it("Should allow withdrawing protocol fees", async function () {
      // First generate some fees
      await pool.addLiquidity(
        INITIAL_LIQUIDITY_A,
        INITIAL_LIQUIDITY_B,
        0,
        0,
        owner.address
      );

      await pool.connect(user1).swap(
        await tokenA.getAddress(),
        ethers.parseEther("1000"),
        0,
        user1.address
      );

      const feesA = await pool.protocolFeesA();
      expect(feesA).to.be.gt(0);

      await pool.withdrawProtocolFees(owner.address);
      expect(await pool.protocolFeesA()).to.equal(0);
    });
  });
});
