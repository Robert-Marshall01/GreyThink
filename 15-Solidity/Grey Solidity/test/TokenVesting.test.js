const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time } = require("@nomicfoundation/hardhat-network-helpers");

describe("TokenVesting", function () {
  let vesting;
  let token;
  let owner, beneficiary1, beneficiary2;

  const TOTAL_AMOUNT = ethers.parseEther("10000");
  const CLIFF_DURATION = 30 * 24 * 60 * 60; // 30 days
  const VESTING_DURATION = 365 * 24 * 60 * 60; // 1 year

  beforeEach(async function () {
    [owner, beneficiary1, beneficiary2] = await ethers.getSigners();

    // Deploy mock token
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    token = await MockERC20.deploy("Vesting Token", "VST", 18);

    // Deploy vesting contract
    const TokenVesting = await ethers.getContractFactory("TokenVesting");
    vesting = await TokenVesting.deploy();

    // Mint tokens to owner
    await token.mint(owner.address, ethers.parseEther("1000000"));

    // Approve vesting contract
    await token.approve(await vesting.getAddress(), ethers.MaxUint256);
  });

  describe("Deployment", function () {
    it("Should grant admin role to deployer", async function () {
      const ADMIN_ROLE = await vesting.DEFAULT_ADMIN_ROLE();
      expect(await vesting.hasRole(ADMIN_ROLE, owner.address)).to.be.true;
    });

    it("Should grant vesting admin role to deployer", async function () {
      const VESTING_ADMIN_ROLE = await vesting.VESTING_ADMIN_ROLE();
      expect(await vesting.hasRole(VESTING_ADMIN_ROLE, owner.address)).to.be.true;
    });
  });

  describe("Schedule Creation", function () {
    it("Should create a linear vesting schedule", async function () {
      const start = await time.latest();
      const cliff = start + CLIFF_DURATION;

      const tx = await vesting.createSchedule(
        beneficiary1.address,
        await token.getAddress(),
        TOTAL_AMOUNT,
        start,
        cliff,
        VESTING_DURATION,
        0, // Linear
        true // Revocable
      );

      const receipt = await tx.wait();
      const event = receipt.logs.find(l => l.fragment?.name === "ScheduleCreated");
      expect(event).to.not.be.undefined;
    });

    it("Should transfer tokens to vesting contract", async function () {
      const start = await time.latest();
      const cliff = start + CLIFF_DURATION;

      const initialBalance = await token.balanceOf(await vesting.getAddress());

      await vesting.createSchedule(
        beneficiary1.address,
        await token.getAddress(),
        TOTAL_AMOUNT,
        start,
        cliff,
        VESTING_DURATION,
        0,
        true
      );

      const finalBalance = await token.balanceOf(await vesting.getAddress());
      expect(finalBalance - initialBalance).to.equal(TOTAL_AMOUNT);
    });

    it("Should reject schedule with zero beneficiary", async function () {
      const start = await time.latest();
      const cliff = start + CLIFF_DURATION;

      await expect(
        vesting.createSchedule(
          ethers.ZeroAddress,
          await token.getAddress(),
          TOTAL_AMOUNT,
          start,
          cliff,
          VESTING_DURATION,
          0,
          true
        )
      ).to.be.revertedWithCustomError(vesting, "InvalidParameters");
    });

    it("Should reject schedule with zero amount", async function () {
      const start = await time.latest();
      const cliff = start + CLIFF_DURATION;

      await expect(
        vesting.createSchedule(
          beneficiary1.address,
          await token.getAddress(),
          0,
          start,
          cliff,
          VESTING_DURATION,
          0,
          true
        )
      ).to.be.revertedWithCustomError(vesting, "InvalidParameters");
    });
  });

  describe("Vesting", function () {
    let scheduleId;
    let start;
    let cliff;

    beforeEach(async function () {
      start = await time.latest();
      cliff = start + CLIFF_DURATION;

      const tx = await vesting.createSchedule(
        beneficiary1.address,
        await token.getAddress(),
        TOTAL_AMOUNT,
        start,
        cliff,
        VESTING_DURATION,
        0, // Linear
        true
      );

      const receipt = await tx.wait();
      const event = receipt.logs.find(l => l.fragment?.name === "ScheduleCreated");
      scheduleId = event.args[0];
    });

    it("Should return zero vested amount before cliff", async function () {
      const vested = await vesting.vestedAmount(scheduleId);
      expect(vested).to.equal(0);
    });

    it("Should vest linearly after cliff", async function () {
      // Move to cliff
      await time.increaseTo(cliff);

      const vestedAtCliff = await vesting.vestedAmount(scheduleId);
      expect(vestedAtCliff).to.be.gt(0);

      // Move to halfway
      await time.increase(VESTING_DURATION / 2);

      const vestedHalfway = await vesting.vestedAmount(scheduleId);
      expect(vestedHalfway).to.be.gt(vestedAtCliff);
    });

    it("Should vest 100% after duration", async function () {
      await time.increaseTo(start + VESTING_DURATION);

      const vested = await vesting.vestedAmount(scheduleId);
      expect(vested).to.equal(TOTAL_AMOUNT);
    });

    it("Should allow releasing vested tokens", async function () {
      // Move to 50% vested
      await time.increaseTo(start + VESTING_DURATION / 2);

      const releasable = await vesting.releasableAmount(scheduleId);
      expect(releasable).to.be.gt(0);

      const initialBalance = await token.balanceOf(beneficiary1.address);
      await vesting.connect(beneficiary1).release(scheduleId);
      const finalBalance = await token.balanceOf(beneficiary1.address);

      expect(finalBalance - initialBalance).to.be.closeTo(releasable, ethers.parseEther("1"));
    });

    it("Should not allow non-beneficiary to release", async function () {
      await time.increaseTo(start + VESTING_DURATION / 2);

      await expect(
        vesting.connect(beneficiary2).release(scheduleId)
      ).to.be.revertedWithCustomError(vesting, "Unauthorized");
    });
  });

  describe("Revocation", function () {
    let scheduleId;
    let start;
    let cliff;

    beforeEach(async function () {
      start = await time.latest();
      cliff = start + CLIFF_DURATION;

      const tx = await vesting.createSchedule(
        beneficiary1.address,
        await token.getAddress(),
        TOTAL_AMOUNT,
        start,
        cliff,
        VESTING_DURATION,
        0,
        true // Revocable
      );

      const receipt = await tx.wait();
      const event = receipt.logs.find(l => l.fragment?.name === "ScheduleCreated");
      scheduleId = event.args[0];
    });

    it("Should allow admin to revoke", async function () {
      await time.increaseTo(start + VESTING_DURATION / 2);

      await expect(vesting.revoke(scheduleId))
        .to.emit(vesting, "ScheduleRevoked");
    });

    it("Should release vested tokens on revoke", async function () {
      await time.increaseTo(start + VESTING_DURATION / 2);

      const initialBalance = await token.balanceOf(beneficiary1.address);
      await vesting.revoke(scheduleId);
      const finalBalance = await token.balanceOf(beneficiary1.address);

      expect(finalBalance).to.be.gt(initialBalance);
    });

    it("Should return unvested tokens to admin", async function () {
      await time.increaseTo(start + VESTING_DURATION / 2);

      const initialBalance = await token.balanceOf(owner.address);
      await vesting.revoke(scheduleId);
      const finalBalance = await token.balanceOf(owner.address);

      expect(finalBalance).to.be.gt(initialBalance);
    });

    it("Should not allow revoking non-revocable schedule", async function () {
      // Create non-revocable schedule
      const tx = await vesting.createSchedule(
        beneficiary1.address,
        await token.getAddress(),
        TOTAL_AMOUNT,
        start,
        cliff,
        VESTING_DURATION,
        0,
        false // Not revocable
      );

      const receipt = await tx.wait();
      const event = receipt.logs.find(l => l.fragment?.name === "ScheduleCreated");
      const nonRevocableId = event.args[0];

      await expect(
        vesting.revoke(nonRevocableId)
      ).to.be.revertedWithCustomError(vesting, "NotRevocable");
    });
  });

  describe("Batch Operations", function () {
    it("Should create batch schedules", async function () {
      const start = await time.latest();
      const cliff = start + CLIFF_DURATION;

      const beneficiaries = [beneficiary1.address, beneficiary2.address];
      const amounts = [TOTAL_AMOUNT, TOTAL_AMOUNT];

      const tx = await vesting.createBatchSchedules(
        beneficiaries,
        await token.getAddress(),
        amounts,
        start,
        cliff,
        VESTING_DURATION,
        0,
        true
      );

      const receipt = await tx.wait();
      const events = receipt.logs.filter(l => l.fragment?.name === "ScheduleCreated");
      expect(events.length).to.equal(2);
    });
  });
});
