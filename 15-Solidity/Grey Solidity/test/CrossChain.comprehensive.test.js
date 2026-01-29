const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time, loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

/**
 * @title Comprehensive Cross-Chain & Oracle Tests
 * @notice Tests for CrossChainBridge, MessageRouter, Oracles, and TWAP
 */
describe("CrossChain & Oracle Modules - Comprehensive Tests", function () {
  // ============================================
  // CROSS-CHAIN BRIDGE FIXTURES
  // ============================================

  async function deployCrossChainFixture() {
    const [owner, validator1, validator2, validator3, relayer, user1, user2] = await ethers.getSigners();

    // Deploy mock tokens
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    const sourceToken = await MockERC20.deploy("Source Token", "SRC", 18);
    const wrappedToken = await MockERC20.deploy("Wrapped Token", "wSRC", 18);

    // Deploy bridge
    const CrossChainBridge = await ethers.getContractFactory("CrossChainBridge");
    const bridge = await CrossChainBridge.deploy(
      1,      // Source chain ID
      [validator1.address, validator2.address, validator3.address],
      2,      // 2 of 3 threshold
      ethers.parseEther("0.001"),  // Bridge fee
      ethers.parseEther("10000")   // Max transfer per tx
    );

    // Deploy message router
    const MessageRouter = await ethers.getContractFactory("MessageRouter");
    const router = await MessageRouter.deploy(
      [relayer.address],
      300000,  // Gas limit
      ethers.parseEther("0.0001")  // Message fee
    );

    // Mint tokens
    await sourceToken.mint(user1.address, ethers.parseEther("100000"));
    await sourceToken.mint(user2.address, ethers.parseEther("100000"));

    // Approve bridge
    await sourceToken.connect(user1).approve(await bridge.getAddress(), ethers.MaxUint256);
    await sourceToken.connect(user2).approve(await bridge.getAddress(), ethers.MaxUint256);

    // Register token pair on bridge
    await bridge.connect(owner).registerTokenPair(
      await sourceToken.getAddress(),
      await wrappedToken.getAddress(),
      56  // Target chain ID (BSC)
    );

    return {
      bridge,
      router,
      sourceToken,
      wrappedToken,
      owner,
      validator1,
      validator2,
      validator3,
      relayer,
      user1,
      user2
    };
  }

  // ============================================
  // CROSS-CHAIN BRIDGE TESTS
  // ============================================

  describe("CrossChainBridge", function () {
    describe("Deployment", function () {
      it("Should set correct chain ID", async function () {
        const { bridge } = await loadFixture(deployCrossChainFixture);
        expect(await bridge.chainId()).to.equal(1);
      });

      it("Should register validators", async function () {
        const { bridge, validator1, validator2, validator3 } = await loadFixture(deployCrossChainFixture);
        
        expect(await bridge.isValidator(validator1.address)).to.be.true;
        expect(await bridge.isValidator(validator2.address)).to.be.true;
        expect(await bridge.isValidator(validator3.address)).to.be.true;
      });

      it("Should set correct threshold", async function () {
        const { bridge } = await loadFixture(deployCrossChainFixture);
        expect(await bridge.validatorThreshold()).to.equal(2);
      });

      it("Should set fee", async function () {
        const { bridge } = await loadFixture(deployCrossChainFixture);
        expect(await bridge.bridgeFee()).to.equal(ethers.parseEther("0.001"));
      });
    });

    describe("Token Registration", function () {
      it("Should register token pair", async function () {
        const { bridge, sourceToken, wrappedToken } = await loadFixture(deployCrossChainFixture);
        
        const mapping = await bridge.tokenMappings(await sourceToken.getAddress(), 56);
        expect(mapping.wrappedToken).to.equal(await wrappedToken.getAddress());
      });

      it("Should emit TokenPairRegistered event", async function () {
        const { bridge, owner } = await loadFixture(deployCrossChainFixture);

        const MockERC20 = await ethers.getContractFactory("MockERC20");
        const newToken = await MockERC20.deploy("New Token", "NEW", 18);
        const wrappedNew = await MockERC20.deploy("Wrapped New", "wNEW", 18);

        await expect(bridge.connect(owner).registerTokenPair(
          await newToken.getAddress(),
          await wrappedNew.getAddress(),
          137
        )).to.emit(bridge, "TokenPairRegistered");
      });

      it("Should reject registration by non-admin", async function () {
        const { bridge, user1 } = await loadFixture(deployCrossChainFixture);

        const MockERC20 = await ethers.getContractFactory("MockERC20");
        const newToken = await MockERC20.deploy("New", "NEW", 18);

        await expect(bridge.connect(user1).registerTokenPair(
          await newToken.getAddress(),
          await newToken.getAddress(),
          137
        )).to.be.reverted;
      });
    });

    describe("Locking (Outbound)", function () {
      it("Should lock tokens for bridge", async function () {
        const { bridge, sourceToken, user1 } = await loadFixture(deployCrossChainFixture);

        await bridge.connect(user1).lock(
          await sourceToken.getAddress(),
          ethers.parseEther("1000"),
          56,
          user1.address,
          { value: ethers.parseEther("0.001") }
        );

        expect(await sourceToken.balanceOf(await bridge.getAddress())).to.equal(ethers.parseEther("1000"));
      });

      it("Should emit TokensLocked event", async function () {
        const { bridge, sourceToken, user1 } = await loadFixture(deployCrossChainFixture);

        await expect(bridge.connect(user1).lock(
          await sourceToken.getAddress(),
          ethers.parseEther("1000"),
          56,
          user1.address,
          { value: ethers.parseEther("0.001") }
        )).to.emit(bridge, "TokensLocked");
      });

      it("Should generate nonce", async function () {
        const { bridge, sourceToken, user1 } = await loadFixture(deployCrossChainFixture);

        await bridge.connect(user1).lock(
          await sourceToken.getAddress(),
          ethers.parseEther("1000"),
          56,
          user1.address,
          { value: ethers.parseEther("0.001") }
        );

        expect(await bridge.nonce()).to.equal(1);
      });

      it("Should reject without fee", async function () {
        const { bridge, sourceToken, user1 } = await loadFixture(deployCrossChainFixture);

        await expect(bridge.connect(user1).lock(
          await sourceToken.getAddress(),
          ethers.parseEther("1000"),
          56,
          user1.address
        )).to.be.revertedWithCustomError(bridge, "InsufficientFee");
      });

      it("Should reject exceeding max transfer", async function () {
        const { bridge, sourceToken, user1 } = await loadFixture(deployCrossChainFixture);

        await expect(bridge.connect(user1).lock(
          await sourceToken.getAddress(),
          ethers.parseEther("20000"),  // Over 10000 max
          56,
          user1.address,
          { value: ethers.parseEther("0.001") }
        )).to.be.revertedWithCustomError(bridge, "ExceedsMaxTransfer");
      });
    });

    describe("Unlocking (Inbound)", function () {
      async function lockedTokensFixture() {
        const fixture = await loadFixture(deployCrossChainFixture);
        const { bridge, sourceToken, user1 } = fixture;

        await bridge.connect(user1).lock(
          await sourceToken.getAddress(),
          ethers.parseEther("1000"),
          56,
          user1.address,
          { value: ethers.parseEther("0.001") }
        );

        return fixture;
      }

      it("Should unlock with validator signatures", async function () {
        const { bridge, sourceToken, validator1, validator2, user1 } = await lockedTokensFixture();

        const unlockId = ethers.id("unlock-1");
        const amount = ethers.parseEther("500");

        // Validators sign
        await bridge.connect(validator1).validateUnlock(
          unlockId,
          await sourceToken.getAddress(),
          user1.address,
          amount,
          56
        );

        await bridge.connect(validator2).validateUnlock(
          unlockId,
          await sourceToken.getAddress(),
          user1.address,
          amount,
          56
        );

        // Execute unlock
        const balanceBefore = await sourceToken.balanceOf(user1.address);
        await bridge.executeUnlock(unlockId);
        const balanceAfter = await sourceToken.balanceOf(user1.address);

        expect(balanceAfter - balanceBefore).to.equal(amount);
      });

      it("Should emit TokensUnlocked event", async function () {
        const { bridge, sourceToken, validator1, validator2, user1 } = await lockedTokensFixture();

        const unlockId = ethers.id("unlock-2");
        const amount = ethers.parseEther("500");

        await bridge.connect(validator1).validateUnlock(unlockId, await sourceToken.getAddress(), user1.address, amount, 56);
        await bridge.connect(validator2).validateUnlock(unlockId, await sourceToken.getAddress(), user1.address, amount, 56);

        await expect(bridge.executeUnlock(unlockId))
          .to.emit(bridge, "TokensUnlocked");
      });

      it("Should reject without enough validations", async function () {
        const { bridge, sourceToken, validator1, user1 } = await lockedTokensFixture();

        const unlockId = ethers.id("unlock-3");

        await bridge.connect(validator1).validateUnlock(
          unlockId,
          await sourceToken.getAddress(),
          user1.address,
          ethers.parseEther("500"),
          56
        );

        await expect(bridge.executeUnlock(unlockId))
          .to.be.revertedWithCustomError(bridge, "InsufficientValidations");
      });

      it("Should reject double validation", async function () {
        const { bridge, sourceToken, validator1, user1 } = await lockedTokensFixture();

        const unlockId = ethers.id("unlock-4");

        await bridge.connect(validator1).validateUnlock(
          unlockId,
          await sourceToken.getAddress(),
          user1.address,
          ethers.parseEther("500"),
          56
        );

        await expect(bridge.connect(validator1).validateUnlock(
          unlockId,
          await sourceToken.getAddress(),
          user1.address,
          ethers.parseEther("500"),
          56
        )).to.be.revertedWithCustomError(bridge, "AlreadyValidated");
      });
    });

    describe("Validator Management", function () {
      it("Should add validator", async function () {
        const { bridge, owner, user1 } = await loadFixture(deployCrossChainFixture);

        await bridge.connect(owner).addValidator(user1.address);
        expect(await bridge.isValidator(user1.address)).to.be.true;
      });

      it("Should remove validator", async function () {
        const { bridge, owner, validator3 } = await loadFixture(deployCrossChainFixture);

        await bridge.connect(owner).removeValidator(validator3.address);
        expect(await bridge.isValidator(validator3.address)).to.be.false;
      });

      it("Should update threshold", async function () {
        const { bridge, owner } = await loadFixture(deployCrossChainFixture);

        await bridge.connect(owner).setValidatorThreshold(3);
        expect(await bridge.validatorThreshold()).to.equal(3);
      });

      it("Should reject threshold higher than validator count", async function () {
        const { bridge, owner } = await loadFixture(deployCrossChainFixture);

        await expect(bridge.connect(owner).setValidatorThreshold(5))
          .to.be.revertedWithCustomError(bridge, "InvalidThreshold");
      });
    });
  });

  // ============================================
  // MESSAGE ROUTER TESTS
  // ============================================

  describe("MessageRouter", function () {
    describe("Message Sending", function () {
      it("Should send cross-chain message", async function () {
        const { router, user1 } = await loadFixture(deployCrossChainFixture);

        await router.connect(user1).sendMessage(
          56,
          user1.address,
          "0x1234",
          { value: ethers.parseEther("0.0001") }
        );

        expect(await router.messageCount()).to.equal(1);
      });

      it("Should emit MessageSent event", async function () {
        const { router, user1 } = await loadFixture(deployCrossChainFixture);

        await expect(router.connect(user1).sendMessage(
          56,
          user1.address,
          "0x1234",
          { value: ethers.parseEther("0.0001") }
        )).to.emit(router, "MessageSent");
      });

      it("Should reject without fee", async function () {
        const { router, user1 } = await loadFixture(deployCrossChainFixture);

        await expect(router.connect(user1).sendMessage(
          56,
          user1.address,
          "0x1234"
        )).to.be.revertedWithCustomError(router, "InsufficientFee");
      });
    });

    describe("Message Receiving", function () {
      it("Should allow relayer to deliver message", async function () {
        const { router, relayer, user1 } = await loadFixture(deployCrossChainFixture);

        await router.connect(relayer).deliverMessage(
          ethers.id("msg-1"),
          1,
          user1.address,
          user1.address,
          "0x1234"
        );

        expect(await router.messageDelivered(ethers.id("msg-1"))).to.be.true;
      });

      it("Should emit MessageDelivered event", async function () {
        const { router, relayer, user1 } = await loadFixture(deployCrossChainFixture);

        await expect(router.connect(relayer).deliverMessage(
          ethers.id("msg-2"),
          1,
          user1.address,
          user1.address,
          "0x1234"
        )).to.emit(router, "MessageDelivered");
      });

      it("Should reject delivery by non-relayer", async function () {
        const { router, user1 } = await loadFixture(deployCrossChainFixture);

        await expect(router.connect(user1).deliverMessage(
          ethers.id("msg-3"),
          1,
          user1.address,
          user1.address,
          "0x1234"
        )).to.be.revertedWithCustomError(router, "NotRelayer");
      });

      it("Should reject duplicate delivery", async function () {
        const { router, relayer, user1 } = await loadFixture(deployCrossChainFixture);

        await router.connect(relayer).deliverMessage(
          ethers.id("msg-4"),
          1,
          user1.address,
          user1.address,
          "0x1234"
        );

        await expect(router.connect(relayer).deliverMessage(
          ethers.id("msg-4"),
          1,
          user1.address,
          user1.address,
          "0x1234"
        )).to.be.revertedWithCustomError(router, "AlreadyDelivered");
      });
    });
  });

  // ============================================
  // ORACLE TESTS
  // ============================================

  async function deployOracleFixture() {
    const [owner, reporter1, reporter2, reporter3, user1] = await ethers.getSigners();

    // Deploy Median Oracle
    const MedianOracle = await ethers.getContractFactory("MedianOracle");
    const medianOracle = await MedianOracle.deploy(
      [reporter1.address, reporter2.address, reporter3.address],
      2,       // min reporters
      3600,    // 1 hour validity
      "ETH/USD"
    );

    // Deploy TWAP Oracle
    const MockERC20 = await ethers.getContractFactory("MockERC20");
    const tokenA = await MockERC20.deploy("Token A", "TKA", 18);
    const tokenB = await MockERC20.deploy("Token B", "TKB", 18);

    const TWAPOracle = await ethers.getContractFactory("TWAPOracle");
    const twapOracle = await TWAPOracle.deploy(
      await tokenA.getAddress(),
      await tokenB.getAddress(),
      1800,    // 30 min window
      12       // 12 observations
    );

    return {
      medianOracle,
      twapOracle,
      tokenA,
      tokenB,
      owner,
      reporter1,
      reporter2,
      reporter3,
      user1
    };
  }

  describe("MedianOracle", function () {
    describe("Price Reporting", function () {
      it("Should accept price from reporter", async function () {
        const { medianOracle, reporter1 } = await loadFixture(deployOracleFixture);

        await medianOracle.connect(reporter1).report(ethers.parseEther("2000"));

        const report = await medianOracle.getReporterPrice(reporter1.address);
        expect(report.price).to.equal(ethers.parseEther("2000"));
      });

      it("Should emit PriceReported event", async function () {
        const { medianOracle, reporter1 } = await loadFixture(deployOracleFixture);

        await expect(medianOracle.connect(reporter1).report(ethers.parseEther("2000")))
          .to.emit(medianOracle, "PriceReported")
          .withArgs(reporter1.address, ethers.parseEther("2000"));
      });

      it("Should reject report from non-reporter", async function () {
        const { medianOracle, user1 } = await loadFixture(deployOracleFixture);

        await expect(medianOracle.connect(user1).report(ethers.parseEther("2000")))
          .to.be.revertedWithCustomError(medianOracle, "NotReporter");
      });
    });

    describe("Median Calculation", function () {
      async function reportedPricesFixture() {
        const fixture = await loadFixture(deployOracleFixture);
        const { medianOracle, reporter1, reporter2, reporter3 } = fixture;

        await medianOracle.connect(reporter1).report(ethers.parseEther("1900"));
        await medianOracle.connect(reporter2).report(ethers.parseEther("2000"));
        await medianOracle.connect(reporter3).report(ethers.parseEther("2100"));

        return fixture;
      }

      it("Should calculate median price", async function () {
        const { medianOracle } = await reportedPricesFixture();

        const median = await medianOracle.getLatestPrice();
        expect(median).to.equal(ethers.parseEther("2000"));
      });

      it("Should handle even number of reporters", async function () {
        const { medianOracle, reporter1, reporter2 } = await loadFixture(deployOracleFixture);

        await medianOracle.connect(reporter1).report(ethers.parseEther("1900"));
        await medianOracle.connect(reporter2).report(ethers.parseEther("2100"));

        const median = await medianOracle.getLatestPrice();
        expect(median).to.equal(ethers.parseEther("2000"));  // Average of middle two
      });

      it("Should reject stale prices", async function () {
        const { medianOracle, reporter1, reporter2 } = await loadFixture(deployOracleFixture);

        await medianOracle.connect(reporter1).report(ethers.parseEther("2000"));
        await medianOracle.connect(reporter2).report(ethers.parseEther("2000"));

        await time.increase(3601);  // Past validity period

        await expect(medianOracle.getLatestPrice())
          .to.be.revertedWithCustomError(medianOracle, "StalePrice");
      });
    });
  });

  describe("TWAPOracle", function () {
    describe("Observations", function () {
      it("Should record observation", async function () {
        const { twapOracle, owner } = await loadFixture(deployOracleFixture);

        await twapOracle.connect(owner).update(
          ethers.parseEther("100000"),  // reserve A
          ethers.parseEther("50000")    // reserve B
        );

        expect(await twapOracle.observationCount()).to.be.gte(1);
      });

      it("Should accumulate observations", async function () {
        const { twapOracle, owner } = await loadFixture(deployOracleFixture);

        for (let i = 0; i < 5; i++) {
          await twapOracle.connect(owner).update(
            ethers.parseEther(String(100000 + i * 1000)),
            ethers.parseEther("50000")
          );
          await time.increase(150);  // Every 2.5 min
        }

        expect(await twapOracle.observationCount()).to.be.gte(5);
      });
    });

    describe("TWAP Calculation", function () {
      async function observationsFixture() {
        const fixture = await loadFixture(deployOracleFixture);
        const { twapOracle, owner } = fixture;

        // Record multiple observations
        for (let i = 0; i < 12; i++) {
          await twapOracle.connect(owner).update(
            ethers.parseEther("100000"),
            ethers.parseEther("50000")
          );
          await time.increase(150);
        }

        return fixture;
      }

      it("Should calculate TWAP", async function () {
        const { twapOracle } = await observationsFixture();

        const twap = await twapOracle.getTWAP();
        expect(twap).to.be.gt(0);
      });

      it("Should smooth out price spikes", async function () {
        const { twapOracle, owner } = await observationsFixture();

        // Get initial TWAP
        const twapBefore = await twapOracle.getTWAP();

        // Add spike
        await twapOracle.connect(owner).update(
          ethers.parseEther("200000"),  // 2x price spike
          ethers.parseEther("50000")
        );

        const twapAfter = await twapOracle.getTWAP();

        // TWAP should be less affected than spot
        const twapChange = Number(twapAfter - twapBefore) / Number(twapBefore);
        expect(twapChange).to.be.lt(0.5);  // Less than 50% change
      });
    });
  });

  // ============================================
  // CROSS-CHAIN INTEGRATION TESTS
  // ============================================

  describe("CrossChain Integration", function () {
    it("Should handle full bridge flow", async function () {
      const { bridge, sourceToken, validator1, validator2, user1 } = 
        await loadFixture(deployCrossChainFixture);

      const initialBalance = await sourceToken.balanceOf(user1.address);

      // Lock tokens
      await bridge.connect(user1).lock(
        await sourceToken.getAddress(),
        ethers.parseEther("1000"),
        56,
        user1.address,
        { value: ethers.parseEther("0.001") }
      );

      // Simulate cross-chain (validators confirm unlock)
      const unlockId = ethers.id("round-trip-1");
      await bridge.connect(validator1).validateUnlock(
        unlockId,
        await sourceToken.getAddress(),
        user1.address,
        ethers.parseEther("990"),  // Less fee
        56
      );
      await bridge.connect(validator2).validateUnlock(
        unlockId,
        await sourceToken.getAddress(),
        user1.address,
        ethers.parseEther("990"),
        56
      );

      // Execute unlock
      await bridge.executeUnlock(unlockId);

      const finalBalance = await sourceToken.balanceOf(user1.address);
      expect(finalBalance).to.be.lt(initialBalance);  // Lost some to fees
    });

    it("Should handle oracle-backed bridging", async function () {
      const { bridge, medianOracle, sourceToken, validator1, validator2, reporter1, reporter2, user1 } = 
        await loadFixture(async () => {
          const crossChain = await deployCrossChainFixture();
          const oracle = await deployOracleFixture();
          return { ...crossChain, ...oracle };
        });

      // Set price
      await medianOracle.connect(reporter1).report(ethers.parseEther("2000"));
      await medianOracle.connect(reporter2).report(ethers.parseEther("2000"));

      const price = await medianOracle.getLatestPrice();
      expect(price).to.equal(ethers.parseEther("2000"));

      // Bridge using price
      await bridge.connect(user1).lock(
        await sourceToken.getAddress(),
        ethers.parseEther("1000"),
        56,
        user1.address,
        { value: ethers.parseEther("0.001") }
      );

      expect(await sourceToken.balanceOf(await bridge.getAddress())).to.equal(ethers.parseEther("1000"));
    });

    it("Should handle message routing with callbacks", async function () {
      const { router, relayer, user1, user2 } = await loadFixture(deployCrossChainFixture);

      // Send message
      await router.connect(user1).sendMessage(
        56,
        user2.address,
        ethers.AbiCoder.defaultAbiCoder().encode(["string"], ["Hello cross-chain!"]),
        { value: ethers.parseEther("0.0001") }
      );

      // Relayer delivers
      const msgId = ethers.id("callback-msg-1");
      await router.connect(relayer).deliverMessage(
        msgId,
        1,
        user1.address,
        user2.address,
        ethers.AbiCoder.defaultAbiCoder().encode(["string"], ["Hello cross-chain!"])
      );

      expect(await router.messageDelivered(msgId)).to.be.true;
    });
  });
});
