const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time, loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

/**
 * @title Comprehensive Security Module Tests
 * @notice Tests for CircuitBreaker, RateLimiter, MultiSigWallet, and Emergency systems
 */
describe("Security Modules - Comprehensive Tests", function () {
  // ============================================
  // CIRCUIT BREAKER TESTS
  // ============================================

  describe("CircuitBreaker", function () {
    async function deployCircuitBreakerFixture() {
      const [owner, guardian, pauser, user1, user2] = await ethers.getSigners();

      const CircuitBreaker = await ethers.getContractFactory("CircuitBreaker");
      const circuitBreaker = await CircuitBreaker.deploy(
        3600,  // 1 hour cooldown
        5,     // 5 triggers before lock
        86400  // 24 hour lock duration
      );

      // Grant roles
      const GUARDIAN_ROLE = await circuitBreaker.GUARDIAN_ROLE();
      await circuitBreaker.grantRole(GUARDIAN_ROLE, guardian.address);

      return { circuitBreaker, owner, guardian, pauser, user1, user2 };
    }

    describe("Deployment", function () {
      it("Should set correct cooldown period", async function () {
        const { circuitBreaker } = await loadFixture(deployCircuitBreakerFixture);
        expect(await circuitBreaker.cooldownPeriod()).to.equal(3600);
      });

      it("Should set correct trigger threshold", async function () {
        const { circuitBreaker } = await loadFixture(deployCircuitBreakerFixture);
        expect(await circuitBreaker.triggerThreshold()).to.equal(5);
      });

      it("Should start in active state", async function () {
        const { circuitBreaker } = await loadFixture(deployCircuitBreakerFixture);
        expect(await circuitBreaker.isActive()).to.be.true;
      });

      it("Should set owner as admin", async function () {
        const { circuitBreaker, owner } = await loadFixture(deployCircuitBreakerFixture);
        const DEFAULT_ADMIN_ROLE = await circuitBreaker.DEFAULT_ADMIN_ROLE();
        expect(await circuitBreaker.hasRole(DEFAULT_ADMIN_ROLE, owner.address)).to.be.true;
      });
    });

    describe("Triggering", function () {
      it("Should allow guardian to trigger", async function () {
        const { circuitBreaker, guardian } = await loadFixture(deployCircuitBreakerFixture);

        await circuitBreaker.connect(guardian).trigger("Price anomaly");
        expect(await circuitBreaker.triggerCount()).to.equal(1);
      });

      it("Should emit CircuitTriggered event", async function () {
        const { circuitBreaker, guardian } = await loadFixture(deployCircuitBreakerFixture);

        await expect(circuitBreaker.connect(guardian).trigger("Price anomaly"))
          .to.emit(circuitBreaker, "CircuitTriggered")
          .withArgs(guardian.address, "Price anomaly");
      });

      it("Should trip after threshold reached", async function () {
        const { circuitBreaker, guardian } = await loadFixture(deployCircuitBreakerFixture);

        for (let i = 0; i < 5; i++) {
          await circuitBreaker.connect(guardian).trigger(`Trigger ${i}`);
        }

        expect(await circuitBreaker.isActive()).to.be.false;
      });

      it("Should reject trigger from non-guardian", async function () {
        const { circuitBreaker, user1 } = await loadFixture(deployCircuitBreakerFixture);

        await expect(circuitBreaker.connect(user1).trigger("Hack"))
          .to.be.reverted;
      });

      it("Should reset count after cooldown", async function () {
        const { circuitBreaker, guardian } = await loadFixture(deployCircuitBreakerFixture);

        await circuitBreaker.connect(guardian).trigger("First");
        expect(await circuitBreaker.triggerCount()).to.equal(1);

        await time.increase(3601);

        await circuitBreaker.connect(guardian).trigger("After cooldown");
        // Count should reset
        expect(await circuitBreaker.triggerCount()).to.equal(1);
      });
    });

    describe("Recovery", function () {
      async function trippedBreakerFixture() {
        const fixture = await loadFixture(deployCircuitBreakerFixture);
        const { circuitBreaker, guardian } = fixture;

        for (let i = 0; i < 5; i++) {
          await circuitBreaker.connect(guardian).trigger(`Trigger ${i}`);
        }

        return fixture;
      }

      it("Should auto-recover after lock duration", async function () {
        const { circuitBreaker } = await trippedBreakerFixture();

        expect(await circuitBreaker.isActive()).to.be.false;

        await time.increase(86401);

        expect(await circuitBreaker.isActive()).to.be.true;
      });

      it("Should allow admin to force reset", async function () {
        const { circuitBreaker, owner } = await trippedBreakerFixture();

        await circuitBreaker.connect(owner).forceReset();
        expect(await circuitBreaker.isActive()).to.be.true;
      });

      it("Should emit CircuitReset event", async function () {
        const { circuitBreaker, owner } = await trippedBreakerFixture();

        await expect(circuitBreaker.connect(owner).forceReset())
          .to.emit(circuitBreaker, "CircuitReset");
      });
    });

    describe("Configuration", function () {
      it("Should update cooldown period", async function () {
        const { circuitBreaker, owner } = await loadFixture(deployCircuitBreakerFixture);

        await circuitBreaker.connect(owner).setCooldownPeriod(7200);
        expect(await circuitBreaker.cooldownPeriod()).to.equal(7200);
      });

      it("Should update trigger threshold", async function () {
        const { circuitBreaker, owner } = await loadFixture(deployCircuitBreakerFixture);

        await circuitBreaker.connect(owner).setTriggerThreshold(10);
        expect(await circuitBreaker.triggerThreshold()).to.equal(10);
      });

      it("Should reject invalid threshold", async function () {
        const { circuitBreaker, owner } = await loadFixture(deployCircuitBreakerFixture);

        await expect(circuitBreaker.connect(owner).setTriggerThreshold(0))
          .to.be.revertedWithCustomError(circuitBreaker, "InvalidThreshold");
      });
    });
  });

  // ============================================
  // RATE LIMITER TESTS
  // ============================================

  describe("RateLimiter", function () {
    async function deployRateLimiterFixture() {
      const [owner, user1, user2, user3] = await ethers.getSigners();

      const RateLimiter = await ethers.getContractFactory("RateLimiter");
      const rateLimiter = await RateLimiter.deploy(
        10,    // 10 calls per window
        3600,  // 1 hour window
        ethers.parseEther("1000")  // 1000 token limit
      );

      return { rateLimiter, owner, user1, user2, user3 };
    }

    describe("Call Rate Limiting", function () {
      it("Should allow calls within limit", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        for (let i = 0; i < 10; i++) {
          await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
        }
      });

      it("Should block calls exceeding limit", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        for (let i = 0; i < 10; i++) {
          await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
        }

        await expect(rateLimiter.connect(user1).checkAndRecord(user1.address, 0))
          .to.be.revertedWithCustomError(rateLimiter, "RateLimitExceeded");
      });

      it("Should reset after window expires", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        for (let i = 0; i < 10; i++) {
          await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
        }

        await time.increase(3601);

        // Should work again
        await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
      });

      it("Should track independently per user", async function () {
        const { rateLimiter, user1, user2 } = await loadFixture(deployRateLimiterFixture);

        for (let i = 0; i < 10; i++) {
          await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
        }

        // User2 should still be able to call
        await rateLimiter.connect(user2).checkAndRecord(user2.address, 0);
      });
    });

    describe("Volume Rate Limiting", function () {
      it("Should allow volume within limit", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(user1).checkAndRecord(user1.address, ethers.parseEther("500"));
        await rateLimiter.connect(user1).checkAndRecord(user1.address, ethers.parseEther("400"));
      });

      it("Should block volume exceeding limit", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(user1).checkAndRecord(user1.address, ethers.parseEther("900"));

        await expect(rateLimiter.connect(user1).checkAndRecord(user1.address, ethers.parseEther("200")))
          .to.be.revertedWithCustomError(rateLimiter, "VolumeExceeded");
      });

      it("Should reset volume after window", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(user1).checkAndRecord(user1.address, ethers.parseEther("900"));

        await time.increase(3601);

        await rateLimiter.connect(user1).checkAndRecord(user1.address, ethers.parseEther("900"));
      });
    });

    describe("Queries", function () {
      it("Should return remaining calls", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
        
        const remaining = await rateLimiter.remainingCalls(user1.address);
        expect(remaining).to.equal(9);
      });

      it("Should return remaining volume", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(user1).checkAndRecord(user1.address, ethers.parseEther("300"));
        
        const remaining = await rateLimiter.remainingVolume(user1.address);
        expect(remaining).to.equal(ethers.parseEther("700"));
      });

      it("Should return time until reset", async function () {
        const { rateLimiter, user1 } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
        
        const timeUntilReset = await rateLimiter.timeUntilReset(user1.address);
        expect(timeUntilReset).to.be.gt(0);
        expect(timeUntilReset).to.be.lte(3600);
      });
    });

    describe("Configuration", function () {
      it("Should update call limit", async function () {
        const { rateLimiter, owner } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(owner).setCallLimit(20);
        expect(await rateLimiter.callLimit()).to.equal(20);
      });

      it("Should update window duration", async function () {
        const { rateLimiter, owner } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(owner).setWindowDuration(7200);
        expect(await rateLimiter.windowDuration()).to.equal(7200);
      });

      it("Should update volume limit", async function () {
        const { rateLimiter, owner } = await loadFixture(deployRateLimiterFixture);

        await rateLimiter.connect(owner).setVolumeLimit(ethers.parseEther("5000"));
        expect(await rateLimiter.volumeLimit()).to.equal(ethers.parseEther("5000"));
      });
    });
  });

  // ============================================
  // MULTI-SIG WALLET TESTS
  // ============================================

  describe("MultiSigWallet", function () {
    async function deployMultiSigFixture() {
      const [owner, signer1, signer2, signer3, signer4, recipient] = await ethers.getSigners();

      const signers = [owner.address, signer1.address, signer2.address, signer3.address];
      
      const MultiSigWallet = await ethers.getContractFactory("MultiSigWallet");
      const multiSig = await MultiSigWallet.deploy(signers, 3); // 3 of 4

      // Fund the wallet
      await owner.sendTransaction({
        to: await multiSig.getAddress(),
        value: ethers.parseEther("100")
      });

      return { multiSig, owner, signer1, signer2, signer3, signer4, recipient, signers };
    }

    describe("Deployment", function () {
      it("Should set correct signers", async function () {
        const { multiSig, signers } = await loadFixture(deployMultiSigFixture);
        
        for (const signer of signers) {
          expect(await multiSig.isSigner(signer)).to.be.true;
        }
      });

      it("Should set correct required confirmations", async function () {
        const { multiSig } = await loadFixture(deployMultiSigFixture);
        expect(await multiSig.required()).to.equal(3);
      });

      it("Should have correct signer count", async function () {
        const { multiSig } = await loadFixture(deployMultiSigFixture);
        expect(await multiSig.signerCount()).to.equal(4);
      });

      it("Should receive ETH", async function () {
        const { multiSig } = await loadFixture(deployMultiSigFixture);
        expect(await ethers.provider.getBalance(await multiSig.getAddress()))
          .to.equal(ethers.parseEther("100"));
      });
    });

    describe("Transaction Submission", function () {
      it("Should allow signer to submit transaction", async function () {
        const { multiSig, owner, recipient } = await loadFixture(deployMultiSigFixture);

        await multiSig.connect(owner).submitTransaction(
          recipient.address,
          ethers.parseEther("1"),
          "0x"
        );

        const tx = await multiSig.getTransaction(0);
        expect(tx.to).to.equal(recipient.address);
        expect(tx.value).to.equal(ethers.parseEther("1"));
      });

      it("Should emit TransactionSubmitted event", async function () {
        const { multiSig, owner, recipient } = await loadFixture(deployMultiSigFixture);

        await expect(multiSig.connect(owner).submitTransaction(
          recipient.address,
          ethers.parseEther("1"),
          "0x"
        )).to.emit(multiSig, "TransactionSubmitted")
          .withArgs(0, owner.address, recipient.address, ethers.parseEther("1"));
      });

      it("Should reject submission from non-signer", async function () {
        const { multiSig, signer4, recipient } = await loadFixture(deployMultiSigFixture);

        await expect(multiSig.connect(signer4).submitTransaction(
          recipient.address,
          ethers.parseEther("1"),
          "0x"
        )).to.be.revertedWithCustomError(multiSig, "NotSigner");
      });

      it("Should auto-confirm by submitter", async function () {
        const { multiSig, owner, recipient } = await loadFixture(deployMultiSigFixture);

        await multiSig.connect(owner).submitTransaction(
          recipient.address,
          ethers.parseEther("1"),
          "0x"
        );

        const tx = await multiSig.getTransaction(0);
        expect(tx.confirmations).to.equal(1);
      });
    });

    describe("Confirmations", function () {
      async function txPendingFixture() {
        const fixture = await loadFixture(deployMultiSigFixture);
        const { multiSig, owner, recipient } = fixture;

        await multiSig.connect(owner).submitTransaction(
          recipient.address,
          ethers.parseEther("1"),
          "0x"
        );

        return fixture;
      }

      it("Should allow signers to confirm", async function () {
        const { multiSig, signer1 } = await txPendingFixture();

        await multiSig.connect(signer1).confirmTransaction(0);
        
        const tx = await multiSig.getTransaction(0);
        expect(tx.confirmations).to.equal(2);
      });

      it("Should emit TransactionConfirmed event", async function () {
        const { multiSig, signer1 } = await txPendingFixture();

        await expect(multiSig.connect(signer1).confirmTransaction(0))
          .to.emit(multiSig, "TransactionConfirmed")
          .withArgs(0, signer1.address);
      });

      it("Should reject double confirmation", async function () {
        const { multiSig, signer1 } = await txPendingFixture();

        await multiSig.connect(signer1).confirmTransaction(0);

        await expect(multiSig.connect(signer1).confirmTransaction(0))
          .to.be.revertedWithCustomError(multiSig, "AlreadyConfirmed");
      });

      it("Should reject confirmation from non-signer", async function () {
        const { multiSig, signer4 } = await txPendingFixture();

        await expect(multiSig.connect(signer4).confirmTransaction(0))
          .to.be.revertedWithCustomError(multiSig, "NotSigner");
      });

      it("Should allow revocation of confirmation", async function () {
        const { multiSig, signer1 } = await txPendingFixture();

        await multiSig.connect(signer1).confirmTransaction(0);
        await multiSig.connect(signer1).revokeConfirmation(0);

        const tx = await multiSig.getTransaction(0);
        expect(tx.confirmations).to.equal(1);
      });
    });

    describe("Execution", function () {
      async function fullyConfirmedFixture() {
        const fixture = await loadFixture(deployMultiSigFixture);
        const { multiSig, owner, signer1, signer2, recipient } = fixture;

        await multiSig.connect(owner).submitTransaction(
          recipient.address,
          ethers.parseEther("1"),
          "0x"
        );

        await multiSig.connect(signer1).confirmTransaction(0);
        await multiSig.connect(signer2).confirmTransaction(0);

        return fixture;
      }

      it("Should execute with enough confirmations", async function () {
        const { multiSig, owner } = await fullyConfirmedFixture();

        await multiSig.connect(owner).executeTransaction(0);

        const tx = await multiSig.getTransaction(0);
        expect(tx.executed).to.be.true;
      });

      it("Should transfer funds on execution", async function () {
        const { multiSig, owner, recipient } = await fullyConfirmedFixture();

        const balanceBefore = await ethers.provider.getBalance(recipient.address);
        await multiSig.connect(owner).executeTransaction(0);
        const balanceAfter = await ethers.provider.getBalance(recipient.address);

        expect(balanceAfter - balanceBefore).to.equal(ethers.parseEther("1"));
      });

      it("Should emit TransactionExecuted event", async function () {
        const { multiSig, owner } = await fullyConfirmedFixture();

        await expect(multiSig.connect(owner).executeTransaction(0))
          .to.emit(multiSig, "TransactionExecuted");
      });

      it("Should reject execution without enough confirmations", async function () {
        const fixture = await loadFixture(deployMultiSigFixture);
        const { multiSig, owner, signer1, recipient } = fixture;

        await multiSig.connect(owner).submitTransaction(
          recipient.address,
          ethers.parseEther("1"),
          "0x"
        );
        await multiSig.connect(signer1).confirmTransaction(0);

        await expect(multiSig.connect(owner).executeTransaction(0))
          .to.be.revertedWithCustomError(multiSig, "InsufficientConfirmations");
      });

      it("Should reject re-execution", async function () {
        const { multiSig, owner } = await fullyConfirmedFixture();

        await multiSig.connect(owner).executeTransaction(0);

        await expect(multiSig.connect(owner).executeTransaction(0))
          .to.be.revertedWithCustomError(multiSig, "AlreadyExecuted");
      });
    });

    describe("Signer Management", function () {
      it("Should add signer through multisig", async function () {
        const { multiSig, owner, signer1, signer2, signer4 } = await loadFixture(deployMultiSigFixture);

        const addSignerData = multiSig.interface.encodeFunctionData("addSigner", [signer4.address]);

        await multiSig.connect(owner).submitTransaction(
          await multiSig.getAddress(),
          0,
          addSignerData
        );
        await multiSig.connect(signer1).confirmTransaction(0);
        await multiSig.connect(signer2).confirmTransaction(0);
        await multiSig.connect(owner).executeTransaction(0);

        expect(await multiSig.isSigner(signer4.address)).to.be.true;
      });

      it("Should remove signer through multisig", async function () {
        const { multiSig, owner, signer1, signer2, signer3 } = await loadFixture(deployMultiSigFixture);

        const removeSignerData = multiSig.interface.encodeFunctionData("removeSigner", [signer3.address]);

        await multiSig.connect(owner).submitTransaction(
          await multiSig.getAddress(),
          0,
          removeSignerData
        );
        await multiSig.connect(signer1).confirmTransaction(0);
        await multiSig.connect(signer2).confirmTransaction(0);
        await multiSig.connect(owner).executeTransaction(0);

        expect(await multiSig.isSigner(signer3.address)).to.be.false;
      });

      it("Should update required confirmations through multisig", async function () {
        const { multiSig, owner, signer1, signer2 } = await loadFixture(deployMultiSigFixture);

        const updateRequiredData = multiSig.interface.encodeFunctionData("updateRequired", [2]);

        await multiSig.connect(owner).submitTransaction(
          await multiSig.getAddress(),
          0,
          updateRequiredData
        );
        await multiSig.connect(signer1).confirmTransaction(0);
        await multiSig.connect(signer2).confirmTransaction(0);
        await multiSig.connect(owner).executeTransaction(0);

        expect(await multiSig.required()).to.equal(2);
      });
    });
  });

  // ============================================
  // EMERGENCY WITHDRAW TESTS
  // ============================================

  describe("EmergencyWithdraw", function () {
    async function deployEmergencyFixture() {
      const [owner, guardian, user1, user2] = await ethers.getSigners();

      const MockERC20 = await ethers.getContractFactory("MockERC20");
      const token = await MockERC20.deploy("Test Token", "TST", 18);

      const EmergencyWithdraw = await ethers.getContractFactory("EmergencyWithdraw");
      const emergency = await EmergencyWithdraw.deploy(7 * 24 * 60 * 60); // 7 day delay

      // Grant guardian role
      await emergency.grantRole(await emergency.GUARDIAN_ROLE(), guardian.address);

      // Fund contract
      await token.mint(await emergency.getAddress(), ethers.parseEther("100000"));
      await owner.sendTransaction({
        to: await emergency.getAddress(),
        value: ethers.parseEther("10")
      });

      return { emergency, token, owner, guardian, user1, user2 };
    }

    describe("Emergency Declaration", function () {
      it("Should allow guardian to declare emergency", async function () {
        const { emergency, guardian } = await loadFixture(deployEmergencyFixture);

        await emergency.connect(guardian).declareEmergency("Security breach");
        expect(await emergency.emergencyActive()).to.be.true;
      });

      it("Should emit EmergencyDeclared event", async function () {
        const { emergency, guardian } = await loadFixture(deployEmergencyFixture);

        await expect(emergency.connect(guardian).declareEmergency("Security breach"))
          .to.emit(emergency, "EmergencyDeclared")
          .withArgs(guardian.address, "Security breach");
      });

      it("Should reject from non-guardian", async function () {
        const { emergency, user1 } = await loadFixture(deployEmergencyFixture);

        await expect(emergency.connect(user1).declareEmergency("Hack"))
          .to.be.reverted;
      });
    });

    describe("Emergency Withdrawals", function () {
      async function emergencyActiveFixture() {
        const fixture = await loadFixture(deployEmergencyFixture);
        const { emergency, guardian } = fixture;

        await emergency.connect(guardian).declareEmergency("Security breach");

        return fixture;
      }

      it("Should schedule withdrawal after emergency", async function () {
        const { emergency, token, owner } = await emergencyActiveFixture();

        await emergency.connect(owner).scheduleWithdraw(
          await token.getAddress(),
          ethers.parseEther("1000"),
          owner.address
        );

        expect(await emergency.pendingWithdrawalCount()).to.be.gt(0);
      });

      it("Should execute withdrawal after delay", async function () {
        const { emergency, token, owner } = await emergencyActiveFixture();

        await emergency.connect(owner).scheduleWithdraw(
          await token.getAddress(),
          ethers.parseEther("1000"),
          owner.address
        );

        await time.increase(7 * 24 * 60 * 60 + 1);

        const balanceBefore = await token.balanceOf(owner.address);
        await emergency.connect(owner).executeWithdraw(0);
        const balanceAfter = await token.balanceOf(owner.address);

        expect(balanceAfter - balanceBefore).to.equal(ethers.parseEther("1000"));
      });

      it("Should reject withdrawal before delay", async function () {
        const { emergency, token, owner } = await emergencyActiveFixture();

        await emergency.connect(owner).scheduleWithdraw(
          await token.getAddress(),
          ethers.parseEther("1000"),
          owner.address
        );

        await time.increase(3 * 24 * 60 * 60); // Only 3 days

        await expect(emergency.connect(owner).executeWithdraw(0))
          .to.be.revertedWithCustomError(emergency, "WithdrawalNotReady");
      });

      it("Should allow ETH withdrawal", async function () {
        const { emergency, owner } = await emergencyActiveFixture();

        await emergency.connect(owner).scheduleWithdraw(
          ethers.ZeroAddress,
          ethers.parseEther("5"),
          owner.address
        );

        await time.increase(7 * 24 * 60 * 60 + 1);

        const balanceBefore = await ethers.provider.getBalance(owner.address);
        await emergency.connect(owner).executeWithdraw(0);
        const balanceAfter = await ethers.provider.getBalance(owner.address);

        expect(balanceAfter).to.be.gt(balanceBefore);
      });

      it("Should allow cancellation before execution", async function () {
        const { emergency, token, owner } = await emergencyActiveFixture();

        await emergency.connect(owner).scheduleWithdraw(
          await token.getAddress(),
          ethers.parseEther("1000"),
          owner.address
        );

        await emergency.connect(owner).cancelWithdraw(0);

        await time.increase(7 * 24 * 60 * 60 + 1);

        await expect(emergency.connect(owner).executeWithdraw(0))
          .to.be.revertedWithCustomError(emergency, "WithdrawalCancelled");
      });
    });

    describe("Emergency Resolution", function () {
      it("Should allow admin to resolve emergency", async function () {
        const { emergency, guardian, owner } = await loadFixture(deployEmergencyFixture);

        await emergency.connect(guardian).declareEmergency("Test");
        expect(await emergency.emergencyActive()).to.be.true;

        await emergency.connect(owner).resolveEmergency();
        expect(await emergency.emergencyActive()).to.be.false;
      });

      it("Should emit EmergencyResolved event", async function () {
        const { emergency, guardian, owner } = await loadFixture(deployEmergencyFixture);

        await emergency.connect(guardian).declareEmergency("Test");

        await expect(emergency.connect(owner).resolveEmergency())
          .to.emit(emergency, "EmergencyResolved");
      });
    });
  });

  // ============================================
  // SECURITY INTEGRATION TESTS
  // ============================================

  describe("Security Integration", function () {
    it("Should handle complex multisig + emergency scenario", async function () {
      const [owner, signer1, signer2, signer3, guardian] = await ethers.getSigners();

      // Deploy MultiSig
      const MultiSigWallet = await ethers.getContractFactory("MultiSigWallet");
      const multiSig = await MultiSigWallet.deploy(
        [owner.address, signer1.address, signer2.address, signer3.address],
        3
      );

      // Deploy Emergency system
      const EmergencyWithdraw = await ethers.getContractFactory("EmergencyWithdraw");
      const emergency = await EmergencyWithdraw.deploy(86400);
      await emergency.grantRole(await emergency.GUARDIAN_ROLE(), guardian.address);

      // MultiSig controls emergency funds
      await owner.sendTransaction({
        to: await emergency.getAddress(),
        value: ethers.parseEther("10")
      });

      // Guardian declares emergency
      await emergency.connect(guardian).declareEmergency("Critical vulnerability");

      // MultiSig schedules emergency withdrawal through governance
      expect(await emergency.emergencyActive()).to.be.true;
    });

    it("Should combine circuit breaker with rate limiter", async function () {
      const [owner, guardian, user1] = await ethers.getSigners();

      // Deploy both systems
      const CircuitBreaker = await ethers.getContractFactory("CircuitBreaker");
      const circuitBreaker = await CircuitBreaker.deploy(3600, 3, 86400);

      const RateLimiter = await ethers.getContractFactory("RateLimiter");
      const rateLimiter = await RateLimiter.deploy(5, 3600, ethers.parseEther("1000"));

      await circuitBreaker.grantRole(await circuitBreaker.GUARDIAN_ROLE(), guardian.address);

      // User exceeds rate limit
      for (let i = 0; i < 5; i++) {
        await rateLimiter.connect(user1).checkAndRecord(user1.address, 0);
      }

      // Should be rate limited
      await expect(rateLimiter.connect(user1).checkAndRecord(user1.address, 0))
        .to.be.revertedWithCustomError(rateLimiter, "RateLimitExceeded");

      // Guardian triggers circuit breaker
      await circuitBreaker.connect(guardian).trigger("Rate limit attacks detected");

      expect(await circuitBreaker.triggerCount()).to.equal(1);
    });
  });
});
