const { expect } = require("chai");
const { ethers } = require("hardhat");
const { time, loadFixture } = require("@nomicfoundation/hardhat-network-helpers");

/**
 * @title Comprehensive Governance & DAO Tests
 * @notice Tests for Governor, Timelock, Treasury, and Voting systems
 */
describe("Governance Modules - Comprehensive Tests", function () {
  // ============================================
  // FIXTURES
  // ============================================

  async function deployGovernanceFixture() {
    const [owner, proposer, voter1, voter2, voter3, executor] = await ethers.getSigners();

    // Deploy governance token
    const GreyToken = await ethers.getContractFactory("GreyToken");
    const token = await GreyToken.deploy(
      "Grey Governance",
      "GREY",
      ethers.parseEther("10000000"),
      owner.address
    );

    // Deploy timelock
    const GreyTimelock = await ethers.getContractFactory("GreyTimelock");
    const timelock = await GreyTimelock.deploy(
      86400,       // 1 day min delay
      [proposer.address],
      [executor.address],
      owner.address
    );

    // Deploy governor
    const GreyGovernor = await ethers.getContractFactory("GreyGovernor");
    const governor = await GreyGovernor.deploy(
      await token.getAddress(),
      await timelock.getAddress(),
      7200,     // 1 day voting delay
      50400,    // 1 week voting period
      ethers.parseEther("100000"),  // proposal threshold
      4         // 4% quorum
    );

    // Setup roles
    const PROPOSER_ROLE = await timelock.PROPOSER_ROLE();
    const EXECUTOR_ROLE = await timelock.EXECUTOR_ROLE();
    await timelock.grantRole(PROPOSER_ROLE, await governor.getAddress());
    await timelock.grantRole(EXECUTOR_ROLE, await governor.getAddress());

    // Distribute tokens and delegate
    await token.transfer(voter1.address, ethers.parseEther("500000"));
    await token.transfer(voter2.address, ethers.parseEther("300000"));
    await token.transfer(voter3.address, ethers.parseEther("200000"));
    await token.transfer(proposer.address, ethers.parseEther("100000"));

    await token.connect(voter1).delegate(voter1.address);
    await token.connect(voter2).delegate(voter2.address);
    await token.connect(voter3).delegate(voter3.address);
    await token.connect(proposer).delegate(proposer.address);

    // Deploy Treasury
    const Treasury = await ethers.getContractFactory("Treasury");
    const treasury = await Treasury.deploy(await timelock.getAddress());

    // Fund treasury
    await owner.sendTransaction({
      to: await treasury.getAddress(),
      value: ethers.parseEther("100")
    });

    return {
      token,
      timelock,
      governor,
      treasury,
      owner,
      proposer,
      voter1,
      voter2,
      voter3,
      executor
    };
  }

  // ============================================
  // GREY TOKEN (GOVERNANCE) TESTS
  // ============================================

  describe("GreyToken (Governance)", function () {
    describe("Delegation", function () {
      it("Should allow self-delegation", async function () {
        const { token, voter1 } = await loadFixture(deployGovernanceFixture);
        
        expect(await token.delegates(voter1.address)).to.equal(voter1.address);
      });

      it("Should track voting power", async function () {
        const { token, voter1 } = await loadFixture(deployGovernanceFixture);
        
        const votes = await token.getVotes(voter1.address);
        expect(votes).to.equal(ethers.parseEther("500000"));
      });

      it("Should allow delegation to another", async function () {
        const { token, voter1, voter2 } = await loadFixture(deployGovernanceFixture);
        
        await token.connect(voter1).delegate(voter2.address);
        
        const voter2Votes = await token.getVotes(voter2.address);
        expect(voter2Votes).to.equal(ethers.parseEther("800000")); // 500k + 300k
      });

      it("Should emit DelegateChanged event", async function () {
        const { token, voter1, voter2 } = await loadFixture(deployGovernanceFixture);
        
        await expect(token.connect(voter1).delegate(voter2.address))
          .to.emit(token, "DelegateChanged")
          .withArgs(voter1.address, voter1.address, voter2.address);
      });

      it("Should track historical votes", async function () {
        const { token, voter1 } = await loadFixture(deployGovernanceFixture);
        
        const blockNumber = await ethers.provider.getBlockNumber();
        
        // Mine a block
        await ethers.provider.send("evm_mine", []);
        
        const pastVotes = await token.getPastVotes(voter1.address, blockNumber);
        expect(pastVotes).to.equal(ethers.parseEther("500000"));
      });
    });

    describe("Checkpointing", function () {
      it("Should create checkpoint on delegate", async function () {
        const { token, voter1 } = await loadFixture(deployGovernanceFixture);
        
        const numCheckpoints = await token.numCheckpoints(voter1.address);
        expect(numCheckpoints).to.be.gte(1);
      });

      it("Should update checkpoint on transfer", async function () {
        const { token, voter1, voter2 } = await loadFixture(deployGovernanceFixture);
        
        await token.connect(voter1).transfer(voter2.address, ethers.parseEther("100000"));
        
        expect(await token.getVotes(voter1.address)).to.equal(ethers.parseEther("400000"));
        expect(await token.getVotes(voter2.address)).to.equal(ethers.parseEther("400000"));
      });
    });
  });

  // ============================================
  // GREY TIMELOCK TESTS
  // ============================================

  describe("GreyTimelock", function () {
    describe("Scheduling", function () {
      it("Should schedule operation", async function () {
        const { timelock, proposer, voter1 } = await loadFixture(deployGovernanceFixture);
        
        const target = voter1.address;
        const value = ethers.parseEther("1");
        const data = "0x";
        const predecessor = ethers.ZeroHash;
        const salt = ethers.id("test");
        const delay = 86400;

        await timelock.connect(proposer).schedule(
          target, value, data, predecessor, salt, delay
        );

        const opId = await timelock.hashOperation(target, value, data, predecessor, salt);
        expect(await timelock.isOperationPending(opId)).to.be.true;
      });

      it("Should respect minimum delay", async function () {
        const { timelock, proposer, voter1 } = await loadFixture(deployGovernanceFixture);
        
        await expect(timelock.connect(proposer).schedule(
          voter1.address,
          0,
          "0x",
          ethers.ZeroHash,
          ethers.id("test"),
          3600  // Too short
        )).to.be.revertedWithCustomError(timelock, "TimelockInsufficientDelay");
      });

      it("Should emit CallScheduled event", async function () {
        const { timelock, proposer, voter1 } = await loadFixture(deployGovernanceFixture);
        
        await expect(timelock.connect(proposer).schedule(
          voter1.address,
          0,
          "0x",
          ethers.ZeroHash,
          ethers.id("test"),
          86400
        )).to.emit(timelock, "CallScheduled");
      });
    });

    describe("Execution", function () {
      async function scheduledOperationFixture() {
        const fixture = await loadFixture(deployGovernanceFixture);
        const { timelock, proposer, voter1 } = fixture;

        await timelock.connect(proposer).schedule(
          voter1.address,
          0,
          "0x",
          ethers.ZeroHash,
          ethers.id("test"),
          86400
        );

        return fixture;
      }

      it("Should execute after delay", async function () {
        const { timelock, executor, voter1 } = await scheduledOperationFixture();
        
        await time.increase(86401);

        await timelock.connect(executor).execute(
          voter1.address,
          0,
          "0x",
          ethers.ZeroHash,
          ethers.id("test")
        );

        const opId = await timelock.hashOperation(
          voter1.address, 0, "0x", ethers.ZeroHash, ethers.id("test")
        );
        expect(await timelock.isOperationDone(opId)).to.be.true;
      });

      it("Should reject execution before delay", async function () {
        const { timelock, executor, voter1 } = await scheduledOperationFixture();
        
        await expect(timelock.connect(executor).execute(
          voter1.address,
          0,
          "0x",
          ethers.ZeroHash,
          ethers.id("test")
        )).to.be.revertedWithCustomError(timelock, "TimelockUnexpectedOperationState");
      });

      it("Should emit CallExecuted event", async function () {
        const { timelock, executor, voter1 } = await scheduledOperationFixture();
        
        await time.increase(86401);

        await expect(timelock.connect(executor).execute(
          voter1.address,
          0,
          "0x",
          ethers.ZeroHash,
          ethers.id("test")
        )).to.emit(timelock, "CallExecuted");
      });
    });

    describe("Cancellation", function () {
      it("Should allow canceller to cancel", async function () {
        const { timelock, proposer, voter1, owner } = await loadFixture(deployGovernanceFixture);
        
        await timelock.connect(proposer).schedule(
          voter1.address,
          0,
          "0x",
          ethers.ZeroHash,
          ethers.id("test"),
          86400
        );

        const opId = await timelock.hashOperation(
          voter1.address, 0, "0x", ethers.ZeroHash, ethers.id("test")
        );

        await timelock.connect(owner).cancel(opId);
        
        expect(await timelock.isOperation(opId)).to.be.false;
      });
    });

    describe("Batch Operations", function () {
      it("Should schedule batch", async function () {
        const { timelock, proposer, voter1, voter2 } = await loadFixture(deployGovernanceFixture);
        
        await timelock.connect(proposer).scheduleBatch(
          [voter1.address, voter2.address],
          [0, 0],
          ["0x", "0x"],
          ethers.ZeroHash,
          ethers.id("batch"),
          86400
        );
      });

      it("Should execute batch", async function () {
        const { timelock, proposer, executor, voter1, voter2 } = await loadFixture(deployGovernanceFixture);
        
        await timelock.connect(proposer).scheduleBatch(
          [voter1.address, voter2.address],
          [0, 0],
          ["0x", "0x"],
          ethers.ZeroHash,
          ethers.id("batch"),
          86400
        );

        await time.increase(86401);

        await timelock.connect(executor).executeBatch(
          [voter1.address, voter2.address],
          [0, 0],
          ["0x", "0x"],
          ethers.ZeroHash,
          ethers.id("batch")
        );
      });
    });
  });

  // ============================================
  // GREY GOVERNOR TESTS
  // ============================================

  describe("GreyGovernor", function () {
    describe("Proposal Creation", function () {
      it("Should create proposal", async function () {
        const { governor, treasury, proposer } = await loadFixture(deployGovernanceFixture);
        
        const targets = [await treasury.getAddress()];
        const values = [0];
        const calldatas = [treasury.interface.encodeFunctionData("withdrawETH", [
          ethers.parseEther("1"),
          proposer.address
        ])];
        const description = "Withdraw 1 ETH";

        await governor.connect(proposer).propose(targets, values, calldatas, description);
        
        const proposalId = await governor.hashProposal(targets, values, calldatas, ethers.id(description));
        expect(await governor.state(proposalId)).to.equal(0); // Pending
      });

      it("Should emit ProposalCreated event", async function () {
        const { governor, treasury, proposer } = await loadFixture(deployGovernanceFixture);
        
        const targets = [await treasury.getAddress()];
        const values = [0];
        const calldatas = [treasury.interface.encodeFunctionData("withdrawETH", [
          ethers.parseEther("1"),
          proposer.address
        ])];

        await expect(governor.connect(proposer).propose(targets, values, calldatas, "Test"))
          .to.emit(governor, "ProposalCreated");
      });

      it("Should reject proposal below threshold", async function () {
        const { governor, treasury, voter3 } = await loadFixture(deployGovernanceFixture);
        
        // voter3 doesn't have enough tokens
        await expect(governor.connect(voter3).propose(
          [await treasury.getAddress()],
          [0],
          ["0x"],
          "Test"
        )).to.be.revertedWithCustomError(governor, "GovernorInsufficientProposerVotes");
      });
    });

    describe("Voting", function () {
      async function activeProposalFixture() {
        const fixture = await loadFixture(deployGovernanceFixture);
        const { governor, treasury, proposer } = fixture;

        const targets = [await treasury.getAddress()];
        const values = [0];
        const calldatas = [treasury.interface.encodeFunctionData("withdrawETH", [
          ethers.parseEther("1"),
          proposer.address
        ])];
        const description = "Withdraw 1 ETH";

        await governor.connect(proposer).propose(targets, values, calldatas, description);

        const proposalId = await governor.hashProposal(targets, values, calldatas, ethers.id(description));

        // Move past voting delay
        await time.increase(7201);

        return { ...fixture, proposalId, targets, values, calldatas, description };
      }

      it("Should allow voting", async function () {
        const { governor, voter1, proposalId } = await activeProposalFixture();
        
        await governor.connect(voter1).castVote(proposalId, 1); // For

        expect(await governor.hasVoted(proposalId, voter1.address)).to.be.true;
      });

      it("Should emit VoteCast event", async function () {
        const { governor, voter1, proposalId } = await activeProposalFixture();
        
        await expect(governor.connect(voter1).castVote(proposalId, 1))
          .to.emit(governor, "VoteCast");
      });

      it("Should allow voting with reason", async function () {
        const { governor, voter1, proposalId } = await activeProposalFixture();
        
        await governor.connect(voter1).castVoteWithReason(proposalId, 1, "Good proposal");
      });

      it("Should reject double voting", async function () {
        const { governor, voter1, proposalId } = await activeProposalFixture();
        
        await governor.connect(voter1).castVote(proposalId, 1);
        
        await expect(governor.connect(voter1).castVote(proposalId, 1))
          .to.be.revertedWithCustomError(governor, "GovernorAlreadyCastVote");
      });

      it("Should track vote counts", async function () {
        const { governor, voter1, voter2, voter3, proposalId } = await activeProposalFixture();
        
        await governor.connect(voter1).castVote(proposalId, 1); // For
        await governor.connect(voter2).castVote(proposalId, 0); // Against
        await governor.connect(voter3).castVote(proposalId, 2); // Abstain

        const [against, forVotes, abstain] = await governor.proposalVotes(proposalId);
        
        expect(forVotes).to.equal(ethers.parseEther("500000"));
        expect(against).to.equal(ethers.parseEther("300000"));
        expect(abstain).to.equal(ethers.parseEther("200000"));
      });
    });

    describe("Proposal States", function () {
      async function votedProposalFixture() {
        const fixture = await activeProposalFixture();
        const { governor, voter1, voter2, proposalId } = fixture;

        // Vote for success
        await governor.connect(voter1).castVote(proposalId, 1);
        await governor.connect(voter2).castVote(proposalId, 1);

        return fixture;
      }

      async function activeProposalFixture() {
        const fixture = await loadFixture(deployGovernanceFixture);
        const { governor, treasury, proposer } = fixture;

        const targets = [await treasury.getAddress()];
        const values = [0];
        const calldatas = [treasury.interface.encodeFunctionData("withdrawETH", [
          ethers.parseEther("1"),
          proposer.address
        ])];
        const description = "Withdraw 1 ETH";

        await governor.connect(proposer).propose(targets, values, calldatas, description);
        const proposalId = await governor.hashProposal(targets, values, calldatas, ethers.id(description));

        await time.increase(7201);

        return { ...fixture, proposalId, targets, values, calldatas, description };
      }

      it("Should move to Active state", async function () {
        const { governor, proposalId } = await activeProposalFixture();
        
        expect(await governor.state(proposalId)).to.equal(1); // Active
      });

      it("Should move to Succeeded state", async function () {
        const { governor, proposalId } = await votedProposalFixture();
        
        await time.increase(50401);
        
        expect(await governor.state(proposalId)).to.equal(4); // Succeeded
      });

      it("Should move to Defeated if no quorum", async function () {
        const { governor, voter3, proposalId } = await activeProposalFixture();
        
        // Only small voter votes
        await governor.connect(voter3).castVote(proposalId, 1);
        
        await time.increase(50401);
        
        expect(await governor.state(proposalId)).to.equal(3); // Defeated
      });
    });

    describe("Proposal Execution", function () {
      async function queuedProposalFixture() {
        const fixture = await loadFixture(deployGovernanceFixture);
        const { governor, treasury, proposer, voter1, voter2 } = fixture;

        const targets = [await treasury.getAddress()];
        const values = [0];
        const calldatas = [treasury.interface.encodeFunctionData("withdrawETH", [
          ethers.parseEther("1"),
          proposer.address
        ])];
        const description = "Withdraw 1 ETH";

        await governor.connect(proposer).propose(targets, values, calldatas, description);
        const proposalId = await governor.hashProposal(targets, values, calldatas, ethers.id(description));

        await time.increase(7201);
        
        await governor.connect(voter1).castVote(proposalId, 1);
        await governor.connect(voter2).castVote(proposalId, 1);

        await time.increase(50401);

        await governor.connect(proposer).queue(targets, values, calldatas, ethers.id(description));

        return { ...fixture, proposalId, targets, values, calldatas, description };
      }

      it("Should queue succeeded proposal", async function () {
        const { governor, proposalId } = await queuedProposalFixture();
        
        expect(await governor.state(proposalId)).to.equal(5); // Queued
      });

      it("Should execute after timelock delay", async function () {
        const { governor, proposalId, targets, values, calldatas, description, executor } = 
          await queuedProposalFixture();
        
        await time.increase(86401);

        await governor.connect(executor).execute(targets, values, calldatas, ethers.id(description));

        expect(await governor.state(proposalId)).to.equal(7); // Executed
      });
    });
  });

  // ============================================
  // TREASURY TESTS
  // ============================================

  describe("Treasury", function () {
    describe("ETH Management", function () {
      it("Should receive ETH", async function () {
        const { treasury } = await loadFixture(deployGovernanceFixture);
        
        expect(await ethers.provider.getBalance(await treasury.getAddress()))
          .to.equal(ethers.parseEther("100"));
      });

      it("Should only allow timelock to withdraw", async function () {
        const { treasury, voter1 } = await loadFixture(deployGovernanceFixture);
        
        await expect(treasury.connect(voter1).withdrawETH(
          ethers.parseEther("1"),
          voter1.address
        )).to.be.reverted;
      });
    });

    describe("Token Management", function () {
      async function treasuryWithTokensFixture() {
        const fixture = await loadFixture(deployGovernanceFixture);
        const { treasury, owner } = fixture;

        const MockERC20 = await ethers.getContractFactory("MockERC20");
        const mockToken = await MockERC20.deploy("Mock", "MCK", 18);
        await mockToken.mint(await treasury.getAddress(), ethers.parseEther("10000"));

        return { ...fixture, mockToken };
      }

      it("Should hold ERC20 tokens", async function () {
        const { treasury, mockToken } = await treasuryWithTokensFixture();
        
        expect(await mockToken.balanceOf(await treasury.getAddress()))
          .to.equal(ethers.parseEther("10000"));
      });
    });
  });

  // ============================================
  // GOVERNANCE INTEGRATION TESTS
  // ============================================

  describe("Governance Integration", function () {
    it("Should execute full governance cycle", async function () {
      const {
        governor,
        treasury,
        timelock,
        proposer,
        voter1,
        voter2,
        executor
      } = await loadFixture(deployGovernanceFixture);

      // Create proposal
      const targets = [await treasury.getAddress()];
      const values = [0];
      const calldatas = [treasury.interface.encodeFunctionData("withdrawETH", [
        ethers.parseEther("1"),
        proposer.address
      ])];
      const description = "Withdraw 1 ETH for development";

      await governor.connect(proposer).propose(targets, values, calldatas, description);
      const proposalId = await governor.hashProposal(targets, values, calldatas, ethers.id(description));

      // Wait for voting delay
      await time.increase(7201);
      expect(await governor.state(proposalId)).to.equal(1); // Active

      // Vote
      await governor.connect(voter1).castVote(proposalId, 1);
      await governor.connect(voter2).castVote(proposalId, 1);

      // Wait for voting period
      await time.increase(50401);
      expect(await governor.state(proposalId)).to.equal(4); // Succeeded

      // Queue
      await governor.connect(proposer).queue(targets, values, calldatas, ethers.id(description));
      expect(await governor.state(proposalId)).to.equal(5); // Queued

      // Wait for timelock delay
      await time.increase(86401);

      // Execute
      const balanceBefore = await ethers.provider.getBalance(proposer.address);
      await governor.connect(executor).execute(targets, values, calldatas, ethers.id(description));

      expect(await governor.state(proposalId)).to.equal(7); // Executed
    });

    it("Should handle proposal defeat", async function () {
      const { governor, treasury, proposer, voter1, voter2 } = await loadFixture(deployGovernanceFixture);

      const targets = [await treasury.getAddress()];
      const values = [0];
      const calldatas = ["0x"];
      const description = "Bad proposal";

      await governor.connect(proposer).propose(targets, values, calldatas, description);
      const proposalId = await governor.hashProposal(targets, values, calldatas, ethers.id(description));

      await time.increase(7201);

      // Vote against
      await governor.connect(voter1).castVote(proposalId, 0);
      await governor.connect(voter2).castVote(proposalId, 0);

      await time.increase(50401);
      
      expect(await governor.state(proposalId)).to.equal(3); // Defeated
    });

    it("Should handle multiple concurrent proposals", async function () {
      const { governor, treasury, proposer, voter1, voter2 } = await loadFixture(deployGovernanceFixture);

      // Create multiple proposals
      const proposals = [];
      for (let i = 0; i < 3; i++) {
        const description = `Proposal ${i}`;
        await governor.connect(proposer).propose(
          [await treasury.getAddress()],
          [0],
          ["0x"],
          description
        );
        const proposalId = await governor.hashProposal(
          [await treasury.getAddress()],
          [0],
          ["0x"],
          ethers.id(description)
        );
        proposals.push(proposalId);
      }

      await time.increase(7201);

      // Vote on all
      for (const proposalId of proposals) {
        await governor.connect(voter1).castVote(proposalId, 1);
      }

      // All should be active
      for (const proposalId of proposals) {
        expect(await governor.state(proposalId)).to.equal(1);
      }
    });
  });
});
