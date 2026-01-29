const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("Registry", function () {
  let accessControl;
  let registry;
  let owner;
  let user1;
  let user2;
  let nonUser;
  let ADMIN_ROLE;
  let USER_ROLE;

  beforeEach(async function () {
    [owner, user1, user2, nonUser] = await ethers.getSigners();

    // Deploy AccessControlManager
    const AccessControlManager = await ethers.getContractFactory("AccessControlManager");
    accessControl = await AccessControlManager.deploy();
    await accessControl.waitForDeployment();

    ADMIN_ROLE = await accessControl.ADMIN_ROLE();
    USER_ROLE = await accessControl.USER_ROLE();

    // Grant USER_ROLE to user1 and user2
    await accessControl.grantRole(USER_ROLE, user1.address);
    await accessControl.grantRole(USER_ROLE, user2.address);

    // Deploy Registry
    const Registry = await ethers.getContractFactory("Registry");
    registry = await Registry.deploy(await accessControl.getAddress());
    await registry.waitForDeployment();
  });

  describe("Deployment", function () {
    it("Should set correct access control reference", async function () {
      expect(await registry.accessControl()).to.equal(await accessControl.getAddress());
    });

    it("Should revert with zero address for access control", async function () {
      const Registry = await ethers.getContractFactory("Registry");
      await expect(Registry.deploy(ethers.ZeroAddress))
        .to.be.revertedWith("Registry: invalid access control address");
    });
  });

  describe("Record Creation", function () {
    it("Should allow user to create a record", async function () {
      await expect(registry.connect(user1).createRecord("Test Record", "Test Data"))
        .to.emit(registry, "RecordCreated")
        .withArgs(1, "Test Record", user1.address);

      const record = await registry.getRecord(1);
      expect(record.name).to.equal("Test Record");
      expect(record.data).to.equal("Test Data");
      expect(record.owner).to.equal(user1.address);
      expect(record.active).to.be.true;
    });

    it("Should allow admin to create a record", async function () {
      await expect(registry.connect(owner).createRecord("Admin Record", "Admin Data"))
        .to.emit(registry, "RecordCreated")
        .withArgs(1, "Admin Record", owner.address);
    });

    it("Should revert when non-user tries to create record", async function () {
      await expect(
        registry.connect(nonUser).createRecord("Test", "Data")
      ).to.be.revertedWith("Registry: caller is not a user or admin");
    });

    it("Should revert with empty name", async function () {
      await expect(
        registry.connect(user1).createRecord("", "Data")
      ).to.be.revertedWithCustomError(registry, "EmptyName");
    });

    it("Should increment record IDs correctly", async function () {
      await registry.connect(user1).createRecord("Record 1", "Data 1");
      await registry.connect(user1).createRecord("Record 2", "Data 2");
      await registry.connect(user2).createRecord("Record 3", "Data 3");

      expect(await registry.getTotalRecords()).to.equal(3);
      
      const allIds = await registry.getAllRecordIds();
      expect(allIds.length).to.equal(3);
    });
  });

  describe("Record Updates", function () {
    beforeEach(async function () {
      await registry.connect(user1).createRecord("Original Name", "Original Data");
    });

    it("Should allow owner to update their record", async function () {
      await expect(registry.connect(user1).updateRecord(1, "New Name", "New Data"))
        .to.emit(registry, "RecordUpdated")
        .withArgs(1, "New Name", "New Data");

      const record = await registry.getRecord(1);
      expect(record.name).to.equal("New Name");
      expect(record.data).to.equal("New Data");
    });

    it("Should allow admin to update any record", async function () {
      await registry.connect(owner).updateRecord(1, "Admin Updated", "Admin Data");
      
      const record = await registry.getRecord(1);
      expect(record.name).to.equal("Admin Updated");
    });

    it("Should revert when non-owner/non-admin updates record", async function () {
      await expect(
        registry.connect(user2).updateRecord(1, "New Name", "New Data")
      ).to.be.revertedWithCustomError(registry, "NotRecordOwner");
    });

    it("Should revert when updating non-existent record", async function () {
      await expect(
        registry.connect(user1).updateRecord(999, "Name", "Data")
      ).to.be.revertedWithCustomError(registry, "RecordNotFound");
    });

    it("Should revert with empty name on update", async function () {
      await expect(
        registry.connect(user1).updateRecord(1, "", "Data")
      ).to.be.revertedWithCustomError(registry, "EmptyName");
    });
  });

  describe("Record Deactivation/Reactivation", function () {
    beforeEach(async function () {
      await registry.connect(user1).createRecord("Test Record", "Test Data");
    });

    it("Should allow owner to deactivate record", async function () {
      await expect(registry.connect(user1).deactivateRecord(1))
        .to.emit(registry, "RecordDeactivated")
        .withArgs(1);

      const record = await registry.getRecord(1);
      expect(record.active).to.be.false;
    });

    it("Should allow admin to deactivate any record", async function () {
      await registry.connect(owner).deactivateRecord(1);
      const record = await registry.getRecord(1);
      expect(record.active).to.be.false;
    });

    it("Should only allow admin to reactivate record", async function () {
      await registry.connect(user1).deactivateRecord(1);

      await expect(
        registry.connect(user1).reactivateRecord(1)
      ).to.be.revertedWith("Registry: caller is not an admin");

      await expect(registry.connect(owner).reactivateRecord(1))
        .to.emit(registry, "RecordReactivated")
        .withArgs(1);

      const record = await registry.getRecord(1);
      expect(record.active).to.be.true;
    });
  });

  describe("Record Queries", function () {
    beforeEach(async function () {
      await registry.connect(user1).createRecord("Record 1", "Data 1");
      await registry.connect(user1).createRecord("Record 2", "Data 2");
      await registry.connect(user2).createRecord("Record 3", "Data 3");
    });

    it("Should return records by owner", async function () {
      const user1Records = await registry.getRecordsByOwner(user1.address);
      expect(user1Records.length).to.equal(2);
      expect(user1Records[0]).to.equal(1);
      expect(user1Records[1]).to.equal(2);

      const user2Records = await registry.getRecordsByOwner(user2.address);
      expect(user2Records.length).to.equal(1);
      expect(user2Records[0]).to.equal(3);
    });

    it("Should return empty array for owner with no records", async function () {
      const records = await registry.getRecordsByOwner(nonUser.address);
      expect(records.length).to.equal(0);
    });

    it("Should return all record IDs", async function () {
      const allIds = await registry.getAllRecordIds();
      expect(allIds.length).to.equal(3);
    });
  });
});
