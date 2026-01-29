const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("AccessControlManager", function () {
  let accessControl;
  let owner;
  let user1;
  let user2;
  let ADMIN_ROLE;
  let USER_ROLE;

  beforeEach(async function () {
    [owner, user1, user2] = await ethers.getSigners();

    const AccessControlManager = await ethers.getContractFactory("AccessControlManager");
    accessControl = await AccessControlManager.deploy();
    await accessControl.waitForDeployment();

    ADMIN_ROLE = await accessControl.ADMIN_ROLE();
    USER_ROLE = await accessControl.USER_ROLE();
  });

  describe("Deployment", function () {
    it("Should set deployer as admin", async function () {
      expect(await accessControl.hasRole(ADMIN_ROLE, owner.address)).to.be.true;
    });

    it("Should set ADMIN_ROLE as admin of itself", async function () {
      expect(await accessControl.getRoleAdmin(ADMIN_ROLE)).to.equal(ADMIN_ROLE);
    });

    it("Should set ADMIN_ROLE as admin of USER_ROLE", async function () {
      expect(await accessControl.getRoleAdmin(USER_ROLE)).to.equal(ADMIN_ROLE);
    });
  });

  describe("Role Management", function () {
    it("Should allow admin to grant USER_ROLE", async function () {
      await expect(accessControl.grantRole(USER_ROLE, user1.address))
        .to.emit(accessControl, "RoleGranted")
        .withArgs(USER_ROLE, user1.address, owner.address);

      expect(await accessControl.hasRole(USER_ROLE, user1.address)).to.be.true;
    });

    it("Should allow admin to grant ADMIN_ROLE", async function () {
      await accessControl.grantRole(ADMIN_ROLE, user1.address);
      expect(await accessControl.hasRole(ADMIN_ROLE, user1.address)).to.be.true;
    });

    it("Should allow admin to revoke roles", async function () {
      await accessControl.grantRole(USER_ROLE, user1.address);
      
      await expect(accessControl.revokeRole(USER_ROLE, user1.address))
        .to.emit(accessControl, "RoleRevoked")
        .withArgs(USER_ROLE, user1.address, owner.address);

      expect(await accessControl.hasRole(USER_ROLE, user1.address)).to.be.false;
    });

    it("Should not emit event when granting role to account that already has it", async function () {
      await accessControl.grantRole(USER_ROLE, user1.address);
      
      await expect(accessControl.grantRole(USER_ROLE, user1.address))
        .to.not.emit(accessControl, "RoleGranted");
    });

    it("Should revert when non-admin tries to grant role", async function () {
      await expect(
        accessControl.connect(user1).grantRole(USER_ROLE, user2.address)
      ).to.be.revertedWithCustomError(accessControl, "AccessControlUnauthorized");
    });

    it("Should revert when non-admin tries to revoke role", async function () {
      await accessControl.grantRole(USER_ROLE, user1.address);
      
      await expect(
        accessControl.connect(user2).revokeRole(USER_ROLE, user1.address)
      ).to.be.revertedWithCustomError(accessControl, "AccessControlUnauthorized");
    });
  });

  describe("Role Renouncement", function () {
    it("Should allow user to renounce their own role", async function () {
      await accessControl.grantRole(USER_ROLE, user1.address);
      
      await expect(accessControl.connect(user1).renounceRole(USER_ROLE))
        .to.emit(accessControl, "RoleRevoked")
        .withArgs(USER_ROLE, user1.address, user1.address);

      expect(await accessControl.hasRole(USER_ROLE, user1.address)).to.be.false;
    });

    it("Should not emit event when renouncing role not held", async function () {
      await expect(accessControl.connect(user1).renounceRole(USER_ROLE))
        .to.not.emit(accessControl, "RoleRevoked");
    });
  });

  describe("New Admin Granting Roles", function () {
    it("Should allow new admin to grant roles", async function () {
      // Grant admin role to user1
      await accessControl.grantRole(ADMIN_ROLE, user1.address);
      
      // user1 should now be able to grant USER_ROLE to user2
      await accessControl.connect(user1).grantRole(USER_ROLE, user2.address);
      expect(await accessControl.hasRole(USER_ROLE, user2.address)).to.be.true;
    });
  });
});
