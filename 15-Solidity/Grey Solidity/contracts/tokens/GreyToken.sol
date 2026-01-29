// SPDX-License-Identifier: MIT
pragma solidity ^0.8.28;

import "@openzeppelin/contracts/token/ERC20/ERC20.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Burnable.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Pausable.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Permit.sol";
import "@openzeppelin/contracts/token/ERC20/extensions/ERC20Votes.sol";
import "@openzeppelin/contracts/access/AccessControl.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Nonces.sol";

/**
 * @title GreyToken
 * @author Grey Solidity Project
 * @notice Full-featured ERC20 token with governance capabilities
 * @dev Implements EIP-2612 permit, snapshots, pausable, votes, mint/burn
 */
contract GreyToken is 
    ERC20,
    ERC20Burnable,
    ERC20Pausable,
    ERC20Permit,
    ERC20Votes,
    AccessControl,
    ReentrancyGuard 
{
    /// @notice Role for minting tokens
    bytes32 public constant MINTER_ROLE = keccak256("MINTER_ROLE");
    
    /// @notice Role for pausing operations
    bytes32 public constant PAUSER_ROLE = keccak256("PAUSER_ROLE");
    
    /// @notice Role for snapshot operations
    bytes32 public constant SNAPSHOT_ROLE = keccak256("SNAPSHOT_ROLE");

    /// @notice Maximum supply cap (0 = no cap)
    uint256 public immutable maxSupply;

    /// @notice Current snapshot ID
    uint256 private _currentSnapshotId;

    /// @notice Snapshot data structure
    struct Snapshots {
        uint256[] ids;
        uint256[] values;
    }

    /// @notice Account balance snapshots
    mapping(address => Snapshots) private _accountBalanceSnapshots;

    /// @notice Total supply snapshots
    Snapshots private _totalSupplySnapshots;

    /// @notice Emitted when a snapshot is created
    event Snapshot(uint256 indexed id);

    /// @notice Emitted when tokens are minted
    event TokensMinted(address indexed to, uint256 amount, address indexed minter);

    /// @notice Emitted when tokens are burned
    event TokensBurned(address indexed from, uint256 amount);

    /// @notice Error for exceeding max supply
    error MaxSupplyExceeded(uint256 requested, uint256 available);

    /**
     * @notice Initializes the token
     * @param name_ The token name
     * @param symbol_ The token symbol
     * @param maxSupply_ The maximum supply (0 = no cap)
     * @param initialSupply The initial supply to mint to deployer
     */
    constructor(
        string memory name_,
        string memory symbol_,
        uint256 maxSupply_,
        uint256 initialSupply
    ) ERC20(name_, symbol_) ERC20Permit(name_) {
        maxSupply = maxSupply_;

        _grantRole(DEFAULT_ADMIN_ROLE, msg.sender);
        _grantRole(MINTER_ROLE, msg.sender);
        _grantRole(PAUSER_ROLE, msg.sender);
        _grantRole(SNAPSHOT_ROLE, msg.sender);

        if (initialSupply > 0) {
            if (maxSupply_ > 0 && initialSupply > maxSupply_) {
                revert MaxSupplyExceeded(initialSupply, maxSupply_);
            }
            _mint(msg.sender, initialSupply);
        }
    }

    // ============ Minting ============

    /**
     * @notice Mints tokens to an address
     * @param to The recipient address
     * @param amount The amount to mint
     */
    function mint(address to, uint256 amount) external onlyRole(MINTER_ROLE) nonReentrant {
        if (maxSupply > 0) {
            uint256 available = maxSupply - totalSupply();
            if (amount > available) {
                revert MaxSupplyExceeded(amount, available);
            }
        }
        
        _mint(to, amount);
        emit TokensMinted(to, amount, msg.sender);
    }

    /**
     * @notice Batch mints tokens to multiple addresses
     * @param recipients Array of recipient addresses
     * @param amounts Array of amounts
     */
    function batchMint(
        address[] calldata recipients,
        uint256[] calldata amounts
    ) external onlyRole(MINTER_ROLE) nonReentrant {
        require(recipients.length == amounts.length, "GreyToken: length mismatch");
        
        uint256 totalAmount;
        for (uint256 i = 0; i < amounts.length; i++) {
            totalAmount += amounts[i];
        }

        if (maxSupply > 0) {
            uint256 available = maxSupply - totalSupply();
            if (totalAmount > available) {
                revert MaxSupplyExceeded(totalAmount, available);
            }
        }

        for (uint256 i = 0; i < recipients.length; i++) {
            _mint(recipients[i], amounts[i]);
            emit TokensMinted(recipients[i], amounts[i], msg.sender);
        }
    }

    // ============ Burning ============

    /**
     * @notice Burns tokens from caller
     * @param amount The amount to burn
     */
    function burn(uint256 amount) public override nonReentrant {
        super.burn(amount);
        emit TokensBurned(msg.sender, amount);
    }

    /**
     * @notice Burns tokens from an address with allowance
     * @param account The account to burn from
     * @param amount The amount to burn
     */
    function burnFrom(address account, uint256 amount) public override nonReentrant {
        super.burnFrom(account, amount);
        emit TokensBurned(account, amount);
    }

    // ============ Pausing ============

    /**
     * @notice Pauses all token transfers
     */
    function pause() external onlyRole(PAUSER_ROLE) {
        _pause();
    }

    /**
     * @notice Unpauses token transfers
     */
    function unpause() external onlyRole(PAUSER_ROLE) {
        _unpause();
    }

    // ============ Snapshots ============

    /**
     * @notice Creates a new snapshot
     * @return The new snapshot ID
     */
    function snapshot() external onlyRole(SNAPSHOT_ROLE) returns (uint256) {
        _currentSnapshotId++;
        emit Snapshot(_currentSnapshotId);
        return _currentSnapshotId;
    }

    /**
     * @notice Returns the current snapshot ID
     * @return The current snapshot ID
     */
    function getCurrentSnapshotId() external view returns (uint256) {
        return _currentSnapshotId;
    }

    /**
     * @notice Returns the balance at a snapshot
     * @param account The account to query
     * @param snapshotId The snapshot ID
     * @return The balance at that snapshot
     */
    function balanceOfAt(address account, uint256 snapshotId) external view returns (uint256) {
        require(snapshotId > 0 && snapshotId <= _currentSnapshotId, "GreyToken: invalid snapshot id");
        
        (bool found, uint256 value) = _valueAt(snapshotId, _accountBalanceSnapshots[account]);
        return found ? value : balanceOf(account);
    }

    /**
     * @notice Returns the total supply at a snapshot
     * @param snapshotId The snapshot ID
     * @return The total supply at that snapshot
     */
    function totalSupplyAt(uint256 snapshotId) external view returns (uint256) {
        require(snapshotId > 0 && snapshotId <= _currentSnapshotId, "GreyToken: invalid snapshot id");
        
        (bool found, uint256 value) = _valueAt(snapshotId, _totalSupplySnapshots);
        return found ? value : totalSupply();
    }

    // ============ Governance Helpers ============

    /**
     * @notice Returns the voting power for an account
     * @param account The account to query
     * @return The current voting power
     */
    function getVotingPower(address account) external view returns (uint256) {
        return getVotes(account);
    }

    /**
     * @notice Delegates votes to self
     */
    function selfDelegate() external {
        delegate(msg.sender);
    }

    // ============ Internal Functions ============

    /**
     * @dev Updates snapshots before token transfer
     */
    function _update(
        address from,
        address to,
        uint256 value
    ) internal virtual override(ERC20, ERC20Pausable, ERC20Votes) {
        // Update snapshots
        if (_currentSnapshotId > 0) {
            if (from != address(0)) {
                _updateAccountSnapshot(from);
            }
            if (to != address(0)) {
                _updateAccountSnapshot(to);
            }
            _updateTotalSupplySnapshot();
        }
        
        super._update(from, to, value);
    }

    /**
     * @dev Updates account balance snapshot
     */
    function _updateAccountSnapshot(address account) private {
        _updateSnapshot(_accountBalanceSnapshots[account], balanceOf(account));
    }

    /**
     * @dev Updates total supply snapshot
     */
    function _updateTotalSupplySnapshot() private {
        _updateSnapshot(_totalSupplySnapshots, totalSupply());
    }

    /**
     * @dev Updates a snapshot with current value
     */
    function _updateSnapshot(Snapshots storage snapshots, uint256 currentValue) private {
        uint256 currentId = _currentSnapshotId;
        if (_lastSnapshotId(snapshots.ids) < currentId) {
            snapshots.ids.push(currentId);
            snapshots.values.push(currentValue);
        }
    }

    /**
     * @dev Returns the last snapshot ID
     */
    function _lastSnapshotId(uint256[] storage ids) private view returns (uint256) {
        if (ids.length == 0) {
            return 0;
        } else {
            return ids[ids.length - 1];
        }
    }

    /**
     * @dev Returns value at a snapshot
     */
    function _valueAt(
        uint256 snapshotId,
        Snapshots storage snapshots
    ) private view returns (bool, uint256) {
        require(snapshotId > 0, "GreyToken: id is 0");
        require(snapshotId <= _currentSnapshotId, "GreyToken: nonexistent id");

        uint256 index = _findUpperBound(snapshots.ids, snapshotId);

        if (index == snapshots.ids.length) {
            return (false, 0);
        } else {
            return (true, snapshots.values[index]);
        }
    }

    /**
     * @dev Binary search for upper bound
     */
    function _findUpperBound(uint256[] storage array, uint256 element) private view returns (uint256) {
        if (array.length == 0) {
            return 0;
        }

        uint256 low = 0;
        uint256 high = array.length;

        while (low < high) {
            uint256 mid = (low + high) / 2;
            if (array[mid] > element) {
                high = mid;
            } else {
                low = mid + 1;
            }
        }

        if (low > 0 && array[low - 1] == element) {
            return low - 1;
        } else {
            return low;
        }
    }

    /**
     * @dev Required override for nonces
     */
    function nonces(address owner) public view virtual override(ERC20Permit, Nonces) returns (uint256) {
        return super.nonces(owner);
    }

    // ============ View Functions ============

    /**
     * @notice Returns remaining mintable supply
     * @return The remaining supply (type(uint256).max if no cap)
     */
    function remainingMintableSupply() external view returns (uint256) {
        if (maxSupply == 0) {
            return type(uint256).max;
        }
        return maxSupply - totalSupply();
    }

    /**
     * @notice Checks if supply cap is reached
     * @return True if capped and at max
     */
    function isAtMaxSupply() external view returns (bool) {
        return maxSupply > 0 && totalSupply() >= maxSupply;
    }
}
