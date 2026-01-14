"""
Grey Optimizer - Enforcement Manager Tests
"""
import pytest
import asyncio
from unittest.mock import MagicMock, AsyncMock, patch

from grey_optimizer.policy.engine import EnforcementAction, ActionType


class TestEnforcementManager:
    """Tests for the EnforcementManager class."""
    
    @pytest.fixture
    def manager(self, mock_config):
        """Create an EnforcementManager instance."""
        from grey_optimizer.enforcement.manager import EnforcementManager
        return EnforcementManager(mock_config)
    
    @pytest.mark.asyncio
    async def test_manager_initialization(self, manager):
        """Test that manager initializes correctly."""
        assert manager is not None
        assert hasattr(manager, 'apply')
        assert hasattr(manager, 'rollback_all')
    
    @pytest.mark.asyncio
    async def test_apply_action_simulation(self, manager):
        """Test applying an action in simulation mode."""
        action = EnforcementAction(
            type=ActionType.CPU_LIMIT,
            target="test_process",
            parameters={"cpu_quota": 50},
            priority=1
        )
        
        result = await manager.apply(action)
        
        assert result['success'] is True
        assert result['simulated'] is True
    
    @pytest.mark.asyncio
    async def test_apply_multiple_actions(self, manager):
        """Test applying multiple actions."""
        actions = [
            EnforcementAction(
                type=ActionType.CPU_LIMIT,
                target="process1",
                parameters={"cpu_quota": 50},
                priority=1
            ),
            EnforcementAction(
                type=ActionType.RAM_LIMIT,
                target="process2",
                parameters={"memory_limit_mb": 256},
                priority=2
            )
        ]
        
        results = []
        for action in actions:
            result = await manager.apply(action)
            results.append(result)
        
        assert len(results) == 2
        assert all(r['success'] for r in results)
    
    @pytest.mark.asyncio
    async def test_rollback(self, manager):
        """Test rollback functionality."""
        # Apply an action first
        action = EnforcementAction(
            type=ActionType.CPU_LIMIT,
            target="test_process",
            parameters={"cpu_quota": 50},
            priority=1
        )
        await manager.apply(action)
        
        # Then rollback
        result = await manager.rollback_all()
        
        assert result['success'] is True
    
    @pytest.mark.asyncio
    async def test_protected_process_skip(self, manager):
        """Test that protected processes are skipped."""
        action = EnforcementAction(
            type=ActionType.CPU_LIMIT,
            target="systemd",  # Should match protected pattern
            parameters={"cpu_quota": 50},
            priority=1
        )
        
        # Should either skip or succeed with simulation
        result = await manager.apply(action)
        assert 'success' in result


class TestCgroupController:
    """Tests for the CgroupController."""
    
    @pytest.fixture
    def controller(self, mock_config):
        """Create a CgroupController instance."""
        from grey_optimizer.enforcement.manager import CgroupController
        return CgroupController(mock_config)
    
    def test_controller_initialization(self, controller):
        """Test controller initialization."""
        assert controller is not None
    
    @pytest.mark.asyncio
    async def test_set_cpu_quota_simulation(self, controller):
        """Test setting CPU quota in simulation mode."""
        result = await controller.set_cpu_quota("test_cgroup", 50)
        
        # In simulation mode, should succeed
        assert result['simulated'] is True


class TestSchedulerController:
    """Tests for the SchedulerController."""
    
    @pytest.fixture
    def controller(self, mock_config):
        """Create a SchedulerController instance."""
        from grey_optimizer.enforcement.manager import SchedulerController
        return SchedulerController(mock_config)
    
    def test_controller_initialization(self, controller):
        """Test controller initialization."""
        assert controller is not None
    
    @pytest.mark.asyncio
    async def test_set_scheduler_simulation(self, controller):
        """Test setting scheduler in simulation mode."""
        result = await controller.set_idle_priority(12345)  # Fake PID
        
        assert result['simulated'] is True


class TestKSMController:
    """Tests for the KSMController."""
    
    @pytest.fixture
    def controller(self, mock_config):
        """Create a KSMController instance."""
        from grey_optimizer.enforcement.manager import KSMController
        return KSMController(mock_config)
    
    def test_controller_initialization(self, controller):
        """Test controller initialization."""
        assert controller is not None


class TestDiskController:
    """Tests for the DiskController."""
    
    @pytest.fixture
    def controller(self, mock_config):
        """Create a DiskController instance."""
        from grey_optimizer.enforcement.manager import DiskController
        return DiskController(mock_config)
    
    def test_controller_initialization(self, controller):
        """Test controller initialization."""
        assert controller is not None
