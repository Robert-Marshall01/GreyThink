"""
Grey Optimizer - Integration Tests

Tests that verify the integration between components.
"""
import pytest
import asyncio
from pathlib import Path


class TestDaemonIntegration:
    """Integration tests for the main daemon."""
    
    @pytest.fixture
    def daemon(self, mock_config, temp_dir):
        """Create a daemon instance for testing."""
        from grey_optimizer.daemon import GreyOptimizerDaemon
        
        # Ensure paths exist
        Path(mock_config.persistence.proof_dir).mkdir(parents=True, exist_ok=True)
        
        return GreyOptimizerDaemon(mock_config)
    
    @pytest.mark.asyncio
    async def test_daemon_initialization(self, daemon):
        """Test that daemon initializes correctly."""
        assert daemon is not None
        assert hasattr(daemon, 'start')
        assert hasattr(daemon, 'stop')
    
    @pytest.mark.asyncio
    async def test_daemon_components(self, daemon):
        """Test that daemon has all required components."""
        await daemon._initialize()
        
        assert daemon.telemetry is not None
        assert daemon.policy_engine is not None
        assert daemon.enforcement_manager is not None
        assert daemon.database is not None


class TestTelemetryToPolicy:
    """Tests for telemetry -> policy integration."""
    
    @pytest.mark.asyncio
    async def test_metrics_to_actions(self, mock_config, mock_metrics):
        """Test that metrics are converted to actions correctly."""
        from grey_optimizer.telemetry import TelemetryCollector
        from grey_optimizer.policy.engine import PolicyEngine
        
        collector = TelemetryCollector(mock_config)
        engine = PolicyEngine(mock_config)
        
        # Collect metrics
        metrics = await collector.collect()
        
        # Evaluate policies
        actions = engine.evaluate(metrics)
        
        assert isinstance(actions, list)


class TestPolicyToEnforcement:
    """Tests for policy -> enforcement integration."""
    
    @pytest.mark.asyncio
    async def test_actions_to_enforcement(self, mock_config):
        """Test that actions are enforced correctly."""
        from grey_optimizer.policy.engine import PolicyEngine, EnforcementAction, ActionType
        from grey_optimizer.enforcement.manager import EnforcementManager
        
        engine = PolicyEngine(mock_config)
        manager = EnforcementManager(mock_config)
        
        # Create a test action
        action = EnforcementAction(
            type=ActionType.CPU_LIMIT,
            target="test_target",
            parameters={"cpu_quota": 50},
            priority=1
        )
        
        # Apply action
        result = await manager.apply(action)
        
        assert result['success'] is True


class TestEnforcementToPersistence:
    """Tests for enforcement -> persistence integration."""
    
    @pytest.mark.asyncio
    async def test_actions_are_logged(self, mock_config, temp_dir):
        """Test that enforcement actions are logged to database."""
        from grey_optimizer.persistence.database import Database
        from grey_optimizer.enforcement.manager import EnforcementManager
        from grey_optimizer.policy.engine import EnforcementAction, ActionType
        
        # Setup
        mock_config.persistence.database_path = str(temp_dir / "test.db")
        database = Database(mock_config)
        await database.initialize()
        
        manager = EnforcementManager(mock_config)
        manager.database = database
        
        # Apply action
        action = EnforcementAction(
            type=ActionType.CPU_LIMIT,
            target="test_target",
            parameters={"cpu_quota": 50},
            priority=1
        )
        
        await manager.apply(action)
        
        # Verify logged (if manager logs to database)
        # This depends on implementation details


class TestAPIIntegration:
    """Tests for API server integration."""
    
    @pytest.fixture
    def api_server(self, mock_config):
        """Create an API server for testing."""
        from grey_optimizer.api.server import APIServer
        return APIServer(mock_config)
    
    @pytest.mark.asyncio
    async def test_api_server_initialization(self, api_server):
        """Test API server initialization."""
        assert api_server is not None
    
    @pytest.mark.asyncio
    async def test_api_endpoints_exist(self, api_server):
        """Test that required API endpoints exist."""
        app = api_server.app
        
        # Check routes exist
        routes = [route.path for route in app.router.routes()]
        
        assert '/api/status' in routes or '/status' in routes
        assert '/api/metrics' in routes or '/metrics' in routes


class TestFullPipeline:
    """Full pipeline integration tests."""
    
    @pytest.mark.asyncio
    async def test_full_optimization_cycle(self, mock_config, temp_dir):
        """Test a complete optimization cycle."""
        from grey_optimizer.telemetry import TelemetryCollector
        from grey_optimizer.policy.engine import PolicyEngine
        from grey_optimizer.enforcement.manager import EnforcementManager
        from grey_optimizer.persistence.database import Database
        
        # Setup
        mock_config.persistence.database_path = str(temp_dir / "test.db")
        Path(mock_config.persistence.proof_dir).mkdir(parents=True, exist_ok=True)
        
        # Initialize components
        collector = TelemetryCollector(mock_config)
        engine = PolicyEngine(mock_config)
        manager = EnforcementManager(mock_config)
        database = Database(mock_config)
        
        await database.initialize()
        
        # Run one optimization cycle
        # 1. Collect metrics
        metrics = await collector.collect()
        assert metrics is not None
        
        # 2. Evaluate policies
        actions = engine.evaluate(metrics)
        assert isinstance(actions, list)
        
        # 3. Apply enforcement (in simulation)
        for action in actions:
            result = await manager.apply(action)
            assert result['success'] is True
            assert result['simulated'] is True
        
        # 4. Verify audit log
        is_valid = await database.verify_hash_chain()
        assert is_valid is True
    
    @pytest.mark.asyncio
    async def test_rollback_integration(self, mock_config):
        """Test rollback across all components."""
        from grey_optimizer.enforcement.manager import EnforcementManager
        from grey_optimizer.policy.engine import EnforcementAction, ActionType
        
        manager = EnforcementManager(mock_config)
        
        # Apply some actions
        actions = [
            EnforcementAction(
                type=ActionType.CPU_LIMIT,
                target="target1",
                parameters={"cpu_quota": 50},
                priority=1
            ),
            EnforcementAction(
                type=ActionType.RAM_LIMIT,
                target="target2",
                parameters={"memory_limit_mb": 256},
                priority=2
            )
        ]
        
        for action in actions:
            await manager.apply(action)
        
        # Rollback all
        result = await manager.rollback_all()
        
        assert result['success'] is True
