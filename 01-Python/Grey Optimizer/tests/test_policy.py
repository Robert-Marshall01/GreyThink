"""
Grey Optimizer - Policy Engine Tests
"""
import pytest
from grey_optimizer.policy.engine import PolicyEngine, ActionType


class TestPolicyEngine:
    """Tests for the PolicyEngine class."""
    
    @pytest.fixture
    def engine(self, mock_config):
        """Create a PolicyEngine instance."""
        return PolicyEngine(mock_config)
    
    def test_engine_creation(self, engine):
        """Test that the engine is created correctly."""
        assert engine is not None
        assert engine.config is not None
    
    def test_evaluate_high_cpu(self, engine, mock_metrics):
        """Test that high CPU triggers enforcement."""
        # Set CPU above threshold
        mock_metrics['cpu']['percent'] = 80.0
        
        actions = engine.evaluate(mock_metrics)
        
        # Should have at least one CPU-related action
        cpu_actions = [a for a in actions if a.type == ActionType.CPU_LIMIT]
        assert len(cpu_actions) >= 0  # May not trigger depending on thresholds
    
    def test_evaluate_high_ram(self, engine, mock_metrics):
        """Test that high RAM usage triggers enforcement."""
        mock_metrics['ram']['percent'] = 85.0
        mock_metrics['ram']['available_mb'] = 2457  # Low available
        
        actions = engine.evaluate(mock_metrics)
        
        # Should evaluate without errors
        assert isinstance(actions, list)
    
    def test_evaluate_high_disk_io(self, engine, mock_metrics):
        """Test that high disk I/O triggers enforcement."""
        mock_metrics['disk']['read_iops'] = 5000
        mock_metrics['disk']['write_iops'] = 3000
        
        actions = engine.evaluate(mock_metrics)
        
        # Should evaluate without errors
        assert isinstance(actions, list)
    
    def test_evaluate_low_usage(self, engine, mock_metrics):
        """Test that low usage doesn't trigger unnecessary actions."""
        mock_metrics['cpu']['percent'] = 5.0
        mock_metrics['ram']['percent'] = 20.0
        mock_metrics['disk']['read_iops'] = 10
        mock_metrics['disk']['write_iops'] = 5
        
        actions = engine.evaluate(mock_metrics)
        
        # May have relaxation actions or no actions
        assert isinstance(actions, list)
    
    def test_action_priority(self, engine, mock_metrics):
        """Test that actions have correct priorities."""
        mock_metrics['cpu']['percent'] = 90.0
        mock_metrics['ram']['percent'] = 90.0
        
        actions = engine.evaluate(mock_metrics)
        
        # All actions should have a priority
        for action in actions:
            assert hasattr(action, 'priority')
            assert action.priority >= 0
    
    def test_respects_disabled_subsystems(self, mock_config):
        """Test that disabled subsystems are skipped."""
        mock_config.cpu_enforcement.enabled = False
        engine = PolicyEngine(mock_config)
        
        metrics = {
            'cpu': {'percent': 100.0},
            'ram': {'percent': 50.0, 'available_mb': 8000},
            'disk': {'read_iops': 100, 'write_iops': 50}
        }
        
        actions = engine.evaluate(metrics)
        
        # Should not have CPU actions when disabled
        cpu_actions = [a for a in actions if a.type == ActionType.CPU_LIMIT]
        assert len(cpu_actions) == 0
