"""
Grey Optimizer - Telemetry Tests
"""
import pytest
import asyncio
from unittest.mock import MagicMock, patch


class TestTelemetryCollector:
    """Tests for the TelemetryCollector class."""
    
    @pytest.fixture
    def collector(self, mock_config):
        """Create a TelemetryCollector instance."""
        from grey_optimizer.telemetry import TelemetryCollector
        return TelemetryCollector(mock_config)
    
    @pytest.mark.asyncio
    async def test_collector_initialization(self, collector):
        """Test that collector initializes correctly."""
        assert collector is not None
        assert hasattr(collector, 'collect')
    
    @pytest.mark.asyncio
    async def test_collect_returns_metrics(self, collector):
        """Test that collect returns valid metrics."""
        metrics = await collector.collect()
        
        assert 'cpu' in metrics
        assert 'ram' in metrics
        assert 'disk' in metrics
        assert 'timestamp' in metrics
    
    @pytest.mark.asyncio
    async def test_cpu_metrics(self, collector):
        """Test CPU metrics collection."""
        metrics = await collector.collect()
        
        cpu = metrics['cpu']
        assert 'percent' in cpu
        assert 0 <= cpu['percent'] <= 100
    
    @pytest.mark.asyncio
    async def test_ram_metrics(self, collector):
        """Test RAM metrics collection."""
        metrics = await collector.collect()
        
        ram = metrics['ram']
        assert 'percent' in ram
        assert 'total_mb' in ram
        assert 'available_mb' in ram
        assert 0 <= ram['percent'] <= 100
        assert ram['total_mb'] > 0
    
    @pytest.mark.asyncio
    async def test_disk_metrics(self, collector):
        """Test disk metrics collection."""
        metrics = await collector.collect()
        
        disk = metrics['disk']
        assert 'read_bytes_per_sec' in disk or 'read_iops' in disk
    
    @pytest.mark.asyncio
    async def test_history_tracking(self, collector):
        """Test that metrics history is tracked."""
        # Collect multiple times
        for _ in range(5):
            await collector.collect()
            await asyncio.sleep(0.01)
        
        history = collector.get_history()
        assert len(history) >= 1


class TestCPUTelemetry:
    """Tests for CPU-specific telemetry."""
    
    @pytest.fixture
    def cpu_telemetry(self):
        """Create CPUTelemetry instance."""
        from grey_optimizer.telemetry.cpu import CPUTelemetry
        return CPUTelemetry()
    
    @pytest.mark.asyncio
    async def test_cpu_collection(self, cpu_telemetry):
        """Test CPU telemetry collection."""
        data = await cpu_telemetry.collect()
        
        assert 'percent' in data
        assert isinstance(data['percent'], (int, float))


class TestRAMTelemetry:
    """Tests for RAM-specific telemetry."""
    
    @pytest.fixture
    def ram_telemetry(self):
        """Create RAMTelemetry instance."""
        from grey_optimizer.telemetry.ram import RAMTelemetry
        return RAMTelemetry()
    
    @pytest.mark.asyncio
    async def test_ram_collection(self, ram_telemetry):
        """Test RAM telemetry collection."""
        data = await ram_telemetry.collect()
        
        assert 'percent' in data
        assert 'total_mb' in data
        assert data['total_mb'] > 0


class TestDiskTelemetry:
    """Tests for disk-specific telemetry."""
    
    @pytest.fixture
    def disk_telemetry(self):
        """Create DiskTelemetry instance."""
        from grey_optimizer.telemetry.disk import DiskTelemetry
        return DiskTelemetry()
    
    @pytest.mark.asyncio
    async def test_disk_collection(self, disk_telemetry):
        """Test disk telemetry collection."""
        data = await disk_telemetry.collect()
        
        # Should have I/O metrics
        assert isinstance(data, dict)
