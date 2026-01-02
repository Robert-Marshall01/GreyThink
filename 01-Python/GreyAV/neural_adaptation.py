"""
Neural Threat Adaptation Engine for GreyAV EDR System.

This module implements a lightweight neural model for threat classification
with online learning capabilities, dynamic threshold adjustment, and
feature extraction from system events.
"""

import uuid
import math
import random
import hashlib
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional, Dict, Any, List, Tuple, Callable

# Configure module logger
logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


@dataclass
class Event:
    """
    Represents a system event for neural analysis.
    
    Attributes:
        event_id: Unique identifier for the event.
        event_type: Type of event (process_spawn, file_write, etc.).
        source: The entity initiating the event.
        target: The entity affected by the event.
        timestamp: When the event occurred.
        metadata: Additional context data.
    """
    event_id: str
    event_type: str
    source: str
    target: str
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class AdaptationAlert:
    """
    Emitted when new malicious behaviors are detected by the neural model.
    
    Attributes:
        alert_id: Unique identifier for this alert.
        event_id: ID of the event that triggered the alert.
        features: Extracted feature dictionary.
        prediction: Classification result ("malicious" or "benign").
        confidence: Model confidence score (0.0 - 1.0).
        description: Human-readable description.
        timestamp: When the alert was generated.
        model_version: Version of the model that made the prediction.
        threshold_used: Detection threshold at time of prediction.
        metadata: Additional context.
    """
    alert_id: str
    event_id: str
    features: Dict[str, float]
    prediction: str
    confidence: float
    description: str
    timestamp: datetime = field(default_factory=datetime.now)
    model_version: str = "1.0.0"
    threshold_used: float = 0.5
    metadata: Dict[str, Any] = field(default_factory=dict)


class FeatureExtractor:
    """
    Extracts numerical features from system events.
    
    Converts event attributes into a feature vector suitable for
    neural network classification.
    """
    
    # Event type risk mapping
    EVENT_TYPE_SCORES: Dict[str, float] = {
        "process_spawn": 0.3,
        "file_write": 0.4,
        "file_read": 0.1,
        "file_delete": 0.5,
        "registry_write": 0.6,
        "registry_read": 0.2,
        "network_connect": 0.5,
        "network_listen": 0.4,
        "dns_query": 0.3,
        "privilege_escalation": 0.9,
        "process_injection": 0.95,
        "credential_access": 0.9,
        "scheduled_task": 0.6,
        "service_install": 0.7,
        "module_load": 0.3,
        "script_execution": 0.5,
    }
    
    # Suspicious path patterns
    SUSPICIOUS_PATHS: List[str] = [
        "temp", "tmp", "appdata", "public", "downloads",
        "/var/tmp", "/dev/shm", "system32", "syswow64"
    ]
    
    # Known malicious extensions
    SUSPICIOUS_EXTENSIONS: List[str] = [
        ".exe", ".dll", ".scr", ".bat", ".cmd", ".ps1",
        ".vbs", ".js", ".hta", ".msi", ".jar"
    ]
    
    def __init__(self):
        """Initialize the feature extractor."""
        self._feature_stats: Dict[str, Dict[str, float]] = {}
        logger.info("FeatureExtractor initialized")
    
    def _normalize(self, value: float, min_val: float = 0.0, max_val: float = 1.0) -> float:
        """Normalize a value to the range [0, 1]."""
        if max_val == min_val:
            return 0.5
        return max(0.0, min(1.0, (value - min_val) / (max_val - min_val)))
    
    def _hash_to_float(self, text: str) -> float:
        """Convert a string to a float via hashing."""
        hash_val = int(hashlib.md5(text.encode()).hexdigest()[:8], 16)
        return (hash_val % 1000) / 1000.0
    
    def _calculate_entropy(self, text: str) -> float:
        """Calculate Shannon entropy of a string."""
        if not text:
            return 0.0
        
        freq: Dict[str, int] = {}
        for char in text:
            freq[char] = freq.get(char, 0) + 1
        
        entropy = 0.0
        length = len(text)
        for count in freq.values():
            p = count / length
            entropy -= p * math.log2(p)
        
        # Normalize to [0, 1] (max entropy for ASCII is ~6.5)
        return min(entropy / 6.5, 1.0)
    
    def _extract_path_features(self, path: str) -> Dict[str, float]:
        """Extract features from a file/process path."""
        path_lower = path.lower()
        
        # Path depth
        depth = path.count("/") + path.count("\\")
        depth_normalized = self._normalize(depth, 0, 10)
        
        # Suspicious path indicator
        suspicious_path = 1.0 if any(p in path_lower for p in self.SUSPICIOUS_PATHS) else 0.0
        
        # Extension analysis
        has_suspicious_ext = 1.0 if any(path_lower.endswith(ext) for ext in self.SUSPICIOUS_EXTENSIONS) else 0.0
        
        # Path entropy (randomized names are suspicious)
        path_entropy = self._calculate_entropy(path.split("/")[-1].split("\\")[-1])
        
        # Path length
        path_length = self._normalize(len(path), 0, 200)
        
        return {
            "path_depth": depth_normalized,
            "suspicious_path": suspicious_path,
            "suspicious_extension": has_suspicious_ext,
            "path_entropy": path_entropy,
            "path_length": path_length,
        }
    
    def _extract_process_features(self, event: Event) -> Dict[str, float]:
        """Extract process-related features."""
        features = {}
        
        # Process lineage depth (from metadata)
        lineage_depth = event.metadata.get("lineage_depth", 1)
        features["lineage_depth"] = self._normalize(lineage_depth, 0, 10)
        
        # Parent process suspicion
        parent = event.metadata.get("parent_process", "")
        features["parent_suspicious"] = 1.0 if any(
            s in parent.lower() for s in ["cmd", "powershell", "wscript", "cscript", "mshta"]
        ) else 0.0
        
        # Command line length (long command lines are suspicious)
        cmdline = event.metadata.get("command_line", "")
        features["cmdline_length"] = self._normalize(len(cmdline), 0, 1000)
        features["cmdline_entropy"] = self._calculate_entropy(cmdline)
        
        # Has encoded content (base64 patterns)
        has_encoded = 1.0 if any(
            s in cmdline.lower() for s in ["base64", "encodedcommand", "-enc", "-e "]
        ) else 0.0
        features["has_encoded_content"] = has_encoded
        
        return features
    
    def _extract_network_features(self, event: Event) -> Dict[str, float]:
        """Extract network-related features."""
        features = {}
        
        target = event.target
        
        # External connection
        is_external = 0.0
        if ":" in target:
            ip_part = target.split(":")[0]
            if not ip_part.startswith(("10.", "192.168.", "172.", "127.")):
                is_external = 1.0
        features["is_external"] = is_external
        
        # Port analysis
        port = 0
        if ":" in target:
            try:
                port = int(target.split(":")[-1])
            except ValueError:
                pass
        
        # Suspicious ports
        suspicious_ports = {4444, 5555, 6666, 31337, 8080, 8443, 9001}
        features["suspicious_port"] = 1.0 if port in suspicious_ports else 0.0
        features["high_port"] = 1.0 if port > 10000 else 0.0
        features["standard_port"] = 1.0 if port in {80, 443, 22, 53, 25} else 0.0
        
        # Connection frequency
        conn_count = event.metadata.get("connection_count", 1)
        features["connection_frequency"] = self._normalize(conn_count, 0, 100)
        
        return features
    
    def _extract_file_features(self, event: Event) -> Dict[str, float]:
        """Extract file-related features."""
        features = {}
        
        # File entropy (from metadata or estimated)
        file_entropy = event.metadata.get("file_entropy", 0.5)
        features["file_entropy"] = file_entropy
        
        # File size
        file_size = event.metadata.get("file_size", 0)
        features["file_size"] = self._normalize(math.log10(file_size + 1), 0, 9)
        
        # Is executable
        target_lower = event.target.lower()
        features["is_executable"] = 1.0 if any(
            target_lower.endswith(ext) for ext in [".exe", ".dll", ".so", ".bin"]
        ) else 0.0
        
        # Is document
        features["is_document"] = 1.0 if any(
            target_lower.endswith(ext) for ext in [".doc", ".docx", ".pdf", ".xlsx"]
        ) else 0.0
        
        # Is script
        features["is_script"] = 1.0 if any(
            target_lower.endswith(ext) for ext in [".ps1", ".bat", ".sh", ".py", ".js"]
        ) else 0.0
        
        return features
    
    def _extract_registry_features(self, event: Event) -> Dict[str, float]:
        """Extract registry-related features."""
        features = {}
        
        target = event.target.upper()
        
        # Persistence locations
        persistence_keys = ["RUN", "RUNONCE", "SERVICES", "SHELL", "USERINIT"]
        features["persistence_key"] = 1.0 if any(k in target for k in persistence_keys) else 0.0
        
        # Security-related keys
        security_keys = ["POLICIES", "SECURITY", "SAM", "SYSTEM"]
        features["security_key"] = 1.0 if any(k in target for k in security_keys) else 0.0
        
        # Firewall keys
        features["firewall_key"] = 1.0 if "FIREWALL" in target else 0.0
        
        return features
    
    def extract(self, event: Event) -> Dict[str, float]:
        """
        Extract features from an event.
        
        Converts event attributes into a numerical feature dictionary
        suitable for neural network classification.
        
        Args:
            event: The event to extract features from.
            
        Returns:
            Dictionary of feature names to float values.
        """
        features: Dict[str, float] = {}
        
        # Base event type score
        features["event_type_score"] = self.EVENT_TYPE_SCORES.get(event.event_type, 0.3)
        
        # Time-based features
        hour = event.timestamp.hour
        features["is_business_hours"] = 1.0 if 9 <= hour <= 17 else 0.0
        features["is_night_time"] = 1.0 if hour < 6 or hour > 22 else 0.0
        
        # Source path features
        source_features = self._extract_path_features(event.source)
        for k, v in source_features.items():
            features[f"source_{k}"] = v
        
        # Target path features
        target_features = self._extract_path_features(event.target)
        for k, v in target_features.items():
            features[f"target_{k}"] = v
        
        # Event type specific features
        if event.event_type in ["process_spawn", "script_execution"]:
            process_features = self._extract_process_features(event)
            features.update(process_features)
        
        if event.event_type in ["network_connect", "dns_query", "network_listen"]:
            network_features = self._extract_network_features(event)
            features.update(network_features)
        
        if event.event_type in ["file_write", "file_read", "file_delete"]:
            file_features = self._extract_file_features(event)
            features.update(file_features)
        
        if event.event_type in ["registry_write", "registry_read"]:
            registry_features = self._extract_registry_features(event)
            features.update(registry_features)
        
        # Metadata-based features
        if event.metadata.get("elevated", False):
            features["is_elevated"] = 1.0
        else:
            features["is_elevated"] = 0.0
        
        features["has_signature"] = 1.0 if event.metadata.get("signed", False) else 0.0
        features["reputation_score"] = event.metadata.get("reputation", 0.5)
        
        logger.debug(f"Extracted {len(features)} features from event {event.event_id}")
        return features


class LightweightClassifier:
    """
    A simple perceptron-based classifier for online learning.
    
    Implements a single-layer neural network with sigmoid activation
    for binary classification (malicious vs benign).
    """
    
    def __init__(self, learning_rate: float = 0.01):
        """
        Initialize the classifier.
        
        Args:
            learning_rate: Learning rate for weight updates.
        """
        self.learning_rate = learning_rate
        self.weights: Dict[str, float] = {}
        self.bias: float = 0.0
        self.training_count: int = 0
        self.version: str = "1.0.0"
    
    def _sigmoid(self, x: float) -> float:
        """Sigmoid activation function."""
        # Clip to prevent overflow
        x = max(-500, min(500, x))
        return 1.0 / (1.0 + math.exp(-x))
    
    def _sigmoid_derivative(self, x: float) -> float:
        """Derivative of sigmoid function."""
        s = self._sigmoid(x)
        return s * (1 - s)
    
    def _compute_output(self, features: Dict[str, float]) -> Tuple[float, float]:
        """
        Compute network output.
        
        Returns:
            Tuple of (raw_output, activated_output).
        """
        raw = self.bias
        for feature, value in features.items():
            weight = self.weights.get(feature, 0.0)
            raw += weight * value
        
        activated = self._sigmoid(raw)
        return raw, activated
    
    def predict_proba(self, features: Dict[str, float]) -> float:
        """
        Predict probability of malicious classification.
        
        Args:
            features: Feature dictionary.
            
        Returns:
            Probability of malicious (0.0 - 1.0).
        """
        _, activated = self._compute_output(features)
        return activated
    
    def predict(self, features: Dict[str, float], threshold: float = 0.5) -> Tuple[str, float]:
        """
        Make a classification prediction.
        
        Args:
            features: Feature dictionary.
            threshold: Decision threshold.
            
        Returns:
            Tuple of (prediction, confidence).
        """
        proba = self.predict_proba(features)
        
        if proba >= threshold:
            prediction = "malicious"
            confidence = proba
        else:
            prediction = "benign"
            confidence = 1.0 - proba
        
        return prediction, confidence
    
    def train_single(self, features: Dict[str, float], label: float) -> float:
        """
        Train on a single example (online learning).
        
        Args:
            features: Feature dictionary.
            label: Target label (1.0 for malicious, 0.0 for benign).
            
        Returns:
            Prediction error.
        """
        # Forward pass
        raw, predicted = self._compute_output(features)
        
        # Calculate error
        error = label - predicted
        
        # Backward pass (gradient descent)
        gradient = error * self._sigmoid_derivative(raw)
        
        # Update weights
        for feature, value in features.items():
            if feature not in self.weights:
                self.weights[feature] = random.uniform(-0.1, 0.1)
            self.weights[feature] += self.learning_rate * gradient * value
        
        # Update bias
        self.bias += self.learning_rate * gradient
        
        self.training_count += 1
        
        # Update version periodically
        if self.training_count % 100 == 0:
            major, minor, patch = self.version.split(".")
            self.version = f"{major}.{minor}.{int(patch) + 1}"
        
        return abs(error)
    
    def get_feature_importance(self) -> Dict[str, float]:
        """Get feature importance based on weight magnitudes."""
        if not self.weights:
            return {}
        
        max_weight = max(abs(w) for w in self.weights.values()) or 1.0
        return {
            feature: abs(weight) / max_weight
            for feature, weight in sorted(
                self.weights.items(),
                key=lambda x: abs(x[1]),
                reverse=True
            )
        }
    
    def save_state(self) -> Dict[str, Any]:
        """Save model state for persistence."""
        return {
            "weights": dict(self.weights),
            "bias": self.bias,
            "learning_rate": self.learning_rate,
            "training_count": self.training_count,
            "version": self.version,
        }
    
    def load_state(self, state: Dict[str, Any]) -> None:
        """Load model state from saved state."""
        self.weights = state.get("weights", {})
        self.bias = state.get("bias", 0.0)
        self.learning_rate = state.get("learning_rate", 0.01)
        self.training_count = state.get("training_count", 0)
        self.version = state.get("version", "1.0.0")


class NeuralAdaptationEngine:
    """
    Neural Threat Adaptation Engine for online threat detection.
    
    Combines feature extraction with a lightweight neural classifier
    for adaptive threat detection with online learning capabilities.
    
    Attributes:
        model: The neural classifier.
        extractor: Feature extraction component.
        threshold: Detection threshold.
        subscribers: Callbacks for alert notifications.
    """
    
    def __init__(
        self,
        learning_rate: float = 0.01,
        threshold: float = 0.5
    ):
        """
        Initialize the Neural Adaptation Engine.
        
        Args:
            learning_rate: Learning rate for the model.
            threshold: Initial detection threshold.
        """
        self.model = LightweightClassifier(learning_rate=learning_rate)
        self.extractor = FeatureExtractor()
        self.threshold = threshold
        self.subscribers: List[Callable[[AdaptationAlert], None]] = []
        self._alert_history: List[AdaptationAlert] = []
        self._prediction_history: List[Dict[str, Any]] = []
        self._performance_window: List[Tuple[str, str]] = []  # (predicted, actual)
        
        # Initialize with some baseline knowledge
        self._initialize_baseline()
        
        logger.info(f"NeuralAdaptationEngine initialized (threshold: {threshold})")
    
    def _initialize_baseline(self) -> None:
        """Initialize model with baseline threat patterns."""
        # Simulate training on known patterns
        baseline_patterns = [
            # Malicious patterns
            ({"event_type_score": 0.9, "source_suspicious_path": 1.0, "is_elevated": 1.0}, 1.0),
            ({"event_type_score": 0.95, "cmdline_entropy": 0.8, "has_encoded_content": 1.0}, 1.0),
            ({"event_type_score": 0.5, "suspicious_port": 1.0, "is_external": 1.0}, 1.0),
            ({"event_type_score": 0.6, "persistence_key": 1.0, "source_suspicious_path": 1.0}, 1.0),
            
            # Benign patterns
            ({"event_type_score": 0.1, "has_signature": 1.0, "reputation_score": 0.9}, 0.0),
            ({"event_type_score": 0.3, "is_business_hours": 1.0, "standard_port": 1.0}, 0.0),
            ({"event_type_score": 0.2, "source_suspicious_path": 0.0, "is_document": 1.0}, 0.0),
        ]
        
        for features, label in baseline_patterns:
            self.model.train_single(features, label)
        
        logger.info("Baseline model initialized with %d patterns", len(baseline_patterns))
    
    def subscribe(self, callback: Callable[[AdaptationAlert], None]) -> None:
        """
        Subscribe to adaptation alert notifications.
        
        Args:
            callback: Function to call when an alert is generated.
        """
        self.subscribers.append(callback)
    
    def unsubscribe(self, callback: Callable[[AdaptationAlert], None]) -> None:
        """
        Unsubscribe from adaptation alert notifications.
        
        Args:
            callback: Previously registered callback to remove.
        """
        if callback in self.subscribers:
            self.subscribers.remove(callback)
    
    def _notify_subscribers(self, alert: AdaptationAlert) -> None:
        """Notify all subscribers of a new alert."""
        for callback in self.subscribers:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f"Subscriber notification failed: {e}")
    
    def train(self, event: Event, label: Optional[str] = None) -> float:
        """
        Train the model on an event.
        
        Performs online learning update with the given event.
        
        Args:
            event: The event to train on.
            label: Ground truth label ("malicious" or "benign").
                   If None, inferred from event metadata.
                   
        Returns:
            Training error.
        """
        # Extract features
        features = self.extractor.extract(event)
        
        # Determine label
        if label is None:
            # Infer from metadata or use heuristics
            label = event.metadata.get("label", "benign")
        
        label_value = 1.0 if label == "malicious" else 0.0
        
        # Train model
        error = self.model.train_single(features, label_value)
        
        logger.debug(f"Trained on event {event.event_id}: label={label}, error={error:.4f}")
        
        return error
    
    def predict(self, event: Event) -> AdaptationAlert:
        """
        Predict whether an event is malicious.
        
        Extracts features, runs the neural model, and generates
        an AdaptationAlert with the prediction.
        
        Args:
            event: The event to classify.
            
        Returns:
            AdaptationAlert with prediction results.
        """
        # Extract features
        features = self.extractor.extract(event)
        
        # Make prediction
        prediction, confidence = self.model.predict(features, self.threshold)
        
        # Store prediction for performance tracking
        self._prediction_history.append({
            "event_id": event.event_id,
            "prediction": prediction,
            "confidence": confidence,
            "timestamp": datetime.now(),
        })
        
        # Generate description
        if prediction == "malicious":
            # Get top contributing features
            top_features = sorted(
                [(k, v) for k, v in features.items() if v > 0.5],
                key=lambda x: x[1],
                reverse=True
            )[:3]
            feature_desc = ", ".join(f"{k}={v:.2f}" for k, v in top_features)
            
            description = (
                f"Neural model detected malicious behavior (confidence: {confidence:.2f}). "
                f"Key indicators: {feature_desc}"
            )
        else:
            description = (
                f"Event classified as benign (confidence: {confidence:.2f})"
            )
        
        # Create alert
        alert = AdaptationAlert(
            alert_id=str(uuid.uuid4()),
            event_id=event.event_id,
            features=features,
            prediction=prediction,
            confidence=confidence,
            description=description,
            model_version=self.model.version,
            threshold_used=self.threshold,
            metadata={
                "event_type": event.event_type,
                "source": event.source,
                "target": event.target,
            },
        )
        
        # Store alert
        self._alert_history.append(alert)
        
        # Notify subscribers for malicious predictions
        if prediction == "malicious" and confidence >= self.threshold:
            self._notify_subscribers(alert)
            logger.warning(
                f"Malicious behavior detected: {event.event_id} "
                f"(confidence: {confidence:.2f})"
            )
        
        return alert
    
    def train_and_predict(self, event: Event, label: Optional[str] = None) -> AdaptationAlert:
        """
        Train on event and return prediction.
        
        Combines training and prediction for reinforcement learning scenarios.
        
        Args:
            event: The event to process.
            label: Ground truth label if available.
            
        Returns:
            AdaptationAlert with prediction.
        """
        # Predict first
        alert = self.predict(event)
        
        # Then train (for continuous learning)
        if label:
            self.train(event, label)
        
        return alert
    
    def provide_feedback(self, event_id: str, actual_label: str) -> None:
        """
        Provide feedback on a prediction for reinforcement.
        
        Args:
            event_id: ID of the event.
            actual_label: Actual label ("malicious" or "benign").
        """
        # Find the prediction
        for pred in self._prediction_history:
            if pred["event_id"] == event_id:
                predicted = pred["prediction"]
                self._performance_window.append((predicted, actual_label))
                
                # Keep window size manageable
                if len(self._performance_window) > 1000:
                    self._performance_window = self._performance_window[-1000:]
                
                logger.info(f"Feedback received: {event_id} was {actual_label} (predicted: {predicted})")
                break
    
    def adjust_threshold(self, performance_metrics: Optional[Dict[str, float]] = None) -> None:
        """
        Dynamically adjust detection threshold based on performance.
        
        Args:
            performance_metrics: Optional metrics dict with 'fpr', 'fnr', etc.
                                If None, calculates from recent predictions.
        """
        if performance_metrics is None:
            performance_metrics = self.calculate_performance()
        
        if not performance_metrics:
            return
        
        fpr = performance_metrics.get("false_positive_rate", 0.0)
        fnr = performance_metrics.get("false_negative_rate", 0.0)
        
        old_threshold = self.threshold
        
        # Adjust threshold based on error rates
        if fpr > 0.1 and fnr < 0.05:
            # Too many false positives, raise threshold
            self.threshold = min(0.9, self.threshold + 0.05)
        elif fnr > 0.1 and fpr < 0.05:
            # Too many false negatives, lower threshold
            self.threshold = max(0.1, self.threshold - 0.05)
        elif fpr > 0.05 or fnr > 0.05:
            # Both elevated, minor adjustment toward balance
            adjustment = (fnr - fpr) * 0.1
            self.threshold = max(0.1, min(0.9, self.threshold - adjustment))
        
        if old_threshold != self.threshold:
            logger.info(
                f"Threshold adjusted: {old_threshold:.3f} -> {self.threshold:.3f} "
                f"(FPR: {fpr:.3f}, FNR: {fnr:.3f})"
            )
    
    def calculate_performance(self) -> Dict[str, float]:
        """
        Calculate performance metrics from feedback history.
        
        Returns:
            Dictionary with accuracy, precision, recall, etc.
        """
        if len(self._performance_window) < 10:
            return {}
        
        tp = fp = tn = fn = 0
        
        for predicted, actual in self._performance_window:
            if predicted == "malicious" and actual == "malicious":
                tp += 1
            elif predicted == "malicious" and actual == "benign":
                fp += 1
            elif predicted == "benign" and actual == "benign":
                tn += 1
            else:
                fn += 1
        
        total = len(self._performance_window)
        
        accuracy = (tp + tn) / total if total > 0 else 0.0
        precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
        recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
        f1 = 2 * (precision * recall) / (precision + recall) if (precision + recall) > 0 else 0.0
        fpr = fp / (fp + tn) if (fp + tn) > 0 else 0.0
        fnr = fn / (fn + tp) if (fn + tp) > 0 else 0.0
        
        return {
            "accuracy": accuracy,
            "precision": precision,
            "recall": recall,
            "f1_score": f1,
            "false_positive_rate": fpr,
            "false_negative_rate": fnr,
            "true_positives": tp,
            "false_positives": fp,
            "true_negatives": tn,
            "false_negatives": fn,
            "sample_count": total,
        }
    
    def get_feature_importance(self) -> Dict[str, float]:
        """
        Get feature importance from the model.
        
        Returns:
            Dictionary of feature names to importance scores.
        """
        return self.model.get_feature_importance()
    
    def get_alert_history(
        self,
        prediction: Optional[str] = None,
        min_confidence: float = 0.0
    ) -> List[AdaptationAlert]:
        """
        Get alert history with optional filters.
        
        Args:
            prediction: Filter by prediction ("malicious" or "benign").
            min_confidence: Minimum confidence filter.
            
        Returns:
            List of matching alerts.
        """
        results = self._alert_history
        
        if prediction:
            results = [a for a in results if a.prediction == prediction]
        
        if min_confidence > 0:
            results = [a for a in results if a.confidence >= min_confidence]
        
        return results
    
    def get_statistics(self) -> Dict[str, Any]:
        """
        Get engine statistics.
        
        Returns:
            Dictionary with engine statistics.
        """
        return {
            "model_version": self.model.version,
            "training_count": self.model.training_count,
            "feature_count": len(self.model.weights),
            "current_threshold": self.threshold,
            "total_predictions": len(self._prediction_history),
            "total_alerts": len(self._alert_history),
            "malicious_detections": sum(
                1 for a in self._alert_history if a.prediction == "malicious"
            ),
            "performance": self.calculate_performance(),
        }
    
    def save_model(self) -> Dict[str, Any]:
        """Save model state for persistence."""
        return {
            "model_state": self.model.save_state(),
            "threshold": self.threshold,
        }
    
    def load_model(self, state: Dict[str, Any]) -> None:
        """Load model from saved state."""
        if "model_state" in state:
            self.model.load_state(state["model_state"])
        if "threshold" in state:
            self.threshold = state["threshold"]
        logger.info("Model loaded from saved state")
    
    def clear(self) -> None:
        """Clear history and reset statistics."""
        self._alert_history.clear()
        self._prediction_history.clear()
        self._performance_window.clear()
        logger.info("NeuralAdaptationEngine history cleared")


# Factory function for pipeline integration
def create_adaptation_engine(
    learning_rate: float = 0.01,
    threshold: float = 0.5
) -> NeuralAdaptationEngine:
    """
    Factory function to create a NeuralAdaptationEngine instance.
    
    Args:
        learning_rate: Learning rate for the model.
        threshold: Initial detection threshold.
        
    Returns:
        New NeuralAdaptationEngine instance.
    """
    return NeuralAdaptationEngine(learning_rate=learning_rate, threshold=threshold)


if __name__ == "__main__":
    # Demo usage
    logging.basicConfig(level=logging.INFO)
    
    engine = create_adaptation_engine(threshold=0.5)
    
    # Subscribe to alerts
    def on_alert(alert: AdaptationAlert):
        print(f"\n>>> NEURAL ALERT [{alert.prediction.upper()}]")
        print(f"    Confidence: {alert.confidence:.2f}")
        print(f"    Event: {alert.event_id}")
        print(f"    Description: {alert.description}\n")
    
    engine.subscribe(on_alert)
    
    # Test events
    test_events = [
        # Suspicious: Process injection
        Event(
            "evt-001", "process_injection",
            "/tmp/malware.exe", "explorer.exe",
            metadata={"elevated": True, "lineage_depth": 5}
        ),
        # Benign: Normal file read
        Event(
            "evt-002", "file_read",
            "/usr/bin/python3", "/home/user/document.txt",
            metadata={"signed": True, "reputation": 0.9}
        ),
        # Suspicious: Encoded PowerShell
        Event(
            "evt-003", "script_execution",
            "C:\\Windows\\System32\\cmd.exe", "powershell.exe",
            metadata={"command_line": "powershell -encodedCommand SGVsbG8gV29ybGQ="}
        ),
        # Suspicious: C2 connection
        Event(
            "evt-004", "network_connect",
            "/tmp/backdoor.exe", "185.123.45.67:4444",
            metadata={"connection_count": 50}
        ),
        # Benign: Normal web request
        Event(
            "evt-005", "network_connect",
            "/usr/bin/curl", "api.github.com:443",
            metadata={"signed": True}
        ),
        # Suspicious: Registry persistence
        Event(
            "evt-006", "registry_write",
            "C:\\Users\\Public\\malware.exe",
            "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run\\Malware",
            metadata={"elevated": True}
        ),
    ]
    
    print("=== Neural Threat Adaptation Demo ===\n")
    
    for event in test_events:
        print(f"Processing: {event.event_type} - {event.source}")
        alert = engine.predict(event)
        print(f"  Prediction: {alert.prediction} (confidence: {alert.confidence:.2f})")
        
        # Simulate feedback
        actual = "malicious" if event.event_type in ["process_injection", "script_execution"] else "benign"
        engine.provide_feedback(event.event_id, actual)
        engine.train(event, actual)
    
    # Adjust threshold based on performance
    print("\n=== Adjusting Threshold ===")
    engine.adjust_threshold()
    print(f"New threshold: {engine.threshold}")
    
    # Statistics
    print("\n=== Engine Statistics ===")
    stats = engine.get_statistics()
    print(f"Model Version: {stats['model_version']}")
    print(f"Training Count: {stats['training_count']}")
    print(f"Feature Count: {stats['feature_count']}")
    print(f"Malicious Detections: {stats['malicious_detections']}")
    
    # Feature importance
    print("\n=== Top Feature Importance ===")
    importance = engine.get_feature_importance()
    for feature, score in list(importance.items())[:10]:
        print(f"  {feature}: {score:.3f}")
