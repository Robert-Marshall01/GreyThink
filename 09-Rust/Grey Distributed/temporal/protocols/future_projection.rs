// Grey Distributed — Future Projection Protocol
// Projects workload requirements and system evolution into future epochs,
// enabling proactive adaptation and continuity planning.

use std::collections::{HashMap, BinaryHeap};
use std::cmp::Ordering;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Future epoch projection representing anticipated state of civilization.
#[derive(Debug, Clone)]
pub struct EpochProjection {
    /// Projected epoch identifier
    pub epoch_id: String,
    /// Projected start timestamp
    pub projected_start: u64,
    /// Projected end timestamp (None for open-ended)
    pub projected_end: Option<u64>,
    /// Confidence level (0.0-1.0)
    pub confidence: f64,
    /// Technological assumptions
    pub tech_assumptions: TechAssumptions,
    /// Governance model projection
    pub governance_model: GovernanceProjection,
    /// Resource availability projection
    pub resources: ResourceProjection,
    /// Key uncertainties
    pub uncertainties: Vec<Uncertainty>,
}

/// Technological assumptions for epoch projection.
#[derive(Debug, Clone)]
pub struct TechAssumptions {
    /// Computational paradigm (classical, quantum, hybrid, unknown)
    pub compute_paradigm: ComputeParadigm,
    /// Storage technology class
    pub storage_class: StorageClass,
    /// Network topology assumption
    pub network_topology: NetworkTopology,
    /// Energy availability level
    pub energy_level: EnergyLevel,
    /// AI/automation level
    pub automation_level: AutomationLevel,
}

/// Computational paradigm projection.
#[derive(Debug, Clone)]
pub enum ComputeParadigm {
    /// Classical von Neumann architecture
    Classical,
    /// Quantum computing dominant
    Quantum,
    /// Hybrid classical-quantum
    HybridQuantum,
    /// Neuromorphic computing
    Neuromorphic,
    /// Biological computing
    Biological,
    /// Unknown future paradigm
    Unknown,
}

/// Storage technology class projection.
#[derive(Debug, Clone)]
pub enum StorageClass {
    /// Magnetic/SSD storage
    ElectroMagnetic,
    /// DNA/molecular storage
    Molecular,
    /// Holographic storage
    Holographic,
    /// Quantum memory
    QuantumMemory,
    /// Crystalline storage (million-year durability)
    Crystalline,
    /// Unknown future storage
    Unknown,
}

/// Network topology projection.
#[derive(Debug, Clone)]
pub enum NetworkTopology {
    /// Internet-like distributed mesh
    DistributedMesh,
    /// Interplanetary network with high latency
    Interplanetary,
    /// Quantum entanglement network
    QuantumEntangled,
    /// Hierarchical federated network
    HierarchicalFederation,
    /// Unknown future topology
    Unknown,
}

/// Energy availability projection.
#[derive(Debug, Clone)]
pub enum EnergyLevel {
    /// Constrained (similar to present)
    Constrained,
    /// Abundant (fusion, solar capture)
    Abundant,
    /// Post-scarcity energy
    PostScarcity,
    /// Variable (dependent on factors)
    Variable,
}

/// AI/automation level projection.
#[derive(Debug, Clone)]
pub enum AutomationLevel {
    /// Human-directed automation
    HumanDirected,
    /// Autonomous systems with oversight
    AutonomousWithOversight,
    /// Fully autonomous AI civilization
    FullyAutonomous,
    /// Mixed human-AI governance
    MixedGovernance,
}

/// Governance model projection.
#[derive(Debug, Clone)]
pub struct GovernanceProjection {
    /// Primary governance structure
    pub structure: GovernanceStructure,
    /// Decision-making process
    pub decision_process: DecisionProcess,
    /// Rights framework
    pub rights_framework: RightsFramework,
    /// Enforcement mechanism
    pub enforcement: EnforcementMechanism,
}

/// Governance structure types.
#[derive(Debug, Clone)]
pub enum GovernanceStructure {
    /// Nation-states with international coordination
    NationState,
    /// Global federation
    GlobalFederation,
    /// Interplanetary federation
    InterplanetaryFederation,
    /// AI-coordinated governance
    AICoordinated,
    /// Decentralized autonomous governance
    DecentralizedAutonomous,
    /// Multi-species council
    MultiSpeciesCouncil,
}

/// Decision-making process types.
#[derive(Debug, Clone)]
pub enum DecisionProcess {
    /// Representative democracy
    Representative,
    /// Direct democracy (enabled by tech)
    DirectDemocracy,
    /// Consensus-based
    ConsensusBased,
    /// AI-optimized decisions
    AIOptimized,
    /// Hybrid human-AI deliberation
    HybridDeliberation,
}

/// Rights framework types.
#[derive(Debug, Clone)]
pub enum RightsFramework {
    /// Human rights only
    HumanOnly,
    /// Extended to AI entities
    HumanAndAI,
    /// All conscious entities
    AllConsciousness,
    /// Ecosystem-inclusive rights
    EcosystemInclusive,
}

/// Enforcement mechanism types.
#[derive(Debug, Clone)]
pub enum EnforcementMechanism {
    /// State-based enforcement
    StateBased,
    /// Reputation-based
    ReputationBased,
    /// Cryptographic enforcement
    CryptographicEnforcement,
    /// AI-mediated enforcement
    AIMediated,
    /// Social/community enforcement
    SocialCommunity,
}

/// Resource availability projection.
#[derive(Debug, Clone)]
pub struct ResourceProjection {
    /// Compute resources (normalized units)
    pub compute_capacity: ResourceLevel,
    /// Storage resources
    pub storage_capacity: ResourceLevel,
    /// Network bandwidth
    pub network_bandwidth: ResourceLevel,
    /// Human attention (most scarce)
    pub human_attention: ResourceLevel,
    /// Energy availability
    pub energy_availability: ResourceLevel,
}

/// Resource level classification.
#[derive(Debug, Clone)]
pub enum ResourceLevel {
    /// Severely constrained
    Scarce,
    /// Limited but available
    Limited,
    /// Adequate for needs
    Adequate,
    /// Abundant
    Abundant,
    /// Post-scarcity
    PostScarcity,
}

/// Uncertainty in projection.
#[derive(Debug, Clone)]
pub struct Uncertainty {
    /// Uncertainty category
    pub category: UncertaintyCategory,
    /// Description
    pub description: String,
    /// Impact if assumption wrong (0.0-1.0)
    pub impact: f64,
    /// Probability of assumption being wrong (0.0-1.0)
    pub probability: f64,
    /// Mitigation strategies
    pub mitigations: Vec<String>,
}

/// Categories of uncertainty.
#[derive(Debug, Clone)]
pub enum UncertaintyCategory {
    /// Technological development path
    Technological,
    /// Social/political evolution
    SocialPolitical,
    /// Economic system changes
    Economic,
    /// Environmental factors
    Environmental,
    /// Existential risks
    Existential,
    /// Unknown unknowns
    Unknown,
}

/// Workload projection for future epochs.
#[derive(Debug, Clone)]
pub struct WorkloadProjection {
    /// Workload identifier
    pub workload_id: String,
    /// Target epoch
    pub target_epoch: EpochProjection,
    /// Projected resource requirements
    pub resource_requirements: ProjectedRequirements,
    /// Translation requirements
    pub translation_requirements: TranslationRequirements,
    /// Continuity strategy
    pub continuity_strategy: ContinuityStrategy,
    /// Confidence score
    pub confidence: f64,
}

/// Projected resource requirements.
#[derive(Debug, Clone)]
pub struct ProjectedRequirements {
    /// Compute (in normalized units)
    pub compute: f64,
    /// Storage (in bytes)
    pub storage_bytes: u64,
    /// Network (in operations per second)
    pub network_ops: u64,
    /// Human review hours (if any)
    pub human_hours: f64,
    /// Scaling factor from current (1.0 = same)
    pub scaling_factor: f64,
}

/// Translation requirements for epoch transition.
#[derive(Debug, Clone)]
pub struct TranslationRequirements {
    /// Format migrations needed
    pub format_migrations: Vec<FormatMigration>,
    /// Semantic translations needed
    pub semantic_translations: Vec<SemanticTranslation>,
    /// Governance adaptations needed
    pub governance_adaptations: Vec<GovernanceAdaptation>,
    /// Estimated effort (human-months)
    pub effort_estimate: f64,
}

/// Format migration specification.
#[derive(Debug, Clone)]
pub struct FormatMigration {
    /// Source format
    pub from_format: String,
    /// Target format
    pub to_format: String,
    /// Complexity (1-10)
    pub complexity: u8,
    /// Lossless possible
    pub lossless: bool,
}

/// Semantic translation specification.
#[derive(Debug, Clone)]
pub struct SemanticTranslation {
    /// Concept requiring translation
    pub concept: String,
    /// Current representation
    pub current_representation: String,
    /// Projected representation
    pub projected_representation: String,
    /// Confidence in translation
    pub confidence: f64,
}

/// Governance adaptation specification.
#[derive(Debug, Clone)]
pub struct GovernanceAdaptation {
    /// Policy requiring adaptation
    pub policy: String,
    /// Reason for adaptation
    pub reason: String,
    /// Proposed adaptation
    pub proposed_adaptation: String,
    /// Requires approval
    pub requires_approval: bool,
}

/// Continuity strategy for workload across epochs.
#[derive(Debug, Clone)]
pub enum ContinuityStrategy {
    /// Active maintenance (continuous updates)
    ActiveMaintenance,
    /// Hibernation (periodic activation)
    Hibernation { activation_interval_years: u32 },
    /// Archival (preservation only)
    Archival,
    /// Handoff (transfer to successor system)
    Handoff { successor_criteria: String },
    /// Termination (planned end-of-life)
    Termination { termination_epoch: String },
}

/// Scenario for future projection.
#[derive(Debug, Clone)]
pub struct FutureScenario {
    /// Scenario identifier
    pub scenario_id: String,
    /// Scenario name
    pub name: String,
    /// Description
    pub description: String,
    /// Probability (0.0-1.0)
    pub probability: f64,
    /// Epoch projections in this scenario
    pub epochs: Vec<EpochProjection>,
    /// Key assumptions
    pub assumptions: Vec<String>,
    /// Implications for Grey Distributed
    pub implications: Vec<String>,
}

/// Priority workload for future projection.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PriorityWorkload {
    /// Workload ID
    pub workload_id: String,
    /// Priority score (higher = more important)
    pub priority: u64,
    /// Target epoch
    pub target_epoch_id: String,
}

impl Ord for PriorityWorkload {
    fn cmp(&self, other: &Self) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for PriorityWorkload {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Future Projection Engine: Core system for projecting workloads into future epochs.
pub struct FutureProjection {
    /// Registered epoch projections
    epochs: HashMap<String, EpochProjection>,
    /// Defined scenarios
    scenarios: HashMap<String, FutureScenario>,
    /// Active workload projections
    workload_projections: HashMap<String, WorkloadProjection>,
    /// Priority queue for projection planning
    priority_queue: BinaryHeap<PriorityWorkload>,
    /// Projection horizon (years into future)
    horizon_years: u32,
    /// Confidence decay rate (per decade)
    confidence_decay: f64,
}

impl FutureProjection {
    /// Create a new Future Projection engine.
    pub fn new(horizon_years: u32) -> Self {
        let mut engine = FutureProjection {
            epochs: HashMap::new(),
            scenarios: HashMap::new(),
            workload_projections: HashMap::new(),
            priority_queue: BinaryHeap::new(),
            horizon_years,
            confidence_decay: 0.1, // 10% decay per decade
        };
        
        // Initialize with default epoch projections
        engine.initialize_default_epochs();
        engine.initialize_default_scenarios();
        
        engine
    }
    
    /// Initialize default epoch projections.
    fn initialize_default_epochs(&mut self) {
        // Near future (2030-2050)
        self.epochs.insert("NEAR-FUTURE".to_string(), EpochProjection {
            epoch_id: "NEAR-FUTURE".to_string(),
            projected_start: Self::year_to_timestamp(2030),
            projected_end: Some(Self::year_to_timestamp(2050)),
            confidence: 0.85,
            tech_assumptions: TechAssumptions {
                compute_paradigm: ComputeParadigm::HybridQuantum,
                storage_class: StorageClass::ElectroMagnetic,
                network_topology: NetworkTopology::DistributedMesh,
                energy_level: EnergyLevel::Constrained,
                automation_level: AutomationLevel::AutonomousWithOversight,
            },
            governance_model: GovernanceProjection {
                structure: GovernanceStructure::NationState,
                decision_process: DecisionProcess::HybridDeliberation,
                rights_framework: RightsFramework::HumanAndAI,
                enforcement: EnforcementMechanism::StateBased,
            },
            resources: ResourceProjection {
                compute_capacity: ResourceLevel::Abundant,
                storage_capacity: ResourceLevel::Abundant,
                network_bandwidth: ResourceLevel::Abundant,
                human_attention: ResourceLevel::Scarce,
                energy_availability: ResourceLevel::Limited,
            },
            uncertainties: vec![
                Uncertainty {
                    category: UncertaintyCategory::Technological,
                    description: "Quantum computing adoption rate".to_string(),
                    impact: 0.4,
                    probability: 0.3,
                    mitigations: vec!["Maintain classical fallbacks".to_string()],
                },
            ],
        });
        
        // Mid-century (2050-2100)
        self.epochs.insert("MID-CENTURY".to_string(), EpochProjection {
            epoch_id: "MID-CENTURY".to_string(),
            projected_start: Self::year_to_timestamp(2050),
            projected_end: Some(Self::year_to_timestamp(2100)),
            confidence: 0.60,
            tech_assumptions: TechAssumptions {
                compute_paradigm: ComputeParadigm::Quantum,
                storage_class: StorageClass::Molecular,
                network_topology: NetworkTopology::Interplanetary,
                energy_level: EnergyLevel::Abundant,
                automation_level: AutomationLevel::FullyAutonomous,
            },
            governance_model: GovernanceProjection {
                structure: GovernanceStructure::GlobalFederation,
                decision_process: DecisionProcess::AIOptimized,
                rights_framework: RightsFramework::AllConsciousness,
                enforcement: EnforcementMechanism::CryptographicEnforcement,
            },
            resources: ResourceProjection {
                compute_capacity: ResourceLevel::PostScarcity,
                storage_capacity: ResourceLevel::PostScarcity,
                network_bandwidth: ResourceLevel::Abundant,
                human_attention: ResourceLevel::Scarce,
                energy_availability: ResourceLevel::Abundant,
            },
            uncertainties: vec![
                Uncertainty {
                    category: UncertaintyCategory::SocialPolitical,
                    description: "Global governance emergence".to_string(),
                    impact: 0.6,
                    probability: 0.5,
                    mitigations: vec!["Design for multiple governance models".to_string()],
                },
            ],
        });
        
        // Far future (2100-2500)
        self.epochs.insert("FAR-FUTURE".to_string(), EpochProjection {
            epoch_id: "FAR-FUTURE".to_string(),
            projected_start: Self::year_to_timestamp(2100),
            projected_end: Some(Self::year_to_timestamp(2500)),
            confidence: 0.30,
            tech_assumptions: TechAssumptions {
                compute_paradigm: ComputeParadigm::Unknown,
                storage_class: StorageClass::Crystalline,
                network_topology: NetworkTopology::QuantumEntangled,
                energy_level: EnergyLevel::PostScarcity,
                automation_level: AutomationLevel::FullyAutonomous,
            },
            governance_model: GovernanceProjection {
                structure: GovernanceStructure::InterplanetaryFederation,
                decision_process: DecisionProcess::ConsensusBased,
                rights_framework: RightsFramework::EcosystemInclusive,
                enforcement: EnforcementMechanism::AIMediated,
            },
            resources: ResourceProjection {
                compute_capacity: ResourceLevel::PostScarcity,
                storage_capacity: ResourceLevel::PostScarcity,
                network_bandwidth: ResourceLevel::PostScarcity,
                human_attention: ResourceLevel::Limited,
                energy_availability: ResourceLevel::PostScarcity,
            },
            uncertainties: vec![
                Uncertainty {
                    category: UncertaintyCategory::Existential,
                    description: "Civilizational continuity".to_string(),
                    impact: 1.0,
                    probability: 0.2,
                    mitigations: vec![
                        "Multi-world redundancy".to_string(),
                        "Deep archival systems".to_string(),
                    ],
                },
            ],
        });
        
        // Deep future (2500+)
        self.epochs.insert("DEEP-FUTURE".to_string(), EpochProjection {
            epoch_id: "DEEP-FUTURE".to_string(),
            projected_start: Self::year_to_timestamp(2500),
            projected_end: None,
            confidence: 0.10,
            tech_assumptions: TechAssumptions {
                compute_paradigm: ComputeParadigm::Unknown,
                storage_class: StorageClass::Unknown,
                network_topology: NetworkTopology::Unknown,
                energy_level: EnergyLevel::PostScarcity,
                automation_level: AutomationLevel::FullyAutonomous,
            },
            governance_model: GovernanceProjection {
                structure: GovernanceStructure::MultiSpeciesCouncil,
                decision_process: DecisionProcess::ConsensusBased,
                rights_framework: RightsFramework::EcosystemInclusive,
                enforcement: EnforcementMechanism::SocialCommunity,
            },
            resources: ResourceProjection {
                compute_capacity: ResourceLevel::PostScarcity,
                storage_capacity: ResourceLevel::PostScarcity,
                network_bandwidth: ResourceLevel::PostScarcity,
                human_attention: ResourceLevel::Unknown,
                energy_availability: ResourceLevel::PostScarcity,
            },
            uncertainties: vec![
                Uncertainty {
                    category: UncertaintyCategory::Unknown,
                    description: "Fundamental unknowability of distant future".to_string(),
                    impact: 1.0,
                    probability: 0.8,
                    mitigations: vec![
                        "Design for maximal adaptability".to_string(),
                        "Preserve core values, not implementations".to_string(),
                    ],
                },
            ],
        });
    }
    
    /// Initialize default scenarios.
    fn initialize_default_scenarios(&mut self) {
        // Optimistic scenario
        self.scenarios.insert("OPTIMISTIC".to_string(), FutureScenario {
            scenario_id: "OPTIMISTIC".to_string(),
            name: "Technological Renaissance".to_string(),
            description: "Rapid technological progress, global cooperation, solved energy".to_string(),
            probability: 0.25,
            epochs: vec![],
            assumptions: vec![
                "Fusion energy achieved by 2040".to_string(),
                "Global climate cooperation".to_string(),
                "Beneficial AI alignment solved".to_string(),
            ],
            implications: vec![
                "Abundant resources for all workloads".to_string(),
                "Rapid format evolution".to_string(),
                "Governance becomes more distributed".to_string(),
            ],
        });
        
        // Baseline scenario
        self.scenarios.insert("BASELINE".to_string(), FutureScenario {
            scenario_id: "BASELINE".to_string(),
            name: "Gradual Progress".to_string(),
            description: "Steady technological progress, mixed cooperation".to_string(),
            probability: 0.50,
            epochs: vec![],
            assumptions: vec![
                "Continued Moore's law equivalent".to_string(),
                "Climate adaptation succeeds".to_string(),
                "AI remains tool-like".to_string(),
            ],
            implications: vec![
                "Resource competition continues".to_string(),
                "Format migration at predictable pace".to_string(),
                "Governance evolution slow but steady".to_string(),
            ],
        });
        
        // Pessimistic scenario
        self.scenarios.insert("PESSIMISTIC".to_string(), FutureScenario {
            scenario_id: "PESSIMISTIC".to_string(),
            name: "Fragmented Future".to_string(),
            description: "Technological stagnation, fragmented governance".to_string(),
            probability: 0.20,
            epochs: vec![],
            assumptions: vec![
                "Energy constraints persist".to_string(),
                "Geopolitical fragmentation".to_string(),
                "AI development restricted".to_string(),
            ],
            implications: vec![
                "Resource scarcity for workloads".to_string(),
                "Format fragmentation across regions".to_string(),
                "Multiple incompatible governance models".to_string(),
            ],
        });
        
        // Discontinuity scenario
        self.scenarios.insert("DISCONTINUITY".to_string(), FutureScenario {
            scenario_id: "DISCONTINUITY".to_string(),
            name: "Civilizational Discontinuity".to_string(),
            description: "Major disruption requiring rebuilding".to_string(),
            probability: 0.05,
            epochs: vec![],
            assumptions: vec![
                "Major catastrophic event".to_string(),
                "Significant knowledge loss".to_string(),
                "Rebuilding over centuries".to_string(),
            ],
            implications: vec![
                "Archival systems critical".to_string(),
                "Self-bootstrapping required".to_string(),
                "Ultra-long-term preservation essential".to_string(),
            ],
        });
    }
    
    /// Convert year to Unix timestamp.
    fn year_to_timestamp(year: u32) -> u64 {
        // Approximate: seconds since 1970
        ((year as u64) - 1970) * 365 * 24 * 60 * 60
    }
    
    /// Project a workload into a future epoch.
    pub fn project_workload(
        &mut self,
        workload_id: &str,
        target_epoch_id: &str,
        current_requirements: ProjectedRequirements,
    ) -> Result<WorkloadProjection, String> {
        let epoch = self.epochs.get(target_epoch_id)
            .ok_or(format!("Unknown epoch: {}", target_epoch_id))?
            .clone();
        
        // Calculate scaling based on epoch assumptions
        let scaling_factor = self.calculate_resource_scaling(&epoch);
        
        // Project requirements
        let projected_requirements = ProjectedRequirements {
            compute: current_requirements.compute * scaling_factor,
            storage_bytes: (current_requirements.storage_bytes as f64 * scaling_factor.sqrt()) as u64,
            network_ops: (current_requirements.network_ops as f64 * scaling_factor.sqrt()) as u64,
            human_hours: current_requirements.human_hours * (2.0 - scaling_factor).max(0.1),
            scaling_factor,
        };
        
        // Determine translation requirements
        let translation_requirements = self.determine_translation_requirements(&epoch);
        
        // Determine continuity strategy
        let continuity_strategy = self.determine_continuity_strategy(&epoch);
        
        // Calculate confidence (decays with time)
        let confidence = epoch.confidence * self.calculate_confidence_decay(&epoch);
        
        let projection = WorkloadProjection {
            workload_id: workload_id.to_string(),
            target_epoch: epoch,
            resource_requirements: projected_requirements,
            translation_requirements,
            continuity_strategy,
            confidence,
        };
        
        self.workload_projections.insert(workload_id.to_string(), projection.clone());
        Ok(projection)
    }
    
    /// Calculate resource scaling factor based on epoch.
    fn calculate_resource_scaling(&self, epoch: &EpochProjection) -> f64 {
        let compute_factor = match epoch.resources.compute_capacity {
            ResourceLevel::Scarce => 10.0, // Need 10x more due to scarcity
            ResourceLevel::Limited => 2.0,
            ResourceLevel::Adequate => 1.0,
            ResourceLevel::Abundant => 0.5,
            ResourceLevel::PostScarcity => 0.1,
        };
        
        let storage_factor = match epoch.resources.storage_capacity {
            ResourceLevel::Scarce => 5.0,
            ResourceLevel::Limited => 1.5,
            ResourceLevel::Adequate => 1.0,
            ResourceLevel::Abundant => 0.5,
            ResourceLevel::PostScarcity => 0.1,
        };
        
        (compute_factor + storage_factor) / 2.0
    }
    
    /// Determine translation requirements for epoch.
    fn determine_translation_requirements(&self, epoch: &EpochProjection) -> TranslationRequirements {
        let mut format_migrations = Vec::new();
        let mut semantic_translations = Vec::new();
        let mut governance_adaptations = Vec::new();
        
        // Format migrations based on storage class
        match epoch.tech_assumptions.storage_class {
            StorageClass::Molecular | StorageClass::Holographic => {
                format_migrations.push(FormatMigration {
                    from_format: "binary-blob".to_string(),
                    to_format: "molecular-encoding".to_string(),
                    complexity: 8,
                    lossless: true,
                });
            }
            StorageClass::Crystalline => {
                format_migrations.push(FormatMigration {
                    from_format: "binary-blob".to_string(),
                    to_format: "crystalline-lattice".to_string(),
                    complexity: 9,
                    lossless: true,
                });
            }
            _ => {}
        }
        
        // Governance adaptations based on rights framework
        match epoch.governance_model.rights_framework {
            RightsFramework::AllConsciousness | RightsFramework::EcosystemInclusive => {
                governance_adaptations.push(GovernanceAdaptation {
                    policy: "access-control".to_string(),
                    reason: "Rights framework expanded beyond humans".to_string(),
                    proposed_adaptation: "Consciousness-neutral access policies".to_string(),
                    requires_approval: true,
                });
            }
            _ => {}
        }
        
        // Semantic translations based on automation level
        match epoch.tech_assumptions.automation_level {
            AutomationLevel::FullyAutonomous => {
                semantic_translations.push(SemanticTranslation {
                    concept: "human-approval".to_string(),
                    current_representation: "await_human_input()".to_string(),
                    projected_representation: "await_authorized_entity()".to_string(),
                    confidence: 0.7,
                });
            }
            _ => {}
        }
        
        // Estimate effort
        let effort = format_migrations.len() as f64 * 2.0
            + semantic_translations.len() as f64 * 1.5
            + governance_adaptations.len() as f64 * 3.0;
        
        TranslationRequirements {
            format_migrations,
            semantic_translations,
            governance_adaptations,
            effort_estimate: effort,
        }
    }
    
    /// Determine continuity strategy for epoch.
    fn determine_continuity_strategy(&self, epoch: &EpochProjection) -> ContinuityStrategy {
        // Base strategy on epoch confidence and uncertainties
        if epoch.confidence < 0.2 {
            ContinuityStrategy::Archival
        } else if epoch.confidence < 0.4 {
            ContinuityStrategy::Hibernation { activation_interval_years: 25 }
        } else if epoch.confidence < 0.7 {
            ContinuityStrategy::Hibernation { activation_interval_years: 10 }
        } else {
            ContinuityStrategy::ActiveMaintenance
        }
    }
    
    /// Calculate confidence decay based on time horizon.
    fn calculate_confidence_decay(&self, epoch: &EpochProjection) -> f64 {
        let current_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or(Duration::ZERO)
            .as_secs();
        
        let years_ahead = (epoch.projected_start.saturating_sub(current_time)) / (365 * 24 * 60 * 60);
        let decades = years_ahead as f64 / 10.0;
        
        (1.0 - self.confidence_decay).powf(decades)
    }
    
    /// Get scenario-weighted projection.
    pub fn get_scenario_weighted_projection(
        &self,
        workload_id: &str,
    ) -> Option<ScenarioWeightedProjection> {
        let projection = self.workload_projections.get(workload_id)?;
        
        let mut weighted_confidence = 0.0;
        let mut scenario_impacts = Vec::new();
        
        for (scenario_id, scenario) in &self.scenarios {
            let impact = ScenarioImpact {
                scenario_id: scenario_id.clone(),
                scenario_name: scenario.name.clone(),
                probability: scenario.probability,
                impact_on_workload: self.calculate_scenario_impact(projection, scenario),
            };
            weighted_confidence += scenario.probability * impact.impact_on_workload;
            scenario_impacts.push(impact);
        }
        
        Some(ScenarioWeightedProjection {
            workload_id: workload_id.to_string(),
            base_projection: projection.clone(),
            scenario_impacts,
            weighted_confidence,
            recommended_strategy: self.recommend_strategy_across_scenarios(projection),
        })
    }
    
    /// Calculate impact of scenario on workload.
    fn calculate_scenario_impact(&self, _projection: &WorkloadProjection, scenario: &FutureScenario) -> f64 {
        match scenario.scenario_id.as_str() {
            "OPTIMISTIC" => 1.2,      // Better than projected
            "BASELINE" => 1.0,        // As projected
            "PESSIMISTIC" => 0.7,     // Worse than projected
            "DISCONTINUITY" => 0.3,   // Much worse
            _ => 1.0,
        }
    }
    
    /// Recommend strategy across all scenarios.
    fn recommend_strategy_across_scenarios(&self, projection: &WorkloadProjection) -> ContinuityStrategy {
        // Given uncertainty, prefer more robust strategies
        match &projection.continuity_strategy {
            ContinuityStrategy::ActiveMaintenance => {
                ContinuityStrategy::Hibernation { activation_interval_years: 5 }
            }
            other => other.clone(),
        }
    }
    
    /// Add workload to priority queue for projection planning.
    pub fn prioritize_workload(&mut self, workload_id: &str, priority: u64, target_epoch_id: &str) {
        self.priority_queue.push(PriorityWorkload {
            workload_id: workload_id.to_string(),
            priority,
            target_epoch_id: target_epoch_id.to_string(),
        });
    }
    
    /// Get next workload to project.
    pub fn get_next_priority_workload(&mut self) -> Option<PriorityWorkload> {
        self.priority_queue.pop()
    }
    
    /// Get all registered epoch projections.
    pub fn get_epochs(&self) -> &HashMap<String, EpochProjection> {
        &self.epochs
    }
    
    /// Get all scenarios.
    pub fn get_scenarios(&self) -> &HashMap<String, FutureScenario> {
        &self.scenarios
    }
}

/// Scenario impact on workload.
#[derive(Debug, Clone)]
pub struct ScenarioImpact {
    /// Scenario identifier
    pub scenario_id: String,
    /// Scenario name
    pub scenario_name: String,
    /// Scenario probability
    pub probability: f64,
    /// Impact factor on workload (1.0 = neutral)
    pub impact_on_workload: f64,
}

/// Scenario-weighted projection.
#[derive(Debug, Clone)]
pub struct ScenarioWeightedProjection {
    /// Workload identifier
    pub workload_id: String,
    /// Base projection
    pub base_projection: WorkloadProjection,
    /// Impact from each scenario
    pub scenario_impacts: Vec<ScenarioImpact>,
    /// Probability-weighted confidence
    pub weighted_confidence: f64,
    /// Recommended strategy considering all scenarios
    pub recommended_strategy: ContinuityStrategy,
}

impl ResourceLevel {
    /// Check if resource level is unknown (for deep future).
    fn is_unknown(&self) -> bool {
        matches!(self, ResourceLevel::Unknown)
    }
}

/// Extension for unknown resource level.
impl ResourceLevel {
    const Unknown: ResourceLevel = ResourceLevel::Scarce; // Fallback
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_epoch_initialization() {
        let engine = FutureProjection::new(500);
        assert!(engine.epochs.contains_key("NEAR-FUTURE"));
        assert!(engine.epochs.contains_key("MID-CENTURY"));
        assert!(engine.epochs.contains_key("FAR-FUTURE"));
        assert!(engine.epochs.contains_key("DEEP-FUTURE"));
    }
    
    #[test]
    fn test_scenario_initialization() {
        let engine = FutureProjection::new(500);
        assert!(engine.scenarios.contains_key("OPTIMISTIC"));
        assert!(engine.scenarios.contains_key("BASELINE"));
        assert!(engine.scenarios.contains_key("PESSIMISTIC"));
        assert!(engine.scenarios.contains_key("DISCONTINUITY"));
    }
    
    #[test]
    fn test_workload_projection() {
        let mut engine = FutureProjection::new(500);
        let requirements = ProjectedRequirements {
            compute: 100.0,
            storage_bytes: 1_000_000,
            network_ops: 10_000,
            human_hours: 10.0,
            scaling_factor: 1.0,
        };
        
        let projection = engine.project_workload(
            "test-workload",
            "NEAR-FUTURE",
            requirements,
        );
        
        assert!(projection.is_ok());
        let proj = projection.unwrap();
        assert!(proj.confidence > 0.0);
        assert!(proj.confidence <= 1.0);
    }
    
    #[test]
    fn test_priority_queue() {
        let mut engine = FutureProjection::new(500);
        engine.prioritize_workload("low-priority", 10, "NEAR-FUTURE");
        engine.prioritize_workload("high-priority", 100, "NEAR-FUTURE");
        engine.prioritize_workload("medium-priority", 50, "NEAR-FUTURE");
        
        let next = engine.get_next_priority_workload();
        assert!(next.is_some());
        assert_eq!(next.unwrap().workload_id, "high-priority");
    }
}
