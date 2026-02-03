# Grey Firmware Documentation

## Overview

Grey Firmware is a comprehensive embedded firmware demonstration project showcasing production-grade architecture, design patterns, and implementation techniques used in professional firmware development across multiple industries.

## Purpose

This project serves as a portfolio piece demonstrating:

- **Modular Architecture**: Clean separation of concerns with well-defined interfaces
- **Production Patterns**: Error handling, logging, message passing, scheduling
- **Spotlight Subsystems**: Deep-dive implementations of CAN bus, secure bootloader, power management, networking, Edge AI/ML, Smart Home (Zigbee/Matter), Energy Harvesting/Smart Grid, Drone Flight Control, Edge Vision Processing, Marine Sonar & Telemetry, Wind Turbine Control & Grid Sync, Space Habitat Life Support, Battery Management System, Avionics Flight Data Recorder, Satellite Communications, Microgrid Controller, Firefighting Thermal Imaging & Suppression, Pipeline Leak Detection & Safety, Hydrogen Fuel Cell Control & Telemetry, Rocket Engine Telemetry & Safety, Radiation Shielding Control, Orbital Debris Tracking & Collision Prediction, Autonomous Vessel Control & Safety, Regolith Extraction & Yield Telemetry, Zero-Gravity Fabrication Control, Asteroid Drill Control & Telemetry, and Hyperloop Pod Control & Safety
- **Domain Expertise**: Audio/multimedia, HMI, safety-critical, timekeeping, medical devices, aerospace, industrial IoT, wearables, cloud integration, space systems, quantum hardware, smart agriculture, advanced robotics, medical imaging, automotive infotainment, drone systems, smart manufacturing, edge security, biomedical wearables, smart transportation, defense systems, environmental monitoring, marine systems, smart cities, space robotics, edge blockchain, renewable infrastructure, biomedical robotics, smart retail, disaster response, edge cryptography, space habitats, smart healthcare, advanced logistics, renewable water systems, edge quantum security, neurotech interfaces, smart rail systems, disaster recovery networks, edge robotics swarms, smart aviation, precision medicine devices, smart mining systems, edge financial systems, smart education technology, orbital manufacturing, asteroid mining
- **Reliability Engineering**: Task watchdogs, fault logging, recovery strategies, fault injection
- **Testing Practices**: Unit, integration, fault injection, network, AI, Smart Home, Energy, Drone, Vision, Marine, Renewable, Habitat, BMS, FDR, Satellite, Microgrid, Firefighting, Pipeline, Hydrogen, Rocket, Radiation, Debris, Vessel, Lunar, Orbital, Asteroid, and Hyperloop tests (1190+ total)
- **Build System Expertise**: Makefile and CMake with cross-compilation support

## Project Structure

```
Grey Firmware/
├── include/                    # Header files
│   ├── grey_firmware.h         # Master header
│   ├── core/                   # Core framework headers
│   │   ├── scheduler.h         # RTOS-style scheduler
│   │   ├── driver_registry.h   # Hardware abstraction
│   │   ├── message_bus.h       # Pub/sub messaging
│   │   ├── error_handler.h     # Error management
│   │   └── logging.h           # Logging with fault injection
│   ├── automotive/             # Automotive module headers
│   │   ├── can_bus.h           # CAN 2.0B driver
│   │   ├── airbag_sensor.h     # ASIL-D crash sensor
│   │   ├── abs_control.h       # ABS control loop
│   │   └── safety_diagnostics.h # ISO 26262 diagnostics
│   ├── iot/                    # IoT module headers
│   ├── consumer/               # Consumer electronics headers
│   ├── medical/                # Medical device headers
│   │   ├── biosensor.h         # ECG/PPG biosensor driver
│   │   ├── signal_processing.h # Medical signal filtering
│   │   └── compliance_log.h    # HIPAA compliance logging
│   ├── security/               # Security module headers
│   ├── power/                  # Power management headers
│   ├── audio/                  # Audio/multimedia headers
│   ├── hmi/                    # HMI display/touch headers
│   ├── safety/                 # Safety-critical headers
│   ├── time/                   # Timekeeping/RTC headers
│   ├── net/                    # Networking stack headers
│   ├── aerospace/              # Aerospace/avionics headers
│   │   ├── sensor_fusion.h     # IMU+GPS sensor fusion
│   │   ├── redundancy_manager.h # Triple modular redundancy
│   │   └── fault_tolerant_fsm.h # DO-178C state machine
│   ├── industrial/             # Industrial IoT headers
│   │   ├── modbus.h            # Modbus RTU/TCP driver
│   │   ├── plc_interface.h     # PLC communication
│   │   └── telemetry_collector.h # Industrial telemetry
│   ├── ai/                     # Edge AI/ML headers (SPOTLIGHT)
│   │   ├── inference.h         # Neural network inference
│   │   ├── model_loader.h      # Model loading/verification
│   │   └── feature_extraction.h # Preprocessing pipeline
│   ├── smarthome/              # Smart Home headers (SPOTLIGHT)
│   │   ├── zigbee.h            # Zigbee 3.0 protocol
│   │   ├── matter.h            # Matter/CHIP protocol
│   │   ├── lighting.h          # Smart lighting control
│   │   └── event_bus.h         # Home automation events
│   ├── wearables/              # Wearables domain headers
│   │   ├── heart_rate.h        # PPG sensor driver
│   │   ├── step_counter.h      # Accelerometer pedometer
│   │   └── haptic.h            # Haptic feedback driver
│   ├── industrial_safety/      # Industrial safety headers
│   │   ├── gas_sensor.h        # Multi-gas detection
│   │   ├── emergency_shutdown.h # ESD system
│   │   └── safety_logger.h     # Compliance logging
│   ├── cloud/                  # Cloud integration headers
│   │   ├── rest_client.h       # RESTful API client
│   │   ├── graphql.h           # GraphQL client
│   │   └── telemetry.h         # Cloud telemetry uploader
│   ├── storage/                # Edge storage headers
│   │   ├── cache.h             # Local caching module
│   │   ├── compression.h       # Data compression
│   │   └── sync_manager.h      # Cloud/offline sync
│   ├── energy/                 # Energy harvesting headers (SPOTLIGHT)
│   │   ├── solar_panel.h       # Solar panel driver with MPPT
│   │   ├── energy_harvesting.h # Multi-source harvesting
│   │   └── smart_grid.h        # Smart grid telemetry
│   ├── space/                  # Space systems headers (SPOTLIGHT)
│   │   ├── radiation_sensor.h  # Rad-hard sensor driver
│   │   ├── orbital_telemetry.h # CCSDS Space Packet Protocol
│   │   ├── fault_tolerant_comm.h # TMR communications
│   │   ├── sat_uplink.h        # Satellite uplink/downlink protocol
│   │   ├── error_correction.h  # Reed-Solomon/LDPC/Turbo FEC
│   │   └── secure_channel.h    # Space-to-ground encryption
│   ├── quantum/                # Quantum hardware headers
│   │   ├── quantum_rng.h       # Quantum random number generator
│   │   ├── coprocessor.h       # NPU/TPU/FPGA interface
│   │   └── entropy_pool.h      # Secure entropy pool
│   ├── agriculture/            # Smart agriculture headers
│   │   ├── soil_sensor.h       # Soil moisture/pH/EC/NPK
│   │   ├── irrigation.h        # Zone-based irrigation control
│   │   ├── crop_telemetry.h    # Crop analytics and alerts
│   │   ├── tractor_control.h   # Autonomous tractor control (RTK GPS)
│   │   ├── crop_monitor.h      # Multispectral crop monitoring
│   │   └── irrigation_telemetry.h # Variable rate irrigation
│   ├── robotics/               # Advanced robotics headers
│   │   ├── motor_controller.h  # Multi-axis servo control
│   │   ├── robot_sensor_fusion.h # EKF sensor fusion
│   │   └── safety_interlock.h  # PLe Category 4 safety
│   ├── construction/           # Smart Construction headers
│   │   ├── crane_control.h     # Tower crane anti-collision
│   │   ├── structural_sensor.h # Structural health monitoring
│   │   └── safety_interlock.h  # SIL 3 construction safety
│   ├── ocean/                  # Smart Ocean Monitoring headers
│   │   ├── buoy_sensor.h       # CTD/wave/current sensors
│   │   ├── ocean_current.h     # ADCP current profiling
│   │   └── environmental_logger.h # Marine environmental compliance
│   ├── drone/                  # Drone flight control headers (SPOTLIGHT)
│   │   ├── flight_control.h    # PID-based flight control
│   │   ├── sensor_fusion.h     # GPS+IMU EKF fusion
│   │   └── telemetry.h         # MAVLink-compatible telemetry
│   ├── vision/                 # Edge Vision Processing headers (SPOTLIGHT)
│   │   ├── camera_driver.h     # Camera interface and buffering
│   │   ├── image_preprocessing.h # Grayscale, blur, normalization
│   │   └── object_detection.h  # Blob and edge-based detection
│   ├── marine/                 # Marine Systems headers (SPOTLIGHT)
│   │   ├── sonar_sensor.h      # Sonar transducer driver
│   │   ├── underwater_telemetry.h # Acoustic modem telemetry
│   │   └── pressure_depth.h    # Pressure sensor calibration
│   ├── renewable/              # Renewable Infrastructure headers (SPOTLIGHT)
│   │   ├── wind_turbine.h      # Wind turbine control
│   │   ├── grid_sync.h         # Grid synchronization
│   │   └── energy_telemetry.h  # Energy telemetry collection
│   ├── biomedical_robotics/    # Biomedical Robotics headers
│   │   ├── prosthetic_actuator.h # Prosthetic motor control
│   │   ├── sensor_feedback.h   # Force/position feedback
│   │   └── biomedical_compliance.h # FDA/MDR compliance
│   ├── smart_retail/           # Smart Retail headers
│   │   ├── rfid_scanner.h      # RFID reader interface
│   │   ├── inventory_telemetry.h # Stock tracking
│   │   └── pos_device.h        # Point-of-sale interface
│   ├── disaster_response/      # Disaster Response headers
│   │   ├── emergency_beacon.h  # Rescue beacon protocol
│   │   ├── hazard_detection.h  # Multi-sensor hazard fusion
│   │   └── resilient_comm.h    # Mesh/satellite fallback
│   ├── edge_crypto/            # Edge Cryptography headers
│   │   ├── post_quantum.h      # Lattice-based crypto
│   │   ├── secure_handshake.h  # TLS-like handshake
│   │   └── key_rotation.h      # Key lifecycle management
│   ├── space_habitat/          # Space Habitat Life Support (SPOTLIGHT)
│   │   ├── life_support_sensor.h # O2/CO2/humidity sensors
│   │   ├── env_control.h       # ECLSS closed-loop control
│   │   └── habitat_telemetry.h # CCSDS telemetry reporting
│   ├── energy_storage/         # Smart Energy Storage (BMS SPOTLIGHT)
│   │   ├── bms_driver.h        # Battery Management System driver
│   │   ├── thermal_monitor.h   # Multi-zone thermal monitoring
│   │   └── charge_telemetry.h  # Charge/discharge telemetry
│   ├── neurotech/              # Neurotech Interfaces
│   │   ├── eeg_sensor.h        # 256-channel EEG sensor
│   │   ├── neural_preprocessing.h # Neural signal processing
│   │   └── neuro_compliance.h  # FDA/IEC 62304 compliance
│   ├── rail_systems/           # Smart Rail Systems
│   │   ├── train_control.h     # ETCS-compatible train control
│   │   ├── track_sensor.h      # Track circuit/axle counter
│   │   └── rail_safety.h       # SIL-4 safety interlocks
│   ├── disaster_recovery/      # Disaster Recovery Networks
│   │   ├── mesh_comm.h         # Self-healing mesh networking
│   │   ├── emergency_routing.h # Priority emergency routing
│   │   └── resilient_telemetry.h # Triple-redundant telemetry
│   ├── swarm_robotics/         # Edge Robotics Swarms
│   │   ├── swarm_coordinator.h # Decentralized swarm coordination
│   │   ├── distributed_fusion.h # Multi-agent sensor fusion
│   │   └── collision_avoidance.h # Real-time collision avoidance
│   ├── aviation/               # Smart Aviation (FDR SPOTLIGHT)
│   │   ├── avionics_sensor.h   # ARINC 429/MIL-STD-1553 interface
│   │   ├── flight_data_recorder.h # FDR types and constants
│   │   └── safety_compliance.h # DO-178C/DO-254 compliance
│   ├── precision_medicine/     # Precision Medicine Devices
│   │   ├── dna_sequencer.h     # Illumina/ONT sequencer HAL
│   │   ├── sample_processing.h # Sample workflow management
│   │   └── medical_compliance.h # FDA/HIPAA compliance
│   ├── mining/                 # Smart Mining Systems
│   │   ├── drill_control.h     # Mining drill PID control
│   │   ├── geological_sensor.h # Seismic/GPR telemetry
│   │   └── mining_safety.h     # MSHA safety interlocks
│   ├── financial/              # Edge Financial Systems
│   │   ├── secure_transaction.h # PCI DSS/HSM interface
│   │   ├── lightweight_ledger.h # Merkle tree ledger
│   │   └── fraud_detection.h   # ML anomaly detection
│   ├── education/              # Smart Education Technology
│   │   ├── classroom_sensor.h  # Environmental sensing
│   │   ├── interactive_device.h # Touch/stylus interface
│   │   └── learning_telemetry.h # xAPI/SCORM analytics
│   ├── smart_healthcare/       # Smart Healthcare Facilities
│   │   ├── patient_monitor.h   # Vital signs monitoring
│   │   ├── nurse_call.h        # Nurse call station interface
│   │   ├── healthcare_compliance.h # FDA/HIPAA compliance
│   │   ├── patient_vitals.h    # Real-time ECG/SpO2/BP/temp
│   │   ├── ai_inference.h      # Medical edge AI (INT8 quantized)
│   │   └── compliance_logging.h # HIPAA/FDA 21 CFR Part 11
│   ├── microgrid/              # Microgrid Controller headers (SPOTLIGHT)
│   │   ├── grid_controller.h   # IEEE 2030.5 grid controller
│   │   ├── demand_response.h   # OpenADR 2.0 demand response
│   │   └── microgrid_telemetry.h # SCADA/Modbus telemetry
│   ├── firefighting/           # Firefighting Thermal Imaging (SPOTLIGHT)
│   │   ├── thermal_sensor.h    # 32x32 thermal imaging interface
│   │   ├── suppression_actuator.h # Valve and agent control
│   │   └── fire_telemetry.h    # NFPA-compliant telemetry
│   ├── energy_pipelines/       # Pipeline Leak Detection (SPOTLIGHT)
│   │   ├── flow_sensor.h       # Ultrasonic/Coriolis flow measurement
│   │   ├── leak_detection.h    # Pressure differential detection
│   │   └── pipeline_compliance.h # PHMSA/API compliance
│   ├── hydrogen_systems/       # Hydrogen Fuel Cell Control (SPOTLIGHT)
│   │   ├── fuelcell_sensor.h   # Fuel cell voltage/current/temp sensing
│   │   ├── electrolyzer.h      # Electrolyzer control interface
│   │   ├── electrolyzer_control.h # Electrolyzer control module
│   │   └── hydrogen_telemetry.h # Production efficiency telemetry
│   ├── wildlife_monitoring/    # Edge Wildlife Monitoring headers
│   │   ├── animal_tracker.h    # GPS/VHF/satellite tracking
│   │   ├── env_telemetry.h     # Environmental sensor telemetry
│   │   └── conservation_compliance.h # ESA/CITES compliance
│   ├── urban_mobility/         # Smart Urban Mobility headers
│   │   ├── scooter_control.h   # Autonomous scooter control
│   │   ├── traffic_telemetry.h # V2X traffic data
│   │   └── safety_interlock.h  # ISO 13849 safety interlocks
│   ├── disaster_prediction/    # Edge Disaster Prediction headers
│   │   ├── seismic_sensor.h    # MEMS accelerometer seismic sensing
│   │   ├── prediction_algorithm.h # ML prediction algorithms
│   │   └── emergency_telemetry.h # CAP/EDXL emergency telemetry
│   ├── entertainment_systems/  # Smart Entertainment Systems headers
│   │   ├── arvr_headset.h      # 90-120Hz VR display driver
│   │   ├── interactive_device.h # Haptic/controller interface
│   │   └── user_telemetry.h    # GDPR-compliant analytics
│   ├── law_enforcement/        # Smart Law Enforcement headers
│   │   ├── body_camera.h       # BWC recording (CJIS compliance)
│   │   ├── evidence_telemetry.h # Digital evidence audit trails
│   │   └── custody_compliance.h # Chain-of-custody logging
│   ├── marine_robotics/        # Edge Marine Robotics headers
│   │   ├── auv_driver.h        # AUV thruster/depth control
│   │   ├── nav_fusion.h        # DVL+IMU+depth EKF fusion
│   │   └── marine_telemetry.h  # Acoustic modem telemetry
│   ├── retail_robotics/        # Edge Retail Robotics headers
│   │   ├── shelf_robot.h       # Autonomous shelf-stocking
│   │   ├── inventory_telemetry.h # RFID/barcode tracking
│   │   └── collision_avoidance.h # LiDAR/depth safety
│   ├── cultural_heritage/      # Smart Cultural Heritage headers
│   │   ├── env_sensor.h        # Museum environment monitoring
│   │   ├── preservation_telemetry.h # Artifact conservation
│   │   └── heritage_compliance.h # UNESCO/ICOM compliance
│   ├── ports_shipping/         # Smart Ports & Shipping headers
│   │   ├── cargo_telemetry.h   # Container tracking telemetry
│   │   ├── crane_sensor.h      # Crane load and position sensing
│   │   └── loading_safety.h    # ISPS code safety compliance
│   ├── agri_biotech/           # Edge Agriculture Biotech headers
│   │   ├── crop_genetics.h     # CRISPR/gene editing telemetry
│   │   ├── nutrient_sensor.h   # NPK/micronutrient sensing
│   │   └── biotech_compliance.h # EPA/USDA compliance logging
│   ├── finance_risk/           # Edge Finance Risk headers
│   │   ├── market_feed.h       # Low-latency market data
│   │   ├── risk_analysis.h     # VaR/CVA risk calculation
│   │   └── finance_compliance.h # SEC/FINRA compliance
│   ├── hospitality/            # Smart Hospitality headers
│   │   ├── room_automation.h   # HVAC/lighting/access control
│   │   ├── guest_telemetry.h   # Guest preference analytics
│   │   └── energy_optimization.h # Energy efficiency control
│   ├── space_exploration/      # Smart Space Exploration headers
│   │   ├── rover_arm.h         # 6-DOF rover robotic arm (rad-hard)
│   │   ├── soil_analyzer.h     # Planetary soil analysis (XRF, ISRU)
│   │   └── exploration_telemetry.h # Deep space store-forward telemetry
│   ├── medical_robotics/       # Edge Medical Robotics headers
│   │   ├── surgical_actuator.h # Surgical robot actuator (IEC 62304)
│   │   ├── sensor_feedback.h   # Force/vision sensor fusion
│   │   └── medical_compliance.h # FDA/MDR compliance logging
│   ├── disaster_relief/        # Smart Disaster Relief headers
│   │   ├── swarm_coordinator.h # Multi-UAV swarm coordination
│   │   ├── supply_telemetry.h  # Emergency supply chain tracking
│   │   └── resilient_comm.h    # LoRa/satellite mesh fallback
│   ├── ai_security/            # Edge AI Security headers
│   │   ├── ai_intrusion_detection.h # AI-powered IDS (RNN/autoencoder)
│   │   ├── secure_inference.h  # Secure AI pipeline (encrypted models)
│   │   └── tamper_response.h   # AI-enhanced tamper detection
│   ├── logistics/              # Advanced Logistics
│   │   ├── warehouse_robot.h   # AMR control and path planning
│   │   ├── package_tracking.h  # RFID/barcode telemetry
│   │   └── route_optimizer.h   # TSP/VRP optimization
│   ├── water_systems/          # Renewable Water Systems
│   │   ├── desalination.h      # RO/MED/MSF control
│   │   ├── water_recycling.h   # Greywater/blackwater processing
│   │   └── water_quality.h     # Multi-parameter water sensing
│   ├── quantum_security/       # Edge Quantum Security
│   │   ├── qkd.h               # Quantum key distribution
│   │   ├── secure_channel.h    # Hybrid classical/quantum channels
│   │   └── quantum_tamper.h    # Quantum-enhanced intrusion detection
│   ├── smartcity/              # Smart Cities headers
│   │   ├── traffic_light.h     # Traffic light controller
│   │   ├── env_sensor_network.h # Environmental sensing
│   │   └── public_safety.h     # Public safety telemetry
│   ├── spacerobotics/          # Space Robotics headers
│   │   ├── rover_motor.h       # Planetary rover control
│   │   ├── planetary_sensor_fusion.h # Multi-sensor fusion
│   │   └── fault_tolerant_comm.h # Deep space comms
│   ├── blockchain/             # Edge Blockchain headers
│   │   ├── blockchain_client.h # Lightweight blockchain
│   │   ├── secure_transaction.h # Transaction signing
│   │   └── consensus.h         # Consensus algorithms
│   ├── biomedical/             # Biomedical Wearables headers
│   │   ├── glucose_monitor.h   # Continuous glucose monitoring
│   │   ├── pulse_oximeter.h    # SpO2 sensor interface
│   │   └── medical_compliance.h # FDA/HIPAA compliance logging
│   ├── transportation/         # Smart Transportation headers
│   │   ├── ev_charging.h       # EV charging station control
│   │   ├── vehicle_to_grid.h   # V2G bidirectional power
│   │   └── traffic_telemetry.h # Traffic flow monitoring
│   ├── defense/                # Defense/High-Security headers
│   │   ├── secure_comms.h      # Encrypted radio communications
│   │   ├── tamper_response.h   # Tamper detection/zeroization
│   │   └── redundancy_checker.h # Mission-critical TMR
│   ├── environmental/          # Environmental Monitoring headers
│   │   ├── air_quality.h       # Air quality sensors (PM2.5, VOC)
│   │   ├── water_quality.h     # Water quality telemetry
│   │   └── climate_data.h      # Climate/weather monitoring
│   ├── medical_imaging/        # Medical imaging headers
│   │   ├── ultrasound_sensor.h # FDA/IEC 60601 ultrasound
│   │   ├── image_preprocessing.h # Medical image pipeline
│   │   └── regulatory_compliance.h # FDA 21 CFR Part 11
│   ├── infotainment/           # Automotive infotainment headers
│   │   ├── audio_codec.h       # I2S multi-channel audio
│   │   ├── navigation.h        # GPS navigation interface
│   │   └── touchscreen_hmi.h   # Multi-touch HMI driver
│   ├── manufacturing/          # Smart manufacturing headers
│   │   ├── robot_arm.h         # 6-axis robotic arm control
│   │   ├── conveyor.h          # Conveyor belt control
│   │   └── factory_telemetry.h # OEE monitoring
│   ├── edge_security/          # Edge security headers
│   │   ├── intrusion_detection.h # Anomaly-based IDS
│   │   ├── secure_enclave.h    # TrustZone/SGX abstraction
│   │   └── tamper_detection.h  # Physical tamper sensors
│   └── reliability/            # Watchdog, fault logging, recovery
├── src/                        # Implementation files
│   ├── core/                   # Core framework implementation
│   ├── automotive/             # CAN bus driver (SPOTLIGHT)
│   ├── iot/                    # MQTT, OTA updates
│   ├── consumer/               # USB, Display drivers
│   ├── medical/                # Biosensor, signal processing stubs
│   ├── security/               # Secure bootloader (SPOTLIGHT)
│   ├── power/                  # Power management (SPOTLIGHT)
│   ├── audio/                  # Audio subsystem stubs
│   ├── hmi/                    # HMI subsystem stubs
│   ├── safety/                 # Safety-critical stubs
│   ├── time/                   # Timekeeping stubs
│   ├── net/                    # Networking stack (SPOTLIGHT)
│   ├── aerospace/              # Avionics stubs
│   ├── industrial/             # Industrial IoT stubs
│   ├── ai/                     # Edge AI/ML (SPOTLIGHT)
│   ├── smarthome/              # Smart Home stack (SPOTLIGHT)
│   ├── energy/                 # Energy Harvesting (SPOTLIGHT)
│   ├── drone/                  # Drone Flight Control (SPOTLIGHT)
│   ├── vision/                 # Edge Vision Processing (SPOTLIGHT)
│   ├── marine/                 # Marine Sonar & Telemetry (SPOTLIGHT)
│   ├── renewable/              # Wind Turbine & Grid Sync (SPOTLIGHT)
│   ├── space_habitat/          # Space Habitat Life Support (SPOTLIGHT)
│   ├── energy_storage/         # Battery Management System (SPOTLIGHT)
│   ├── aviation/               # Avionics Flight Data Recorder (SPOTLIGHT)
│   ├── wearables/              # Wearables domain stubs
│   ├── industrial_safety/      # Industrial safety stubs
│   ├── space/                  # Space communications (SPOTLIGHT)
│   ├── microgrid/              # Microgrid Controller (SPOTLIGHT)
│   ├── firefighting/           # Firefighting Thermal Imaging (SPOTLIGHT)
│   ├── pipeline/               # Pipeline Leak Detection (SPOTLIGHT)
│   ├── hydrogen/               # Hydrogen Fuel Cell Control (SPOTLIGHT)
│   ├── quantum/                # Quantum hardware stubs
│   ├── agriculture/            # Smart agriculture stubs
│   ├── robotics/               # Advanced robotics stubs
│   ├── cloud/                  # Cloud integration stubs
│   ├── storage/                # Edge storage stubs
│   ├── reliability/            # Reliability infrastructure
│   └── demo/                   # Demo application
├── tests/                      # Test suite
│   ├── test_framework.h        # Lightweight test framework
│   ├── unit_tests.c            # Unit tests (25 tests)
│   ├── integration_tests.c     # Integration tests (15 tests)
│   ├── fault_injection_tests.c # Fault/reliability tests (19 tests)
│   ├── network_tests.c         # Networking tests (31 tests)
│   ├── ai_tests.c              # AI/ML tests (19 tests)
│   ├── smarthome_tests.c       # Smart Home tests (22 tests)
│   ├── energy_tests.c          # Energy harvesting tests (24 tests)
│   ├── drone_tests.c           # Drone flight control tests (33 tests)
│   ├── vision_tests.c          # Edge Vision processing tests (31 tests)
│   ├── marine_tests.c          # Marine sonar & telemetry tests (34 tests)
│   ├── renewable_tests.c       # Wind turbine & grid sync tests (35 tests)
│   ├── habitat_tests.c         # Space Habitat life support tests (112 tests)
│   ├── bms_tests.c             # Battery Management System tests (78 tests)
│   ├── fdr_tests.c             # Avionics Flight Data Recorder tests (65 tests)
│   ├── satellite_tests.c       # Satellite communication tests (56 tests)
│   └── microgrid_tests.c       # Microgrid Controller tests (38 tests)
│   └── firefighting_tests.c    # Firefighting Thermal Imaging tests (44 tests)
│   └── test_pipeline.c         # Pipeline Leak Detection tests (44 tests)
│   └── test_hydrogen.c         # Hydrogen Fuel Cell tests (39 tests)
├── docs/                       # Documentation
│   ├── overview.md             # This file
│   ├── module_map.md           # Module dependencies
│   ├── integration.md          # Integration guide
│   ├── networking.md           # Networking stack guide
│   ├── ai_inference.md         # Edge AI/ML guide
│   ├── energy_systems.md       # Energy harvesting guide
│   ├── vision_systems.md       # Edge Vision processing guide
│   ├── marine_systems.md       # Marine sonar & telemetry guide
│   ├── marine_systems.md       # Marine sonar & telemetry guide
│   ├── renewable_systems.md    # Wind turbine & grid sync guide
│   ├── space_habitat.md        # Space Habitat life support guide
│   ├── battery_systems.md      # Battery Management System guide
│   ├── aviation_systems.md     # Avionics Flight Data Recorder guide
│   ├── satellite_comms.md      # Satellite Communication guide
│   ├── microgrid_systems.md    # Microgrid Controller guide
│   ├── firefighting_systems.md # Firefighting Thermal Imaging guide
│   ├── pipeline_systems.md     # Pipeline Leak Detection guide
│   ├── hydrogen_systems.md     # Hydrogen Fuel Cell Control guide
│   ├── rocket_systems.md       # Rocket Engine Telemetry & Safety guide
│   ├── radiation_systems.md    # Radiation Shielding Control guide
│   ├── debris_systems.md       # Orbital Debris Tracking guide
│   ├── vessel_systems.md       # Autonomous Vessel Control guide
│   ├── lunar_mining.md         # Regolith Extraction & Telemetry guide
│   └── orbital_manufacturing.md # Zero-Gravity Fabrication Control guide
├── Makefile                    # GNU Make build
├── CMakeLists.txt              # CMake build
└── README.md                   # Project introduction
```

## Core Framework

### Scheduler (`core/scheduler.h`)

A lightweight cooperative scheduler providing:
- Priority-based task execution (IDLE → CRITICAL)
- Configurable task periods and deadlines
- Deadline monitoring with callbacks
- CPU load tracking

### Message Bus (`core/message_bus.h`)

Topic-based publish/subscribe system:
- Wildcard topic matching (`sensor/*`)
- QoS levels (fire-forget, at-least-once, exactly-once)
- Priority-based message queuing
- Typed payload support for type safety

### Error Handler (`core/error_handler.h`)

Centralized error management:
- Severity classification (DEBUG → FATAL)
- Recovery action recommendations
- Error history logging
- Watchdog integration

### Logging (`core/logging.h`)

Production logging infrastructure:
- Five severity levels
- Ring buffer storage
- Configurable output sinks
- Fault injection hooks for testing

## Spotlight Subsystems

### CAN Bus Driver (`automotive/can_bus.c`)

Production-grade CAN 2.0B implementation (~700 lines):

- **Bit Timing**: SAE sampling calculations
- **Error Handling**: Full ISO 11898 error state machine
  - Error Active → Passive → Bus-Off → Recovery
- **TX Queue**: Priority-sorted with retry logic
- **RX Filtering**: Hardware acceptance filter configuration
- **Statistics**: Error counters, bus load monitoring

### Secure Bootloader (`security/secure_boot.c`)

Complete secure boot chain (~850 lines):

- **Image Verification**: SHA-256 hash + ECDSA-P256 signatures
- **A/B Slot Management**: Dual partition with fallback
- **Anti-Rollback**: Security version enforcement
- **Boot Confirmation**: Watchdog-backed health check
- **Debug Control**: Production lockdown support

### Power Management (`power/power_management.c`)

Advanced power state control (~450 lines):

- **Sleep States**: Active, Idle, Standby, Deep Sleep
- **Wake Sources**: GPIO, RTC, USB, Network configurable
- **Voltage Scaling**: Dynamic frequency/voltage adjustment
- **Power Monitoring**: Current sensing, battery tracking

### Networking Stack (`net/`)

Lightweight network stack (~1500 lines total):

- **TCP/IP Stack** (`tcp_ip.c`): BSD socket API, TCP state machine
- **DNS Resolver** (`dns.c`): Recursive resolution with caching
- **HTTP Client** (`http_client.c`): HTTP/1.1 with retry and timeout

See [docs/networking.md](networking.md) for detailed documentation.

### Edge AI/ML Engine (`ai/`)

Lightweight neural network inference (~1000 lines total):

- **Inference Engine** (`inference.h`, `ai.c`): Arena-based runtime with layer execution
- **Model Loader** (`model_loader.h`): Secure loading with CRC32/SHA-256 verification
- **Feature Extraction** (`feature_extraction.h`): Preprocessing pipeline for sensors

Supported layer types: Dense, Conv2D, MaxPool, BatchNorm, Flatten, Activation
Supported activations: ReLU, Sigmoid, Tanh, Softmax, Leaky ReLU
Quantization support: FP32, INT16, INT8

See [docs/ai_inference.md](ai_inference.md) for detailed documentation.

### Smart Home Stack (`smarthome/`)

Production-grade Zigbee/Matter smart home implementation (~1800 lines total):

- **Zigbee 3.0 Protocol** (`zigbee.h`, `smarthome_spotlight.c`): Full ZCL stack with clustering
- **Matter Protocol** (`matter.h`): Thread/WiFi device commissioning
- **Device Discovery**: Mesh networking with permit join and commissioning state machine
- **Secure Communication**: AES-CCM encryption with network/link key management
- **ZCL Commands**: On/Off, Level Control, Color Control cluster support
- **Scene Management**: Device grouping and scene recall
- **Event System**: Callback-based device join/leave/report notifications

Protocol features:
- IEEE 802.15.4 frame handling
- Network address assignment and routing
- Install code commissioning with derived keys
- Frame counter for replay attack prevention

See [docs/smart_home.md](smart_home.md) for detailed documentation.

### Energy Harvesting & Smart Grid (`energy/`)

Production-grade energy harvesting system (~1600 lines):

- **MPPT Algorithms**: Perturb & Observe, Incremental Conductance, Constant Voltage modes
- **Battery Management**: CC/CV charging profiles, precharge mode, temperature protection
- **Power Budget**: Critical/Normal/Surplus state machine with power request/release API
- **Smart Grid Interface**: Bidirectional grid connection, export configuration, demand response
- **Statistics**: Comprehensive tracking for solar, battery, and grid metrics

Power budget features:
- Power source registration (solar, wind, grid, battery)
- Budget state callbacks for power management decisions
- Automatic power allocation with priority-based distribution
- Variable sunlight simulation support for testing

See [docs/energy_systems.md](energy_systems.md) for detailed documentation.

### Drone Flight Control (`drone/`)

Production-grade UAV flight control system (~1000 lines):

- **PID Control Loops**: Cascaded rate/attitude/velocity/position control
- **Sensor Fusion**: Complementary filter for GPS+IMU fusion
- **Motor Mixing**: Quadcopter X configuration with PWM output
- **Flight Modes**: Stabilize, Altitude Hold, Position Hold, RTL, Land
- **Safety Interlocks**: Geofencing, altitude limits, battery failsafe, RC loss

Safety features:
- Cylindrical geofence with home position center
- Maximum/minimum altitude enforcement
- Automatic RTL on RC signal loss
- Forced landing on critical battery
- Pre-flight checks before arming

See [docs/drone_systems.md](drone_systems.md) for detailed documentation.

### Edge Vision Processing (`vision/`)

Production-grade embedded computer vision system (~1400 lines):

- **Image Preprocessing**: Grayscale conversion, normalization, blur, histogram equalization
- **Edge Detection**: Sobel operators with gradient magnitude calculation
- **Feature Detection**: Harris corner detection with non-maximum suppression
- **Blob Detection**: Connected component labeling with bounding box extraction
- **Motion Detection**: Frame differencing with configurable sensitivity
- **Lighting Adaptation**: Automatic lighting condition detection (dark/dim/normal/bright)

Vision pipeline features:
- Ring buffer for multi-frame management
- BT.601 color-to-grayscale conversion
- Otsu's method for automatic thresholding
- ROI-based processing for efficiency
- Message bus integration for detection events

See [docs/vision_systems.md](vision_systems.md) for detailed documentation.

### Marine Sonar & Telemetry (`marine/`)

Production-grade underwater sonar and telemetry system (~1200 lines):

- **Sonar Processing**: Ping transmission, echo capture, peak detection
- **Sound Speed**: Chen-Millero equation for temperature/salinity/depth compensation
- **Depth Measurement**: Pressure-to-depth conversion with calibration
- **Multi-beam Scanning**: Simultaneous beam processing with swath coverage
- **Telemetry**: Packet creation, queuing, and transmission scheduling
- **Environment Modeling**: Thermocline, light zones, water density

Marine features:
- Single-beam, dual-beam, multi-beam, side-scan, forward-looking sonar types
- Seawater, freshwater, brackish water type support
- Automatic light zone classification (epipelagic to abyssal)
- Depth warning callbacks for safety

See [docs/marine_systems.md](marine_systems.md) for detailed documentation.

### Wind Turbine Control & Grid Sync (`renewable/`)

Production-grade wind turbine control and grid synchronization system (~1500 lines):

- **Turbine Control**: PID-based pitch adjustment for optimal power extraction
- **Yaw Control**: Wind direction tracking with motor actuation
- **Grid Synchronization**: Phase-locked loop for frequency/voltage matching
- **Anti-Islanding**: IEEE 1547 compliant grid disconnect protection
- **MPPT**: Power curve optimization for variable wind conditions
- **Safety Interlocks**: Overspeed protection, vibration monitoring, ice detection
- **Emergency Shutdown**: Multi-level shutdown with brake engagement
- **Telemetry**: SCADA-compatible power output and status reporting

Turbine features:
- Horizontal/vertical axis turbine support
- DFIG, PMSG, SCIG generator types
- Anemometer and wind vane integration
- Blade pitch control with feathering
- Nacelle temperature monitoring

Grid synchronization features:
- Phase-locked loop for grid frequency tracking
- Voltage regulation with reactive power control
- Frequency droop response for grid support
- Island detection with active/passive methods
- Soft-start ramp rate limiting

See [docs/renewable_systems.md](renewable_systems.md) for detailed documentation.

### Space Habitat Life Support (`space_habitat/`)

Production-grade Environmental Control and Life Support System (ECLSS) (~1400 lines):

- **Life Support Sensors**: O2, CO2, humidity, temperature, pressure, trace contaminants
- **Environmental Control**: PID-based closed-loop atmospheric regulation
- **ECLSS Actuators**: O2 generator, CO2 scrubber, humidifier, heater, cooler
- **Emergency Response**: Auto-triggered emergency mode for dangerous atmospheres
- **Telemetry**: CCSDS-compatible packet generation for ground communication
- **Blackout Handling**: Store-and-forward during communication gaps

ECLSS features:
- Quad-redundant sensor voting with fault isolation
- Sensor drift detection and compensation
- Multiple operating modes (Standby, Nominal, Economy, Sleep, EVA, Emergency)
- Crew count-based metabolic modeling
- Power consumption tracking for ECLSS subsystems
- Phase-locked loop grid synchronization patterns

See [docs/space_habitat.md](space_habitat.md) for detailed documentation.

### Battery Management System (`energy_storage/`)

Production-grade Battery Management System (~1700 lines):

- **Cell Monitoring**: Per-cell voltage tracking for 24-cell packs
- **Charge Control**: CC-CV charging with temperature derating
- **Thermal Management**: 8-zone temperature monitoring with thermal runaway detection
- **SOC/SOH Estimation**: Coulomb counting, OCV correlation, capacity fade tracking
- **Cell Balancing**: Passive balancing with configurable threshold
- **Fault Detection**: Overvoltage, undervoltage, overtemp, overcurrent, short circuit, isolation
- **State Machine**: Idle, Precharge, Charging, Discharging, Balancing, Fault, Shutdown
- **Telemetry**: Message bus integration for real-time monitoring

Supported cell chemistries:
- Li-ion, LiFePO4, LiPo, NiMH, Solid-state

Safety features:
- Contactor sequencing with precharge
- Emergency shutdown on thermal runaway
- Multi-fault handling with priority escalation
- Current limit management

See [docs/battery_systems.md](battery_systems.md) for detailed documentation.

### Avionics Flight Data Recorder (`aviation/`)

Production-grade Flight Data Recorder system (~1200 lines):

- **Parameter Recording**: 88+ mandatory parameters per TSO-C124b/ED-112A
- **Sensor Voting**: Mid-value select (MVS) for triple-redundant sensors
- **Frame Recording**: 8 Hz sampling with CRC-16-CCITT integrity
- **Event Triggering**: Automatic detection of exceedances and emergencies
- **Telemetry**: Real-time flight state and event broadcasting
- **Fault Detection**: Cross-channel disagreement monitoring

Recorded parameters:
- Time, altitude, airspeed (IAS, TAS, Mach, ground speed)
- Heading, attitude (pitch/roll/yaw), accelerations
- Engine (N1, N2, EGT, fuel flow), control surfaces
- Configuration (flaps, slats, gear), autopilot status
- Warnings (GPWS, TCAS, stall), navigation (lat/lon)

Safety features:
- Emergency event triggering (GPWS, TCAS RA, stall, impact)
- Overspeed, high-G, high bank angle detection
- 25-hour circular recording buffer (simulated)
- Crash-survivable memory interface (abstracted)

See [docs/aviation_systems.md](aviation_systems.md) for detailed documentation.

### Satellite Communications (`space/`)

Production-grade satellite uplink/downlink system (~1800 lines):

- **CCSDS Protocol**: Space Packet Protocol compatible framing
- **Forward Error Correction**: Reed-Solomon RS(255,223) with 16-symbol correction
- **Authenticated Encryption**: AES-128-GCM with anti-replay protection
- **Link Management**: Acquisition, sync, connected, store-forward modes
- **Doppler Tracking**: Real-time frequency compensation for LEO satellites
- **Retransmission**: Selective acknowledgment with exponential backoff

Communication features:
- Ground station secure handshake with key exchange
- Priority-based TX queue (64 packets)
- Configurable data rates (1200-9600 bps)
- Store-and-forward for intermittent coverage
- Link budget calculation (EIRP, G/T, C/N0)
- Packet loss tolerance up to 50%

Security features:
- Session key separation (TX/RX)
- Anti-replay sliding window (64 packets)
- Key age tracking for rotation
- Secure zeroization

See [docs/satellite_comms.md](satellite_comms.md) for detailed documentation.

### Microgrid Controller (`microgrid/`)

Production-grade microgrid control and demand response system (~1450 lines):

- **Grid Mode Management**: Grid-connected, islanded, transitioning, emergency states
- **Source Management**: Solar, wind, diesel, battery, utility grid dispatch
- **Load Priority**: Five-tier priority system (Critical, Essential, Normal, Deferrable, Curtailable)
- **Demand Response**: OpenADR 2.0 pattern event handling and load curtailment
- **Battery Storage**: SOC tracking with charge/discharge limits
- **Renewable Integration**: Variable solar/wind with forecasting
- **Frequency/Voltage Regulation**: Droop control with IEEE 1547 patterns
- **SCADA Telemetry**: Power flows, efficiency metrics, alarm generation

Grid modes:
- Off, Grid-Connected, Islanded, Island Transition, Reconnecting, Emergency Shutdown

Demand response types:
- Load shed, price response, capacity bid, spinning reserve, regulation

Safety features:
- Anti-islanding detection per IEEE 1547
- Load shedding by priority during capacity shortfall
- Emergency shutdown with critical-only mode
- Automatic reconnection sync checking

See [docs/microgrid_systems.md](microgrid_systems.md) for detailed documentation.

### Firefighting Thermal Imaging & Suppression (`firefighting/`)

Production-grade fire detection and suppression control system (~1300 lines):

- **Thermal Imaging**: 32×32 pixel thermal frame processing per sensor
- **Preprocessing**: Gaussian blur noise reduction, gradient edge detection
- **Hot Spot Detection**: Local maxima detection with rate-of-rise tracking
- **Zone Management**: Up to 16 suppression zones with multi-sensor assignment
- **System Types**: Wet pipe, dry pipe, preaction, deluge, gaseous systems
- **Agent Support**: Water, foam, CO2, FM-200, water mist
- **Suppression Control**: Manual trigger, abort capability, preaction delays
- **Incident Tracking**: Fire stage progression, affected zones, resolution

Temperature thresholds:
- Ambient (25°C), Warning (60°C), Alarm (85°C), Fire (150°C), Flashover (600°C)

Zone state machine:
- Idle, Monitoring, Warning, Alarm, Preaction, Discharging, Discharged

Standards compliance:
- NFPA 13 (Sprinkler Systems)
- NFPA 72 (Fire Alarm Code)
- NFPA 2001 (Clean Agent Systems)
- IEC 61508 (Functional Safety)

See [docs/firefighting_systems.md](firefighting_systems.md) for detailed documentation.

### Pipeline Leak Detection & Safety (`pipeline/`)

Production-grade oil/gas/liquid pipeline monitoring system (~1100 lines):

- **Flow Monitoring**: Ultrasonic, Coriolis, turbine, vortex flow sensors
- **Leak Detection Methods**:
  - Mass Balance: Compares upstream/downstream flow rates
  - Pressure Point Analysis (PPA): Gradient analysis between sensors
  - Negative Pressure Wave (NPW): Detects sudden drops (ruptures)
  - Statistical Pattern Recognition (SPR): Learning-based anomaly detection
  - Rate of Change (ROC): Monitors rapid parameter changes
- **Segment Management**: Up to 16 pipeline segments with individual monitoring
- **Valve Control**: Emergency Shutdown Valve (ESV) automation
- **Blockage Detection**: Flow ratio with pressure correlation
- **Telemetry**: SCADA-compatible packet generation

Leak severity classification:
- Minor (<1% flow), Moderate (1-5%), Major (5-20%), Rupture (>20%)

Segment state machine:
- Normal, Warning, Alarm, Shutdown, Maintenance, Isolated

Standards compliance:
- API 1130 (Computational Pipeline Monitoring)
- API 1160 (Managing System Integrity)
- PHMSA 49 CFR 192 (Natural Gas)
- PHMSA 49 CFR 195 (Hazardous Liquids)

See [docs/pipeline_systems.md](pipeline_systems.md) for detailed documentation.

### Hydrogen Fuel Cell Control & Telemetry (`hydrogen/`)

Production-grade hydrogen fuel cell and electrolyzer control system (~1200 lines):

- **Fuel Cell Control**: PEM stack voltage/current regulation, power setpoints
- **Electrolyzer Control**: PEM electrolyzer for green hydrogen production
- **Temperature Management**: Stack temperature monitoring and cooling control
- **Safety Systems**: H2 leak detection, over-temperature protection, E-stop
- **Efficiency Monitoring**: Real-time FC and EL efficiency calculation
- **Alarm Management**: Multi-level alarms with acknowledgment workflow
- **Telemetry**: SCADA-compatible binary packet generation

Fuel cell features:
- 100-cell PEM stack support (configurable)
- Power setpoint control with load following
- Cell voltage monitoring and spread calculation
- H2 consumption tracking

Electrolyzer features:
- Power mode and production mode operation
- 99.99% H2 purity (PEM electrolyzer)
- 10:1 turndown ratio for load following
- Water consumption tracking

Safety features:
- H2 leak detection: warning at 1000 ppm, alarm at 4000 ppm (1% LFL)
- Temperature protection: 80°C warning, 90°C emergency shutoff
- Emergency stop with immediate system shutdown
- Ventilation flow interlock

Standards compliance:
- IEC 62282-3 (Fuel cell technologies - Safety)
- SAE J2615 (PEM fuel cell systems testing)
- ISO 22734 (Hydrogen generators - Safety)
- NFPA 2 (Hydrogen technologies code)

See [docs/hydrogen_systems.md](hydrogen_systems.md) for detailed documentation.

### Rocket Engine Telemetry & Safety (`rocket/`)

Production-grade rocket engine monitoring and safety interlock system (~1200 lines):

- **Multi-Engine Array**: Up to 9 engines with individual state machines
- **TMR Sensor Voting**: Triple-modular redundancy with outlier rejection
- **Safety Interlocks**: 7 abort conditions with <10ms deterministic response
- **Telemetry Generation**: CCSDS-compatible frame formatting
- **Launch Readiness**: Comprehensive pre-launch verification

Safety features:
- Thrust bounds: 85%-105% nominal with inter-engine asymmetry detection
- Thermal protection: 3500°C warning, 3600°C abort threshold
- Vibration monitoring: 15g warning, 20g abort threshold
- Pressure protection: 3500 PSI warning, 4000 PSI abort
- Sensor health: Automatic abort on critical TMR failure

Launch phases:
- PRELAUNCH → TERMINAL_COUNT → IGNITION → LIFTOFF → MAX_Q → MECO → MISSION_COMPLETE

Standards compliance:
- NASA-STD-8719 (Software Safety)
- DO-178C (Software Considerations in Airborne Systems)
- MIL-STD-1553 / SpaceWire data bus compatibility
- CCSDS Blue Book (Telemetry Channel Coding)

See [docs/rocket_systems.md](rocket_systems.md) for detailed documentation.

### Radiation Shielding Control (`radiation/`)

Production-grade radiation monitoring and active shielding for crewed spacecraft (~1200 lines):

- **Multi-Sensor Array**: GCR, SPE, and neutron detection with TMR voting
- **Dose Tracking**: Real-time and cumulative dose calculation per NASA-STD-3001
- **Active Shielding**: Magnetic and water-wall shielding with power optimization
- **SPE Detection**: Solar particle event prediction with preemptive response
- **Crew Dosimetry**: Individual dose tracking with career limit monitoring

Safety features:
- Zone classification: NOMINAL, ELEVATED, HIGH, CRITICAL, EMERGENCY
- Storm shelter activation with <500ms response time
- 30-day rolling dose limits per ICRP 103 guidelines
- Automatic EVA suspension during elevated radiation

Shielding modes:
- PASSIVE → NOMINAL → ELEVATED → MAXIMUM → STORM_SHELTER

Standards compliance:
- NASA-STD-3001 (Space Flight Human-System Standard)
- ICRP 103 (Radiological Protection)
- ESA ECSS-E-HB-10-12A (Space Environment)
- CCSDS Blue Book (Telemetry Channel Coding)

See [docs/radiation_systems.md](radiation_systems.md) for detailed documentation.

### Autonomous Vessel Control (`vessel/`)

Production-grade Maritime Autonomous Surface Ship (MASS) control (~1560 lines):

- **Navigation Control**: Multi-mode autopilot with heading/speed PID control
- **Waypoint Management**: Route planning with up to 64 waypoints
- **ARPA Target Tracking**: Up to 128 tracked targets with CPA/TCPA calculation
- **Cargo Management**: Bay monitoring with weight limits and overload detection
- **Stability Monitoring**: TMR sensor voting for roll/pitch with GM calculation
- **Collision Avoidance**: Automatic mode transition for dangerous targets
- **Weather Response**: Wind speed and wave height limit enforcement
- **Emergency Stop**: Immediate engine shutdown with alarm propagation
- **NMEA Telemetry**: Standard marine telemetry generation
- **Alarm Management**: Up to 32 active alarms with acknowledgment

Test coverage: 54 tests, 110 assertions covering:
- Initialization and configuration
- Navigation state machine
- Heading and speed control
- Waypoint management
- ARPA target tracking
- Cargo bay management
- Stability monitoring
- Weather response
- Emergency stop
- Alarm management
- Telemetry generation
- Integration scenarios

Standards compliance:
- IMO SOLAS (Safety of Life at Sea)
- COLREGS (Collision Regulations)
- ISM Code (International Safety Management)
- MARPOL (Marine Pollution Prevention)
- IMO MSC.1/Circ.1604 (MASS Regulatory Scoping)
- ISO 19847 (Ship Data Management)

See [docs/vessel_systems.md](vessel_systems.md) for detailed documentation.

### Regolith Extraction & Yield Telemetry (`lunar/`)

Production-grade lunar mining control for In-Situ Resource Utilization (ISRU) (~1250 lines):

- **Drill Control Loop**: PID-based drill with torque and penetration control
- **Zone Management**: Up to 16 extraction zones with resource tracking
- **TMR Sensor Voting**: Triple Modular Redundancy for critical sensors
- **Thermal Management**: Drill head temperature monitoring with auto-pause
- **Resource Processing**: Yield calculation for water ice, oxygen, metals
- **Safety Interlocks**: Automatic fault response for overheating and wear
- **CCSDS Telemetry**: Space-grade telemetry frame generation
- **Fault Detection**: Mechanical wear, sensor degradation, thermal faults

Test coverage: 38 tests, 89 assertions covering:
- Initialization and configuration
- Drill operations and state machine
- Zone management
- TMR sensor voting
- Thermal monitoring
- Yield calculation
- Telemetry generation
- Safety interlocks
- Integration scenarios

Standards compliance:
- NASA-STD-3001 (Space Flight Human-System Standard)
- CCSDS 133.0-B-2 (Space Packet Protocol)
- ESA ECSS-E-HB-10-12A (Space Environment)
- ISO 11783 (Tractors and machinery for agriculture and forestry)

See [docs/lunar_mining.md](lunar_mining.md) for detailed documentation.

### Zero-Gravity Fabrication Control (`orbital/`)

Production-grade orbital manufacturing control for space-based fabrication (~1400 lines):

- **Multi-Chamber Control**: Support for 4 independent chambers
- **Process Types**: Crystal growth, vapor deposition, alloy melting, fiber drawing, 3D printing, containerless
- **TMR Sensor Network**: Temperature, pressure, and vibration with voting
- **PID Temperature Control**: Sub-degree precision with anti-windup
- **Safety Interlocks**: Thermal runaway, vacuum loss, vibration, attitude loss
- **CCSDS Telemetry**: Space Packet Protocol compatible frames
- **Yield Tracking**: Sample-level quality metrics and statistics

State machine progression:
```
IDLE → VACUUM_PUMP → HEATING → DEPOSITION/GROWTH → COOLING → QUALITY_CHECK → UNLOADING → IDLE
                                                                    ↓
                                                                  ABORT (on fault)
```

Standards compliance:
- ISS National Lab Protocol Compatibility
- NASA Marshall Space Center Guidelines
- ESA Columbus Laboratory Specifications
- CCSDS 133.0-B (Space Packet Protocol)

See [docs/orbital_manufacturing.md](orbital_manufacturing.md) for detailed documentation.

### Asteroid Drill Control & Telemetry (`asteroid/`)

Production-grade asteroid mining control for deep space resource extraction (~1200 lines):

- **Multi-Drill Support**: Up to 4 independent drill units
- **Material Classification**: Regolith, silicate, carbonaceous, metallic, ice detection
- **TMR Sensor Network**: Temperature, torque, vibration, depth with voting
- **Adaptive Drilling**: Automatic parameter adjustment based on material density
- **Safety Interlocks**: Overheat, overtorque, stall, bit break, anchor slip
- **CCSDS Telemetry**: Space Packet Protocol compatible frames
- **Yield Tracking**: Sample-level purity metrics and extraction statistics

State machine progression:
```
IDLE → PRE_CHECK → ANCHORING → SURFACE_SURVEY → DRILLING → SAMPLE_COLLECT → RETRACT → IDLE
                                                                       ↓
                                                                     FAULT (on error)
```

Standards compliance:
- NASA Near-Earth Object Survey Program
- ESA Asteroid Prospection Explorer Guidelines
- ISRU (In-Situ Resource Utilization) Standards
- CCSDS 133.0-B (Space Packet Protocol)

See [docs/asteroid_mining.md](asteroid_mining.md) for detailed documentation.

### Hyperloop Pod Control & Safety (`hyperloop/`)

Production-grade high-speed vacuum tube transit control (~1300 lines):

- **Multi-Pod Fleet**: Up to 4 independent pod units
- **Magnetic Levitation**: PID-controlled gap maintenance at 15mm nominal
- **Propulsion Control**: Linear motor acceleration with jerk limiting
- **Cabin Environment**: Pressure, temperature, O₂ management
- **TMR Sensors**: Gap, cabin pressure, tube pressure with voting
- **Safety Interlocks**: Levitation fault, pressure breach, thermal limits
- **Emergency Systems**: Maximum braking with levitation maintenance
- **Passenger Safety**: G-force limiting (≤0.5g normal, ≤1.5g emergency)
- **CCSDS Telemetry**: Real-time status and diagnostics

State machine progression:
```
IDLE → BOARDING → SEALED → LEVITATING → ACCELERATING → CRUISING → DECELERATING
                                                                        ↓
                                                        LANDING → DOCKING → IDLE
         ↓ (any fault)
    EMERGENCY → MAINTENANCE
```

Standards compliance:
- EN 50126 (Railway RAMS)
- IEC 62278 (Railway applications safety)
- ISO 26262 (adapted for transit)
- NFPA 130 (Fixed Guideway Transit Systems)

See [docs/hyperloop_systems.md](hyperloop_systems.md) for detailed documentation.

## Specialized Domain Stubs

### Medical Devices (`medical/`)

HIPAA/IEC 60601 focused medical device stubs:

- **Biosensor Driver**: ECG/PPG sensor acquisition with configurable channels
- **Signal Processing**: Bandpass/notch filtering, baseline correction
- **Compliance Logging**: Audit trail with PHI protection metadata

### Automotive Safety (`automotive/`)

ASIL-D and ISO 26262 compliant automotive stubs:

- **Airbag Sensor**: Multi-zone crash detection with saturation monitoring
- **ABS Control**: Wheel slip control loop with pressure modulation
- **Safety Diagnostics**: Self-test sequences with fault reporting

### Aerospace/Avionics (`aerospace/`)

DO-178C compliant avionics stubs:

- **Sensor Fusion**: IMU+GPS complementary filter with integrity monitoring
- **Redundancy Manager**: Triple Modular Redundancy (TMR) with voting
- **Fault-Tolerant FSM**: Watchdog-backed state machine with recovery

### Industrial IoT (`industrial/`)

IEC 61131 and industrial protocol stubs:

- **Modbus Driver**: RTU and TCP modes with function code support
- **PLC Interface**: Tag-based variable access and program execution
- **Telemetry Collector**: Multi-channel aggregation with compression

### Space Systems (`space/`)

Radiation-hardened space mission stubs:

- **Radiation Sensor**: TID/SEE/SEL detection with ASIL-D integrity
- **Orbital Telemetry**: CCSDS Space Packet Protocol housekeeping
- **Fault-Tolerant Comm**: Triple Modular Redundancy (TMR) voting

### Quantum Hardware (`quantum/`)

Next-generation computing interfaces:

- **Quantum RNG**: NIST SP 800-90B compliant random generation
- **Co-processor Interface**: NPU/TPU/FPGA/QC accelerator management
- **Entropy Pool**: Secure mixing with FIPS 140-2 compliance

### Smart Agriculture (`agriculture/`)

Precision farming and IoT stubs:

- **Soil Sensor**: Moisture, pH, EC, NPK, temperature monitoring
- **Irrigation Control**: Zone-based scheduling with water budgets
- **Crop Telemetry**: Yield prediction, alerts, prescription maps

### Advanced Robotics (`robotics/`)

PLe Category 4 robotic systems:

- **Motor Controller**: Multi-axis servo with cascaded PID control
- **Sensor Fusion**: Extended Kalman Filter for IMU+vision fusion
- **Safety Interlock**: E-stop, zone monitoring, safe torque off

### Medical Imaging (`medical_imaging/`)

FDA 21 CFR Part 11 and IEC 60601 compliant imaging:

- **Ultrasound Sensor**: Multi-mode acquisition (B-mode, M-mode, Doppler)
- **Image Preprocessing**: Spatial filtering, enhancement, artifact removal
- **Regulatory Compliance**: Audit logging, electronic signatures, PHI protection

### Automotive Infotainment (`infotainment/`)

ASIL-integrated automotive infotainment:

- **Audio Codec**: I2S multi-channel with DSP effects and spatial audio
- **Navigation**: GPS interface with turn-by-turn and ADAS integration
- **Touchscreen HMI**: Multi-touch gestures, haptic feedback, distraction compliance

### Smart Manufacturing (`manufacturing/`)

ISO 10218 compliant factory automation:

- **Robot Arm**: 6-axis control with trajectory planning and force sensing
- **Conveyor**: Belt control with object detection and accumulation zones
- **Factory Telemetry**: OEE monitoring, predictive maintenance, MTBF tracking

### Edge Security (`edge_security/`)

Defense-in-depth embedded security:

- **Intrusion Detection**: Anomaly-based IDS with packet inspection
- **Secure Enclave**: ARM TrustZone/Intel SGX abstraction for key storage
- **Tamper Detection**: Physical sensors with mesh detection and zeroization

### Biomedical Robotics (`biomedical_robotics/`)

FDA-compliant prosthetic and rehabilitation robotics:

- **Prosthetic Actuator**: Multi-axis motor control with torque limiting
- **Sensor Feedback**: Force/position feedback with sensor fusion
- **Biomedical Compliance**: FDA 21 CFR Part 11, IEC 62304, MDR audit logging

### Smart Retail (`smart_retail/`)

Retail IoT and inventory management systems:

- **RFID Scanner**: UHF/HF RFID with anti-collision and tag encryption
- **Inventory Telemetry**: Real-time stock tracking with threshold alerts
- **Point-of-Sale Interface**: Transaction processing with payment integration

### Disaster Response (`disaster_response/`)

Emergency and first-responder communication systems:

- **Emergency Beacon**: EPIRB/PLB protocols with GPS integration
- **Hazard Detection**: Multi-sensor fusion for threat classification
- **Resilient Communications**: Mesh networking with satellite fallback

### Edge Cryptography (`edge_crypto/`)

Post-quantum and lightweight cryptography:

- **Post-Quantum Crypto**: Kyber, Dilithium, NTRU lattice-based algorithms
- **Secure Handshake**: Lightweight TLS-like protocol for constrained devices
- **Key Rotation**: Key lifecycle management with secure erasure

### Biomedical Wearables (`biomedical/`)

FDA/HIPAA compliant medical wearables:

- **Glucose Monitor**: Continuous glucose monitoring with trend analysis
- **Pulse Oximeter**: SpO2 sensor with PPG waveform capture
- **Medical Compliance**: FDA 21 CFR Part 11 audit logging

### Smart Transportation (`transportation/`)

EV and connected vehicle systems:

- **EV Charging**: OCPP/SAE J1772 charging station control
- **Vehicle-to-Grid**: IEEE 2030.5 bidirectional power management
- **Traffic Telemetry**: V2X communication and traffic flow analytics

### Defense/High-Security (`defense/`)

Military-grade secure systems:

- **Secure Communications**: AES-256-GCM encrypted radio links
- **Tamper Response**: Intrusion detection with data zeroization
- **Redundancy Checker**: Byzantine fault-tolerant TMR systems

### Environmental Monitoring (`environmental/`)

Environmental sensor networks:

- **Air Quality**: PM2.5/PM10, VOC, CO2, EPA AQI calculation
- **Water Quality**: pH, turbidity, dissolved oxygen monitoring
- **Climate Data**: Temperature, humidity, pressure, radiation tracking

### Smart Space Launch Systems (`space_launch/`)

Commercial and government rocket launch systems:

- **Rocket Engine Sensor**: High-frequency thrust, temperature, vibration sensing
- **Launch Telemetry**: CCSDS-compatible real-time downlink with prioritization
- **Safety Interlock**: TMR voting with deterministic abort response (<10ms)

### Edge Aquaculture (`aquaculture/`)

Precision aquaculture and sustainable fish farming:

- **Water Quality Sensor**: DO, pH, ammonia, salinity, turbidity monitoring
- **Feeding Automation**: Demand-based feeding with FCR optimization
- **Aqua Compliance**: HACCP logging with farm-to-fork traceability

### Smart Disaster Shelters (`disaster_shelter/`)

Emergency preparedness and civil defense systems:

- **Environment Sensor**: Multi-gas detection, pressure differential, radiation
- **Power/Airflow Control**: NBC filtration, positive pressure, load shedding
- **Emergency Telemetry**: Multi-path communication with distress beacon

### Edge Legal Tech (`legal_tech/`)

Legal technology with compliance focus:

- **Evidence Logging**: Cryptographic chain of custody for digital evidence
- **Case Telemetry**: Deadline tracking, document access, billing capture
- **Legal Compliance**: GDPR/CCPA/HIPAA audit trails and retention policies

### Smart Sports Analytics (`sports_analytics/`)

Professional and collegiate athletic performance systems:

- **Athlete Sensor**: IMU motion tracking, HR/HRV, GPS positioning
- **Performance Telemetry**: Player load, intensity metrics, fatigue tracking
- **Sports AI Inference**: Activity classification, injury risk prediction

### Smart Space Habitats Advanced (`space_habitats_adv/`)

Advanced space habitat protection systems:

- **Radiation Sensor**: GCR/SPE detection with energy spectrum analysis
- **Shielding Control**: Active magnetic and water-wall shielding control
- **Astronaut Safety Telemetry**: Personal dosimetry and exposure tracking

### Edge Food Tech (`food_tech/`)

Smart commercial kitchen and food processing systems:

- **Smart Oven Driver**: Precision temperature control with multi-zone support
- **Nutritional Telemetry**: Macro/micro nutrient tracking and logging
- **Food Compliance Logging**: HACCP/FDA audit trails with temperature history

### Smart Policing Drones (`policing_drones/`)

Law enforcement aerial surveillance platforms:

- **Surveillance Sensor**: Multi-spectral imaging with privacy masking
- **Crowd Monitoring Telemetry**: Density estimation and flow analysis
- **Safety Compliance Module**: Use-of-force logging and audit trails

### Edge Climate Monitoring (`climate_monitoring/`)

Distributed environmental sensing networks:

- **Atmospheric Sensor**: CO2, methane, particulate, and ozone monitoring
- **Prediction Algorithm**: ML-based weather and air quality forecasting
- **Climate Telemetry**: Long-term trend logging and anomaly detection

### Smart Retail Experience (`retail_experience/`)

Next-generation retail technology systems:

- **AR Shopping Interface**: Product visualization and virtual try-on
- **Customer Telemetry**: Journey tracking and engagement analytics
- **Energy Optimization**: HVAC and lighting control based on occupancy

## Reliability Infrastructure

### Task Watchdog (`reliability/task_watchdog.c`)

Per-task timeout monitoring:

- Task registration with individual timeouts
- Periodic and deadline modes
- Automatic fault escalation on timeout

### Fault Logging (`reliability/fault_log.c`)

Persistent fault recording:

- Ring buffer with rollover protection
- Severity tracking and filtering
- Fault acknowledgment workflow

### Recovery Manager (`reliability/recovery_manager.c`)

Automated recovery strategies:

- Configurable policies per subsystem
- Recovery hooks for custom handling
- Graceful degradation support

## Building

### Using Make

```bash
make              # Build debug version
make release      # Build optimized release
make test         # Run all tests
make test-unit    # Run unit tests only
make test-integ   # Run integration tests only
make clean        # Clean build artifacts
```

### Using CMake

```bash
mkdir build && cd build
cmake ..
make
make test
```

### Cross-Compilation

```bash
make CROSS_COMPILE=arm-none-eabi-
```

## Testing

The project includes 859+ automated tests:

**Unit Tests (25)**:
- CAN bus: initialization, frame handling, error recovery
- Message bus: subscribe, publish, wildcards
- Scheduler: task creation, priority ordering
- Error handler: reporting, severity tracking

**Integration Tests (15)**:
- Sensor → CAN → MQTT data pipeline
- Secure boot → OTA update flow
- Error chain propagation
- Stress testing for memory stability

**Fault Injection Tests (19)**:
- Task watchdog: registration, kick, multi-task, deadline
- Fault logging: record, rollover, severity, acknowledgment
- Recovery: policies, hooks, safe mode, degradation
- Fault injection: enable/disable, triggers, subsystem targeting

**Network Tests (31)**:
- TCP/IP: socket creation, bind, listen, connect, options
- Network utilities: byte order, checksum, IP parsing
- DNS: resolution, caching, server configuration
- HTTP: URL parsing, encoding, retry delays, connection pool

**AI/ML Tests (19)**:
- Tensor: creation, shape, element access
- Layers: dense forward pass, activation functions
- Inference: classifier simulation, batch processing
- Features: mean, variance, RMS, normalization pipeline
- Quantization: INT8 conversion, error bounds
- Model: CRC32 checksum verification

**Smart Home Tests (22)**:
- Network formation: initialization, permit join, channel setup
- Device discovery: join, leave, multiple devices
- Commissioning: secure pairing, authentication state
- ZCL commands: on/off, level control, color control
- Scene management: create, recall, group association
- Group management: group creation, member assignment
- Security: network key generation, frame counter
- Integration: full device lifecycle, scene-with-devices pipeline

**Energy Tests (24)**:
- MPPT algorithms: P&O tracking, incremental conductance, mode switching
- Battery charging: CC/CV transitions, precharge, temperature protection
- Power budget: state machine, power requests, callback notifications
- Smart grid: connection, export configuration, demand response
- Variable sunlight: irradiance simulation, charge/discharge cycles
- Integration: solar-to-battery-to-grid pipeline

**Drone Tests (33)**:
- PID controller: initialization, proportional, integral, windup, clamping, reset
- Flight controller: init, arm/disarm, battery safety checks
- Motor mixing: hover, roll, pitch, yaw, PWM range validation
- Geofence: inside/outside radius, altitude limits, enable/disable
- Safety system: battery low/critical, RC loss, automatic RTL/land
- Flight modes: transitions, GPS/home requirements
- Sensor fusion: level/tilted attitude, GPS home position
- Control loops: stabilize mode, position hold
- Emergency: emergency stop, preflight checks
- Flight simulation: hover duration, sensor noise tolerance

**Vision Tests (31)**:
- Preprocessing: RGB-to-gray, normalization, 3x3/5x5 blur, histogram, thresholding
- Edge detection: Sobel vertical/horizontal, gradient magnitude
- Corner detection: Harris corners, uniform image rejection
- Blob detection: single/multiple blobs, size filtering
- Motion detection: static scene, moving objects
- Lighting adaptation: dark/normal/bright detection, histogram equalization
- Frame buffers: ring buffer init, read/write, overflow handling
- Integration: full pipeline, lighting enhancement, ROI, callbacks

**Marine Tests (34)**:
- Physics: speed of sound calculations, water density, pressure-depth conversion
- Sonar: ping/echo detection, multi-beam scanning, range validation
- Depth: raw/filtered readings, calibration (zero and scale)
- Environment: temperature, salinity, light zone classification
- Telemetry: packet creation, queue management, retrieval
- Callbacks: depth warning, echo notifications
- Statistics: sonar/depth tracking, min/max depth
- Water types: seawater, freshwater, brackish
- Integration: dive profile simulation, variable environment

**Renewable Tests (35)**:
- Turbine: initialization, configuration, status monitoring
- Pitch control: angle adjustment, wind response, feathering
- Yaw control: wind tracking, motor actuation, alignment
- Grid synchronization: phase lock, frequency matching, voltage regulation
- Power output: generation calculation, power curve optimization
- Safety: overspeed protection, vibration limits, emergency stop
- Telemetry: SCADA integration, power metrics, fault reporting
- Anti-islanding: grid disconnect detection, reconnection sequence
- Integration: full turbine lifecycle, variable wind conditions

**Habitat Tests (112)**:
- Sensor subsystem: initialization, configuration, reading, alarms, calibration, drift detection
- Environmental control: mode transitions, setpoints, actuator control, emergency handling
- Telemetry: packet generation, blackout handling, statistics
- Integration: full system simulation, crew effects, emergency recovery
- PID convergence: atmospheric regulation verification
- Redundancy: sensor voting logic, fault isolation

**BMS Tests (78)**:
- Pack initialization: configuration, cell count, chemistry selection
- Charge control: CC/CV transitions, temperature derating, precharge
- Discharge control: current limits, undervoltage protection
- Cell balancing: passive balancing, threshold detection
- SOC estimation: Coulomb counting, OCV correlation
- SOH tracking: cycle counting, capacity fade
- Fault detection: overvoltage, undervoltage, overtemp, overcurrent, short circuit
- Safety: contactor sequencing, emergency shutdown
- Telemetry: voltage, current, temperature, SOC publishing
- Integration: full charge cycle simulation, thermal runaway, multi-fault

**FDR Tests (65)**:
- Initialization: default/custom config, start/stop recording
- Sensor voting: single, dual, triple redundancy, failed channels
- Frame recording: single/multiple frames, status reporting
- Event triggering: manual, takeoff, landing, multiple events
- Exceedance detection: overspeed, high-G, bank angle, stall, GPWS, TCAS
- Telemetry: generation, content validation, queue management
- Fault handling: sensor, memory, timing faults, recovery
- Status reporting: null checks, memory usage
- Emergency scenarios: impact, combined emergencies, TCAS RA, approach stall
- Boundary conditions: max/min values, negative attitudes
- Timing: process loop validation

**Satellite Tests (56)**:
- Initialization: init, RS codec setup, double-init, shutdown
- Link state: state transitions, store-forward mode
- Packet queue: single, multiple, priorities, overflow
- Security: key loading, encryption status
- Doppler: positive, negative, zero frequency shifts
- Processing: basic, connected, store-forward modes
- Statistics: initial values, after TX, null pointer handling
- Timeouts: link timeout, stats increment
- Error injection: bit errors, CRC verification
- Boundary: zero delta, large delta, rapid changes
- Packet loss: multi-packet recovery simulation
- Latency: high-latency link tolerance
- Handshake: security setup validation

**Microgrid Tests (38)**:
- Initialization: default config, custom config, double-init, shutdown
- Source registration: solar, diesel, multiple sources
- Load registration: critical, curtailable, multiple loads
- Storage: registration, online state
- Grid balancing: balance calculation, source dispatch, load curtailment
- Mode transitions: grid-connected, island, reconnect, request
- Demand response: event receipt, activation, price response
- Statistics: initial state, accumulation, peak tracking
- Alarms: generation, acknowledgment, retrieval
- Telemetry: points, mode
- Emergency: emergency shutdown
- Variable renewable: solar variability, ramp limits
- Integration: 24-hour simulation, island with DR
- Boundary conditions: zero generation, storage depletion, null pointers

Run tests with:
```bash
make test          # Run all tests
make test-unit     # Unit tests only
make test-integ    # Integration tests only
make test-fault    # Fault injection tests only
make test-net      # Network tests only
make test-ai       # AI/ML tests only
make test-smarthome # Smart Home tests only
make test-energy   # Energy tests only
make test-drone    # Drone tests only
make test-vision   # Vision tests only
make test-marine   # Marine tests only
make test-renewable # Renewable tests only
make test-habitat  # Habitat tests only
make test-bms      # BMS tests only
make test-fdr      # FDR tests only
make test-satellite # Satellite tests only
make test-microgrid # Microgrid tests only
make test-firefighting # Firefighting tests only
make test-pipeline # Pipeline tests only
make test-hydrogen  # Hydrogen tests only
make test-rocket    # Rocket tests only
make test-radiation # Radiation tests only
make test-debris    # Debris tests only
make test-vessel    # Vessel tests only
make test-lunar     # Lunar mining tests only
make test-orbital   # Orbital manufacturing tests only
make test-asteroid  # Asteroid drill tests only
make test-hyperloop # Hyperloop pod control tests only
```

## Industry Applications

This architecture demonstrates skills applicable to:

| Module | Industry | Applications |
|--------|----------|--------------|
| CAN Bus | Automotive | ECU development, vehicle networks |
| Airbag/ABS | Automotive | ASIL-D safety systems |
| Secure Boot | Security | IoT devices, payment terminals |
| MQTT | IoT | Connected sensors, cloud telemetry |
| Networking | IoT/Industrial | Connected devices, gateways |
| Smart Home | Consumer/IoT | Zigbee/Matter devices, home automation |
| Energy Harvesting | Green Energy | Solar systems, smart grid, microgrids |
| Drone Flight Control | Aerospace/Consumer | UAVs, delivery drones, eVTOL |
| Edge Vision | Industrial/Automotive | Quality inspection, ADAS, surveillance |
| Marine Systems | Marine/Offshore | AUVs, ROVs, sonar systems, oceanography |
| Smart Cities | Urban/Government | Traffic control, environmental sensing |
| Space Robotics | Aerospace/Defense | Planetary rovers, deep space missions |
| Edge Blockchain | IoT/Enterprise | Device authentication, supply chain |
| Medical Imaging | Medical | Ultrasound devices, diagnostic imaging |
| Biomedical Wearables | Medical | CGM, pulse oximetry, FDA compliance |
| Smart Transportation | Automotive/Energy | EV charging, V2G, traffic systems |
| Defense Systems | Defense | Secure comms, tamper detection, TMR |
| Environmental | Environmental | Air/water quality, climate monitoring |
| Renewable Infrastructure | Energy | Wind turbines, grid sync, SCADA |
| Battery Management | Energy/EV | BMS, EVs, grid storage, UPS |
| Flight Data Recorder | Aviation | FDRs, FOQA, black boxes, UAV telemetry |
| Satellite Communication | Aerospace | LEO terminals, remote IoT, deep space |\n| Microgrid Controller | Energy | Campus grids, DERs, demand response |
| Space Habitat Life Support | Aerospace | ECLSS, space stations, submarines |
| Smart Healthcare | Medical | Patient monitoring, nurse call, compliance |
| Advanced Logistics | Industrial | Warehouse robots, package tracking, routing |
| Renewable Water | Environmental | Desalination, water recycling, quality |
| Edge Quantum Security | Security | QKD, post-quantum crypto, tamper detection |
| Biomedical Robotics | Medical | Prosthetics, rehabilitation, FDA compliance |
| Smart Retail | Retail | RFID, inventory, point-of-sale |
| Disaster Response | Emergency | Beacons, hazard detection, resilient comms |
| Edge Cryptography | Security | Post-quantum crypto, key management |
| Infotainment | Automotive | Head units, navigation, audio |
| Smart Manufacturing | Industrial | Factory automation, robotics, MES |
| Edge Security | Security | IDS, secure enclaves, tamper detection |
| Wearables | Consumer | Fitness trackers, smartwatches |
| Cloud Integration | IoT/Enterprise | REST/GraphQL APIs, telemetry |
| Edge Storage | IoT | Offline-first apps, data caching |
| Power Mgmt | Wearables/IoT | Battery-powered devices |
| Audio | Consumer | Media players, voice assistants |
| HMI | Consumer | Displays, touch interfaces |
| Safety | Medical/Industrial | Redundancy, fail-safe systems |
| Industrial Safety | Industrial | Gas detection, emergency shutdown |
| Biosensor | Medical | Wearables, patient monitoring |
| Sensor Fusion | Aerospace | Flight control, navigation |
| Space Systems | Aerospace | Satellites, space missions |
| Quantum HW | Computing | Quantum computing, secure RNG |
| Smart Agriculture | Agriculture | Precision farming, irrigation |
| Advanced Robotics | Industrial | Manufacturing, automation |
| Modbus/PLC | Industrial | Factory automation, SCADA |
| Edge AI | All | Predictive maintenance, anomaly detection |
| Timekeeping | All | RTC, NTP synchronization |
| Scheduler | All | RTOS alternatives, low-power devices |
| Orbital Manufacturing | Aerospace | Space station manufacturing, crystal growth |
| Asteroid Mining | Aerospace | Deep space mining, ISRU, resource extraction |

## Design Philosophy

1. **Modularity**: Each subsystem can be used independently
2. **Testability**: Interfaces designed for unit testing
3. **Portability**: HAL abstraction for hardware independence
4. **Safety**: Error handling at every layer
5. **Reliability**: Watchdogs, fault logging, recovery strategies
6. **Documentation**: Self-documenting code with clear comments

## Next Steps

For recruiters evaluating this codebase:

1. **CAN Driver**: See [can_bus.c](../src/automotive/can_bus.c) for bit timing and error handling
2. **Secure Boot**: See [secure_boot.c](../src/security/secure_boot.c) for cryptographic verification
3. **Networking**: See [docs/networking.md](networking.md) for TCP/IP, DNS, and HTTP
4. **Edge AI**: See [docs/ai_inference.md](ai_inference.md) for neural network inference
5. **Energy Systems**: See [docs/energy_systems.md](energy_systems.md) for MPPT and smart grid
6. **Drone Control**: See [docs/drone_systems.md](drone_systems.md) for PID flight control
7. **Edge Vision**: See [docs/vision_systems.md](vision_systems.md) for image processing and detection
8. **Marine Systems**: See [docs/marine_systems.md](marine_systems.md) for sonar and underwater telemetry
9. **Renewable Systems**: See [docs/renewable_systems.md](renewable_systems.md) for wind turbine and grid sync
10. **Space Habitat**: See [docs/space_habitat.md](space_habitat.md) for ECLSS life support
11. **Battery Systems**: See [docs/battery_systems.md](battery_systems.md) for BMS and energy storage
12. **Aviation Systems**: See [docs/aviation_systems.md](aviation_systems.md) for FDR and avionics
13. **Satellite Comms**: See [docs/satellite_comms.md](satellite_comms.md) for uplink/downlink
14. **Reliability**: See [fault_injection_tests.c](../tests/fault_injection_tests.c) for resilience testing
15. **Tests**: Run `make test` to see 689+ passing tests across all domains

---

*Grey Firmware - Demonstrating embedded systems expertise across automotive, medical, aerospace, industrial, AI, energy, space, quantum, agriculture, robotics, drone, vision, marine, smart cities, space robotics, blockchain, security, renewable infrastructure, biomedical robotics, smart retail, disaster response, edge cryptography, space habitats, smart healthcare, advanced logistics, renewable water systems, edge quantum security, battery management, neurotech interfaces, smart rail systems, disaster recovery networks, edge robotics swarms, flight data recording, precision medicine, smart mining, edge financial systems, smart education, satellite communications, smart construction, smart ocean monitoring, orbital manufacturing, and asteroid mining domains*
