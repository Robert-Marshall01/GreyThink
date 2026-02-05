// =============================================================================
// greyctl — Grey Distributed CLI Tool
// =============================================================================
//
// Command-line interface for interacting with Grey clusters.
//
// Features:
// - Task submission and status monitoring
// - Cluster health and node management
// - Replay and debugging capabilities
// - Operator commands (quarantine, scaling)
//
// Authentication:
// - GREY_API_KEY environment variable
// - GREY_API_ENDPOINT for cluster URL
// - ~/.grey/config.yaml for persistent config
//
// Installation:
//   go install github.com/grey-io/grey/cmd/greyctl@latest
//
// =============================================================================

package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"gopkg.in/yaml.v3"
)

// =============================================================================
// Constants & Config
// =============================================================================

const (
	DefaultEndpoint = "http://localhost:8080"
	DefaultTimeout  = 30 * time.Second
	ConfigFileName  = "config.yaml"
)

// Config holds greyctl configuration
type Config struct {
	Endpoint  string            `yaml:"endpoint" json:"endpoint"`
	APIKey    string            `yaml:"api_key" json:"api_key"`
	TenantID  string            `yaml:"tenant_id" json:"tenant_id"`
	Output    string            `yaml:"output" json:"output"` // json, yaml, table
	Timeout   time.Duration     `yaml:"timeout" json:"timeout"`
	Labels    map[string]string `yaml:"labels" json:"labels"`
}

var (
	cfgFile string
	config  Config
	verbose bool
)

// =============================================================================
// Main Entry Point
// =============================================================================

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}

// =============================================================================
// Root Command
// =============================================================================

var rootCmd = &cobra.Command{
	Use:   "greyctl",
	Short: "CLI for Grey Distributed task execution platform",
	Long: `greyctl is a command-line tool for interacting with Grey Distributed clusters.

It provides commands for:
  - Submitting and managing tasks
  - Monitoring cluster health
  - Debugging and replay operations
  - Operator functions (quarantine, scaling)

Configuration:
  Set GREY_API_KEY and GREY_API_ENDPOINT environment variables,
  or use 'greyctl config' to set up persistent configuration.

Examples:
  # Submit a simple task
  greyctl task submit --type stateless --payload '{"func": "process"}'

  # Check cluster health
  greyctl cluster health

  # Watch task status
  greyctl task status --watch abc123

  # Replay a failed task
  greyctl replay abc123 --at "2024-01-15T10:30:00Z"`,
	PersistentPreRun: func(cmd *cobra.Command, args []string) {
		initConfig()
	},
}

// =============================================================================
// Task Commands
// =============================================================================

var taskCmd = &cobra.Command{
	Use:   "task",
	Short: "Task submission and management",
	Long:  "Commands for submitting, monitoring, and managing tasks.",
}

var taskSubmitCmd = &cobra.Command{
	Use:   "submit",
	Short: "Submit a new task",
	Long: `Submit a task for execution in the Grey cluster.

The task payload can be provided via:
  - --payload flag (inline JSON)
  - --file flag (JSON/YAML file path)
  - stdin (piped JSON)

Examples:
  # Submit stateless task
  greyctl task submit --type stateless --payload '{"input": [1,2,3]}'

  # Submit from file with priority
  greyctl task submit --file task.yaml --priority high

  # Submit batch from directory
  greyctl task submit --batch ./tasks/

  # Submit with TEE requirement
  greyctl task submit --type stateless --payload '{}' --tee`,
	RunE: runTaskSubmit,
}

var taskStatusCmd = &cobra.Command{
	Use:   "status [task-id]",
	Short: "Get task status",
	Long: `Get the current status of one or more tasks.

Examples:
  # Get single task status
  greyctl task status abc123

  # Watch task until completion
  greyctl task status abc123 --watch

  # Get multiple task statuses
  greyctl task status abc123 def456 ghi789`,
	Args: cobra.MinimumNArgs(1),
	RunE: runTaskStatus,
}

var taskListCmd = &cobra.Command{
	Use:   "list",
	Short: "List tasks",
	Long: `List tasks with optional filtering.

Examples:
  # List all tasks
  greyctl task list

  # Filter by status
  greyctl task list --status running,queued

  # Filter by labels
  greyctl task list --label env=production --label team=data

  # Show only recent tasks
  greyctl task list --since 1h`,
	RunE: runTaskList,
}

var taskCancelCmd = &cobra.Command{
	Use:   "cancel [task-id]",
	Short: "Cancel a task",
	Long: `Cancel a pending or running task.

Examples:
  # Cancel single task
  greyctl task cancel abc123

  # Cancel with reason
  greyctl task cancel abc123 --reason "No longer needed"

  # Cancel multiple tasks
  greyctl task cancel abc123 def456`,
	Args: cobra.MinimumNArgs(1),
	RunE: runTaskCancel,
}

var taskProofCmd = &cobra.Command{
	Use:   "proof [task-id]",
	Short: "Get task proof artifact",
	Long: `Retrieve the TEE proof artifact for a completed task.

Only available for tasks executed in TEE enclaves.
The proof contains attestation data and execution trace.

Examples:
  # Get proof as JSON
  greyctl task proof abc123

  # Save proof to file
  greyctl task proof abc123 --output proof.bin

  # Verify proof
  greyctl task proof abc123 --verify`,
	Args: cobra.ExactArgs(1),
	RunE: runTaskProof,
}

// =============================================================================
// Cluster Commands
// =============================================================================

var clusterCmd = &cobra.Command{
	Use:   "cluster",
	Short: "Cluster health and management",
	Long:  "Commands for monitoring cluster health and managing nodes.",
}

var clusterHealthCmd = &cobra.Command{
	Use:   "health",
	Short: "Get cluster health",
	Long: `Display cluster health status including:
  - Node counts and status
  - Resource utilization
  - Queue depth and throughput
  - Consensus state

Examples:
  # Basic health check
  greyctl cluster health

  # Watch cluster health
  greyctl cluster health --watch --interval 5s

  # JSON output for scripts
  greyctl cluster health --output json`,
	RunE: runClusterHealth,
}

var clusterNodesCmd = &cobra.Command{
	Use:   "nodes",
	Short: "List cluster nodes",
	Long: `List all nodes in the cluster with detailed information.

Examples:
  # List all nodes
  greyctl cluster nodes

  # Filter by status
  greyctl cluster nodes --status active

  # Show only workers
  greyctl cluster nodes --role worker`,
	RunE: runClusterNodes,
}

var clusterUsageCmd = &cobra.Command{
	Use:   "usage",
	Short: "Get tenant resource usage",
	Long: `Display resource usage and quota status for current tenant.

Examples:
  greyctl cluster usage`,
	RunE: runClusterUsage,
}

// =============================================================================
// Replay Command
// =============================================================================

var replayCmd = &cobra.Command{
	Use:   "replay [task-id]",
	Short: "Replay a task",
	Long: `Replay a previously executed task with optional modifications.

Useful for:
  - Debugging failed tasks
  - Testing with modified payloads
  - Point-in-time state reconstruction

Examples:
  # Replay task as-is
  greyctl replay abc123

  # Replay with modified payload
  greyctl replay abc123 --payload-override '{"debug": true}'

  # Replay at specific point in time
  greyctl replay abc123 --at "2024-01-15T10:30:00Z"

  # Replay and watch execution
  greyctl replay abc123 --watch`,
	Args: cobra.ExactArgs(1),
	RunE: runReplay,
}

// =============================================================================
// Quarantine Command
// =============================================================================

var quarantineCmd = &cobra.Command{
	Use:   "quarantine",
	Short: "Quarantine node management",
	Long:  "Commands for quarantining and unquarantining nodes.",
}

var quarantineNodeCmd = &cobra.Command{
	Use:   "node [node-id]",
	Short: "Quarantine a node",
	Long: `Quarantine a node to prevent task scheduling.

Quarantined nodes:
  - Stop receiving new tasks
  - Drain existing tasks
  - Are excluded from consensus
  - Remain available for debugging

Examples:
  # Quarantine node
  greyctl quarantine node grey-worker-7 --reason "High error rate"

  # Quarantine with immediate effect (kill running tasks)
  greyctl quarantine node grey-worker-7 --immediate`,
	Args: cobra.ExactArgs(1),
	RunE: runQuarantineNode,
}

var quarantineListCmd = &cobra.Command{
	Use:   "list",
	Short: "List quarantined nodes",
	RunE:  runQuarantineList,
}

var quarantineLiftCmd = &cobra.Command{
	Use:   "lift [node-id]",
	Short: "Lift quarantine from a node",
	Long: `Remove quarantine from a node, returning it to active service.

Examples:
  greyctl quarantine lift grey-worker-7`,
	Args: cobra.ExactArgs(1),
	RunE: runQuarantineLift,
}

// =============================================================================
// Attestation Command
// =============================================================================

var attestCmd = &cobra.Command{
	Use:   "attest",
	Short: "TEE attestation operations",
	Long:  "Commands for verifying cluster attestation.",
}

var attestVerifyCmd = &cobra.Command{
	Use:   "verify [node-id]",
	Short: "Verify node attestation",
	Long: `Verify TEE attestation for a specific node.

Returns platform-specific measurements (SGX mrenclave, etc.)
and verification status.

Examples:
  # Verify single node
  greyctl attest verify grey-worker-7

  # Verify with expected measurements
  greyctl attest verify grey-worker-7 --mrenclave abc123...

  # Verify all nodes
  greyctl attest verify --all`,
	RunE: runAttestVerify,
}

var attestRequestCmd = &cobra.Command{
	Use:   "request",
	Short: "Request fresh attestation",
	Long: `Request fresh TEE attestation from the cluster.

Examples:
  # Request from any node
  greyctl attest request

  # Request from specific node
  greyctl attest request --node grey-worker-7`,
	RunE: runAttestRequest,
}

// =============================================================================
// Config Command
// =============================================================================

var configCmd = &cobra.Command{
	Use:   "config",
	Short: "Manage greyctl configuration",
	Long:  "Commands for viewing and modifying greyctl configuration.",
}

var configViewCmd = &cobra.Command{
	Use:   "view",
	Short: "View current configuration",
	RunE:  runConfigView,
}

var configSetCmd = &cobra.Command{
	Use:   "set [key] [value]",
	Short: "Set configuration value",
	Long: `Set a configuration value.

Available keys:
  - endpoint: Grey API endpoint URL
  - api_key: API authentication key
  - tenant_id: Default tenant ID
  - output: Default output format (json, yaml, table)
  - timeout: Request timeout

Examples:
  greyctl config set endpoint https://api.grey.io
  greyctl config set output json`,
	Args: cobra.ExactArgs(2),
	RunE: runConfigSet,
}

var configInitCmd = &cobra.Command{
	Use:   "init",
	Short: "Initialize configuration file",
	Long: `Create a new configuration file interactively.

Examples:
  greyctl config init`,
	RunE: runConfigInit,
}

// =============================================================================
// Version Command
// =============================================================================

var versionCmd = &cobra.Command{
	Use:   "version",
	Short: "Print version information",
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Printf("greyctl version %s\n", "1.0.0")
		fmt.Printf("  Build: %s\n", "dev")
		fmt.Printf("  Go: %s\n", "1.21")
	},
}

// =============================================================================
// Command Registration
// =============================================================================

func init() {
	// Global flags
	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.grey/config.yaml)")
	rootCmd.PersistentFlags().BoolVarP(&verbose, "verbose", "v", false, "verbose output")
	rootCmd.PersistentFlags().StringP("endpoint", "e", "", "Grey API endpoint")
	rootCmd.PersistentFlags().StringP("api-key", "k", "", "API key")
	rootCmd.PersistentFlags().StringP("output", "o", "table", "Output format (table, json, yaml)")
	rootCmd.PersistentFlags().Duration("timeout", DefaultTimeout, "Request timeout")

	// Bind to viper
	viper.BindPFlag("endpoint", rootCmd.PersistentFlags().Lookup("endpoint"))
	viper.BindPFlag("api_key", rootCmd.PersistentFlags().Lookup("api-key"))
	viper.BindPFlag("output", rootCmd.PersistentFlags().Lookup("output"))
	viper.BindPFlag("timeout", rootCmd.PersistentFlags().Lookup("timeout"))

	// Task command flags
	taskSubmitCmd.Flags().String("type", "stateless", "Task type (stateless, stateful, pipeline, batch)")
	taskSubmitCmd.Flags().String("payload", "", "Task payload (JSON)")
	taskSubmitCmd.Flags().StringP("file", "f", "", "Task definition file")
	taskSubmitCmd.Flags().String("batch", "", "Directory containing batch task files")
	taskSubmitCmd.Flags().String("priority", "normal", "Priority (low, normal, high, critical)")
	taskSubmitCmd.Flags().Duration("task-timeout", 5*time.Minute, "Task execution timeout")
	taskSubmitCmd.Flags().StringSlice("label", nil, "Labels (key=value format)")
	taskSubmitCmd.Flags().Bool("tee", false, "Require TEE execution")
	taskSubmitCmd.Flags().String("idempotency-key", "", "Idempotency key for deduplication")
	taskSubmitCmd.Flags().Bool("wait", false, "Wait for task completion")

	taskStatusCmd.Flags().BoolP("watch", "w", false, "Watch for status changes")
	taskStatusCmd.Flags().Duration("interval", 2*time.Second, "Polling interval for --watch")

	taskListCmd.Flags().StringSlice("status", nil, "Filter by status")
	taskListCmd.Flags().StringSlice("label", nil, "Filter by labels")
	taskListCmd.Flags().Duration("since", 0, "Show tasks created within this duration")
	taskListCmd.Flags().Int("limit", 100, "Maximum number of results")

	taskCancelCmd.Flags().String("reason", "", "Cancellation reason")

	taskProofCmd.Flags().String("output", "", "Save proof to file")
	taskProofCmd.Flags().Bool("verify", false, "Verify proof locally")

	// Cluster command flags
	clusterHealthCmd.Flags().BoolP("watch", "w", false, "Watch health status")
	clusterHealthCmd.Flags().Duration("interval", 5*time.Second, "Polling interval for --watch")

	clusterNodesCmd.Flags().String("status", "", "Filter by status")
	clusterNodesCmd.Flags().String("role", "", "Filter by role")

	// Replay command flags
	replayCmd.Flags().String("payload-override", "", "Override original payload (JSON)")
	replayCmd.Flags().String("at", "", "Point-in-time for state reconstruction (RFC3339)")
	replayCmd.Flags().BoolP("watch", "w", false, "Watch replay execution")

	// Quarantine command flags
	quarantineNodeCmd.Flags().String("reason", "", "Quarantine reason")
	quarantineNodeCmd.Flags().Bool("immediate", false, "Kill running tasks immediately")

	// Attest command flags
	attestVerifyCmd.Flags().Bool("all", false, "Verify all nodes")
	attestVerifyCmd.Flags().String("mrenclave", "", "Expected mrenclave value")
	attestVerifyCmd.Flags().String("mrsigner", "", "Expected mrsigner value")

	attestRequestCmd.Flags().String("node", "", "Specific node to request from")

	// Build command tree
	taskCmd.AddCommand(taskSubmitCmd, taskStatusCmd, taskListCmd, taskCancelCmd, taskProofCmd)
	clusterCmd.AddCommand(clusterHealthCmd, clusterNodesCmd, clusterUsageCmd)
	quarantineCmd.AddCommand(quarantineNodeCmd, quarantineListCmd, quarantineLiftCmd)
	attestCmd.AddCommand(attestVerifyCmd, attestRequestCmd)
	configCmd.AddCommand(configViewCmd, configSetCmd, configInitCmd)

	rootCmd.AddCommand(taskCmd, clusterCmd, replayCmd, quarantineCmd, attestCmd, configCmd, versionCmd)
}

// =============================================================================
// Configuration
// =============================================================================

func initConfig() {
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
	} else {
		home, err := os.UserHomeDir()
		if err == nil {
			configDir := filepath.Join(home, ".grey")
			viper.AddConfigPath(configDir)
			viper.SetConfigName("config")
			viper.SetConfigType("yaml")
		}
	}

	// Environment variable overrides
	viper.SetEnvPrefix("GREY")
	viper.AutomaticEnv()
	viper.SetEnvKeyReplacer(strings.NewReplacer("-", "_"))

	// Read config file (ignore if not found)
	viper.ReadInConfig()

	// Populate config struct
	config.Endpoint = viper.GetString("endpoint")
	if config.Endpoint == "" {
		config.Endpoint = DefaultEndpoint
	}
	config.APIKey = viper.GetString("api_key")
	config.TenantID = viper.GetString("tenant_id")
	config.Output = viper.GetString("output")
	if config.Output == "" {
		config.Output = "table"
	}
	config.Timeout = viper.GetDuration("timeout")
	if config.Timeout == 0 {
		config.Timeout = DefaultTimeout
	}
}

func getConfigPath() string {
	if cfgFile != "" {
		return cfgFile
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".grey", ConfigFileName)
}

// =============================================================================
// Command Implementations
// =============================================================================

// runTaskSubmit submits a new task
func runTaskSubmit(cmd *cobra.Command, args []string) error {
	taskType, _ := cmd.Flags().GetString("type")
	payload, _ := cmd.Flags().GetString("payload")
	file, _ := cmd.Flags().GetString("file")
	batch, _ := cmd.Flags().GetString("batch")
	priority, _ := cmd.Flags().GetString("priority")
	labels, _ := cmd.Flags().GetStringSlice("label")
	teeRequired, _ := cmd.Flags().GetBool("tee")
	idempotencyKey, _ := cmd.Flags().GetString("idempotency-key")
	wait, _ := cmd.Flags().GetBool("wait")
	taskTimeout, _ := cmd.Flags().GetDuration("task-timeout")

	// Validate input source
	if payload == "" && file == "" && batch == "" {
		return fmt.Errorf("must provide --payload, --file, or --batch")
	}

	// Build task definition
	task := map[string]interface{}{
		"type":        taskType,
		"priority":    priorityValue(priority),
		"tee_required": teeRequired,
	}

	if taskTimeout > 0 {
		task["timeout_seconds"] = int(taskTimeout.Seconds())
	}
	if idempotencyKey != "" {
		task["idempotency_key"] = idempotencyKey
	}

	// Parse labels
	if len(labels) > 0 {
		labelMap := make(map[string]string)
		for _, l := range labels {
			parts := strings.SplitN(l, "=", 2)
			if len(parts) == 2 {
				labelMap[parts[0]] = parts[1]
			}
		}
		task["labels"] = labelMap
	}

	// Parse payload
	if payload != "" {
		var p map[string]interface{}
		if err := json.Unmarshal([]byte(payload), &p); err != nil {
			return fmt.Errorf("invalid payload JSON: %w", err)
		}
		task["payload"] = p
	} else if file != "" {
		p, err := loadPayloadFile(file)
		if err != nil {
			return err
		}
		task["payload"] = p
	}

	// TODO: Implement actual API call
	// client := newAPIClient()
	// result, err := client.SubmitTask(context.Background(), task)

	// Mock response for now
	result := map[string]interface{}{
		"task_id":    "550e8400-e29b-41d4-a716-446655440000",
		"status":     "pending",
		"created_at": time.Now().Format(time.RFC3339),
	}

	if wait {
		fmt.Println("Task submitted, waiting for completion...")
		// TODO: Poll for completion
	}

	return outputResult(result)
}

// runTaskStatus gets task status
func runTaskStatus(cmd *cobra.Command, args []string) error {
	watch, _ := cmd.Flags().GetBool("watch")
	interval, _ := cmd.Flags().GetDuration("interval")

	if watch {
		return watchTaskStatus(args, interval)
	}

	// TODO: Implement actual API call
	results := make([]map[string]interface{}, len(args))
	for i, taskID := range args {
		results[i] = map[string]interface{}{
			"task_id":    taskID,
			"status":     "running",
			"created_at": time.Now().Add(-5 * time.Minute).Format(time.RFC3339),
			"updated_at": time.Now().Format(time.RFC3339),
		}
	}

	if len(results) == 1 {
		return outputResult(results[0])
	}
	return outputResult(results)
}

func watchTaskStatus(taskIDs []string, interval time.Duration) error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Handle graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigChan
		cancel()
	}()

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return nil
		case <-ticker.C:
			// TODO: Fetch and display status
			fmt.Printf("\r[%s] Checking status...", time.Now().Format("15:04:05"))
		}
	}
}

// runTaskList lists tasks
func runTaskList(cmd *cobra.Command, args []string) error {
	statusFilter, _ := cmd.Flags().GetStringSlice("status")
	labelFilter, _ := cmd.Flags().GetStringSlice("label")
	since, _ := cmd.Flags().GetDuration("since")
	limit, _ := cmd.Flags().GetInt("limit")

	// Build query params
	params := map[string]interface{}{
		"limit": limit,
	}
	if len(statusFilter) > 0 {
		params["status"] = statusFilter
	}
	if len(labelFilter) > 0 {
		params["labels"] = labelFilter
	}
	if since > 0 {
		params["since"] = time.Now().Add(-since).Format(time.RFC3339)
	}

	// TODO: Implement actual API call
	result := map[string]interface{}{
		"tasks": []map[string]interface{}{
			{"task_id": "abc123", "status": "completed", "type": "stateless"},
			{"task_id": "def456", "status": "running", "type": "stateful"},
		},
		"total": 2,
	}

	return outputResult(result)
}

// runTaskCancel cancels tasks
func runTaskCancel(cmd *cobra.Command, args []string) error {
	reason, _ := cmd.Flags().GetString("reason")

	for _, taskID := range args {
		// TODO: Implement actual API call
		fmt.Printf("Cancelled task %s", taskID)
		if reason != "" {
			fmt.Printf(" (reason: %s)", reason)
		}
		fmt.Println()
	}

	return nil
}

// runTaskProof retrieves task proof
func runTaskProof(cmd *cobra.Command, args []string) error {
	taskID := args[0]
	outputFile, _ := cmd.Flags().GetString("output")
	verify, _ := cmd.Flags().GetBool("verify")

	// TODO: Implement actual API call
	result := map[string]interface{}{
		"task_id":   taskID,
		"proof":     "0x...",
		"algorithm": "SGX",
		"verified":  true,
	}

	if outputFile != "" {
		// Save to file
		proof := []byte(result["proof"].(string))
		if err := os.WriteFile(outputFile, proof, 0600); err != nil {
			return fmt.Errorf("failed to write proof: %w", err)
		}
		fmt.Printf("Proof saved to %s\n", outputFile)
		return nil
	}

	if verify {
		// Local verification
		fmt.Println("Verifying proof locally...")
		// TODO: Implement local verification
	}

	return outputResult(result)
}

// runClusterHealth gets cluster health
func runClusterHealth(cmd *cobra.Command, args []string) error {
	watch, _ := cmd.Flags().GetBool("watch")
	interval, _ := cmd.Flags().GetDuration("interval")

	if watch {
		return watchClusterHealth(interval)
	}

	// TODO: Implement actual API call
	result := map[string]interface{}{
		"healthy": true,
		"nodes": map[string]interface{}{
			"total":       15,
			"active":      14,
			"draining":    1,
			"quarantined": 0,
		},
		"utilization": map[string]interface{}{
			"cpu":    0.72,
			"memory": 0.65,
		},
		"queue": map[string]interface{}{
			"depth":                1234,
			"oldest_task_age_secs": 2.5,
		},
		"throughput": map[string]interface{}{
			"tasks_per_second": 5000,
		},
	}

	return outputResult(result)
}

func watchClusterHealth(interval time.Duration) error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)
	go func() {
		<-sigChan
		cancel()
	}()

	ticker := time.NewTicker(interval)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			return nil
		case <-ticker.C:
			// Clear screen and show updated health
			fmt.Print("\033[H\033[2J")
			fmt.Printf("Grey Cluster Health [%s]\n", time.Now().Format("15:04:05"))
			fmt.Println("─────────────────────────────────────")
			fmt.Println("Nodes: 14/15 active")
			fmt.Println("CPU: 72%, Memory: 65%")
			fmt.Println("Queue: 1234 tasks")
			fmt.Println("Throughput: 5000 tasks/sec")
		}
	}
}

// runClusterNodes lists cluster nodes
func runClusterNodes(cmd *cobra.Command, args []string) error {
	statusFilter, _ := cmd.Flags().GetString("status")
	roleFilter, _ := cmd.Flags().GetString("role")

	_ = statusFilter
	_ = roleFilter

	// TODO: Implement actual API call
	result := map[string]interface{}{
		"nodes": []map[string]interface{}{
			{"node_id": "grey-coordinator-0", "role": "coordinator", "status": "active"},
			{"node_id": "grey-worker-1", "role": "worker", "status": "active"},
			{"node_id": "grey-worker-2", "role": "worker", "status": "draining"},
		},
	}

	return outputResult(result)
}

// runClusterUsage gets tenant usage
func runClusterUsage(cmd *cobra.Command, args []string) error {
	// TODO: Implement actual API call
	result := map[string]interface{}{
		"tenant_id": "tenant-abc",
		"quotas": map[string]interface{}{
			"cpu_cores":    map[string]int{"limit": 100, "used": 72},
			"memory_gb":    map[string]int{"limit": 256, "used": 168},
			"max_tasks":    map[string]int{"limit": 10000, "used": 3402},
		},
		"throttled": false,
	}

	return outputResult(result)
}

// runReplay replays a task
func runReplay(cmd *cobra.Command, args []string) error {
	taskID := args[0]
	payloadOverride, _ := cmd.Flags().GetString("payload-override")
	atTime, _ := cmd.Flags().GetString("at")
	watch, _ := cmd.Flags().GetBool("watch")

	request := map[string]interface{}{
		"original_task_id": taskID,
	}
	if payloadOverride != "" {
		var p map[string]interface{}
		if err := json.Unmarshal([]byte(payloadOverride), &p); err != nil {
			return fmt.Errorf("invalid payload override: %w", err)
		}
		request["payload_override"] = p
	}
	if atTime != "" {
		request["at_timestamp"] = atTime
	}

	// TODO: Implement actual API call
	result := map[string]interface{}{
		"task_id": "new-task-id",
		"status":  "pending",
		"replay_of": taskID,
	}

	if watch {
		fmt.Println("Replay submitted, watching execution...")
		// TODO: Watch for completion
	}

	return outputResult(result)
}

// runQuarantineNode quarantines a node
func runQuarantineNode(cmd *cobra.Command, args []string) error {
	nodeID := args[0]
	reason, _ := cmd.Flags().GetString("reason")
	immediate, _ := cmd.Flags().GetBool("immediate")

	_ = immediate

	// TODO: Implement actual API call
	fmt.Printf("Quarantined node %s", nodeID)
	if reason != "" {
		fmt.Printf(" (reason: %s)", reason)
	}
	fmt.Println()

	return nil
}

// runQuarantineList lists quarantined nodes
func runQuarantineList(cmd *cobra.Command, args []string) error {
	// TODO: Implement actual API call
	result := map[string]interface{}{
		"quarantined_nodes": []map[string]interface{}{
			{"node_id": "grey-worker-5", "reason": "High error rate", "since": "2024-01-15T10:30:00Z"},
		},
	}

	return outputResult(result)
}

// runQuarantineLift lifts quarantine
func runQuarantineLift(cmd *cobra.Command, args []string) error {
	nodeID := args[0]

	// TODO: Implement actual API call
	fmt.Printf("Lifted quarantine from node %s\n", nodeID)

	return nil
}

// runAttestVerify verifies attestation
func runAttestVerify(cmd *cobra.Command, args []string) error {
	all, _ := cmd.Flags().GetBool("all")
	mrenclave, _ := cmd.Flags().GetString("mrenclave")
	_ = mrenclave

	if all {
		// TODO: Verify all nodes
		fmt.Println("Verifying all nodes...")
	}

	nodeID := ""
	if len(args) > 0 {
		nodeID = args[0]
	}

	// TODO: Implement actual API call
	result := map[string]interface{}{
		"valid":       true,
		"node_id":     nodeID,
		"platform":    "sgx",
		"mrenclave":   "a1b2c3...",
		"verified_at": time.Now().Format(time.RFC3339),
	}

	return outputResult(result)
}

// runAttestRequest requests fresh attestation
func runAttestRequest(cmd *cobra.Command, args []string) error {
	node, _ := cmd.Flags().GetString("node")
	_ = node

	// TODO: Implement actual API call
	result := map[string]interface{}{
		"valid":       true,
		"platform":    "sgx",
		"verified_at": time.Now().Format(time.RFC3339),
	}

	return outputResult(result)
}

// runConfigView shows current config
func runConfigView(cmd *cobra.Command, args []string) error {
	cfg := map[string]interface{}{
		"endpoint":  config.Endpoint,
		"tenant_id": config.TenantID,
		"output":    config.Output,
		"timeout":   config.Timeout.String(),
		// Don't show API key for security
	}

	if config.APIKey != "" {
		cfg["api_key"] = "***" + config.APIKey[len(config.APIKey)-4:]
	}

	return outputResult(cfg)
}

// runConfigSet sets a config value
func runConfigSet(cmd *cobra.Command, args []string) error {
	key := args[0]
	value := args[1]

	configPath := getConfigPath()
	if configPath == "" {
		return fmt.Errorf("could not determine config path")
	}

	// Ensure directory exists
	dir := filepath.Dir(configPath)
	if err := os.MkdirAll(dir, 0700); err != nil {
		return fmt.Errorf("failed to create config directory: %w", err)
	}

	// Read existing config
	var cfg map[string]interface{}
	data, err := os.ReadFile(configPath)
	if err == nil {
		yaml.Unmarshal(data, &cfg)
	}
	if cfg == nil {
		cfg = make(map[string]interface{})
	}

	// Set value
	cfg[key] = value

	// Write back
	data, err = yaml.Marshal(cfg)
	if err != nil {
		return fmt.Errorf("failed to marshal config: %w", err)
	}

	if err := os.WriteFile(configPath, data, 0600); err != nil {
		return fmt.Errorf("failed to write config: %w", err)
	}

	fmt.Printf("Set %s = %s\n", key, value)
	return nil
}

// runConfigInit initializes config interactively
func runConfigInit(cmd *cobra.Command, args []string) error {
	configPath := getConfigPath()
	if configPath == "" {
		return fmt.Errorf("could not determine config path")
	}

	// Ensure directory exists
	dir := filepath.Dir(configPath)
	if err := os.MkdirAll(dir, 0700); err != nil {
		return fmt.Errorf("failed to create config directory: %w", err)
	}

	cfg := map[string]interface{}{
		"endpoint": DefaultEndpoint,
		"output":   "table",
		"timeout":  DefaultTimeout.String(),
	}

	data, err := yaml.Marshal(cfg)
	if err != nil {
		return fmt.Errorf("failed to marshal config: %w", err)
	}

	if err := os.WriteFile(configPath, data, 0600); err != nil {
		return fmt.Errorf("failed to write config: %w", err)
	}

	fmt.Printf("Created config file: %s\n", configPath)
	fmt.Println("\nSet your API key with:")
	fmt.Println("  greyctl config set api_key YOUR_API_KEY")

	return nil
}

// =============================================================================
// Helpers
// =============================================================================

func priorityValue(s string) int {
	switch strings.ToLower(s) {
	case "low":
		return 1
	case "normal":
		return 5
	case "high":
		return 8
	case "critical":
		return 10
	default:
		return 5
	}
}

func loadPayloadFile(path string) (map[string]interface{}, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read file: %w", err)
	}

	var payload map[string]interface{}

	if strings.HasSuffix(path, ".yaml") || strings.HasSuffix(path, ".yml") {
		if err := yaml.Unmarshal(data, &payload); err != nil {
			return nil, fmt.Errorf("failed to parse YAML: %w", err)
		}
	} else {
		if err := json.Unmarshal(data, &payload); err != nil {
			return nil, fmt.Errorf("failed to parse JSON: %w", err)
		}
	}

	return payload, nil
}

func outputResult(result interface{}) error {
	switch config.Output {
	case "json":
		data, err := json.MarshalIndent(result, "", "  ")
		if err != nil {
			return err
		}
		fmt.Println(string(data))

	case "yaml":
		data, err := yaml.Marshal(result)
		if err != nil {
			return err
		}
		fmt.Print(string(data))

	default: // table
		// For table output, use a simple key-value format
		// In production, use tablewriter or similar
		data, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(data))
	}

	return nil
}
