// Package main is the entry point for the Grey Distributed daemon.
package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"
)

// Build information (set via ldflags)
var (
	Version   = "dev"
	Commit    = "unknown"
	BuildTime = "unknown"
)

func main() {
	// Parse flags
	configPath := flag.String("config", "", "Path to configuration file")
	showVersion := flag.Bool("version", false, "Show version and exit")
	flag.Parse()

	// Show version if requested
	if *showVersion {
		fmt.Printf("Grey Distributed %s\n", Version)
		fmt.Printf("  Commit:     %s\n", Commit)
		fmt.Printf("  Build Time: %s\n", BuildTime)
		os.Exit(0)
	}

	// Load configuration
	if *configPath == "" {
		// Check environment variable
		*configPath = os.Getenv("GREY_CONFIG")
	}

	if *configPath == "" {
		log.Fatal("Configuration file required. Use --config or GREY_CONFIG")
	}

	log.Printf("Grey Distributed %s starting...", Version)
	log.Printf("Configuration: %s", *configPath)

	// Setup context with cancellation
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Handle shutdown signals
	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		sig := <-sigCh
		log.Printf("Received signal: %v", sig)
		cancel()
	}()

	// TODO: Initialize and start Grey node
	// This is a placeholder - the actual implementation would:
	// 1. Parse configuration file
	// 2. Initialize consensus module
	// 3. Initialize storage
	// 4. Initialize scheduler
	// 5. Start HTTP/gRPC servers
	// 6. Join cluster

	log.Println("Grey Distributed daemon started")
	log.Println("Press Ctrl+C to stop")

	// Wait for shutdown
	<-ctx.Done()

	log.Println("Shutting down...")
	log.Println("Grey Distributed stopped")
}
