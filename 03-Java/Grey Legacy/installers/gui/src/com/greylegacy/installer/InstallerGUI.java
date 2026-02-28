package com.greylegacy.installer;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Cross-platform GUI installer for the Grey Legacy Claims System.
 * Provides a wizard-style interface that delegates to the platform-specific
 * install scripts (install.sh / install.cmd).
 */
public class InstallerGUI extends JFrame {

    private static final String APP_NAME = "Grey Legacy Claims System";
    private static final String APP_VERSION = "1.0.0";

    // Wizard panels
    private CardLayout cardLayout;
    private JPanel cardPanel;
    private int currentStep = 0;
    private static final String[] STEPS = {"welcome", "config", "progress", "complete"};

    // Navigation buttons
    private JButton btnBack;
    private JButton btnNext;
    private JButton btnCancel;

    // Config fields
    private JTextField txtInstallDir;
    private JTextField txtPort;
    private JTextField txtServiceUser;
    private JCheckBox chkStartService;
    private JCheckBox chkFirewall;

    // Progress
    private JProgressBar progressBar;
    private JTextArea txtLog;
    private boolean installComplete = false;

    // Platform detection
    private final String os;

    public InstallerGUI() {
        os = detectOS();
        initUI();
    }

    private String detectOS() {
        String osName = System.getProperty("os.name", "").toLowerCase();
        if (osName.contains("win")) return "windows";
        if (osName.contains("mac") || osName.contains("darwin")) return "macos";
        return "linux";
    }

    private void initUI() {
        setTitle(APP_NAME + " - Installer");
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowClosing(java.awt.event.WindowEvent e) {
                confirmExit();
            }
        });
        setSize(700, 520);
        setMinimumSize(new Dimension(600, 450));
        setLocationRelativeTo(null);
        setResizable(true);

        // Try to set system look and feel
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            SwingUtilities.updateComponentTreeUI(this);
        } catch (Exception ignored) {}

        JPanel mainPanel = new JPanel(new BorderLayout());

        // Header
        JPanel header = createHeader();
        mainPanel.add(header, BorderLayout.NORTH);

        // Card panel for wizard steps
        cardLayout = new CardLayout();
        cardPanel = new JPanel(cardLayout);
        cardPanel.add(createWelcomePanel(), "welcome");
        cardPanel.add(createConfigPanel(), "config");
        cardPanel.add(createProgressPanel(), "progress");
        cardPanel.add(createCompletePanel(), "complete");
        mainPanel.add(cardPanel, BorderLayout.CENTER);

        // Navigation buttons
        JPanel navPanel = createNavigationPanel();
        mainPanel.add(navPanel, BorderLayout.SOUTH);

        setContentPane(mainPanel);
        updateButtons();
    }

    private JPanel createHeader() {
        JPanel header = new JPanel(new BorderLayout());
        header.setBackground(new Color(44, 62, 80));
        header.setBorder(new EmptyBorder(15, 20, 15, 20));

        JLabel titleLabel = new JLabel(APP_NAME);
        titleLabel.setFont(new Font("SansSerif", Font.BOLD, 20));
        titleLabel.setForeground(Color.WHITE);

        JLabel versionLabel = new JLabel("Version " + APP_VERSION + " — Installer");
        versionLabel.setFont(new Font("SansSerif", Font.PLAIN, 12));
        versionLabel.setForeground(new Color(189, 195, 199));

        header.add(titleLabel, BorderLayout.CENTER);
        header.add(versionLabel, BorderLayout.EAST);
        return header;
    }

    // -------------------------------------------------------------------------
    //  Step 1: Welcome Panel
    // -------------------------------------------------------------------------
    private JPanel createWelcomePanel() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(new EmptyBorder(30, 40, 30, 40));

        JTextArea welcomeText = new JTextArea();
        welcomeText.setEditable(false);
        welcomeText.setOpaque(false);
        welcomeText.setFont(new Font("SansSerif", Font.PLAIN, 14));
        welcomeText.setLineWrap(true);
        welcomeText.setWrapStyleWord(true);
        welcomeText.setText(
            "Welcome to the " + APP_NAME + " Installer.\n\n" +
            "This wizard will guide you through installing the Grey Legacy " +
            "Enterprise Insurance Claims Processing System on your computer.\n\n" +
            "The installer will:\n" +
            "  \u2022  Verify Java is installed\n" +
            "  \u2022  Download and install Apache Tomcat 8.5\n" +
            "  \u2022  Deploy the Grey Legacy application\n" +
            "  \u2022  Configure the application as a system service\n" +
            "  \u2022  Set up firewall rules\n\n" +
            "Detected operating system: " + capitalize(os) + "\n\n" +
            "Click \"Next\" to continue."
        );

        panel.add(welcomeText, BorderLayout.CENTER);

        // Step indicator
        JPanel stepPanel = createStepIndicator(0);
        panel.add(stepPanel, BorderLayout.SOUTH);

        return panel;
    }

    // -------------------------------------------------------------------------
    //  Step 2: Configuration Panel
    // -------------------------------------------------------------------------
    private JPanel createConfigPanel() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(new EmptyBorder(20, 40, 20, 40));

        JPanel formPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(8, 5, 8, 5);

        // Install directory
        gbc.gridx = 0; gbc.gridy = 0; gbc.weightx = 0;
        formPanel.add(new JLabel("Install Directory:"), gbc);

        String defaultDir;
        switch (os) {
            case "windows": defaultDir = "C:\\GreyLegacy"; break;
            case "macos":   defaultDir = "/usr/local/greylegacy"; break;
            default:        defaultDir = "/opt/greylegacy"; break;
        }
        txtInstallDir = new JTextField(defaultDir, 30);
        gbc.gridx = 1; gbc.weightx = 1;
        formPanel.add(txtInstallDir, gbc);

        JButton btnBrowse = new JButton("Browse...");
        btnBrowse.addActionListener(e -> browseDirectory());
        gbc.gridx = 2; gbc.weightx = 0;
        formPanel.add(btnBrowse, gbc);

        // Port
        gbc.gridx = 0; gbc.gridy = 1; gbc.weightx = 0;
        formPanel.add(new JLabel("HTTP Port:"), gbc);
        txtPort = new JTextField("8080", 8);
        gbc.gridx = 1; gbc.gridwidth = 2; gbc.weightx = 1;
        formPanel.add(txtPort, gbc);
        gbc.gridwidth = 1;

        // Service user (Linux/macOS only)
        if (!os.equals("windows")) {
            gbc.gridx = 0; gbc.gridy = 2; gbc.weightx = 0;
            formPanel.add(new JLabel("Service User:"), gbc);
            txtServiceUser = new JTextField("greylegacy", 20);
            gbc.gridx = 1; gbc.gridwidth = 2; gbc.weightx = 1;
            formPanel.add(txtServiceUser, gbc);
            gbc.gridwidth = 1;
        }

        // Checkboxes
        JPanel checkPanel = new JPanel();
        checkPanel.setLayout(new BoxLayout(checkPanel, BoxLayout.Y_AXIS));
        checkPanel.setBorder(new TitledBorder("Options"));

        chkStartService = new JCheckBox("Start service after installation", true);
        chkFirewall = new JCheckBox("Configure firewall rules", true);
        checkPanel.add(chkStartService);
        checkPanel.add(Box.createVerticalStrut(5));
        checkPanel.add(chkFirewall);

        gbc.gridx = 0; gbc.gridy = 3; gbc.gridwidth = 3;
        gbc.insets = new Insets(15, 5, 8, 5);
        formPanel.add(checkPanel, gbc);

        // System info
        JPanel infoPanel = new JPanel(new GridLayout(0, 1, 0, 3));
        infoPanel.setBorder(new TitledBorder("System Information"));
        infoPanel.add(new JLabel("  OS: " + System.getProperty("os.name") + " " + System.getProperty("os.arch")));
        infoPanel.add(new JLabel("  Java: " + System.getProperty("java.version") + " (" + System.getProperty("java.vendor") + ")"));
        infoPanel.add(new JLabel("  JAVA_HOME: " + System.getProperty("java.home")));

        gbc.gridy = 4;
        gbc.insets = new Insets(10, 5, 8, 5);
        formPanel.add(infoPanel, gbc);

        panel.add(formPanel, BorderLayout.CENTER);

        JPanel stepPanel = createStepIndicator(1);
        panel.add(stepPanel, BorderLayout.SOUTH);

        return panel;
    }

    // -------------------------------------------------------------------------
    //  Step 3: Progress Panel
    // -------------------------------------------------------------------------
    private JPanel createProgressPanel() {
        JPanel panel = new JPanel(new BorderLayout(0, 10));
        panel.setBorder(new EmptyBorder(20, 40, 20, 40));

        JLabel lblStatus = new JLabel("Installing...");
        lblStatus.setFont(new Font("SansSerif", Font.BOLD, 14));
        panel.add(lblStatus, BorderLayout.NORTH);

        txtLog = new JTextArea();
        txtLog.setEditable(false);
        txtLog.setFont(new Font("Monospaced", Font.PLAIN, 12));
        txtLog.setBackground(new Color(30, 30, 30));
        txtLog.setForeground(new Color(200, 200, 200));
        txtLog.setCaretColor(Color.WHITE);
        JScrollPane scrollPane = new JScrollPane(txtLog);
        scrollPane.setPreferredSize(new Dimension(600, 280));
        panel.add(scrollPane, BorderLayout.CENTER);

        progressBar = new JProgressBar();
        progressBar.setIndeterminate(true);
        progressBar.setStringPainted(true);
        progressBar.setString("Installing...");
        panel.add(progressBar, BorderLayout.SOUTH);

        return panel;
    }

    // -------------------------------------------------------------------------
    //  Step 4: Complete Panel
    // -------------------------------------------------------------------------
    private JPanel createCompletePanel() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(new EmptyBorder(30, 40, 30, 40));
        // Content is set dynamically after installation
        return panel;
    }

    private void updateCompletePanel(boolean success) {
        JPanel panel = (JPanel) cardPanel.getComponent(3);
        panel.removeAll();

        JTextArea text = new JTextArea();
        text.setEditable(false);
        text.setOpaque(false);
        text.setFont(new Font("SansSerif", Font.PLAIN, 14));
        text.setLineWrap(true);
        text.setWrapStyleWord(true);

        if (success) {
            String installDir = txtInstallDir.getText().trim();
            String port = txtPort.getText().trim();
            text.setText(
                "\u2705  Installation Completed Successfully!\n\n" +
                "The " + APP_NAME + " has been installed.\n\n" +
                "Installation Details:\n" +
                "  Install Directory:  " + installDir + "\n" +
                "  Application URL:    http://localhost:" + port + "/greylegacy/\n\n" +
                getServiceInstructions() + "\n\n" +
                "Click \"Finish\" to close the installer."
            );
        } else {
            text.setText(
                "\u274C  Installation Failed\n\n" +
                "The installation encountered errors.\n" +
                "Please review the log output in the previous step.\n\n" +
                "Common issues:\n" +
                "  \u2022  Insufficient permissions (run as admin/sudo)\n" +
                "  \u2022  Port already in use\n" +
                "  \u2022  No internet connection (for Tomcat download)\n" +
                "  \u2022  Insufficient disk space\n\n" +
                "Click \"Finish\" to close the installer."
            );
        }

        panel.add(text, BorderLayout.CENTER);

        JPanel stepPanel = createStepIndicator(3);
        panel.add(stepPanel, BorderLayout.SOUTH);

        panel.revalidate();
        panel.repaint();
    }

    private String getServiceInstructions() {
        switch (os) {
            case "windows":
                return "To manage the service:\n" +
                       "  Start:  net start GreyLegacyTomcat\n" +
                       "  Stop:   net stop GreyLegacyTomcat";
            case "macos":
                return "To manage the service:\n" +
                       "  Start:  sudo launchctl load /Library/LaunchDaemons/com.greylegacy.tomcat.plist\n" +
                       "  Stop:   sudo launchctl unload /Library/LaunchDaemons/com.greylegacy.tomcat.plist";
            default:
                return "To manage the service:\n" +
                       "  Start:  sudo systemctl start greylegacy\n" +
                       "  Stop:   sudo systemctl stop greylegacy\n" +
                       "  Status: sudo systemctl status greylegacy";
        }
    }

    // -------------------------------------------------------------------------
    //  Navigation Panel
    // -------------------------------------------------------------------------
    private JPanel createNavigationPanel() {
        JPanel nav = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 10));
        nav.setBorder(new EmptyBorder(5, 20, 10, 20));

        btnCancel = new JButton("Cancel");
        btnCancel.addActionListener(e -> confirmExit());

        btnBack = new JButton("\u25C0 Back");
        btnBack.addActionListener(e -> navigateBack());

        btnNext = new JButton("Next \u25B6");
        btnNext.addActionListener(e -> navigateNext());

        nav.add(btnCancel);
        nav.add(Box.createHorizontalStrut(20));
        nav.add(btnBack);
        nav.add(btnNext);

        return nav;
    }

    private JPanel createStepIndicator(int activeStep) {
        JPanel panel = new JPanel(new FlowLayout(FlowLayout.CENTER, 15, 10));
        String[] labels = {"Welcome", "Configure", "Install", "Complete"};
        for (int i = 0; i < labels.length; i++) {
            JLabel lbl = new JLabel((i + 1) + ". " + labels[i]);
            if (i == activeStep) {
                lbl.setFont(new Font("SansSerif", Font.BOLD, 12));
                lbl.setForeground(new Color(41, 128, 185));
            } else if (i < activeStep) {
                lbl.setFont(new Font("SansSerif", Font.PLAIN, 12));
                lbl.setForeground(new Color(39, 174, 96));
            } else {
                lbl.setFont(new Font("SansSerif", Font.PLAIN, 12));
                lbl.setForeground(Color.GRAY);
            }
            panel.add(lbl);
            if (i < labels.length - 1) {
                JLabel arrow = new JLabel("\u2192");
                arrow.setForeground(Color.GRAY);
                panel.add(arrow);
            }
        }
        return panel;
    }

    // -------------------------------------------------------------------------
    //  Navigation Logic
    // -------------------------------------------------------------------------
    private void updateButtons() {
        btnBack.setEnabled(currentStep > 0 && currentStep < 2);
        if (currentStep == STEPS.length - 1) {
            btnNext.setText("Finish");
        } else if (currentStep == 1) {
            btnNext.setText("Install \u25B6");
        } else {
            btnNext.setText("Next \u25B6");
        }
        btnNext.setEnabled(currentStep != 2 || installComplete);
        btnCancel.setEnabled(currentStep < 2);
    }

    private void navigateBack() {
        if (currentStep > 0) {
            currentStep--;
            cardLayout.show(cardPanel, STEPS[currentStep]);
            updateButtons();
        }
    }

    private void navigateNext() {
        if (currentStep == STEPS.length - 1) {
            // Finish
            dispose();
            System.exit(0);
            return;
        }

        if (currentStep == 1) {
            // Validate config before proceeding
            if (!validateConfig()) return;

            // Confirm installation
            int confirm = JOptionPane.showConfirmDialog(this,
                "Ready to install " + APP_NAME + "?\n\n" +
                "Install Directory: " + txtInstallDir.getText().trim() + "\n" +
                "Port: " + txtPort.getText().trim() + "\n\n" +
                (os.equals("windows")
                    ? "The installer requires Administrator privileges."
                    : "The installer requires root/sudo privileges."),
                "Confirm Installation",
                JOptionPane.YES_NO_OPTION);
            if (confirm != JOptionPane.YES_OPTION) return;
        }

        currentStep++;
        cardLayout.show(cardPanel, STEPS[currentStep]);
        updateButtons();

        if (currentStep == 2) {
            startInstallation();
        }
    }

    private boolean validateConfig() {
        String dir = txtInstallDir.getText().trim();
        if (dir.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Install directory cannot be empty.",
                "Validation Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        String port = txtPort.getText().trim();
        try {
            int p = Integer.parseInt(port);
            if (p < 1 || p > 65535) throw new NumberFormatException();
        } catch (NumberFormatException e) {
            JOptionPane.showMessageDialog(this, "Port must be a number between 1 and 65535.",
                "Validation Error", JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return true;
    }

    private void browseDirectory() {
        JFileChooser chooser = new JFileChooser();
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Select Installation Directory");
        if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            txtInstallDir.setText(chooser.getSelectedFile().getAbsolutePath());
        }
    }

    private void confirmExit() {
        int result = JOptionPane.showConfirmDialog(this,
            "Are you sure you want to cancel the installation?",
            "Cancel Installation", JOptionPane.YES_NO_OPTION);
        if (result == JOptionPane.YES_OPTION) {
            dispose();
            System.exit(0);
        }
    }

    // -------------------------------------------------------------------------
    //  Installation Execution
    // -------------------------------------------------------------------------
    private void startInstallation() {
        SwingWorker<Boolean, String> worker = new SwingWorker<Boolean, String>() {
            @Override
            protected Boolean doInBackground() {
                try {
                    String scriptPath = findInstallerScript();
                    if (scriptPath == null) {
                        publish("[ERROR] Could not locate installer script for " + os);
                        return false;
                    }

                    publish("[INFO]  Running installer: " + scriptPath);
                    publish("[INFO]  Install dir: " + txtInstallDir.getText().trim());
                    publish("[INFO]  Port: " + txtPort.getText().trim());
                    publish("---------------------------------------------------");
                    publish("");

                    ProcessBuilder pb = buildInstallerProcess(scriptPath);
                    pb.redirectErrorStream(true);

                    Process process = pb.start();

                    try (BufferedReader reader = new BufferedReader(
                            new InputStreamReader(process.getInputStream()))) {
                        String line;
                        while ((line = reader.readLine()) != null) {
                            publish(line);
                        }
                    }

                    int exitCode = process.waitFor();
                    publish("");
                    publish("---------------------------------------------------");
                    publish("[INFO]  Installer exited with code: " + exitCode);

                    return exitCode == 0;
                } catch (Exception e) {
                    publish("[ERROR] " + e.getMessage());
                    return false;
                }
            }

            @Override
            protected void process(java.util.List<String> chunks) {
                for (String line : chunks) {
                    txtLog.append(line + "\n");
                    // Auto-scroll
                    txtLog.setCaretPosition(txtLog.getDocument().getLength());
                }
            }

            @Override
            protected void done() {
                try {
                    boolean success = get();
                    installComplete = true;
                    progressBar.setIndeterminate(false);
                    progressBar.setValue(100);
                    progressBar.setString(success ? "Installation complete" : "Installation failed");

                    updateCompletePanel(success);
                    updateButtons();

                    // Auto-advance to complete panel
                    currentStep = 3;
                    cardLayout.show(cardPanel, STEPS[currentStep]);
                    updateButtons();
                } catch (Exception e) {
                    progressBar.setIndeterminate(false);
                    progressBar.setString("Installation failed");
                    txtLog.append("[ERROR] " + e.getMessage() + "\n");
                    installComplete = true;
                    updateCompletePanel(false);
                    updateButtons();
                }
            }
        };

        worker.execute();
    }

    private String findInstallerScript() {
        // Look relative to the GUI jar location
        String jarDir = getJarDirectory();
        String[] searchPaths;

        switch (os) {
            case "windows":
                searchPaths = new String[]{
                    jarDir + File.separator + "install.cmd",
                    jarDir + File.separator + ".." + File.separator + "windows" + File.separator + "install.cmd",
                    jarDir + File.separator + "windows" + File.separator + "install.cmd"
                };
                break;
            case "macos":
                searchPaths = new String[]{
                    jarDir + File.separator + "install.sh",
                    jarDir + File.separator + ".." + File.separator + "macos" + File.separator + "install.sh",
                    jarDir + File.separator + "macos" + File.separator + "install.sh"
                };
                break;
            default:
                searchPaths = new String[]{
                    jarDir + File.separator + "install.sh",
                    jarDir + File.separator + ".." + File.separator + "linux" + File.separator + "install.sh",
                    jarDir + File.separator + "linux" + File.separator + "install.sh"
                };
                break;
        }

        for (String path : searchPaths) {
            File f = new File(path);
            if (f.exists()) {
                try {
                    return f.getCanonicalPath();
                } catch (IOException e) {
                    return f.getAbsolutePath();
                }
            }
        }
        return null;
    }

    private ProcessBuilder buildInstallerProcess(String scriptPath) {
        String installDir = txtInstallDir.getText().trim();
        String port = txtPort.getText().trim();

        ProcessBuilder pb;
        if (os.equals("windows")) {
            pb = new ProcessBuilder("cmd", "/c", scriptPath,
                "--silent", "--install-dir", installDir, "--port", port);
        } else {
            // Build argument list
            java.util.List<String> args = new java.util.ArrayList<>();
            args.add("bash");
            args.add(scriptPath);
            args.add("--silent");
            args.add("--install-dir");
            args.add(installDir);
            args.add("--port");
            args.add(port);
            if (txtServiceUser != null && !txtServiceUser.getText().trim().isEmpty()) {
                args.add("--user");
                args.add(txtServiceUser.getText().trim());
            }
            if (!chkFirewall.isSelected()) {
                args.add("--skip-firewall");
            }
            pb = new ProcessBuilder(args);
        }

        pb.environment().put("TERM", "dumb");
        pb.redirectErrorStream(true);
        return pb;
    }

    private String getJarDirectory() {
        try {
            String path = InstallerGUI.class.getProtectionDomain()
                .getCodeSource().getLocation().toURI().getPath();
            File f = new File(path);
            return f.isDirectory() ? f.getAbsolutePath() : f.getParent();
        } catch (Exception e) {
            return System.getProperty("user.dir");
        }
    }

    private static String capitalize(String s) {
        if (s == null || s.isEmpty()) return s;
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    // -------------------------------------------------------------------------
    //  Main
    // -------------------------------------------------------------------------
    public static void main(String[] args) {
        // Check for --uninstall flag
        for (String arg : args) {
            if ("--uninstall".equalsIgnoreCase(arg)) {
                SwingUtilities.invokeLater(() -> {
                    UninstallerGUI gui = new UninstallerGUI();
                    gui.setVisible(true);
                });
                return;
            }
        }

        SwingUtilities.invokeLater(() -> {
            InstallerGUI gui = new InstallerGUI();
            gui.setVisible(true);
        });
    }
}
