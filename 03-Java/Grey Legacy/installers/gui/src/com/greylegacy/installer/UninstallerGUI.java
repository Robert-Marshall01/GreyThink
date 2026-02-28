package com.greylegacy.installer;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.io.*;

/**
 * Cross-platform GUI uninstaller for the Grey Legacy Claims System.
 * Provides a wizard-style interface that delegates to the platform-specific
 * uninstall scripts (uninstall.sh / uninstall.cmd).
 */
public class UninstallerGUI extends JFrame {

    private static final String APP_NAME = "Grey Legacy Claims System";

    private CardLayout cardLayout;
    private JPanel cardPanel;
    private int currentStep = 0;
    private static final String[] STEPS = {"confirm", "progress", "complete"};

    private JButton btnBack;
    private JButton btnNext;
    private JButton btnCancel;

    private JCheckBox chkKeepData;
    private JCheckBox chkKeepLogs;
    private JCheckBox chkKeepUser;
    private JTextField txtInstallDir;

    private JProgressBar progressBar;
    private JTextArea txtLog;
    private boolean uninstallComplete = false;

    private final String os;

    public UninstallerGUI() {
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
        setTitle(APP_NAME + " - Uninstaller");
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowClosing(java.awt.event.WindowEvent e) {
                confirmExit();
            }
        });
        setSize(650, 480);
        setMinimumSize(new Dimension(550, 400));
        setLocationRelativeTo(null);
        setResizable(true);

        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            SwingUtilities.updateComponentTreeUI(this);
        } catch (Exception ignored) {}

        JPanel mainPanel = new JPanel(new BorderLayout());

        // Header
        JPanel header = createHeader();
        mainPanel.add(header, BorderLayout.NORTH);

        cardLayout = new CardLayout();
        cardPanel = new JPanel(cardLayout);
        cardPanel.add(createConfirmPanel(), "confirm");
        cardPanel.add(createProgressPanel(), "progress");
        cardPanel.add(createCompletePanel(), "complete");
        mainPanel.add(cardPanel, BorderLayout.CENTER);

        JPanel navPanel = createNavigationPanel();
        mainPanel.add(navPanel, BorderLayout.SOUTH);

        setContentPane(mainPanel);
        updateButtons();
    }

    private JPanel createHeader() {
        JPanel header = new JPanel(new BorderLayout());
        header.setBackground(new Color(192, 57, 43));
        header.setBorder(new EmptyBorder(15, 20, 15, 20));

        JLabel titleLabel = new JLabel(APP_NAME);
        titleLabel.setFont(new Font("SansSerif", Font.BOLD, 20));
        titleLabel.setForeground(Color.WHITE);

        JLabel versionLabel = new JLabel("Uninstaller");
        versionLabel.setFont(new Font("SansSerif", Font.PLAIN, 12));
        versionLabel.setForeground(new Color(245, 215, 210));

        header.add(titleLabel, BorderLayout.CENTER);
        header.add(versionLabel, BorderLayout.EAST);
        return header;
    }

    // -------------------------------------------------------------------------
    //  Step 1: Confirm Panel
    // -------------------------------------------------------------------------
    private JPanel createConfirmPanel() {
        JPanel panel = new JPanel(new BorderLayout(0, 15));
        panel.setBorder(new EmptyBorder(25, 40, 25, 40));

        JTextArea infoText = new JTextArea();
        infoText.setEditable(false);
        infoText.setOpaque(false);
        infoText.setFont(new Font("SansSerif", Font.PLAIN, 14));
        infoText.setLineWrap(true);
        infoText.setWrapStyleWord(true);
        infoText.setText(
            "This will uninstall the " + APP_NAME + " from your computer.\n\n" +
            "The following components will be removed:\n" +
            "  \u2022  Apache Tomcat and deployed application\n" +
            "  \u2022  System service registration\n" +
            "  \u2022  Configuration files\n" +
            "  \u2022  Firewall rules\n\n" +
            "You can choose to keep your data and logs."
        );
        panel.add(infoText, BorderLayout.NORTH);

        // Install dir detection
        JPanel dirPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        dirPanel.add(new JLabel("Install Directory:"));
        String defaultDir;
        switch (os) {
            case "windows": defaultDir = "C:\\GreyLegacy"; break;
            case "macos":   defaultDir = "/usr/local/greylegacy"; break;
            default:        defaultDir = "/opt/greylegacy"; break;
        }
        // Try to read from install.meta
        String detectedDir = readInstallMeta(defaultDir);
        txtInstallDir = new JTextField(detectedDir, 30);
        dirPanel.add(txtInstallDir);

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
        optionsPanel.setBorder(new TitledBorder("Preservation Options"));

        chkKeepData = new JCheckBox("Keep application data", false);
        chkKeepLogs = new JCheckBox("Keep log files", false);
        chkKeepUser = new JCheckBox("Keep service user account (Linux only)", false);

        optionsPanel.add(chkKeepData);
        optionsPanel.add(Box.createVerticalStrut(5));
        optionsPanel.add(chkKeepLogs);
        if (!os.equals("windows")) {
            optionsPanel.add(Box.createVerticalStrut(5));
            optionsPanel.add(chkKeepUser);
        }

        JPanel centerPanel = new JPanel(new BorderLayout(0, 10));
        centerPanel.add(dirPanel, BorderLayout.NORTH);
        centerPanel.add(optionsPanel, BorderLayout.CENTER);
        panel.add(centerPanel, BorderLayout.CENTER);

        return panel;
    }

    private String readInstallMeta(String defaultDir) {
        // Try common install locations for install.meta
        String[] candidates = {defaultDir, "/opt/greylegacy", "/usr/local/greylegacy", "C:\\GreyLegacy"};
        for (String dir : candidates) {
            File meta = new File(dir, "install.meta");
            if (meta.exists()) {
                try (BufferedReader br = new BufferedReader(new FileReader(meta))) {
                    String line;
                    while ((line = br.readLine()) != null) {
                        if (line.startsWith("INSTALL_DIR=")) {
                            return line.substring("INSTALL_DIR=".length()).trim();
                        }
                    }
                } catch (IOException ignored) {}
            }
        }
        return defaultDir;
    }

    // -------------------------------------------------------------------------
    //  Step 2: Progress Panel
    // -------------------------------------------------------------------------
    private JPanel createProgressPanel() {
        JPanel panel = new JPanel(new BorderLayout(0, 10));
        panel.setBorder(new EmptyBorder(20, 40, 20, 40));

        JLabel lblStatus = new JLabel("Uninstalling...");
        lblStatus.setFont(new Font("SansSerif", Font.BOLD, 14));
        panel.add(lblStatus, BorderLayout.NORTH);

        txtLog = new JTextArea();
        txtLog.setEditable(false);
        txtLog.setFont(new Font("Monospaced", Font.PLAIN, 12));
        txtLog.setBackground(new Color(30, 30, 30));
        txtLog.setForeground(new Color(200, 200, 200));
        JScrollPane scrollPane = new JScrollPane(txtLog);
        panel.add(scrollPane, BorderLayout.CENTER);

        progressBar = new JProgressBar();
        progressBar.setIndeterminate(true);
        progressBar.setStringPainted(true);
        progressBar.setString("Uninstalling...");
        panel.add(progressBar, BorderLayout.SOUTH);

        return panel;
    }

    // -------------------------------------------------------------------------
    //  Step 3: Complete Panel
    // -------------------------------------------------------------------------
    private JPanel createCompletePanel() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBorder(new EmptyBorder(30, 40, 30, 40));
        return panel;
    }

    private void updateCompletePanel(boolean success) {
        JPanel panel = (JPanel) cardPanel.getComponent(2);
        panel.removeAll();

        JTextArea text = new JTextArea();
        text.setEditable(false);
        text.setOpaque(false);
        text.setFont(new Font("SansSerif", Font.PLAIN, 14));
        text.setLineWrap(true);
        text.setWrapStyleWord(true);

        if (success) {
            StringBuilder sb = new StringBuilder();
            sb.append("\u2705  Uninstallation Completed Successfully!\n\n");
            sb.append("The ").append(APP_NAME).append(" has been removed.\n\n");
            if (chkKeepData.isSelected()) {
                sb.append("  \u2022  Data files have been preserved.\n");
            }
            if (chkKeepLogs.isSelected()) {
                sb.append("  \u2022  Log files have been preserved.\n");
            }
            sb.append("\nClick \"Finish\" to close.");
            text.setText(sb.toString());
        } else {
            text.setText(
                "\u274C  Uninstallation encountered errors.\n\n" +
                "Please review the log output.\n\n" +
                "Click \"Finish\" to close."
            );
        }

        panel.add(text, BorderLayout.CENTER);
        panel.revalidate();
        panel.repaint();
    }

    // -------------------------------------------------------------------------
    //  Navigation
    // -------------------------------------------------------------------------
    private JPanel createNavigationPanel() {
        JPanel nav = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 10));
        nav.setBorder(new EmptyBorder(5, 20, 10, 20));

        btnCancel = new JButton("Cancel");
        btnCancel.addActionListener(e -> confirmExit());

        btnBack = new JButton("\u25C0 Back");
        btnBack.addActionListener(e -> navigateBack());

        btnNext = new JButton("Uninstall");
        btnNext.addActionListener(e -> navigateNext());

        nav.add(btnCancel);
        nav.add(Box.createHorizontalStrut(20));
        nav.add(btnBack);
        nav.add(btnNext);

        return nav;
    }

    private void updateButtons() {
        btnBack.setEnabled(false);
        btnCancel.setEnabled(currentStep == 0);
        if (currentStep == 0) {
            btnNext.setText("Uninstall");
            btnNext.setEnabled(true);
        } else if (currentStep == 1) {
            btnNext.setText("Finish");
            btnNext.setEnabled(uninstallComplete);
        } else {
            btnNext.setText("Finish");
            btnNext.setEnabled(true);
        }
    }

    private void navigateBack() {}

    private void navigateNext() {
        if (currentStep == STEPS.length - 1) {
            dispose();
            System.exit(0);
            return;
        }

        if (currentStep == 0) {
            int confirm = JOptionPane.showConfirmDialog(this,
                "Are you sure you want to uninstall " + APP_NAME + "?\n\n" +
                "This action cannot be undone.",
                "Confirm Uninstallation",
                JOptionPane.YES_NO_OPTION,
                JOptionPane.WARNING_MESSAGE);
            if (confirm != JOptionPane.YES_OPTION) return;
        }

        currentStep++;
        cardLayout.show(cardPanel, STEPS[currentStep]);
        updateButtons();

        if (currentStep == 1) {
            startUninstallation();
        }
    }

    private void confirmExit() {
        int result = JOptionPane.showConfirmDialog(this,
            "Cancel the uninstallation?",
            "Cancel", JOptionPane.YES_NO_OPTION);
        if (result == JOptionPane.YES_OPTION) {
            dispose();
            System.exit(0);
        }
    }

    // -------------------------------------------------------------------------
    //  Uninstallation Execution
    // -------------------------------------------------------------------------
    private void startUninstallation() {
        SwingWorker<Boolean, String> worker = new SwingWorker<Boolean, String>() {
            @Override
            protected Boolean doInBackground() {
                try {
                    String scriptPath = findUninstallerScript();
                    if (scriptPath == null) {
                        publish("[ERROR] Could not locate uninstaller script for " + os);
                        return false;
                    }

                    publish("[INFO]  Running uninstaller: " + scriptPath);
                    publish("---------------------------------------------------");
                    publish("");

                    ProcessBuilder pb = buildUninstallerProcess(scriptPath);
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
                    publish("[INFO]  Uninstaller exited with code: " + exitCode);

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
                    txtLog.setCaretPosition(txtLog.getDocument().getLength());
                }
            }

            @Override
            protected void done() {
                try {
                    boolean success = get();
                    uninstallComplete = true;
                    progressBar.setIndeterminate(false);
                    progressBar.setValue(100);
                    progressBar.setString(success ? "Uninstallation complete" : "Uninstallation failed");

                    updateCompletePanel(success);
                    updateButtons();

                    currentStep = 2;
                    cardLayout.show(cardPanel, STEPS[currentStep]);
                    updateButtons();
                } catch (Exception e) {
                    progressBar.setIndeterminate(false);
                    progressBar.setString("Uninstallation failed");
                    txtLog.append("[ERROR] " + e.getMessage() + "\n");
                    uninstallComplete = true;
                    updateCompletePanel(false);
                    updateButtons();
                }
            }
        };

        worker.execute();
    }

    private String findUninstallerScript() {
        String jarDir = getJarDirectory();

        // First check install directory for uninstaller
        String installDir = txtInstallDir.getText().trim();
        String installedScript;
        if (os.equals("windows")) {
            installedScript = installDir + File.separator + "uninstall.cmd";
        } else {
            installedScript = installDir + File.separator + "uninstall.sh";
        }
        if (new File(installedScript).exists()) return installedScript;

        // Fall back to script alongside this jar
        String[] searchPaths;
        switch (os) {
            case "windows":
                searchPaths = new String[]{
                    jarDir + File.separator + "uninstall.cmd",
                    jarDir + File.separator + ".." + File.separator + "windows" + File.separator + "uninstall.cmd",
                    jarDir + File.separator + "windows" + File.separator + "uninstall.cmd"
                };
                break;
            case "macos":
                searchPaths = new String[]{
                    jarDir + File.separator + "uninstall.sh",
                    jarDir + File.separator + ".." + File.separator + "macos" + File.separator + "uninstall.sh",
                    jarDir + File.separator + "macos" + File.separator + "uninstall.sh"
                };
                break;
            default:
                searchPaths = new String[]{
                    jarDir + File.separator + "uninstall.sh",
                    jarDir + File.separator + ".." + File.separator + "linux" + File.separator + "uninstall.sh",
                    jarDir + File.separator + "linux" + File.separator + "uninstall.sh"
                };
                break;
        }

        for (String path : searchPaths) {
            File f = new File(path);
            if (f.exists()) {
                try { return f.getCanonicalPath(); }
                catch (IOException e) { return f.getAbsolutePath(); }
            }
        }
        return null;
    }

    private ProcessBuilder buildUninstallerProcess(String scriptPath) {
        ProcessBuilder pb;
        if (os.equals("windows")) {
            java.util.List<String> args = new java.util.ArrayList<>();
            args.add("cmd");
            args.add("/c");
            args.add(scriptPath);
            args.add("--silent");
            if (chkKeepData.isSelected()) args.add("--keep-data");
            if (chkKeepLogs.isSelected()) args.add("--keep-logs");
            pb = new ProcessBuilder(args);
        } else {
            java.util.List<String> args = new java.util.ArrayList<>();
            args.add("bash");
            args.add(scriptPath);
            args.add("--silent");
            if (chkKeepData.isSelected()) args.add("--keep-data");
            if (chkKeepLogs.isSelected()) args.add("--keep-logs");
            if (chkKeepUser != null && chkKeepUser.isSelected()) args.add("--keep-user");
            pb = new ProcessBuilder(args);
        }

        pb.environment().put("TERM", "dumb");
        pb.redirectErrorStream(true);
        return pb;
    }

    private String getJarDirectory() {
        try {
            String path = UninstallerGUI.class.getProtectionDomain()
                .getCodeSource().getLocation().toURI().getPath();
            File f = new File(path);
            return f.isDirectory() ? f.getAbsolutePath() : f.getParent();
        } catch (Exception e) {
            return System.getProperty("user.dir");
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            UninstallerGUI gui = new UninstallerGUI();
            gui.setVisible(true);
        });
    }
}
