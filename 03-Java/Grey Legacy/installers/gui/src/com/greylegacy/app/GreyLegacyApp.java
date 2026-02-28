package com.greylegacy.app;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.nio.file.*;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

/**
 * Grey Legacy Desktop Application — dashboard and management console.
 *
 * Provides:
 *  - Service status monitoring with auto-refresh
 *  - One-click launch of the web UI in the default browser
 *  - Start / Stop / Restart service controls
 *  - Live log viewer
 *  - System tray integration (minimize to tray)
 */
public class GreyLegacyApp extends JFrame {

    private static final String APP_NAME = "Grey Legacy Claims System";
    private static final String APP_VERSION = "1.0.0";

    // Detected from install.meta or defaults
    private String installDir;
    private String tomcatDir;
    private String logDir;
    private String confDir;
    private int appPort = 8080;
    private String serviceName;
    private final String os;

    // UI components
    private JLabel lblStatus;
    private JLabel lblStatusDot;
    private JButton btnOpen;
    private JButton btnStart;
    private JButton btnStop;
    private JButton btnRestart;
    private JTextArea txtLog;
    private JLabel lblUrl;
    private Timer statusTimer;
    private TrayIcon trayIcon;

    public GreyLegacyApp() {
        os = detectOS();
        loadConfig();
        initUI();
        initSystemTray();
        startStatusMonitor();
    }

    // =========================================================================
    //  OS & Config
    // =========================================================================

    private String detectOS() {
        String osName = System.getProperty("os.name", "").toLowerCase();
        if (osName.contains("win")) return "windows";
        if (osName.contains("mac") || osName.contains("darwin")) return "macos";
        return "linux";
    }

    private void loadConfig() {
        // Determine install dir: check system property, env var, then defaults
        installDir = System.getProperty("gl.home",
                     System.getenv().getOrDefault("GL_HOME", getDefaultInstallDir()));

        // Try reading install.meta
        Path metaPath = Paths.get(installDir, "install.meta");
        if (Files.exists(metaPath)) {
            try {
                for (String line : Files.readAllLines(metaPath)) {
                    String[] parts = line.split("=", 2);
                    if (parts.length != 2) continue;
                    String key = parts[0].trim();
                    String val = parts[1].trim();
                    switch (key) {
                        case "INSTALL_DIR":   installDir = val; break;
                        case "TOMCAT_DIR":    tomcatDir = val; break;
                        case "LOG_DIR":       logDir = val; break;
                        case "CONF_DIR":      confDir = val; break;
                        case "APP_PORT":      appPort = parseInt(val, 8080); break;
                        case "SERVICE_NAME":  serviceName = val; break;
                        case "SERVICE_LABEL": serviceName = val; break;
                    }
                }
            } catch (IOException ignored) {}
        }

        // Fill in defaults for anything not loaded
        if (tomcatDir == null) tomcatDir = installDir + File.separator + "tomcat";
        if (confDir == null) confDir = installDir + File.separator + "conf";
        if (serviceName == null) serviceName = os.equals("windows") ? "GreyLegacyTomcat" :
                                              os.equals("macos") ? "com.greylegacy.tomcat" : "greylegacy";
        if (logDir == null) {
            logDir = os.equals("linux") ? "/var/log/greylegacy" : installDir + File.separator + "logs";
        }
    }

    private String getDefaultInstallDir() {
        switch (os) {
            case "windows": return "C:\\GreyLegacy";
            case "macos":   return "/usr/local/greylegacy";
            default:        return "/opt/greylegacy";
        }
    }

    private static int parseInt(String s, int def) {
        try { return Integer.parseInt(s); } catch (NumberFormatException e) { return def; }
    }

    // =========================================================================
    //  UI Init
    // =========================================================================

    private void initUI() {
        setTitle(APP_NAME);
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setSize(780, 560);
        setMinimumSize(new Dimension(640, 450));
        setLocationRelativeTo(null);

        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            SwingUtilities.updateComponentTreeUI(this);
        } catch (Exception ignored) {}

        JPanel root = new JPanel(new BorderLayout());

        // ---- Header ----
        JPanel header = new JPanel(new BorderLayout());
        header.setBackground(new Color(44, 62, 80));
        header.setBorder(new EmptyBorder(12, 20, 12, 20));

        JLabel title = new JLabel(APP_NAME);
        title.setFont(new Font("SansSerif", Font.BOLD, 18));
        title.setForeground(Color.WHITE);

        JLabel version = new JLabel("v" + APP_VERSION);
        version.setFont(new Font("SansSerif", Font.PLAIN, 12));
        version.setForeground(new Color(189, 195, 199));

        header.add(title, BorderLayout.WEST);
        header.add(version, BorderLayout.EAST);
        root.add(header, BorderLayout.NORTH);

        // ---- Main content ----
        JPanel content = new JPanel(new BorderLayout(0, 12));
        content.setBorder(new EmptyBorder(15, 20, 15, 20));

        // Top row: status + controls
        JPanel topRow = new JPanel(new BorderLayout(15, 0));

        // Status panel
        JPanel statusPanel = new JPanel(new GridBagLayout());
        statusPanel.setBorder(new TitledBorder("Service Status"));
        GridBagConstraints g = new GridBagConstraints();
        g.anchor = GridBagConstraints.WEST;
        g.insets = new Insets(4, 8, 4, 8);

        g.gridx = 0; g.gridy = 0;
        statusPanel.add(new JLabel("Status:"), g);
        g.gridx = 1;
        lblStatusDot = new JLabel("\u2B24");
        lblStatusDot.setForeground(Color.GRAY);
        statusPanel.add(lblStatusDot, g);
        g.gridx = 2;
        lblStatus = new JLabel("Checking...");
        lblStatus.setFont(new Font("SansSerif", Font.BOLD, 13));
        statusPanel.add(lblStatus, g);

        g.gridx = 0; g.gridy = 1;
        statusPanel.add(new JLabel("URL:"), g);
        g.gridx = 1; g.gridwidth = 2;
        lblUrl = new JLabel("http://localhost:" + appPort + "/greylegacy/");
        lblUrl.setForeground(new Color(41, 128, 185));
        lblUrl.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        lblUrl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) { openBrowser(); }
        });
        statusPanel.add(lblUrl, g);
        g.gridwidth = 1;

        g.gridx = 0; g.gridy = 2;
        statusPanel.add(new JLabel("Install:"), g);
        g.gridx = 1; g.gridwidth = 2;
        JLabel lblInstallDir = new JLabel(installDir);
        lblInstallDir.setFont(new Font("SansSerif", Font.PLAIN, 11));
        statusPanel.add(lblInstallDir, g);

        topRow.add(statusPanel, BorderLayout.CENTER);

        // Control buttons panel
        JPanel controlPanel = new JPanel(new GridLayout(5, 1, 0, 8));
        controlPanel.setBorder(new TitledBorder("Controls"));

        btnOpen = createButton("Open Web UI", new Color(41, 128, 185), e -> openBrowser());
        btnStart = createButton("Start Service", new Color(39, 174, 96), e -> runServiceCommand("start"));
        btnStop = createButton("Stop Service", new Color(192, 57, 43), e -> runServiceCommand("stop"));
        btnRestart = createButton("Restart Service", new Color(243, 156, 18), e -> runServiceCommand("restart"));
        JButton btnRefresh = createButton("Refresh Status", new Color(127, 140, 141), e -> checkStatus());

        controlPanel.add(btnOpen);
        controlPanel.add(btnStart);
        controlPanel.add(btnStop);
        controlPanel.add(btnRestart);
        controlPanel.add(btnRefresh);

        topRow.add(controlPanel, BorderLayout.EAST);
        content.add(topRow, BorderLayout.NORTH);

        // Log viewer
        JPanel logPanel = new JPanel(new BorderLayout(0, 5));
        logPanel.setBorder(new TitledBorder("Application Log"));

        txtLog = new JTextArea();
        txtLog.setEditable(false);
        txtLog.setFont(new Font("Monospaced", Font.PLAIN, 11));
        txtLog.setBackground(new Color(30, 30, 30));
        txtLog.setForeground(new Color(200, 200, 200));
        txtLog.setCaretColor(Color.WHITE);
        JScrollPane logScroll = new JScrollPane(txtLog);

        JPanel logButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 8, 0));
        JButton btnLoadLog = new JButton("Load Latest Log");
        btnLoadLog.addActionListener(e -> loadLog());
        JButton btnClearLog = new JButton("Clear");
        btnClearLog.addActionListener(e -> txtLog.setText(""));
        JButton btnOpenLogDir = new JButton("Open Log Folder");
        btnOpenLogDir.addActionListener(e -> openFolder(logDir));
        logButtons.add(btnLoadLog);
        logButtons.add(btnClearLog);
        logButtons.add(btnOpenLogDir);

        logPanel.add(logButtons, BorderLayout.NORTH);
        logPanel.add(logScroll, BorderLayout.CENTER);
        content.add(logPanel, BorderLayout.CENTER);

        root.add(content, BorderLayout.CENTER);

        // ---- Footer ----
        JPanel footer = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 5));
        footer.setBackground(new Color(236, 240, 241));
        JLabel footerText = new JLabel("Grey Legacy \u00A9 GreyThink");
        footerText.setFont(new Font("SansSerif", Font.PLAIN, 11));
        footerText.setForeground(new Color(127, 140, 141));
        footer.add(footerText);
        root.add(footer, BorderLayout.SOUTH);

        setContentPane(root);
    }

    private JButton createButton(String text, Color bg, ActionListener action) {
        JButton btn = new JButton(text);
        btn.setBackground(bg);
        btn.setForeground(Color.WHITE);
        btn.setFocusPainted(false);
        btn.setBorderPainted(false);
        btn.setOpaque(true);
        btn.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        btn.setFont(new Font("SansSerif", Font.BOLD, 12));
        btn.setPreferredSize(new Dimension(160, 32));
        btn.addActionListener(action);
        return btn;
    }

    // =========================================================================
    //  System Tray
    // =========================================================================

    private void initSystemTray() {
        if (!SystemTray.isSupported()) return;

        // Create a simple icon (a filled circle)
        Image icon = createTrayImage();

        PopupMenu popup = new PopupMenu();

        MenuItem openItem = new MenuItem("Open Dashboard");
        openItem.addActionListener(e -> { setVisible(true); toFront(); });

        MenuItem browserItem = new MenuItem("Open Web UI");
        browserItem.addActionListener(e -> openBrowser());

        MenuItem startItem = new MenuItem("Start Service");
        startItem.addActionListener(e -> runServiceCommand("start"));

        MenuItem stopItem = new MenuItem("Stop Service");
        stopItem.addActionListener(e -> runServiceCommand("stop"));

        MenuItem exitItem = new MenuItem("Exit");
        exitItem.addActionListener(e -> {
            if (statusTimer != null) statusTimer.stop();
            SystemTray.getSystemTray().remove(trayIcon);
            System.exit(0);
        });

        popup.add(openItem);
        popup.add(browserItem);
        popup.addSeparator();
        popup.add(startItem);
        popup.add(stopItem);
        popup.addSeparator();
        popup.add(exitItem);

        trayIcon = new TrayIcon(icon, APP_NAME, popup);
        trayIcon.setImageAutoSize(true);
        trayIcon.addActionListener(e -> { setVisible(true); toFront(); });

        try {
            SystemTray.getSystemTray().add(trayIcon);
        } catch (AWTException ignored) {}
    }

    private Image createTrayImage() {
        int size = 16;
        java.awt.image.BufferedImage img =
            new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB);
        Graphics2D g2 = img.createGraphics();
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setColor(new Color(44, 62, 80));
        g2.fillOval(1, 1, size - 2, size - 2);
        g2.setColor(Color.WHITE);
        g2.setFont(new Font("SansSerif", Font.BOLD, 10));
        g2.drawString("G", 4, 12);
        g2.dispose();
        return img;
    }

    // =========================================================================
    //  Status Monitoring
    // =========================================================================

    private void startStatusMonitor() {
        checkStatus();
        statusTimer = new Timer(10_000, e -> checkStatus());
        statusTimer.start();
    }

    private void checkStatus() {
        SwingWorker<Boolean, Void> w = new SwingWorker<Boolean, Void>() {
            @Override
            protected Boolean doInBackground() {
                return isServiceRunning();
            }

            @Override
            protected void done() {
                try {
                    boolean running = get();
                    if (running) {
                        lblStatus.setText("Running");
                        lblStatus.setForeground(new Color(39, 174, 96));
                        lblStatusDot.setForeground(new Color(39, 174, 96));
                        btnStart.setEnabled(false);
                        btnStop.setEnabled(true);
                        btnRestart.setEnabled(true);
                        btnOpen.setEnabled(true);
                    } else {
                        lblStatus.setText("Stopped");
                        lblStatus.setForeground(new Color(192, 57, 43));
                        lblStatusDot.setForeground(new Color(192, 57, 43));
                        btnStart.setEnabled(true);
                        btnStop.setEnabled(false);
                        btnRestart.setEnabled(false);
                        btnOpen.setEnabled(false);
                    }
                } catch (Exception e) {
                    lblStatus.setText("Unknown");
                    lblStatus.setForeground(Color.GRAY);
                    lblStatusDot.setForeground(Color.GRAY);
                }
            }
        };
        w.execute();
    }

    private boolean isServiceRunning() {
        // Try HTTP health check first
        try {
            URL url = new URL("http://localhost:" + appPort + "/greylegacy/");
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setConnectTimeout(3000);
            conn.setReadTimeout(3000);
            conn.setRequestMethod("HEAD");
            int code = conn.getResponseCode();
            conn.disconnect();
            return code >= 200 && code < 500;
        } catch (Exception ignored) {}

        // Fall back to OS service check
        try {
            ProcessBuilder pb;
            switch (os) {
                case "windows":
                    pb = new ProcessBuilder("sc", "query", serviceName);
                    break;
                case "macos":
                    pb = new ProcessBuilder("launchctl", "list", serviceName);
                    break;
                default:
                    pb = new ProcessBuilder("systemctl", "is-active", "--quiet", serviceName);
                    break;
            }
            pb.redirectErrorStream(true);
            Process p = pb.start();
            int exit = p.waitFor();
            if (os.equals("windows")) {
                String output = readProcessOutput(p);
                return output.contains("RUNNING");
            }
            return exit == 0;
        } catch (Exception e) {
            return false;
        }
    }

    // =========================================================================
    //  Actions
    // =========================================================================

    private void openBrowser() {
        try {
            Desktop.getDesktop().browse(new URI("http://localhost:" + appPort + "/greylegacy/"));
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this, "Could not open browser:\n" + e.getMessage(),
                "Error", JOptionPane.ERROR_MESSAGE);
        }
    }

    private void openFolder(String path) {
        try {
            File dir = new File(path);
            if (dir.exists()) {
                Desktop.getDesktop().open(dir);
            } else {
                JOptionPane.showMessageDialog(this, "Directory not found: " + path);
            }
        } catch (Exception e) {
            appendLog("[ERROR] Could not open folder: " + e.getMessage());
        }
    }

    private void runServiceCommand(String action) {
        appendLog("[INFO]  " + capitalize(action) + " service '" + serviceName + "'...");

        SwingWorker<Integer, String> worker = new SwingWorker<Integer, String>() {
            @Override
            protected Integer doInBackground() {
                try {
                    ProcessBuilder pb;
                    switch (os) {
                        case "windows":
                            if ("restart".equals(action)) {
                                // Windows doesn't have a single restart command
                                exec("net", "stop", serviceName);
                                Thread.sleep(3000);
                                pb = new ProcessBuilder("net", "start", serviceName);
                            } else {
                                pb = new ProcessBuilder("net", action, serviceName);
                            }
                            break;
                        case "macos":
                            if ("start".equals(action)) {
                                pb = new ProcessBuilder("launchctl", "load",
                                    "/Library/LaunchDaemons/" + serviceName + ".plist");
                            } else if ("stop".equals(action)) {
                                pb = new ProcessBuilder("launchctl", "unload",
                                    "/Library/LaunchDaemons/" + serviceName + ".plist");
                            } else {
                                exec("launchctl", "unload",
                                    "/Library/LaunchDaemons/" + serviceName + ".plist");
                                Thread.sleep(3000);
                                pb = new ProcessBuilder("launchctl", "load",
                                    "/Library/LaunchDaemons/" + serviceName + ".plist");
                            }
                            break;
                        default:
                            pb = new ProcessBuilder("systemctl", action, serviceName);
                            break;
                    }

                    pb.redirectErrorStream(true);
                    Process process = pb.start();
                    String output = readProcessOutput(process);
                    int exitCode = process.waitFor();

                    if (!output.trim().isEmpty()) {
                        publish(output.trim());
                    }
                    return exitCode;
                } catch (Exception e) {
                    publish("[ERROR] " + e.getMessage());
                    return -1;
                }
            }

            @Override
            protected void process(java.util.List<String> chunks) {
                for (String s : chunks) appendLog(s);
            }

            @Override
            protected void done() {
                try {
                    int code = get();
                    if (code == 0) {
                        appendLog("[OK]    Service " + action + " completed.");
                    } else {
                        appendLog("[WARN]  Service " + action + " exited with code " + code + ".");
                        appendLog("[INFO]  You may need to run the app with elevated privileges.");
                    }
                } catch (Exception e) {
                    appendLog("[ERROR] " + e.getMessage());
                }
                // Refresh status after a short delay
                Timer t = new Timer(2000, ev -> checkStatus());
                t.setRepeats(false);
                t.start();
            }
        };
        worker.execute();
    }

    private int exec(String... cmd) {
        try {
            ProcessBuilder pb = new ProcessBuilder(cmd);
            pb.redirectErrorStream(true);
            Process p = pb.start();
            readProcessOutput(p); // consume
            return p.waitFor();
        } catch (Exception e) {
            return -1;
        }
    }

    // =========================================================================
    //  Log Viewer
    // =========================================================================

    private void loadLog() {
        SwingWorker<String, Void> worker = new SwingWorker<String, Void>() {
            @Override
            protected String doInBackground() {
                // Try catalina.out first, then other log files
                String[] logFiles = {
                    tomcatDir + File.separator + "logs" + File.separator + "catalina.out",
                    logDir + File.separator + "catalina.out",
                    logDir + File.separator + "catalina-stdout.log"
                };

                for (String logFile : logFiles) {
                    File f = new File(logFile);
                    if (f.exists() && f.length() > 0) {
                        return readTail(f, 200);
                    }
                }
                return "[INFO]  No log file found. The service may not have been started yet.\n" +
                       "[INFO]  Searched:\n" +
                       "          " + String.join("\n          ", logFiles);
            }

            @Override
            protected void done() {
                try {
                    txtLog.setText(get());
                    txtLog.setCaretPosition(txtLog.getDocument().getLength());
                } catch (Exception e) {
                    txtLog.setText("[ERROR] " + e.getMessage());
                }
            }
        };
        worker.execute();
    }

    private String readTail(File file, int lines) {
        try {
            java.util.List<String> allLines = Files.readAllLines(file.toPath());
            int start = Math.max(0, allLines.size() - lines);
            StringBuilder sb = new StringBuilder();
            sb.append("--- Last ").append(Math.min(lines, allLines.size()))
              .append(" lines of ").append(file.getAbsolutePath()).append(" ---\n\n");
            for (int i = start; i < allLines.size(); i++) {
                sb.append(allLines.get(i)).append('\n');
            }
            return sb.toString();
        } catch (IOException e) {
            return "[ERROR] Cannot read log: " + e.getMessage();
        }
    }

    // =========================================================================
    //  Helpers
    // =========================================================================

    private void appendLog(String msg) {
        String ts = new SimpleDateFormat("HH:mm:ss").format(new Date());
        txtLog.append("[" + ts + "] " + msg + "\n");
        txtLog.setCaretPosition(txtLog.getDocument().getLength());
    }

    private static String readProcessOutput(Process p) throws IOException {
        StringBuilder sb = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()))) {
            String line;
            while ((line = br.readLine()) != null) {
                sb.append(line).append('\n');
            }
        }
        return sb.toString();
    }

    private static String capitalize(String s) {
        return (s == null || s.isEmpty()) ? s : s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    // =========================================================================
    //  Entry Point
    // =========================================================================

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            GreyLegacyApp app = new GreyLegacyApp();
            app.setVisible(true);
        });
    }
}
