package searchengine;

import java.awt.*;
import java.awt.event.*;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import javax.swing.*;
import javax.swing.border.*;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import javafx.scene.layout.BorderPane;

/**
 * Web Search Engine GUI - Full web browser experience with multiple search engines.
 * Features embedded browser with Google, DuckDuckGo, Bing, Wikipedia, and YouTube search.
 */
public class WebSearchGUI extends JFrame {
    
    private static final Color BACKGROUND_DARK = new Color(32, 33, 36);
    private static final Color TEXT_PRIMARY = new Color(232, 234, 237);
    private static final Color TEXT_SECONDARY = new Color(154, 160, 166);
    private static final Color LINK_COLOR = new Color(138, 180, 248);
    private static final Color ACCENT_COLOR = new Color(66, 133, 244);
    private static final Color SEARCH_BAR_BG = new Color(32, 33, 36);
    private static final Color SEARCH_BAR_BORDER = new Color(95, 99, 104);
    private static final Color SEARCH_BAR_HOVER = new Color(60, 64, 67);
    
    // Search engine options
    private static final String[] SEARCH_ENGINES = {"Google", "DuckDuckGo", "Bing", "Wikipedia", "YouTube"};
    private static final String[] SEARCH_ENGINE_URLS = {
        "https://www.google.com/search?q=",
        "https://duckduckgo.com/?q=",
        "https://www.bing.com/search?q=",
        "https://en.wikipedia.org/wiki/Special:Search?search=",
        "https://www.youtube.com/results?search_query="
    };
    private static final String[] SEARCH_ENGINE_HOMES = {
        "https://www.google.com",
        "https://duckduckgo.com",
        "https://www.bing.com",
        "https://en.wikipedia.org",
        "https://www.youtube.com"
    };
    
    private JPanel mainPanel;
    private CardLayout cardLayout;
    private JTextField homeSearchField;
    private JTextField browserUrlField;
    private JComboBox<String> homeEngineSelector;
    
    // Embedded browser components
    private JFXPanel browserPanel;
    private WebEngine webEngine;
    private JButton backButton;
    private JButton forwardButton;
    
    public WebSearchGUI() {
        initializeUI();
        initializeBrowser();
    }
    
    private void initializeBrowser() {
        // Initialize JavaFX on its thread
        Platform.runLater(() -> {
            WebView webView = new WebView();
            webEngine = webView.getEngine();
            
            // Listen for URL changes to update the address bar
            webEngine.locationProperty().addListener((obs, oldUrl, newUrl) -> {
                SwingUtilities.invokeLater(() -> {
                    browserUrlField.setText(newUrl);
                });
            });
            
            // Listen for page title changes
            webEngine.titleProperty().addListener((obs, oldTitle, newTitle) -> {
                SwingUtilities.invokeLater(() -> {
                    if (newTitle != null && !newTitle.isEmpty()) {
                        setTitle("Web Search - " + newTitle);
                    }
                });
            });
            
            BorderPane root = new BorderPane();
            root.setCenter(webView);
            
            Scene scene = new Scene(root);
            browserPanel.setScene(scene);
        });
    }
    
    private void initializeUI() {
        setTitle("Web Search");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(1200, 800);
        setLocationRelativeTo(null);
        setBackground(BACKGROUND_DARK);
        
        cardLayout = new CardLayout();
        mainPanel = new JPanel(cardLayout);
        mainPanel.setBackground(BACKGROUND_DARK);
        
        mainPanel.add(createHomePage(), "home");
        mainPanel.add(createBrowserPage(), "browser");
        
        add(mainPanel);
        cardLayout.show(mainPanel, "home");
    }
    
    private JPanel createHomePage() {
        JPanel homePanel = new JPanel(new GridBagLayout());
        homePanel.setBackground(BACKGROUND_DARK);
        
        JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setBackground(BACKGROUND_DARK);
        centerPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        // Logo
        JLabel logoLabel = new JLabel("Web Search");
        logoLabel.setFont(new Font("Product Sans", Font.BOLD, 72));
        logoLabel.setForeground(ACCENT_COLOR);
        logoLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        // Subtitle
        JLabel subtitleLabel = new JLabel("Search the World Wide Web");
        subtitleLabel.setFont(new Font("Segoe UI", Font.PLAIN, 16));
        subtitleLabel.setForeground(TEXT_SECONDARY);
        subtitleLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        // Search engine selector
        JPanel enginePanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 10, 0));
        enginePanel.setBackground(BACKGROUND_DARK);
        
        JLabel engineLabel = new JLabel("Search with:");
        engineLabel.setFont(new Font("Segoe UI", Font.PLAIN, 14));
        engineLabel.setForeground(TEXT_SECONDARY);
        
        homeEngineSelector = new JComboBox<>(SEARCH_ENGINES);
        homeEngineSelector.setFont(new Font("Segoe UI", Font.PLAIN, 14));
        homeEngineSelector.setPreferredSize(new Dimension(150, 35));
        homeEngineSelector.setBackground(SEARCH_BAR_BG);
        homeEngineSelector.setForeground(TEXT_PRIMARY);
        
        enginePanel.add(engineLabel);
        enginePanel.add(homeEngineSelector);
        
        // Search bar
        JPanel searchPanel = createHomeSearchBar();
        searchPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        // Quick links to search engines
        JPanel quickLinksPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 0));
        quickLinksPanel.setBackground(BACKGROUND_DARK);
        
        for (int i = 0; i < SEARCH_ENGINES.length; i++) {
            final int index = i;
            JButton engineButton = new JButton(SEARCH_ENGINES[i]);
            engineButton.setFont(new Font("Segoe UI", Font.PLAIN, 12));
            engineButton.setForeground(LINK_COLOR);
            engineButton.setBackground(BACKGROUND_DARK);
            engineButton.setBorderPainted(false);
            engineButton.setContentAreaFilled(false);
            engineButton.setFocusPainted(false);
            engineButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            engineButton.addActionListener(e -> openInBrowser(SEARCH_ENGINE_HOMES[index]));
            engineButton.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseEntered(MouseEvent e) {
                    engineButton.setForeground(LINK_COLOR.brighter());
                }
                @Override
                public void mouseExited(MouseEvent e) {
                    engineButton.setForeground(LINK_COLOR);
                }
            });
            quickLinksPanel.add(engineButton);
        }
        
        // Powered by label
        JLabel poweredByLabel = new JLabel("Click a link above to browse directly, or search below");
        poweredByLabel.setFont(new Font("Segoe UI", Font.PLAIN, 12));
        poweredByLabel.setForeground(TEXT_SECONDARY);
        poweredByLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        centerPanel.add(logoLabel);
        centerPanel.add(Box.createVerticalStrut(10));
        centerPanel.add(subtitleLabel);
        centerPanel.add(Box.createVerticalStrut(25));
        centerPanel.add(enginePanel);
        centerPanel.add(Box.createVerticalStrut(15));
        centerPanel.add(searchPanel);
        centerPanel.add(Box.createVerticalStrut(30));
        centerPanel.add(quickLinksPanel);
        centerPanel.add(Box.createVerticalStrut(15));
        centerPanel.add(poweredByLabel);
        
        homePanel.add(centerPanel);
        
        return homePanel;
    }
    
    private JPanel createHomeSearchBar() {
        JPanel searchPanel = new JPanel(new BorderLayout());
        searchPanel.setBackground(SEARCH_BAR_BG);
        searchPanel.setBorder(BorderFactory.createCompoundBorder(
            new LineBorder(SEARCH_BAR_BORDER, 1, true),
            new EmptyBorder(12, 20, 12, 20)
        ));
        searchPanel.setMaximumSize(new Dimension(600, 50));
        searchPanel.setPreferredSize(new Dimension(600, 50));
        
        // Search icon
        JLabel searchIcon = new JLabel("🔍");
        searchIcon.setFont(new Font("Segoe UI Emoji", Font.PLAIN, 18));
        searchIcon.setBorder(new EmptyBorder(0, 0, 0, 15));
        
        homeSearchField = new JTextField();
        homeSearchField.setFont(new Font("Segoe UI", Font.PLAIN, 16));
        homeSearchField.setBorder(null);
        homeSearchField.setBackground(SEARCH_BAR_BG);
        homeSearchField.setForeground(TEXT_PRIMARY);
        homeSearchField.setCaretColor(TEXT_PRIMARY);
        
        homeSearchField.addActionListener(e -> performSearch(homeSearchField.getText()));
        
        // Search button
        JButton searchButton = new JButton("Search");
        searchButton.setFont(new Font("Segoe UI", Font.BOLD, 14));
        searchButton.setForeground(Color.WHITE);
        searchButton.setBackground(ACCENT_COLOR);
        searchButton.setBorder(new EmptyBorder(8, 20, 8, 20));
        searchButton.setFocusPainted(false);
        searchButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        searchButton.addActionListener(e -> performSearch(homeSearchField.getText()));
        
        // Hover effect
        searchPanel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                searchPanel.setBackground(SEARCH_BAR_HOVER);
                homeSearchField.setBackground(SEARCH_BAR_HOVER);
            }
            
            @Override
            public void mouseExited(MouseEvent e) {
                searchPanel.setBackground(SEARCH_BAR_BG);
                homeSearchField.setBackground(SEARCH_BAR_BG);
            }
        });
        
        searchPanel.add(searchIcon, BorderLayout.WEST);
        searchPanel.add(homeSearchField, BorderLayout.CENTER);
        searchPanel.add(searchButton, BorderLayout.EAST);
        
        return searchPanel;
    }
    
    private JPanel createBrowserPage() {
        JPanel browserPage = new JPanel(new BorderLayout());
        browserPage.setBackground(BACKGROUND_DARK);
        
        // Browser toolbar
        JPanel toolbar = new JPanel(new BorderLayout());
        toolbar.setBackground(BACKGROUND_DARK);
        toolbar.setBorder(new EmptyBorder(8, 10, 8, 10));
        
        // Navigation buttons panel
        JPanel navPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        navPanel.setBackground(BACKGROUND_DARK);
        
        // Home button
        JButton homeButton = new JButton("🏠");
        homeButton.setFont(new Font("Segoe UI Emoji", Font.PLAIN, 16));
        homeButton.setForeground(TEXT_PRIMARY);
        homeButton.setBackground(BACKGROUND_DARK);
        homeButton.setBorderPainted(false);
        homeButton.setContentAreaFilled(false);
        homeButton.setFocusPainted(false);
        homeButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        homeButton.setToolTipText("Home");
        homeButton.addActionListener(e -> {
            cardLayout.show(mainPanel, "home");
            setTitle("Web Search");
        });
        
        // Back button
        backButton = new JButton("←");
        backButton.setFont(new Font("Segoe UI", Font.BOLD, 18));
        backButton.setForeground(TEXT_PRIMARY);
        backButton.setBackground(BACKGROUND_DARK);
        backButton.setBorderPainted(false);
        backButton.setContentAreaFilled(false);
        backButton.setFocusPainted(false);
        backButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        backButton.setToolTipText("Back");
        backButton.addActionListener(e -> {
            if (webEngine != null) {
                Platform.runLater(() -> {
                    if (webEngine.getHistory().getCurrentIndex() > 0) {
                        webEngine.getHistory().go(-1);
                    }
                });
            }
        });
        
        // Forward button
        forwardButton = new JButton("→");
        forwardButton.setFont(new Font("Segoe UI", Font.BOLD, 18));
        forwardButton.setForeground(TEXT_PRIMARY);
        forwardButton.setBackground(BACKGROUND_DARK);
        forwardButton.setBorderPainted(false);
        forwardButton.setContentAreaFilled(false);
        forwardButton.setFocusPainted(false);
        forwardButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        forwardButton.setToolTipText("Forward");
        forwardButton.addActionListener(e -> {
            if (webEngine != null) {
                Platform.runLater(() -> {
                    if (webEngine.getHistory().getCurrentIndex() < webEngine.getHistory().getEntries().size() - 1) {
                        webEngine.getHistory().go(1);
                    }
                });
            }
        });
        
        // Refresh button
        JButton refreshButton = new JButton("↻");
        refreshButton.setFont(new Font("Segoe UI", Font.BOLD, 16));
        refreshButton.setForeground(TEXT_PRIMARY);
        refreshButton.setBackground(BACKGROUND_DARK);
        refreshButton.setBorderPainted(false);
        refreshButton.setContentAreaFilled(false);
        refreshButton.setFocusPainted(false);
        refreshButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        refreshButton.setToolTipText("Refresh");
        refreshButton.addActionListener(e -> {
            if (webEngine != null) {
                Platform.runLater(() -> webEngine.reload());
            }
        });
        
        navPanel.add(homeButton);
        navPanel.add(backButton);
        navPanel.add(forwardButton);
        navPanel.add(refreshButton);
        
        // URL bar (also acts as search bar)
        browserUrlField = new JTextField();
        browserUrlField.setFont(new Font("Segoe UI", Font.PLAIN, 13));
        browserUrlField.setBackground(new Color(41, 42, 45));
        browserUrlField.setForeground(TEXT_PRIMARY);
        browserUrlField.setCaretColor(TEXT_PRIMARY);
        browserUrlField.setBorder(BorderFactory.createCompoundBorder(
            new LineBorder(SEARCH_BAR_BORDER, 1, true),
            new EmptyBorder(8, 12, 8, 12)
        ));
        browserUrlField.addActionListener(e -> {
            String input = browserUrlField.getText().trim();
            if (!input.isEmpty()) {
                // Check if it's a URL or a search query
                if (input.startsWith("http://") || input.startsWith("https://") || 
                    input.contains(".") && !input.contains(" ")) {
                    // It's a URL
                    if (!input.startsWith("http")) {
                        input = "https://" + input;
                    }
                    loadUrl(input);
                } else {
                    // It's a search query - use Google
                    performSearchInBrowser(input, 0);
                }
            }
        });
        
        // Right side buttons
        JPanel rightPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 10, 0));
        rightPanel.setBackground(BACKGROUND_DARK);
        
        // Open in Firefox button
        JButton externalButton = new JButton("Open in Firefox");
        externalButton.setFont(new Font("Segoe UI", Font.PLAIN, 12));
        externalButton.setForeground(ACCENT_COLOR);
        externalButton.setBackground(BACKGROUND_DARK);
        externalButton.setBorderPainted(false);
        externalButton.setContentAreaFilled(false);
        externalButton.setFocusPainted(false);
        externalButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        externalButton.addActionListener(e -> {
            String currentUrl = browserUrlField.getText();
            if (!currentUrl.isEmpty()) {
                openInExternalBrowser(currentUrl);
            }
        });
        
        rightPanel.add(externalButton);
        
        toolbar.add(navPanel, BorderLayout.WEST);
        toolbar.add(browserUrlField, BorderLayout.CENTER);
        toolbar.add(rightPanel, BorderLayout.EAST);
        
        // Browser panel (JavaFX WebView)
        browserPanel = new JFXPanel();
        browserPanel.setBackground(Color.WHITE);
        
        browserPage.add(toolbar, BorderLayout.NORTH);
        browserPage.add(browserPanel, BorderLayout.CENTER);
        
        return browserPage;
    }
    
    private void performSearch(String query) {
        if (query == null || query.trim().isEmpty()) {
            return;
        }
        
        int selectedEngine = homeEngineSelector.getSelectedIndex();
        performSearchInBrowser(query.trim(), selectedEngine);
    }
    
    private void performSearchInBrowser(String query, int engineIndex) {
        try {
            String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8.toString());
            String searchUrl = SEARCH_ENGINE_URLS[engineIndex] + encodedQuery;
            openInBrowser(searchUrl);
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this, "Error encoding search query: " + e.getMessage(),
                "Search Error", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    private void openInBrowser(String url) {
        System.out.println("Opening URL: " + url);
        
        // Update URL field
        browserUrlField.setText(url);
        
        // Load URL in embedded WebView
        loadUrl(url);
        
        // Switch to browser view
        cardLayout.show(mainPanel, "browser");
    }
    
    private void loadUrl(String url) {
        Platform.runLater(() -> {
            if (webEngine != null) {
                webEngine.load(url);
            }
        });
    }
    
    private void openInExternalBrowser(String url) {
        System.out.println("Opening URL in external browser: " + url);
        
        // Try Firefox snap directly
        try {
            ProcessBuilder pb = new ProcessBuilder("/snap/bin/firefox", "--new-tab", url);
            pb.inheritIO();
            pb.start();
            return;
        } catch (Exception e) {
            System.err.println("Firefox snap failed: " + e.getMessage());
        }
        
        // Try Desktop API as fallback
        if (Desktop.isDesktopSupported()) {
            Desktop desktop = Desktop.getDesktop();
            if (desktop.isSupported(Desktop.Action.BROWSE)) {
                try {
                    desktop.browse(new java.net.URI(url));
                    return;
                } catch (Exception e) {
                    System.err.println("Desktop.browse() failed: " + e.getMessage());
                }
            }
        }
        
        // Show URL for manual copy
        JTextArea urlArea = new JTextArea(url);
        urlArea.setEditable(false);
        urlArea.setWrapStyleWord(true);
        urlArea.setLineWrap(true);
        urlArea.setColumns(50);
        JOptionPane.showMessageDialog(this,
            new Object[]{"Could not open browser. Copy this URL:", urlArea},
            "Open URL", JOptionPane.INFORMATION_MESSAGE);
    }
    
    public static void main(String[] args) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            // Use default
        }
        
        SwingUtilities.invokeLater(() -> {
            WebSearchGUI gui = new WebSearchGUI();
            gui.setVisible(true);
        });
    }
}
