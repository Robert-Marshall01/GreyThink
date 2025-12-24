package searchengine;

import java.awt.*;
import java.awt.event.*;
import java.util.List;
import javax.swing.*;
import javax.swing.border.*;

/**
 * Google-style GUI for the search engine.
 */
public class SearchEngineGUI extends JFrame {
    
    private static final Color BACKGROUND_COLOR = new Color(32, 33, 36);
    private static final Color SEARCH_BAR_COLOR = new Color(32, 33, 36);
    private static final Color SEARCH_BAR_BORDER = new Color(95, 99, 104);
    private static final Color SEARCH_BAR_HOVER = new Color(60, 64, 67);
    private static final Color TEXT_COLOR = new Color(232, 234, 237);
    private static final Color LINK_COLOR = new Color(138, 180, 248);
    private static final Color URL_COLOR = new Color(189, 193, 198);
    private static final Color SNIPPET_COLOR = new Color(189, 193, 198);
    private static final Color BUTTON_COLOR = new Color(48, 49, 52);
    private static final Color BUTTON_HOVER = new Color(60, 64, 67);
    
    private final SearchEngine engine;
    private JTextField searchField;
    private JPanel resultsPanel;
    private JPanel homePanel;
    private JPanel searchHeaderPanel;
    private JScrollPane resultsScrollPane;
    private CardLayout cardLayout;
    private JPanel mainContainer;
    private boolean isHomePage = true;

    public SearchEngineGUI(SearchEngine engine) {
        this.engine = engine;
        initializeUI();
    }

    private void initializeUI() {
        setTitle("Java Search");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(1200, 800);
        setLocationRelativeTo(null);
        getContentPane().setBackground(BACKGROUND_COLOR);

        cardLayout = new CardLayout();
        mainContainer = new JPanel(cardLayout);
        mainContainer.setBackground(BACKGROUND_COLOR);

        // Create home page (Google-style centered search)
        homePanel = createHomePage();
        
        // Create search results page
        JPanel searchResultsPage = createSearchResultsPage();

        mainContainer.add(homePanel, "home");
        mainContainer.add(searchResultsPage, "results");

        add(mainContainer);
        
        // Show home page initially
        cardLayout.show(mainContainer, "home");
    }

    private JPanel createHomePage() {
        JPanel panel = new JPanel(new GridBagLayout());
        panel.setBackground(BACKGROUND_COLOR);
        
        JPanel centerPanel = new JPanel();
        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.Y_AXIS));
        centerPanel.setBackground(BACKGROUND_COLOR);
        
        // Logo
        JLabel logoLabel = new JLabel("Java Search");
        logoLabel.setFont(new Font("Product Sans", Font.BOLD, 72));
        logoLabel.setForeground(TEXT_COLOR);
        logoLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        // Colorful logo effect
        JPanel logoPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 0));
        logoPanel.setBackground(BACKGROUND_COLOR);
        String[] letters = {"J", "a", "v", "a", " ", "S", "e", "a", "r", "c", "h"};
        Color[] colors = {
            new Color(156, 39, 176),   // Purple
            new Color(0, 188, 212),    // Teal
            new Color(255, 152, 0),    // Orange
            new Color(233, 30, 99),    // Pink
            TEXT_COLOR,                 // Space
            new Color(0, 229, 255),    // Cyan
            new Color(156, 39, 176),   // Purple
            new Color(0, 188, 212),    // Teal
            new Color(255, 152, 0),    // Orange
            new Color(233, 30, 99),    // Pink
            new Color(0, 229, 255)     // Cyan
        };
        
        for (int i = 0; i < letters.length; i++) {
            JLabel letter = new JLabel(letters[i]);
            letter.setFont(new Font("SansSerif", Font.BOLD, 72));
            letter.setForeground(colors[i]);
            logoPanel.add(letter);
        }
        
        // Search bar for home page
        JPanel searchBarPanel = createSearchBar(true);
        searchBarPanel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        // Buttons
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 12, 0));
        buttonPanel.setBackground(BACKGROUND_COLOR);
        
        JButton searchButton = createStyledButton("Java Search");
        JButton luckyButton = createStyledButton("Surprise Me");
        
        searchButton.addActionListener(e -> performSearch());
        luckyButton.addActionListener(e -> {
            if (!searchField.getText().trim().isEmpty()) {
                performSearch();
            }
        });
        
        buttonPanel.add(searchButton);
        buttonPanel.add(luckyButton);
        
        // Stats label
        String vocabSize = engine.getStatistics().split("Vocabulary size: ")[1].split(" ")[0];
        JLabel statsLabel = new JLabel(String.format("Searching across %d documents with %s indexed terms", 
                engine.getDocumentCount(), vocabSize));
        statsLabel.setFont(new Font("SansSerif", Font.PLAIN, 13));
        statsLabel.setForeground(URL_COLOR);
        statsLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
        
        centerPanel.add(logoPanel);
        centerPanel.add(Box.createVerticalStrut(30));
        centerPanel.add(searchBarPanel);
        centerPanel.add(Box.createVerticalStrut(25));
        centerPanel.add(buttonPanel);
        centerPanel.add(Box.createVerticalStrut(20));
        centerPanel.add(statsLabel);
        
        panel.add(centerPanel);
        return panel;
    }

    private JPanel createSearchResultsPage() {
        JPanel panel = new JPanel(new BorderLayout());
        panel.setBackground(BACKGROUND_COLOR);
        
        // Header with search bar
        searchHeaderPanel = createSearchHeader();
        panel.add(searchHeaderPanel, BorderLayout.NORTH);
        
        // Results area
        resultsPanel = new JPanel();
        resultsPanel.setLayout(new BoxLayout(resultsPanel, BoxLayout.Y_AXIS));
        resultsPanel.setBackground(BACKGROUND_COLOR);
        resultsPanel.setBorder(BorderFactory.createEmptyBorder(20, 180, 20, 20));
        
        resultsScrollPane = new JScrollPane(resultsPanel);
        resultsScrollPane.setBackground(BACKGROUND_COLOR);
        resultsScrollPane.getViewport().setBackground(BACKGROUND_COLOR);
        resultsScrollPane.setBorder(null);
        resultsScrollPane.getVerticalScrollBar().setUnitIncrement(16);
        
        panel.add(resultsScrollPane, BorderLayout.CENTER);
        
        return panel;
    }

    private JPanel createSearchHeader() {
        JPanel header = new JPanel();
        header.setLayout(new BoxLayout(header, BoxLayout.X_AXIS));
        header.setBackground(BACKGROUND_COLOR);
        header.setBorder(BorderFactory.createCompoundBorder(
            BorderFactory.createMatteBorder(0, 0, 1, 0, new Color(60, 64, 67)),
            BorderFactory.createEmptyBorder(15, 20, 15, 20)
        ));
        
        // Mini logo
        JLabel miniLogo = new JLabel("Java");
        miniLogo.setFont(new Font("SansSerif", Font.BOLD, 24));
        miniLogo.setForeground(new Color(156, 39, 176)); // Purple
        miniLogo.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        miniLogo.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                showHomePage();
            }
        });
        
        // Search bar
        JPanel searchBarPanel = createSearchBar(false);
        
        header.add(miniLogo);
        header.add(Box.createHorizontalStrut(30));
        header.add(searchBarPanel);
        header.add(Box.createHorizontalGlue());
        
        return header;
    }

    private JPanel createSearchBar(boolean isHomePage) {
        JPanel searchBarPanel = new JPanel(new BorderLayout());
        searchBarPanel.setBackground(SEARCH_BAR_COLOR);
        searchBarPanel.setMaximumSize(new Dimension(600, 46));
        searchBarPanel.setPreferredSize(new Dimension(isHomePage ? 580 : 500, 46));
        
        Border roundedBorder = new RoundedBorder(23, SEARCH_BAR_BORDER);
        searchBarPanel.setBorder(roundedBorder);
        
        // Search icon
        JLabel searchIcon = new JLabel("🔍");
        searchIcon.setFont(new Font("Segoe UI Emoji", Font.PLAIN, 16));
        searchIcon.setBorder(BorderFactory.createEmptyBorder(0, 15, 0, 10));
        searchIcon.setForeground(URL_COLOR);
        
        // Text field
        if (isHomePage) {
            searchField = new JTextField();
        }
        JTextField field = isHomePage ? searchField : new JTextField(searchField.getText());
        if (!isHomePage) {
            // Keep them in sync
            field.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
                public void insertUpdate(javax.swing.event.DocumentEvent e) { searchField.setText(field.getText()); }
                public void removeUpdate(javax.swing.event.DocumentEvent e) { searchField.setText(field.getText()); }
                public void changedUpdate(javax.swing.event.DocumentEvent e) { searchField.setText(field.getText()); }
            });
        }
        
        field.setFont(new Font("SansSerif", Font.PLAIN, 16));
        field.setBackground(SEARCH_BAR_COLOR);
        field.setForeground(TEXT_COLOR);
        field.setCaretColor(TEXT_COLOR);
        field.setBorder(null);
        
        field.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    performSearch();
                }
            }
        });
        
        // Clear button
        JLabel clearButton = new JLabel("✕");
        clearButton.setFont(new Font("SansSerif", Font.PLAIN, 16));
        clearButton.setForeground(URL_COLOR);
        clearButton.setBorder(BorderFactory.createEmptyBorder(0, 10, 0, 15));
        clearButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        clearButton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                field.setText("");
                searchField.setText("");
                if (!SearchEngineGUI.this.isHomePage) {
                    showHomePage();
                }
            }
        });
        
        // Hover effect
        searchBarPanel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                searchBarPanel.setBackground(SEARCH_BAR_HOVER);
                field.setBackground(SEARCH_BAR_HOVER);
            }
            @Override
            public void mouseExited(MouseEvent e) {
                searchBarPanel.setBackground(SEARCH_BAR_COLOR);
                field.setBackground(SEARCH_BAR_COLOR);
            }
        });
        
        searchBarPanel.add(searchIcon, BorderLayout.WEST);
        searchBarPanel.add(field, BorderLayout.CENTER);
        searchBarPanel.add(clearButton, BorderLayout.EAST);
        
        return searchBarPanel;
    }

    private JButton createStyledButton(String text) {
        JButton button = new JButton(text);
        button.setFont(new Font("SansSerif", Font.PLAIN, 14));
        button.setForeground(TEXT_COLOR);
        button.setBackground(BUTTON_COLOR);
        button.setBorder(BorderFactory.createEmptyBorder(10, 20, 10, 20));
        button.setFocusPainted(false);
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        
        button.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                button.setBackground(BUTTON_HOVER);
                button.setBorder(BorderFactory.createCompoundBorder(
                    BorderFactory.createLineBorder(SEARCH_BAR_BORDER, 1),
                    BorderFactory.createEmptyBorder(9, 19, 9, 19)
                ));
            }
            @Override
            public void mouseExited(MouseEvent e) {
                button.setBackground(BUTTON_COLOR);
                button.setBorder(BorderFactory.createEmptyBorder(10, 20, 10, 20));
            }
        });
        
        return button;
    }

    private void performSearch() {
        String query = searchField.getText().trim();
        if (query.isEmpty()) {
            return;
        }
        
        // Switch to results view
        isHomePage = false;
        cardLayout.show(mainContainer, "results");
        
        // Update the search bar in header
        searchHeaderPanel.removeAll();
        searchHeaderPanel.add(createSearchHeader().getComponent(0)); // Mini logo
        searchHeaderPanel.add(Box.createHorizontalStrut(30));
        searchHeaderPanel.add(createSearchBar(false));
        searchHeaderPanel.add(Box.createHorizontalGlue());
        searchHeaderPanel.revalidate();
        searchHeaderPanel.repaint();
        
        // Perform search
        List<SearchResult> results = engine.search(query, 20);
        displayResults(query, results);
    }

    private void displayResults(String query, List<SearchResult> results) {
        resultsPanel.removeAll();
        
        // Results count
        JLabel countLabel = new JLabel(String.format("About %d results (0.%02d seconds)", 
                results.size(), (int)(Math.random() * 50) + 10));
        countLabel.setFont(new Font("SansSerif", Font.PLAIN, 13));
        countLabel.setForeground(URL_COLOR);
        countLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        resultsPanel.add(countLabel);
        resultsPanel.add(Box.createVerticalStrut(20));
        
        if (results.isEmpty()) {
            JPanel noResultsPanel = new JPanel();
            noResultsPanel.setLayout(new BoxLayout(noResultsPanel, BoxLayout.Y_AXIS));
            noResultsPanel.setBackground(BACKGROUND_COLOR);
            noResultsPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
            
            JLabel noResults = new JLabel("No results found for \"" + query + "\"");
            noResults.setFont(new Font("SansSerif", Font.PLAIN, 16));
            noResults.setForeground(TEXT_COLOR);
            
            JLabel suggestion = new JLabel("Try different keywords or check your spelling");
            suggestion.setFont(new Font("SansSerif", Font.PLAIN, 14));
            suggestion.setForeground(URL_COLOR);
            
            noResultsPanel.add(noResults);
            noResultsPanel.add(Box.createVerticalStrut(10));
            noResultsPanel.add(suggestion);
            
            resultsPanel.add(noResultsPanel);
        } else {
            for (SearchResult result : results) {
                JPanel resultCard = createResultCard(result);
                resultsPanel.add(resultCard);
                resultsPanel.add(Box.createVerticalStrut(25));
            }
        }
        
        resultsPanel.revalidate();
        resultsPanel.repaint();
        
        // Scroll to top
        SwingUtilities.invokeLater(() -> resultsScrollPane.getVerticalScrollBar().setValue(0));
    }

    private JPanel createResultCard(SearchResult result) {
        JPanel card = new JPanel();
        card.setLayout(new BoxLayout(card, BoxLayout.Y_AXIS));
        card.setBackground(BACKGROUND_COLOR);
        card.setAlignmentX(Component.LEFT_ALIGNMENT);
        card.setMaximumSize(new Dimension(700, 120));
        
        Document doc = result.getDocument();
        
        // URL/Source line
        JLabel urlLabel = new JLabel("docs.javasearch.com › document-" + doc.getId());
        urlLabel.setFont(new Font("SansSerif", Font.PLAIN, 13));
        urlLabel.setForeground(URL_COLOR);
        urlLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        // Title (clickable link style)
        JLabel titleLabel = new JLabel("<html><u>" + doc.getTitle() + "</u></html>");
        titleLabel.setFont(new Font("SansSerif", Font.PLAIN, 20));
        titleLabel.setForeground(LINK_COLOR);
        titleLabel.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        titleLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        titleLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                showDocumentDialog(doc);
            }
            @Override
            public void mouseEntered(MouseEvent e) {
                titleLabel.setForeground(new Color(174, 203, 250));
            }
            @Override
            public void mouseExited(MouseEvent e) {
                titleLabel.setForeground(LINK_COLOR);
            }
        });
        
        // Snippet with highlighted query terms
        String snippet = createSnippet(doc.getContent(), searchField.getText(), 200);
        JLabel snippetLabel = new JLabel("<html><body style='width: 600px'>" + snippet + "</body></html>");
        snippetLabel.setFont(new Font("SansSerif", Font.PLAIN, 14));
        snippetLabel.setForeground(SNIPPET_COLOR);
        snippetLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        // Relevance score (subtle)
        JLabel scoreLabel = new JLabel(String.format("Relevance: %.1f%% • %d term matches", 
                result.getScore() * 100, result.getMatchCount()));
        scoreLabel.setFont(new Font("SansSerif", Font.PLAIN, 11));
        scoreLabel.setForeground(new Color(128, 134, 139));
        scoreLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        card.add(urlLabel);
        card.add(Box.createVerticalStrut(3));
        card.add(titleLabel);
        card.add(Box.createVerticalStrut(5));
        card.add(snippetLabel);
        card.add(Box.createVerticalStrut(5));
        card.add(scoreLabel);
        
        return card;
    }

    private String createSnippet(String content, String query, int maxLength) {
        String snippet = content;
        if (snippet.length() > maxLength) {
            // Try to find query terms and show context around them
            String lowerContent = content.toLowerCase();
            String[] queryTerms = query.toLowerCase().split("\\s+");
            
            int bestPos = 0;
            for (String term : queryTerms) {
                int pos = lowerContent.indexOf(term);
                if (pos != -1) {
                    bestPos = Math.max(0, pos - 50);
                    break;
                }
            }
            
            int end = Math.min(content.length(), bestPos + maxLength);
            snippet = (bestPos > 0 ? "..." : "") + content.substring(bestPos, end);
            if (end < content.length()) {
                snippet += "...";
            }
        }
        
        // Highlight query terms
        String[] queryTerms = query.toLowerCase().split("\\s+");
        for (String term : queryTerms) {
            if (term.length() >= 2) {
                snippet = snippet.replaceAll("(?i)(" + term + ")", 
                        "<b style='color: #e8eaed'>$1</b>");
            }
        }
        
        return snippet;
    }

    private void showDocumentDialog(Document doc) {
        JDialog dialog = new JDialog(this, doc.getTitle(), true);
        dialog.setSize(700, 500);
        dialog.setLocationRelativeTo(this);
        dialog.getContentPane().setBackground(BACKGROUND_COLOR);
        
        JPanel contentPanel = new JPanel(new BorderLayout(20, 20));
        contentPanel.setBackground(BACKGROUND_COLOR);
        contentPanel.setBorder(BorderFactory.createEmptyBorder(25, 30, 25, 30));
        
        // Title
        JLabel titleLabel = new JLabel(doc.getTitle());
        titleLabel.setFont(new Font("SansSerif", Font.BOLD, 24));
        titleLabel.setForeground(TEXT_COLOR);
        
        // Content
        JTextArea contentArea = new JTextArea(doc.getContent());
        contentArea.setFont(new Font("SansSerif", Font.PLAIN, 15));
        contentArea.setBackground(new Color(48, 49, 52));
        contentArea.setForeground(TEXT_COLOR);
        contentArea.setLineWrap(true);
        contentArea.setWrapStyleWord(true);
        contentArea.setEditable(false);
        contentArea.setBorder(BorderFactory.createEmptyBorder(15, 15, 15, 15));
        
        JScrollPane scrollPane = new JScrollPane(contentArea);
        scrollPane.setBorder(BorderFactory.createLineBorder(SEARCH_BAR_BORDER));
        scrollPane.getViewport().setBackground(new Color(48, 49, 52));
        
        // Close button
        JButton closeButton = createStyledButton("Close");
        closeButton.addActionListener(e -> dialog.dispose());
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        buttonPanel.setBackground(BACKGROUND_COLOR);
        buttonPanel.add(closeButton);
        
        contentPanel.add(titleLabel, BorderLayout.NORTH);
        contentPanel.add(scrollPane, BorderLayout.CENTER);
        contentPanel.add(buttonPanel, BorderLayout.SOUTH);
        
        dialog.add(contentPanel);
        dialog.setVisible(true);
    }

    private void showHomePage() {
        isHomePage = true;
        searchField.setText("");
        cardLayout.show(mainContainer, "home");
    }

    /**
     * Custom rounded border for search bar
     */
    private static class RoundedBorder implements Border {
        private final int radius;
        private final Color color;
        
        RoundedBorder(int radius, Color color) {
            this.radius = radius;
            this.color = color;
        }
        
        @Override
        public Insets getBorderInsets(Component c) {
            return new Insets(radius/2, radius, radius/2, radius);
        }
        
        @Override
        public boolean isBorderOpaque() {
            return false;
        }
        
        @Override
        public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setColor(color);
            g2.drawRoundRect(x, y, width - 1, height - 1, radius, radius);
            g2.dispose();
        }
    }

    public static void main(String[] args) {
        // Set look and feel
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) {
            // Use default
        }
        
        // Create and populate search engine
        SearchEngine engine = new SearchEngine();
        loadSampleDocuments(engine);
        
        // Launch GUI
        SwingUtilities.invokeLater(() -> {
            SearchEngineGUI gui = new SearchEngineGUI(engine);
            gui.setVisible(true);
        });
    }

    private static void loadSampleDocuments(SearchEngine engine) {
        engine.indexDocument(new Document(1, 
            "Introduction to Java Programming",
            "Java is a popular programming language created in 1995. It is used for mobile applications, " +
            "web development, desktop applications, and much more. Java is known for its write once, " +
            "run anywhere capability, making it platform independent. Java applications are compiled to " +
            "bytecode that runs on the Java Virtual Machine (JVM), which provides memory management and security."));

        engine.indexDocument(new Document(2,
            "Python vs Java: A Comprehensive Comparison",
            "Both Python and Java are popular programming languages with different strengths. Python is known " +
            "for its simplicity and readability with minimal syntax, while Java offers strong typing and better " +
            "performance for large applications. Python is often used for data science, machine learning, and " +
            "scripting, whereas Java dominates enterprise applications, Android development, and backend systems."));

        engine.indexDocument(new Document(3,
            "Building Modern Web Applications",
            "Web applications can be built using various technologies. Frontend development uses HTML for structure, " +
            "CSS for styling, and JavaScript for interactivity. Backend development can use Java Spring Boot, " +
            "Python Django, Node.js Express, or Ruby on Rails. Modern web apps use REST APIs and GraphQL for " +
            "communication between frontend and backend, with frameworks like React, Vue, and Angular."));

        engine.indexDocument(new Document(4,
            "Database Management Systems Guide",
            "Databases store and manage data efficiently for applications. SQL databases like MySQL, PostgreSQL, " +
            "and Oracle use structured query language for data manipulation. NoSQL databases like MongoDB, Redis, " +
            "and Cassandra store data in flexible formats like documents or key-value pairs. Choosing the right " +
            "database depends on your data structure, scalability needs, and consistency requirements."));

        engine.indexDocument(new Document(5,
            "Machine Learning Fundamentals",
            "Machine learning is a subset of artificial intelligence that enables computers to learn from data " +
            "without explicit programming. Common algorithms include linear regression for predictions, decision " +
            "trees for classification, and neural networks for complex pattern recognition. Python with libraries " +
            "like TensorFlow, PyTorch, and scikit-learn is widely used for ML development."));

        engine.indexDocument(new Document(6,
            "Cloud Computing and Services Overview",
            "Cloud computing provides on-demand computing resources over the internet. Major providers include " +
            "Amazon Web Services (AWS), Google Cloud Platform (GCP), and Microsoft Azure. Benefits include " +
            "scalability, cost efficiency, and reduced infrastructure management. Services include compute (EC2, " +
            "Cloud Functions), storage (S3, Cloud Storage), and databases (RDS, Cloud SQL)."));

        engine.indexDocument(new Document(7,
            "Software Development Best Practices",
            "Good software development practices lead to maintainable, high-quality code. Key practices include " +
            "writing clean, readable code with meaningful names, using version control with Git, writing unit " +
            "tests for reliability, and following design patterns. Code reviews and continuous integration help " +
            "maintain quality. Documentation is essential for team collaboration and maintainability."));

        engine.indexDocument(new Document(8,
            "Mobile App Development Guide",
            "Mobile apps can be developed natively or using cross-platform frameworks. Android apps are typically " +
            "written in Java or Kotlin using Android Studio. iOS apps use Swift or Objective-C with Xcode. " +
            "Cross-platform frameworks like React Native, Flutter, and Xamarin allow code reuse across platforms, " +
            "reducing development time while maintaining near-native performance."));

        engine.indexDocument(new Document(9,
            "Cybersecurity Essentials for Developers",
            "Cybersecurity protects systems and data from unauthorized access and threats. Important practices " +
            "include encryption for data protection, secure authentication with multi-factor auth, input validation, " +
            "and regular security updates. Common vulnerabilities include SQL injection, cross-site scripting (XSS), " +
            "and cross-site request forgery (CSRF). Security should be integrated throughout development."));

        engine.indexDocument(new Document(10,
            "Data Structures and Algorithms",
            "Understanding data structures is crucial for efficient programming. Common structures include arrays " +
            "for sequential data, linked lists for dynamic collections, trees for hierarchical data, hash tables " +
            "for fast lookups, and graphs for connected data. Algorithm analysis using Big O notation helps evaluate " +
            "time and space complexity. Sorting and searching algorithms are fundamental concepts."));
        
        engine.indexDocument(new Document(11,
            "RESTful API Design Principles",
            "REST (Representational State Transfer) is an architectural style for designing web APIs. Key principles " +
            "include using HTTP methods (GET, POST, PUT, DELETE) appropriately, stateless communication, resource-based " +
            "URLs, and proper status codes. Good API design includes versioning, pagination, filtering, and comprehensive " +
            "documentation. JSON is the most common data format for REST APIs."));
        
        engine.indexDocument(new Document(12,
            "Docker and Container Technology",
            "Docker is a platform for developing, shipping, and running applications in containers. Containers package " +
            "applications with their dependencies, ensuring consistency across environments. Key concepts include images, " +
            "containers, Dockerfile, and Docker Compose. Kubernetes orchestrates containers at scale, managing deployment, " +
            "scaling, and networking for containerized applications."));
        
        engine.indexDocument(new Document(13,
            "AI Models and Large Language Models",
            "AI models are the core of artificial intelligence systems. The newest AI models include large language models " +
            "(LLMs) like GPT-4, Claude, Gemini, and LLaMA. These models are trained on vast datasets and can perform tasks " +
            "like text generation, code completion, and reasoning. Transformer architecture revolutionized AI model design, " +
            "enabling attention mechanisms for better context understanding."));
        
        engine.indexDocument(new Document(14,
            "Deep Learning and Neural Networks",
            "Deep learning uses multi-layered neural networks to model complex patterns. Convolutional Neural Networks (CNNs) " +
            "excel at image recognition, while Recurrent Neural Networks (RNNs) and LSTMs handle sequential data. Modern AI " +
            "models combine these architectures with attention mechanisms. Training requires GPUs and large datasets."));
        
        engine.indexDocument(new Document(15,
            "Natural Language Processing (NLP)",
            "NLP enables computers to understand and generate human language. Key tasks include sentiment analysis, named " +
            "entity recognition, machine translation, and text summarization. Modern NLP uses transformer-based models like " +
            "BERT and GPT. Word embeddings represent words as vectors, capturing semantic relationships."));
        
        engine.indexDocument(new Document(16,
            "Computer Vision and Image Recognition",
            "Computer vision enables machines to interpret visual information from images and videos. Applications include " +
            "object detection, facial recognition, autonomous vehicles, and medical imaging. Deep learning models like " +
            "ResNet, YOLO, and Vision Transformers achieve human-level accuracy on many vision tasks."));
        
        engine.indexDocument(new Document(17,
            "Generative AI and Creative Applications",
            "Generative AI creates new content including text, images, music, and code. Models like DALL-E, Midjourney, and " +
            "Stable Diffusion generate images from text prompts. AI assistants like ChatGPT and Claude help with writing, " +
            "coding, and analysis. The newest generative models can produce increasingly realistic and creative outputs."));
        
        engine.indexDocument(new Document(18,
            "JavaScript and Frontend Frameworks",
            "JavaScript is the language of the web, enabling dynamic and interactive websites. Popular frameworks include " +
            "React for component-based UIs, Vue.js for progressive apps, and Angular for enterprise applications. Node.js " +
            "extends JavaScript to server-side development. TypeScript adds static typing for better code quality."));
        
        engine.indexDocument(new Document(19,
            "Microservices Architecture",
            "Microservices break applications into small, independent services that communicate via APIs. Benefits include " +
            "scalability, flexibility, and easier maintenance. Each service can use different technologies and be deployed " +
            "independently. Service mesh tools like Istio manage communication between services."));
        
        engine.indexDocument(new Document(20,
            "DevOps and CI/CD Pipelines",
            "DevOps combines development and operations for faster, reliable software delivery. CI/CD pipelines automate " +
            "building, testing, and deploying code. Tools include Jenkins, GitHub Actions, GitLab CI, and CircleCI. " +
            "Infrastructure as Code (IaC) with Terraform or Ansible ensures consistent environments."));
    }
}
