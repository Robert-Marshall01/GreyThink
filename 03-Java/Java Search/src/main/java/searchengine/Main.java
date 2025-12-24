package searchengine;

import java.util.List;
import java.util.Scanner;

/**
 * Demo application showcasing the search engine capabilities.
 */
public class Main {
    
    public static void main(String[] args) {
        SearchEngine engine = new SearchEngine();
        
        // Index sample documents
        System.out.println("=== Java Search Engine ===\n");
        System.out.println("Indexing sample documents...\n");
        
        indexSampleDocuments(engine);
        
        System.out.println(engine.getStatistics());
        System.out.println();
        
        // Demo searches
        performDemoSearches(engine);
        
        // Interactive mode
        interactiveSearch(engine);
    }

    private static void indexSampleDocuments(SearchEngine engine) {
        engine.indexDocument(new Document(1, 
            "Introduction to Java Programming",
            "Java is a popular programming language created in 1995. It is used for mobile applications, " +
            "web development, desktop applications, and much more. Java is known for its write once, " +
            "run anywhere capability, making it platform independent."));

        engine.indexDocument(new Document(2,
            "Python vs Java Comparison",
            "Both Python and Java are popular programming languages. Python is known for its simplicity " +
            "and readability, while Java offers strong typing and better performance. Python is often " +
            "used for data science and machine learning, whereas Java dominates enterprise applications."));

        engine.indexDocument(new Document(3,
            "Building Web Applications",
            "Web applications can be built using various technologies. Frontend development uses HTML, " +
            "CSS, and JavaScript. Backend development can use Java Spring Boot, Python Django, or Node.js. " +
            "Modern web apps often use REST APIs for communication between frontend and backend."));

        engine.indexDocument(new Document(4,
            "Database Management Systems",
            "Databases store and manage data efficiently. SQL databases like MySQL and PostgreSQL use " +
            "structured query language. NoSQL databases like MongoDB store data in flexible formats. " +
            "Choosing the right database depends on your application requirements and data structure."));

        engine.indexDocument(new Document(5,
            "Machine Learning Fundamentals",
            "Machine learning is a subset of artificial intelligence. It enables computers to learn from " +
            "data without explicit programming. Common algorithms include linear regression, decision trees, " +
            "and neural networks. Python with libraries like TensorFlow and PyTorch is widely used."));

        engine.indexDocument(new Document(6,
            "Cloud Computing Overview",
            "Cloud computing provides on-demand computing resources over the internet. Major providers " +
            "include AWS, Google Cloud, and Microsoft Azure. Benefits include scalability, cost efficiency, " +
            "and reduced infrastructure management. Java and Python applications can be easily deployed to the cloud."));

        engine.indexDocument(new Document(7,
            "Software Development Best Practices",
            "Good software development practices include writing clean code, using version control with Git, " +
            "writing unit tests, and following design patterns. Code reviews and continuous integration help " +
            "maintain quality. Documentation is essential for maintainability."));

        engine.indexDocument(new Document(8,
            "Mobile App Development",
            "Mobile apps can be developed natively or cross-platform. Android apps are typically written " +
            "in Java or Kotlin. iOS apps use Swift or Objective-C. Cross-platform frameworks like React " +
            "Native and Flutter allow code reuse across platforms."));

        engine.indexDocument(new Document(9,
            "Cybersecurity Essentials",
            "Cybersecurity protects systems and data from threats. Important practices include encryption, " +
            "secure authentication, and regular security updates. Common vulnerabilities include SQL injection " +
            "and cross-site scripting. Security should be considered throughout the development lifecycle."));

        engine.indexDocument(new Document(10,
            "Data Structures and Algorithms",
            "Understanding data structures is crucial for efficient programming. Common structures include " +
            "arrays, linked lists, trees, and graphs. Algorithm analysis helps evaluate time and space " +
            "complexity. Sorting and searching algorithms are fundamental concepts every programmer should know."));
    }

    private static void performDemoSearches(SearchEngine engine) {
        System.out.println("=== Demo Searches ===\n");
        
        // Basic search
        System.out.println("Search: 'Java programming'");
        List<SearchResult> results = engine.search("Java programming");
        displayResults(results);
        
        System.out.println("\nSearch: 'machine learning Python'");
        results = engine.search("machine learning Python");
        displayResults(results);
        
        System.out.println("\nSearch: 'database SQL'");
        results = engine.search("database SQL");
        displayResults(results);
        
        // Phrase search
        System.out.println("\n--- Phrase Search ---");
        System.out.println("\nPhrase: 'web applications'");
        results = engine.phraseSearch("web applications", 5);
        displayResults(results);
        
        // Suggestions
        System.out.println("\n--- Auto-complete Suggestions ---");
        System.out.println("\nSuggestions for 'prog':");
        List<String> suggestions = engine.getSuggestions("prog", 5);
        suggestions.forEach(s -> System.out.println("  - " + s));
        
        System.out.println("\nSuggestions for 'data':");
        suggestions = engine.getSuggestions("data", 5);
        suggestions.forEach(s -> System.out.println("  - " + s));
    }

    private static void displayResults(List<SearchResult> results) {
        if (results.isEmpty()) {
            System.out.println("  No results found.");
            return;
        }
        
        for (int i = 0; i < results.size(); i++) {
            SearchResult result = results.get(i);
            System.out.printf("  %d. %s (score: %.4f, matches: %d)%n",
                i + 1,
                result.getDocument().getTitle(),
                result.getScore(),
                result.getMatchCount());
        }
    }

    private static void interactiveSearch(SearchEngine engine) {
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("\n=== Interactive Search ===");
        System.out.println("Commands:");
        System.out.println("  search <query>  - Search for documents");
        System.out.println("  phrase <text>   - Exact phrase search");
        System.out.println("  suggest <prefix> - Get auto-complete suggestions");
        System.out.println("  stats           - Show statistics");
        System.out.println("  quit            - Exit the program");
        System.out.println();
        
        while (true) {
            System.out.print("> ");
            String input = scanner.nextLine().trim();
            
            if (input.isEmpty()) {
                continue;
            }
            
            if (input.equalsIgnoreCase("quit") || input.equalsIgnoreCase("exit")) {
                System.out.println("Goodbye!");
                break;
            }
            
            if (input.equalsIgnoreCase("stats")) {
                System.out.println(engine.getStatistics());
                continue;
            }
            
            if (input.toLowerCase().startsWith("search ")) {
                String query = input.substring(7).trim();
                if (!query.isEmpty()) {
                    List<SearchResult> results = engine.search(query);
                    displayResults(results);
                }
                continue;
            }
            
            if (input.toLowerCase().startsWith("phrase ")) {
                String phrase = input.substring(7).trim();
                if (!phrase.isEmpty()) {
                    List<SearchResult> results = engine.phraseSearch(phrase, 10);
                    displayResults(results);
                }
                continue;
            }
            
            if (input.toLowerCase().startsWith("suggest ")) {
                String prefix = input.substring(8).trim();
                if (!prefix.isEmpty()) {
                    List<String> suggestions = engine.getSuggestions(prefix, 10);
                    if (suggestions.isEmpty()) {
                        System.out.println("  No suggestions found.");
                    } else {
                        suggestions.forEach(s -> System.out.println("  - " + s));
                    }
                }
                continue;
            }
            
            // Default: treat as search query
            List<SearchResult> results = engine.search(input);
            displayResults(results);
        }
        
        scanner.close();
    }
}
