package searchengine;

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.*;

/**
 * Web search engine that fetches real results from the internet.
 * Uses Wikipedia's open API as a reliable source.
 */
public class WebSearchEngine {
    
    private static final String USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36";
    private static final int TIMEOUT = 15000; // 15 seconds
    
    /**
     * Represents a web search result.
     */
    public static class WebResult {
        private final String title;
        private final String url;
        private final String snippet;
        
        public WebResult(String title, String url, String snippet) {
            this.title = title;
            this.url = url;
            this.snippet = snippet;
        }
        
        public String getTitle() { return title; }
        public String getUrl() { return url; }
        public String getSnippet() { return snippet; }
        
        @Override
        public String toString() {
            return String.format("WebResult[title='%s', url='%s']", title, url);
        }
    }
    
    /**
     * Search the web using multiple backends.
     * 
     * @param query The search query
     * @param maxResults Maximum number of results to return
     * @return List of web search results
     */
    public List<WebResult> search(String query, int maxResults) {
        List<WebResult> results = new ArrayList<>();
        
        // Try Wikipedia first (most reliable, open API)
        try {
            results = searchWikipedia(query, maxResults);
            if (!results.isEmpty()) {
                return results;
            }
        } catch (Exception e) {
            System.err.println("Wikipedia search failed: " + e.getMessage());
        }
        
        // Try StackExchange as fallback for programming queries
        try {
            results = searchStackExchange(query, maxResults);
            if (!results.isEmpty()) {
                return results;
            }
        } catch (Exception e) {
            System.err.println("StackExchange search failed: " + e.getMessage());
        }
        
        return results;
    }
    
    /**
     * Search using Wikipedia's open API.
     */
    private List<WebResult> searchWikipedia(String query, int maxResults) throws IOException {
        List<WebResult> results = new ArrayList<>();
        
        String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8);
        String searchUrl = "https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=" 
            + encodedQuery + "&srlimit=" + maxResults + "&format=json";
        
        String json = fetchUrl(searchUrl);
        
        // Parse each search result object
        // Match the pattern: "title":"...","pageid":...,"snippet":"..."
        Pattern resultPattern = Pattern.compile(
            "\\{\"ns\":\\d+,\"title\":\"((?:[^\"\\\\]|\\\\.)*)\",\"pageid\":\\d+,\"size\":\\d+,\"wordcount\":\\d+,\"snippet\":\"((?:[^\"\\\\]|\\\\.)*)\"",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = resultPattern.matcher(json);
        
        while (matcher.find() && results.size() < maxResults) {
            String title = unescapeJson(matcher.group(1));
            String snippet = cleanHtml(unescapeJson(matcher.group(2)));
            
            // Build Wikipedia URL from title
            String urlTitle = title.replace(" ", "_");
            String url = "https://en.wikipedia.org/wiki/" + URLEncoder.encode(urlTitle, StandardCharsets.UTF_8);
            
            results.add(new WebResult(title, url, snippet));
        }
        
        return results;
    }
    
    /**
     * Search StackExchange sites for programming-related queries.
     */
    private List<WebResult> searchStackExchange(String query, int maxResults) throws IOException {
        List<WebResult> results = new ArrayList<>();
        
        String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8);
        String searchUrl = "https://api.stackexchange.com/2.3/search?order=desc&sort=relevance&intitle=" 
            + encodedQuery + "&site=stackoverflow&pagesize=" + maxResults + "&filter=!nNPvSNVZJS";
        
        String json = fetchUrlGzip(searchUrl);
        
        // Parse items array
        Pattern itemPattern = Pattern.compile(
            "\"title\"\\s*:\\s*\"([^\"]+)\"[^}]*\"link\"\\s*:\\s*\"([^\"]+)\"",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = itemPattern.matcher(json);
        
        while (matcher.find() && results.size() < maxResults) {
            String title = unescapeJson(matcher.group(1));
            String url = unescapeJson(matcher.group(2));
            
            results.add(new WebResult(title, url, "Q&A from Stack Overflow"));
        }
        
        return results;
    }
    
    /**
     * Unescape JSON string values.
     */
    private String unescapeJson(String json) {
        if (json == null) return "";
        return json.replace("\\\"", "\"")
                   .replace("\\\\", "\\")
                   .replace("\\/", "/")
                   .replace("\\n", " ")
                   .replace("\\r", "")
                   .replace("\\t", " ")
                   .replace("\\u0026", "&")
                   .replace("\\u003c", "<")
                   .replace("\\u003e", ">")
                   .replace("\\u0027", "'");
    }
    
    /**
     * Fetch URL content as string.
     */
    private String fetchUrl(String urlString) throws IOException {
        URL url;
        try {
            url = new URI(urlString).toURL();
        } catch (java.net.URISyntaxException e) {
            throw new IOException("Invalid URL: " + urlString, e);
        }
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        
        conn.setRequestMethod("GET");
        conn.setRequestProperty("User-Agent", USER_AGENT);
        conn.setRequestProperty("Accept", "application/json,text/html,*/*;q=0.8");
        conn.setRequestProperty("Accept-Language", "en-US,en;q=0.5");
        conn.setConnectTimeout(TIMEOUT);
        conn.setReadTimeout(TIMEOUT);
        conn.setInstanceFollowRedirects(true);
        
        int responseCode = conn.getResponseCode();
        
        if (responseCode != 200) {
            throw new IOException("HTTP error: " + responseCode);
        }
        
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(conn.getInputStream(), StandardCharsets.UTF_8))) {
            StringBuilder content = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
            return content.toString();
        }
    }
    
    /**
     * Fetch URL content as string with gzip decompression (for StackExchange API).
     */
    private String fetchUrlGzip(String urlString) throws IOException {
        URL url;
        try {
            url = new URI(urlString).toURL();
        } catch (java.net.URISyntaxException e) {
            throw new IOException("Invalid URL: " + urlString, e);
        }
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        
        conn.setRequestMethod("GET");
        conn.setRequestProperty("User-Agent", USER_AGENT);
        conn.setRequestProperty("Accept", "application/json");
        conn.setRequestProperty("Accept-Encoding", "gzip");
        conn.setConnectTimeout(TIMEOUT);
        conn.setReadTimeout(TIMEOUT);
        conn.setInstanceFollowRedirects(true);
        
        int responseCode = conn.getResponseCode();
        
        if (responseCode != 200) {
            throw new IOException("HTTP error: " + responseCode);
        }
        
        InputStream inputStream = conn.getInputStream();
        String encoding = conn.getContentEncoding();
        if ("gzip".equalsIgnoreCase(encoding)) {
            inputStream = new java.util.zip.GZIPInputStream(inputStream);
        }
        
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
            StringBuilder content = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
            return content.toString();
        }
    }
    
    /**
     * Clean HTML tags and entities from text.
     */
    private String cleanHtml(String html) {
        if (html == null) return "";
        
        // First unescape any escaped tags 
        String text = html.replace("\\u003c", "<")
                          .replace("\\u003e", ">")
                          .replace("\\u003C", "<")
                          .replace("\\u003E", ">");
        
        // Remove HTML tags (including <span> etc.)
        text = text.replaceAll("<[^>]*>", "");
        text = text.replaceAll("<[^>]*$", "");  // Incomplete tags at end
        
        // Decode HTML entities
        text = text.replace("&amp;", "&")
                   .replace("&lt;", "<")
                   .replace("&gt;", ">")
                   .replace("&quot;", "\"")
                   .replace("&#39;", "'")
                   .replace("&apos;", "'")
                   .replace("&nbsp;", " ")
                   .replace("&#x27;", "'")
                   .replace("&#x2F;", "/");
        
        // Clean up whitespace
        text = text.replaceAll("\\s+", " ").trim();
        
        return text;
    }
    
    /**
     * Test the web search.
     */
    public static void main(String[] args) {
        WebSearchEngine engine = new WebSearchEngine();
        
        String query = args.length > 0 ? String.join(" ", args) : "Java programming tutorial";
        System.out.println("Searching for: " + query + "\n");
        
        List<WebResult> results = engine.search(query, 10);
        
        if (results.isEmpty()) {
            System.out.println("No results found.");
        } else {
            for (int i = 0; i < results.size(); i++) {
                WebResult result = results.get(i);
                System.out.printf("%d. %s%n", i + 1, result.getTitle());
                System.out.printf("   URL: %s%n", result.getUrl());
                System.out.printf("   %s%n%n", result.getSnippet());
            }
        }
    }
}
