package searchengine;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Main search engine class that handles document indexing and searching.
 * Uses an inverted index with TF-IDF scoring for relevance ranking.
 */
public class SearchEngine {
    
    private final InvertedIndex index;
    private final Tokenizer tokenizer;
    private final Map<Integer, Document> documents;

    public SearchEngine() {
        this.index = new InvertedIndex();
        this.tokenizer = new Tokenizer();
        this.documents = new HashMap<>();
    }

    /**
     * Indexes a single document.
     * 
     * @param document The document to index
     */
    public void indexDocument(Document document) {
        documents.put(document.getId(), document);
        
        // Combine title and content for indexing (title terms get indexed twice for higher weight)
        String fullText = document.getTitle() + " " + document.getTitle() + " " + document.getContent();
        List<String> tokens = tokenizer.tokenize(fullText);
        
        for (String token : tokens) {
            index.addTerm(token, document.getId());
        }
        
        index.setDocumentLength(document.getId(), tokens.size());
        index.incrementDocumentCount();
    }

    /**
     * Indexes multiple documents.
     * 
     * @param docs Collection of documents to index
     */
    public void indexDocuments(Collection<Document> docs) {
        for (Document doc : docs) {
            indexDocument(doc);
        }
    }

    /**
     * Searches for documents matching the query.
     * 
     * @param query The search query
     * @param maxResults Maximum number of results to return
     * @return List of search results ranked by relevance
     */
    public List<SearchResult> search(String query, int maxResults) {
        List<String> queryTokens = tokenizer.tokenize(query);
        
        if (queryTokens.isEmpty()) {
            return Collections.emptyList();
        }

        // Find all documents containing at least one query term
        Map<Integer, Double> scores = new HashMap<>();
        Map<Integer, Integer> matchCounts = new HashMap<>();
        
        for (String term : queryTokens) {
            Set<Integer> matchingDocs = index.getDocumentsContaining(term);
            
            for (int docId : matchingDocs) {
                double tfIdf = index.calculateTfIdf(term, docId);
                scores.merge(docId, tfIdf, Double::sum);
                matchCounts.merge(docId, 1, Integer::sum);
            }
        }

        // Convert to search results and sort by score
        return scores.entrySet().stream()
            .map(entry -> new SearchResult(
                documents.get(entry.getKey()),
                entry.getValue(),
                matchCounts.get(entry.getKey())
            ))
            .sorted()
            .limit(maxResults)
            .collect(Collectors.toList());
    }

    /**
     * Searches with default max results of 10.
     */
    public List<SearchResult> search(String query) {
        return search(query, 10);
    }

    /**
     * Performs a phrase search - finds documents containing the exact phrase.
     * 
     * @param phrase The exact phrase to search for
     * @param maxResults Maximum number of results
     * @return List of matching documents
     */
    public List<SearchResult> phraseSearch(String phrase, int maxResults) {
        String normalizedPhrase = phrase.toLowerCase().trim();
        
        List<SearchResult> results = new ArrayList<>();
        
        for (Document doc : documents.values()) {
            String content = (doc.getTitle() + " " + doc.getContent()).toLowerCase();
            
            if (content.contains(normalizedPhrase)) {
                // Count occurrences
                int count = 0;
                int idx = 0;
                while ((idx = content.indexOf(normalizedPhrase, idx)) != -1) {
                    count++;
                    idx += normalizedPhrase.length();
                }
                
                // Score based on frequency and document length
                double score = (double) count / content.length() * 1000;
                results.add(new SearchResult(doc, score, count));
            }
        }
        
        return results.stream()
            .sorted()
            .limit(maxResults)
            .collect(Collectors.toList());
    }

    /**
     * Gets search suggestions based on indexed terms.
     * 
     * @param prefix The prefix to match
     * @param maxSuggestions Maximum number of suggestions
     * @return List of matching terms
     */
    public List<String> getSuggestions(String prefix, int maxSuggestions) {
        String normalizedPrefix = prefix.toLowerCase().trim();
        
        // This is a simple implementation - a production system would use a trie
        Set<String> suggestions = new TreeSet<>();
        
        for (Document doc : documents.values()) {
            List<String> tokens = tokenizer.tokenize(doc.getTitle() + " " + doc.getContent(), false);
            for (String token : tokens) {
                if (token.startsWith(normalizedPrefix)) {
                    suggestions.add(token);
                    if (suggestions.size() >= maxSuggestions) {
                        break;
                    }
                }
            }
            if (suggestions.size() >= maxSuggestions) {
                break;
            }
        }
        
        return new ArrayList<>(suggestions);
    }

    /**
     * Gets statistics about the search engine.
     */
    public String getStatistics() {
        return String.format(
            "Search Engine Statistics:\n" +
            "  Documents indexed: %d\n" +
            "  Vocabulary size: %d unique terms",
            index.getDocumentCount(),
            index.getVocabularySize()
        );
    }

    /**
     * Gets a document by ID.
     */
    public Document getDocument(int id) {
        return documents.get(id);
    }

    /**
     * Gets the total number of indexed documents.
     */
    public int getDocumentCount() {
        return documents.size();
    }

    /**
     * Clears all indexed data.
     */
    public void clear() {
        index.clear();
        documents.clear();
    }
}
