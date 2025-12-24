package searchengine;

import java.util.*;

/**
 * Inverted index data structure for efficient term-to-document lookups.
 * Stores term frequencies for TF-IDF scoring.
 */
public class InvertedIndex {
    
    // Maps term -> (documentId -> term frequency in that document)
    private final Map<String, Map<Integer, Integer>> index;
    
    // Maps documentId -> total number of terms in document
    private final Map<Integer, Integer> documentLengths;
    
    // Total number of documents indexed
    private int documentCount;

    public InvertedIndex() {
        this.index = new HashMap<>();
        this.documentLengths = new HashMap<>();
        this.documentCount = 0;
    }

    /**
     * Adds a term occurrence to the index.
     * 
     * @param term The term to add
     * @param documentId The document containing the term
     */
    public void addTerm(String term, int documentId) {
        index.computeIfAbsent(term, k -> new HashMap<>())
             .merge(documentId, 1, Integer::sum);
    }

    /**
     * Sets the total term count for a document.
     */
    public void setDocumentLength(int documentId, int length) {
        documentLengths.put(documentId, length);
    }

    /**
     * Increments the document count.
     */
    public void incrementDocumentCount() {
        documentCount++;
    }

    /**
     * Gets all document IDs containing the given term.
     */
    public Set<Integer> getDocumentsContaining(String term) {
        Map<Integer, Integer> docs = index.get(term);
        return docs != null ? docs.keySet() : Collections.emptySet();
    }

    /**
     * Gets the term frequency (TF) - how many times a term appears in a document.
     */
    public int getTermFrequency(String term, int documentId) {
        Map<Integer, Integer> docs = index.get(term);
        if (docs == null) {
            return 0;
        }
        return docs.getOrDefault(documentId, 0);
    }

    /**
     * Gets the document frequency (DF) - how many documents contain the term.
     */
    public int getDocumentFrequency(String term) {
        Map<Integer, Integer> docs = index.get(term);
        return docs != null ? docs.size() : 0;
    }

    /**
     * Gets the total number of documents in the index.
     */
    public int getDocumentCount() {
        return documentCount;
    }

    /**
     * Gets the length (total terms) of a document.
     */
    public int getDocumentLength(int documentId) {
        return documentLengths.getOrDefault(documentId, 0);
    }

    /**
     * Calculates TF-IDF score for a term in a document.
     * TF-IDF = (term frequency / document length) * log(total docs / docs containing term)
     */
    public double calculateTfIdf(String term, int documentId) {
        int tf = getTermFrequency(term, documentId);
        if (tf == 0) {
            return 0.0;
        }

        int docLength = getDocumentLength(documentId);
        int df = getDocumentFrequency(term);
        
        // Normalized term frequency
        double normalizedTf = (double) tf / docLength;
        
        // Inverse document frequency with smoothing
        double idf = Math.log((double) (documentCount + 1) / (df + 1)) + 1;
        
        return normalizedTf * idf;
    }

    /**
     * Returns the number of unique terms in the index.
     */
    public int getVocabularySize() {
        return index.size();
    }

    /**
     * Clears all data from the index.
     */
    public void clear() {
        index.clear();
        documentLengths.clear();
        documentCount = 0;
    }
}
