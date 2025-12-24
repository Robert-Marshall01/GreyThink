package searchengine;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Tokenizes text into searchable terms with normalization and stop word removal.
 */
public class Tokenizer {
    
    // Common English stop words to filter out
    private static final Set<String> STOP_WORDS = Set.of(
        "a", "an", "the", "and", "or", "but", "in", "on", "at", "to", "for",
        "of", "with", "by", "from", "as", "is", "was", "are", "were", "been",
        "be", "have", "has", "had", "do", "does", "did", "will", "would", "could",
        "should", "may", "might", "must", "shall", "can", "need", "dare", "ought",
        "used", "it", "its", "this", "that", "these", "those", "i", "you", "he",
        "she", "we", "they", "what", "which", "who", "whom", "whose", "where",
        "when", "why", "how", "all", "each", "every", "both", "few", "more",
        "most", "other", "some", "such", "no", "not", "only", "same", "so",
        "than", "too", "very", "just", "also", "now", "here", "there", "then"
    );

    private static final Pattern WORD_PATTERN = Pattern.compile("[a-zA-Z0-9]+");

    /**
     * Tokenizes the given text into a list of normalized terms.
     * 
     * @param text The text to tokenize
     * @param removeStopWords Whether to remove stop words
     * @return List of tokens
     */
    public List<String> tokenize(String text, boolean removeStopWords) {
        List<String> tokens = new ArrayList<>();
        
        if (text == null || text.isEmpty()) {
            return tokens;
        }

        Matcher matcher = WORD_PATTERN.matcher(text.toLowerCase());
        
        while (matcher.find()) {
            String token = matcher.group();
            
            // Skip very short tokens and optionally stop words
            if (token.length() >= 2) {
                if (!removeStopWords || !STOP_WORDS.contains(token)) {
                    tokens.add(token);
                }
            }
        }
        
        return tokens;
    }

    /**
     * Tokenizes text with stop word removal enabled.
     */
    public List<String> tokenize(String text) {
        return tokenize(text, true);
    }

    /**
     * Checks if a word is a stop word.
     */
    public boolean isStopWord(String word) {
        return STOP_WORDS.contains(word.toLowerCase());
    }
}
