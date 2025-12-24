package searchengine;

/**
 * Represents a search result with the matched document and its relevance score.
 */
public class SearchResult implements Comparable<SearchResult> {
    private final Document document;
    private final double score;
    private final int matchCount;

    public SearchResult(Document document, double score, int matchCount) {
        this.document = document;
        this.score = score;
        this.matchCount = matchCount;
    }

    public Document getDocument() {
        return document;
    }

    public double getScore() {
        return score;
    }

    public int getMatchCount() {
        return matchCount;
    }

    @Override
    public int compareTo(SearchResult other) {
        // Higher score comes first
        return Double.compare(other.score, this.score);
    }

    @Override
    public String toString() {
        return String.format("SearchResult[doc=%s, score=%.4f, matches=%d]", 
                document.getTitle(), score, matchCount);
    }
}
