package searchengine;

/**
 * Represents a searchable document with an ID, title, and content.
 */
public class Document {
    private final int id;
    private final String title;
    private final String content;

    public Document(int id, String title, String content) {
        this.id = id;
        this.title = title;
        this.content = content;
    }

    public int getId() {
        return id;
    }

    public String getTitle() {
        return title;
    }

    public String getContent() {
        return content;
    }

    @Override
    public String toString() {
        return String.format("Document[id=%d, title='%s']", id, title);
    }
}
