# Java Web Search Browser

A full-featured web search browser built in Java with JavaFX, featuring an embedded browser that supports multiple search engines including Google, DuckDuckGo, Bing, Wikipedia, and YouTube.

**Note:** This project implements a search engine (local indexing + web search GUI). Portions of the codebase were generated with the assistance of AI tools and may require manual review. Videos are not supported.

**Stability:** This is an experimental project — there are known stability issues (e.g., incomplete error handling, occasional crashes, and behavior differences between platforms). See the "Stability & Limitations" section below for details.

![Web Search](icon.svg)

## 🚀 Quick Installation

### For Non-Technical Users (Recommended)
Simply double-click the **"Install Web Search"** icon on your desktop and follow the wizard prompts!

### For Developers
```bash
cd "Java Search"
./install.sh
```

## ❌ Uninstallation

### For Non-Technical Users
Double-click the **"Uninstall Web Search"** icon on your desktop and confirm.

### For Developers
```bash
cd "Java Search"
./uninstall.sh
```

---

## Features

### Web Search Browser
- **Embedded Browser**: Full web browsing experience within the app
- **Multiple Search Engines**: Google, DuckDuckGo, Bing, Wikipedia, YouTube
- **Address Bar**: Type any URL or search query
- **Navigation Controls**: Back, Forward, Reload, Home buttons
- **Modern Interface**: Clean, intuitive design

### Local Document Search
- **Document Indexing**: Index text documents with titles and content
- **TF-IDF Ranking**: Results ranked by Term Frequency-Inverse Document Frequency
- **Phrase Search**: Find exact phrase matches in documents
- **Auto-complete Suggestions**: Get word suggestions based on indexed terms
- **Stop Word Removal**: Common words are filtered for better search quality
- **Interactive CLI**: Command-line interface for searching

## Project Structure

```
src/main/java/searchengine/
├── Document.java        # Document model class
├── SearchResult.java    # Search result with score
├── Tokenizer.java       # Text tokenization with stop words
├── InvertedIndex.java   # Inverted index data structure
├── SearchEngine.java    # Main search engine logic
└── Main.java            # Demo application with CLI
```

## How It Works

### Inverted Index
The search engine uses an inverted index - a data structure that maps terms to the documents containing them. This allows O(1) lookup for any term.

### TF-IDF Scoring
Results are ranked using TF-IDF (Term Frequency-Inverse Document Frequency):
- **TF**: How often a term appears in a document (normalized by document length)
- **IDF**: How rare a term is across all documents (rarer terms are more important)

### Tokenization
Text is processed by:
1. Converting to lowercase
2. Extracting alphanumeric words
3. Removing stop words (common words like "the", "is", "and")

## Running the Application

### Compile
```bash
cd "Java Search"
mkdir -p out
javac -d out src/main/java/searchengine/*.java
```

### Run
```bash
java -cp out searchengine.Main
```

### Run GUI
```bash
bash "/home/robert-marshall01/Desktop/Java Search/run-gui.sh" &
```

## Usage Examples

### Interactive Commands

```
> search Java programming
  1. Introduction to Java Programming (score: 0.2341, matches: 2)
  2. Python vs Java Comparison (score: 0.1823, matches: 2)

> phrase web applications
  1. Building Web Applications (score: 12.5000, matches: 2)

> suggest prog
  - program
  - programming
  - programmer

> stats
Search Engine Statistics:
  Documents indexed: 10
  Vocabulary size: 245 unique terms

> quit
```

## API Usage

```java
// Create search engine
SearchEngine engine = new SearchEngine();

// Index documents
engine.indexDocument(new Document(1, "Title", "Content here..."));

// Search
List<SearchResult> results = engine.search("query terms");

// Phrase search
List<SearchResult> exact = engine.phraseSearch("exact phrase", 10);

// Get suggestions
List<String> suggestions = engine.getSuggestions("prefix", 5);
```

## ⚠️ Stability & Limitations

- **AI-generated code**: Portions of this repository were generated with AI assistance. While AI sped up development, manual review is recommended for correctness, security, and style.
- **Stability**: Some features are experimental and may be unstable (UI glitches, crashes, or incomplete error handling). Expect to encounter issues on different platforms.
- **Search limitations**: Programmatic access to public search engines is subject to blocking and rate limits; results and reliability may vary.

---

## Extending the Engine

Ideas for enhancements:
- **Stemming**: Reduce words to root form (running → run)
- **Fuzzy matching**: Find similar words for typo tolerance
- **Boolean operators**: Support AND, OR, NOT queries
- **Field-specific search**: Search only in title or content
- **Persistent storage**: Save index to disk
- **Web crawler**: Automatically index web pages
