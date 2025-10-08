# Generate domain-specific GloVe embeddings
# Selects the most RELEVANT words for AI workflows (not just most common!)

cat("========================================\n")
cat("Domain-Specific Vocabulary Selection\n")
cat("========================================\n\n")

cat("Strategy: Instead of the 10k most common words,\n")
cat("we'll select the 7.5k words most SIMILAR to your project domain.\n")
cat("This gives better accuracy for research admin AI workflows!\n\n")

# Project description - load from comprehensive domain document
cat("Loading project domain description...\n")
project_description <- paste(readLines("project_domain.md"), collapse = " ")
cat("✓ Loaded domain vocabulary\n\n")

cat("========================================\n")
cat("STEP 1: Check for GloVe file\n")
cat("========================================\n\n")

if (!file.exists("glove.6B.100d.txt")) {
  cat("GloVe file not found. Downloading...\n")
  glove_url <- "https://nlp.stanford.edu/data/glove.6B.zip"

  options(timeout = 600)
  download.file(glove_url, "glove.6B.zip", mode = "wb")
  unzip("glove.6B.zip", files = "glove.6B.100d.txt")
  file.remove("glove.6B.zip")
  cat("✓ Download complete!\n\n")
} else {
  cat("✓ GloVe file exists\n\n")
}

cat("========================================\n")
cat("STEP 2: Load GloVe embeddings\n")
cat("========================================\n\n")

cat("Loading embeddings (this takes ~1 minute for 400k words)...\n")

# Pure R implementation - no packages needed!
lines <- readLines("glove.6B.100d.txt", n = 50000)  # Load first 50k words (covers most English)

cat("Parsing", length(lines), "word vectors...\n")

# Parse each line
words <- character(length(lines))
vecs <- matrix(0, nrow = length(lines), ncol = 100)

for (i in seq_along(lines)) {
  parts <- strsplit(lines[i], " ")[[1]]
  words[i] <- parts[1]
  vecs[i, ] <- as.numeric(parts[-1])

  if (i %% 10000 == 0) cat("  Processed", i, "words...\n")
}

rownames(vecs) <- words
glove <- vecs

cat("✓ Loaded", nrow(glove), "word embeddings\n\n")

cat("========================================\n")
cat("STEP 3: Create project embedding\n")
cat("========================================\n\n")

# Cosine similarity function
cosine_sim <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

# Create document embedding by averaging word vectors
doc_embed <- function(text, embeddings) {
  words <- tolower(strsplit(text, "\\s+")[[1]])
  words <- words[words != ""]
  word_vecs <- embeddings[rownames(embeddings) %in% words, , drop = FALSE]

  if (nrow(word_vecs) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  colMeans(word_vecs)
}

# Generate embedding for the project description
project_vec <- doc_embed(project_description, glove)

cat("Project embedding created from",
    sum(rownames(glove) %in% tolower(strsplit(project_description, "\\s+")[[1]])),
    "words\n\n")

cat("========================================\n")
cat("STEP 4: Find most relevant words\n")
cat("========================================\n\n")

cat("Calculating similarity of all words to project domain...\n")
cat("(This may take 1-2 minutes)\n\n")

# Calculate similarity of each word to the project description
similarities <- numeric(nrow(glove))
for (i in 1:nrow(glove)) {
  similarities[i] <- cosine_sim(glove[i, ], project_vec)

  if (i %% 10000 == 0) cat("  Processed", i, "words...\n")
}

# Load curated vocabulary (~1000 high-value words)
cat("Loading curated vocabulary...\n")
curated_text <- paste(readLines("curated_vocabulary.txt"), collapse = " ")
# Extract just the words (remove comments and extra whitespace)
curated_words <- tolower(unlist(strsplit(curated_text, "\\s+")))
curated_words <- curated_words[!grepl("^#", curated_words)]  # Remove comments
curated_words <- curated_words[curated_words != ""]
curated_words <- unique(curated_words)

cat("Curated vocabulary contains", length(curated_words), "unique words\n")

# Load action words (must be included!)
cat("Loading action words (MUST be included)...\n")
action_words_lines <- readLines("app/action_words.txt")
action_words_lines <- action_words_lines[!grepl("^#", action_words_lines)]  # Remove comments
action_words <- tolower(trimws(sapply(strsplit(action_words_lines, "\\|"), `[`, 1)))
action_words <- unique(action_words[action_words != ""])
cat("Action words list contains", length(action_words), "unique words\n")

# Combine curated + action words
all_priority_words <- unique(c(curated_words, action_words))
cat("Total priority words (curated + action):", length(all_priority_words), "\n")

# Find which priority words exist in GloVe
priority_indices <- which(rownames(glove) %in% all_priority_words)
cat("Found", length(priority_indices), "priority words in GloVe vocabulary\n")

# Remove filler words from similarity-based selection
filler_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
                  "of", "with", "by", "from", "as", "is", "was", "are", "were", "be",
                  "been", "being", "have", "has", "had", "do", "does", "did", "will",
                  "would", "should", "could", "may", "might", "must", "can", "shall",
                  "this", "that", "these", "those", "i", "you", "he", "she", "it",
                  "we", "they", "them", "their", "my", "your", "his", "her", "its",
                  "our", "what", "which", "who", "when", "where", "why", "how")

# Get top words by similarity, excluding fillers and priority words
remaining_slots <- 8000 - length(priority_indices)
non_priority_mask <- !(rownames(glove) %in% all_priority_words) & !(rownames(glove) %in% filler_words)
non_priority_similarities <- similarities
non_priority_similarities[!non_priority_mask] <- -Inf
top_by_similarity <- order(non_priority_similarities, decreasing = TRUE)[1:remaining_slots]

# Combine: priority words FIRST, then similarity-based, take 8k total
combined_indices <- unique(c(priority_indices, top_by_similarity))[1:8000]
domain_glove <- glove[combined_indices, ]

# Add missing action words by copying vectors from related words
cat("Adding missing action words using related word vectors...\n")
missing_words_map <- list(
  "formatting" = "format",    # formatting → use format's vector
  "formatter" = "format",
  "retrieves" = "retrieve",
  "fetches" = "fetch",
  "scraper" = "scrape",
  "modifies" = "modify"
)

for (missing_word in names(missing_words_map)) {
  source_word <- missing_words_map[[missing_word]]
  if (source_word %in% rownames(domain_glove) && !(missing_word %in% rownames(domain_glove))) {
    # Add the word with source word's vector
    new_vec <- domain_glove[source_word, , drop = FALSE]
    rownames(new_vec) <- missing_word
    domain_glove <- rbind(domain_glove, new_vec)
    cat("  Added", missing_word, "using", source_word, "vector\n")
  }
}

cat("✓ Vocabulary size after adding missing words:", nrow(domain_glove), "\n\n")

cat("✓ Selected 8,000 domain-relevant words!\n")
cat("  -", length(priority_indices), "priority words (curated + action words)\n")
cat("  -", 8000 - length(priority_indices), "similarity-selected words (no filler)\n\n")

# Verify all action words are included
action_words_included <- sum(action_words %in% rownames(domain_glove))
action_words_missing <- action_words[!(action_words %in% rownames(domain_glove))]
cat("Action words included:", action_words_included, "/", length(action_words), "\n")
if (length(action_words_missing) > 0) {
  cat("WARNING: Missing action words:\n")
  cat(paste(action_words_missing, collapse = ", "), "\n")
}
cat("\n")

cat("========================================\n")
cat("STEP 5: POS tag ALL vocabulary using udpipe\n")
cat("========================================\n\n")

# Load udpipe
if (!require("udpipe", quietly = TRUE)) {
  install.packages("udpipe")
}
library(udpipe)

model_file <- "english-ewt-ud-2.5-191206.udpipe"
if (!file.exists(model_file)) {
  cat("Downloading udpipe model...\n")
  udpipe_download_model(language = "english-ewt")
}
udmodel <- udpipe_load_model(model_file)

# Load manual action word classifications
action_words_full <- readLines("app/action_words.txt")
action_words_full <- action_words_full[!grepl("^#", action_words_full) & action_words_full != ""]
action_word_pos <- data.frame(
  word = tolower(trimws(sapply(strsplit(action_words_full, "\\|"), `[`, 1))),
  pos = trimws(sapply(strsplit(action_words_full, "\\|"), `[`, 2)),
  stringsAsFactors = FALSE
)

vocabulary <- rownames(domain_glove)
pos_tags <- character(length(vocabulary))

cat("Tagging", length(vocabulary), "words with POS...\n")

for (i in seq_along(vocabulary)) {
  word <- vocabulary[i]

  # Use manual classification if available
  manual_pos <- action_word_pos$pos[action_word_pos$word == word]

  if (length(manual_pos) > 0) {
    pos_tags[i] <- manual_pos[1]
  } else {
    # Auto-tag with udpipe using neutral sentence
    sentence <- paste("The", word, "is important for research")
    annotated <- udpipe_annotate(udmodel, x = sentence)
    annotated_df <- as.data.frame(annotated)
    word_row <- which(tolower(annotated_df$token) == word)

    if (length(word_row) > 0) {
      pos_tags[i] <- annotated_df$upos[word_row[1]]
    } else {
      pos_tags[i] <- "NOUN"  # Default
    }
  }

  if (i %% 500 == 0) {
    cat("  ", i, "/", length(vocabulary), "\n")
  }
}

cat("\n✓ POS tagging complete!\n\n")
cat("POS distribution:\n")
print(table(pos_tags))
cat("\n")

cat("========================================\n")
cat("STEP 6: Generate category embeddings\n")
cat("========================================\n\n")

# Category descriptions - emphasize actions strongly
categories <- list(
  extraction = "extract extract extracted extracting retrieve retrieved pull pulled pulling get getting fetch fetched gather gathered gathering collect collected collecting scrape scraped mine mined obtain obtained download downloaded query queried",
  prompt = "analyze analyzed analyzing process processed processing review reviewed reviewing examine examined check checked validate validated verify verified assess assessed evaluate evaluated study studied compare compared interpret interpreted calculate calculated identify identified",
  formatting = "generate generated generating create created creating format formatted formatting produce produced write written export exported render rendered display displayed present presented organize organized structure structured save saved convert converted"
)

# Generate embeddings for each category
extraction_vec <- doc_embed(categories$extraction, domain_glove)
prompt_vec <- doc_embed(categories$prompt, domain_glove)
formatting_vec <- doc_embed(categories$formatting, domain_glove)

cat("✓ Category embeddings generated!\n\n")

cat("Extraction words found:", sum(rownames(domain_glove) %in% strsplit(tolower(categories$extraction), "\\s+")[[1]]), "/", length(strsplit(tolower(categories$extraction), "\\s+")[[1]]), "\n")
cat("Prompt words found:", sum(rownames(domain_glove) %in% strsplit(tolower(categories$prompt), "\\s+")[[1]]), "/", length(strsplit(tolower(categories$prompt), "\\s+")[[1]]), "\n")
cat("Formatting words found:", sum(rownames(domain_glove) %in% strsplit(tolower(categories$formatting), "\\s+")[[1]]), "/", length(strsplit(tolower(categories$formatting), "\\s+")[[1]]), "\n\n")

cat("========================================\n")
cat("STEP 7: Save embeddings\n")
cat("========================================\n\n")

# Save the domain-specific embeddings with POS tags
saveRDS(list(
  word_embeddings = domain_glove,
  pos_tags = pos_tags,
  category_embeddings = list(
    extraction = extraction_vec,
    prompt = prompt_vec,
    formatting = formatting_vec
  )
), "app/glove_embeddings.rds")

cat("✓ Saved to app/glove_embeddings.rds\n\n")

file_size_mb <- round(file.size("app/glove_embeddings.rds") / 1024 / 1024, 2)
cat("File size:", file_size_mb, "MB\n")
cat("Contains: 8,000 domain-relevant words with 100d vectors\n\n")

cat("========================================\n")
cat("✓ All done!\n")
cat("========================================\n\n")

cat("Your app now has vocabulary optimized for:\n")
cat("- AI/ML workflows\n")
cat("- Data processing\n")
cat("- API/database operations\n")
cat("- Output formatting\n\n")

cat("Next step: Test with shiny::runApp('app/app.R')\n")

# Clean up
# file.remove("glove.6B.100d.txt")  # Keep for now in case you want to re-run
