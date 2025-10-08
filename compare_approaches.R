# Compare Current Weighted Average vs SIF (Smooth Inverse Frequency)
# Test on 200 research administration user inputs

library(glue)

cat("========================================\n")
cat("Semantic Similarity Comparison Test\n")
cat("========================================\n\n")

# Load test cases
cat("Loading 200 test cases...\n")
test_data <- read.csv("test_cases.csv", stringsAsFactors = FALSE)
cat("✓ Loaded", nrow(test_data), "test cases\n\n")

# Load embeddings
cat("Loading GloVe embeddings...\n")
embeddings_data <- readRDS('app/glove_embeddings.rds')
word_embeddings <- embeddings_data$word_embeddings
category_embeddings <- embeddings_data$category_embeddings
cat("✓ Loaded", nrow(word_embeddings), "word embeddings\n\n")

# Load action verbs
action_words_lines <- readLines('app/action_words.txt')
action_words_lines <- action_words_lines[!grepl("^#", action_words_lines) & action_words_lines != ""]
action_verb_list <- tolower(trimws(sapply(strsplit(action_words_lines, "\\|"), `[`, 1)))
action_verb_list <- unique(action_verb_list[action_verb_list != ""])

# Filler words to exclude
filler_words <- c("the", "a", "an", "and", "or", "but", "in", "on", "at", "to", "for",
                  "of", "with", "by", "from", "as", "is", "was", "are", "were", "be",
                  "been", "being", "have", "has", "had", "do", "does", "did", "will",
                  "would", "should", "could", "may", "might", "must", "can", "shall",
                  "this", "that", "these", "those", "i", "you", "he", "she", "it",
                  "we", "they", "them", "their", "my", "your", "his", "her", "its",
                  "our", "what", "which", "who", "when", "where", "why", "how")

#------------------------------------------
# APPROACH 1: Current weighted average (5x for action verbs)
#------------------------------------------
find_closest_word <- function(word, vocabulary) {
  if (word %in% vocabulary) return(word)

  scores <- sapply(vocabulary, function(v) {
    word_chars <- strsplit(word, "")[[1]]
    v_chars <- strsplit(v, "")[[1]]
    intersection <- sum(word_chars %in% v_chars)
    union <- length(unique(c(word_chars, v_chars)))
    len_diff <- abs(nchar(word) - nchar(v))
    intersection / union - (len_diff * 0.1)
  })

  vocabulary[which.max(scores)]
}

doc_embed_weighted <- function(text, embeddings = word_embeddings) {
  words <- tolower(text)
  words <- gsub("[^a-z0-9 ]", " ", words)
  words <- strsplit(words, "\\s+")[[1]]
  words <- words[words != ""]

  if (length(words) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  words <- words[!(words %in% filler_words)]

  if (length(words) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  vocab <- rownames(embeddings)
  matched_words <- sapply(words, function(w) {
    if (w %in% vocab) {
      w
    } else {
      find_closest_word(w, vocab)
    }
  })

  word_vecs <- matrix(0, nrow = 0, ncol = ncol(embeddings))

  for (i in seq_along(matched_words)) {
    word <- matched_words[i]
    if (word %in% rownames(embeddings)) {
      vec <- embeddings[word, ]

      weight <- 1
      if (word %in% action_verb_list) {
        weight <- 5  # 5x weight for action verbs
      }

      for (j in 1:weight) {
        word_vecs <- rbind(word_vecs, vec)
      }
    }
  }

  if (nrow(word_vecs) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  colMeans(word_vecs)
}

#------------------------------------------
# APPROACH 2: SIF (Smooth Inverse Frequency) weighting
#------------------------------------------
# Calculate word frequencies from vocabulary (proxy for corpus frequency)
cat("Calculating word frequency estimates...\n")
vocab_words <- rownames(word_embeddings)
# Use word rank as proxy for frequency (earlier in GloVe = more common)
# This is a simplification - ideally we'd use actual corpus frequencies
word_freq <- setNames(1:length(vocab_words) / length(vocab_words), vocab_words)
cat("✓ Estimated frequencies for", length(word_freq), "words\n\n")

doc_embed_sif <- function(text, embeddings = word_embeddings, a = 1e-3) {
  words <- tolower(text)
  words <- gsub("[^a-z0-9 ]", " ", words)
  words <- strsplit(words, "\\s+")[[1]]
  words <- words[words != ""]

  if (length(words) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  words <- words[!(words %in% filler_words)]

  if (length(words) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  vocab <- rownames(embeddings)
  matched_words <- sapply(words, function(w) {
    if (w %in% vocab) {
      w
    } else {
      find_closest_word(w, vocab)
    }
  })

  # Calculate SIF weights: a / (a + p(w))
  word_vecs <- matrix(0, nrow = 0, ncol = ncol(embeddings))
  weights_used <- numeric(0)

  for (word in matched_words) {
    if (word %in% rownames(embeddings)) {
      vec <- embeddings[word, ]

      # Get word frequency (default to 0.5 if not found)
      p_w <- ifelse(word %in% names(word_freq), word_freq[word], 0.5)

      # SIF weight formula
      weight <- a / (a + p_w)

      # Boost action verbs in SIF too (multiply weight by 3)
      if (word %in% action_verb_list) {
        weight <- weight * 3
      }

      word_vecs <- rbind(word_vecs, vec * weight)
      weights_used <- c(weights_used, weight)
    }
  }

  if (nrow(word_vecs) == 0) {
    return(rep(0, ncol(embeddings)))
  }

  # Average the weighted vectors
  embedding <- colMeans(word_vecs)

  # Remove first principal component (simplified - using mean as proxy)
  # Full SIF removes the first PC of all sentence embeddings
  # For this test, we'll use the simpler weighted average

  embedding
}

#------------------------------------------
# Cosine similarity
#------------------------------------------
cosine_similarity <- function(vec1, vec2) {
  dot_product <- sum(vec1 * vec2)
  norm1 <- sqrt(sum(vec1^2))
  norm2 <- sqrt(sum(vec2^2))

  if (norm1 == 0 || norm2 == 0) return(0)

  dot_product / (norm1 * norm2)
}

#------------------------------------------
# Classification function
#------------------------------------------
classify <- function(text, method = "weighted") {
  if (method == "weighted") {
    user_vec <- doc_embed_weighted(text)
  } else {
    user_vec <- doc_embed_sif(text)
  }

  similarities <- c(
    extraction = cosine_similarity(user_vec, category_embeddings$extraction),
    prompt = cosine_similarity(user_vec, category_embeddings$prompt),
    formatting = cosine_similarity(user_vec, category_embeddings$formatting)
  )

  if (all(similarities == 0)) {
    return("prompt")
  }

  return(names(which.max(similarities)))
}

#------------------------------------------
# Run tests
#------------------------------------------
cat("========================================\n")
cat("Testing APPROACH 1: Weighted Average (5x action verbs)\n")
cat("========================================\n\n")

predictions_weighted <- character(nrow(test_data))
for (i in 1:nrow(test_data)) {
  predictions_weighted[i] <- classify(test_data$input[i], method = "weighted")
  if (i %% 50 == 0) {
    cat("  Processed", i, "/", nrow(test_data), "\n")
  }
}

accuracy_weighted <- sum(predictions_weighted == test_data$expected_category) / nrow(test_data)
cat("\n✓ Weighted Average Accuracy:", round(accuracy_weighted * 100, 1), "%\n\n")

cat("========================================\n")
cat("Testing APPROACH 2: SIF (Smooth Inverse Frequency)\n")
cat("========================================\n\n")

predictions_sif <- character(nrow(test_data))
for (i in 1:nrow(test_data)) {
  predictions_sif[i] <- classify(test_data$input[i], method = "sif")
  if (i %% 50 == 0) {
    cat("  Processed", i, "/", nrow(test_data), "\n")
  }
}

accuracy_sif <- sum(predictions_sif == test_data$expected_category) / nrow(test_data)
cat("\n✓ SIF Accuracy:", round(accuracy_sif * 100, 1), "%\n\n")

#------------------------------------------
# Results summary
#------------------------------------------
cat("========================================\n")
cat("RESULTS SUMMARY\n")
cat("========================================\n\n")

cat("Weighted Average (5x verbs):  ", round(accuracy_weighted * 100, 1), "%\n")
cat("SIF (frequency-based):        ", round(accuracy_sif * 100, 1), "%\n\n")

improvement <- accuracy_sif - accuracy_weighted
if (improvement > 0) {
  cat("✓ SIF is BETTER by", round(improvement * 100, 1), "percentage points\n\n")
} else if (improvement < 0) {
  cat("✓ Weighted Average is BETTER by", round(abs(improvement) * 100, 1), "percentage points\n\n")
} else {
  cat("✓ Both methods perform equally\n\n")
}

#------------------------------------------
# Confusion matrices
#------------------------------------------
cat("========================================\n")
cat("CONFUSION MATRIX: Weighted Average\n")
cat("========================================\n\n")

confusion_weighted <- table(Predicted = predictions_weighted, Actual = test_data$expected_category)
print(confusion_weighted)
cat("\n")

cat("========================================\n")
cat("CONFUSION MATRIX: SIF\n")
cat("========================================\n\n")

confusion_sif <- table(Predicted = predictions_sif, Actual = test_data$expected_category)
print(confusion_sif)
cat("\n")

#------------------------------------------
# Error analysis
#------------------------------------------
cat("========================================\n")
cat("ERROR ANALYSIS\n")
cat("========================================\n\n")

errors_weighted <- which(predictions_weighted != test_data$expected_category)
errors_sif <- which(predictions_sif != test_data$expected_category)

cat("Weighted Average Errors:", length(errors_weighted), "\n")
cat("SIF Errors:", length(errors_sif), "\n\n")

# Cases where SIF is better
sif_better <- setdiff(errors_weighted, errors_sif)
if (length(sif_better) > 0) {
  cat("Cases where SIF is CORRECT but Weighted is WRONG (", length(sif_better), "):\n\n")
  for (i in head(sif_better, 5)) {
    cat(glue("  '{test_data$input[i]}'\n"))
    cat(glue("    Expected: {test_data$expected_category[i]}, Weighted: {predictions_weighted[i]}, SIF: {predictions_sif[i]}\n\n"))
  }
}

# Cases where Weighted is better
weighted_better <- setdiff(errors_sif, errors_weighted)
if (length(weighted_better) > 0) {
  cat("Cases where WEIGHTED is CORRECT but SIF is WRONG (", length(weighted_better), "):\n\n")
  for (i in head(weighted_better, 5)) {
    cat(glue("  '{test_data$input[i]}'\n"))
    cat(glue("    Expected: {test_data$expected_category[i]}, Weighted: {predictions_weighted[i]}, SIF: {predictions_sif[i]}\n\n"))
  }
}

cat("========================================\n")
cat("✓ Comparison complete!\n")
cat("========================================\n")
