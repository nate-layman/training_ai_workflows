# Identify which missing words are action words (verbs)

missing_words <- c("pi", "subaward", "closeout", "dashboard", "rebudget", "subcontract",
                   "actuals", "allowable", "carryover", "checklist", "csv", "deliverables",
                   "departmental", "emails", "f", "fellowship", "fringe", "invoicing",
                   "irb", "metrics", "notices", "overruns", "overspending", "pie",
                   "pivot", "reasonableness", "rebudget", "rebudgeting", "remission",
                   "resubmission", "stipend", "subrecipient", "unrecovered", "unspent",
                   "variances", "vs", "workload")

# Install udpipe if needed
if (!require("udpipe", quietly = TRUE)) {
  install.packages("udpipe")
}
library(udpipe)

# Load model
model_file <- "english-ewt-ud-2.5-191206.udpipe"
if (!file.exists(model_file)) {
  cat("Downloading udpipe model...\n")
  udpipe_download_model(language = "english-ewt")
}
udmodel <- udpipe_load_model(model_file)

cat("========================================\n")
cat("POS Tagging Missing Words\n")
cat("========================================\n\n")

results <- data.frame(
  word = character(),
  pos = character(),
  category = character(),
  stringsAsFactors = FALSE
)

for (word in missing_words) {
  # Tag the word in a neutral sentence
  sentence <- paste("The", word, "is important for research")
  annotated <- udpipe_annotate(udmodel, x = sentence)
  annotated_df <- as.data.frame(annotated)
  word_row <- which(tolower(annotated_df$token) == word)

  if (length(word_row) > 0) {
    pos <- annotated_df$upos[word_row[1]]

    # Categorize
    category <- if (pos == "VERB") {
      "ACTION_VERB"
    } else if (pos %in% c("NOUN", "PROPN")) {
      "NOUN"
    } else if (pos == "ADJ") {
      "ADJECTIVE"
    } else {
      "OTHER"
    }

    results <- rbind(results, data.frame(word = word, pos = pos, category = category))
    cat(sprintf("%-20s  %-10s  %s\n", word, pos, category))
  }
}

cat("\n========================================\n")
cat("Summary\n")
cat("========================================\n\n")

cat("Action verbs (need to add to action_words.txt):\n")
action_verbs <- results$word[results$category == "ACTION_VERB"]
if (length(action_verbs) > 0) {
  for (v in action_verbs) {
    cat("  -", v, "\n")
  }
} else {
  cat("  (none)\n")
}

cat("\nNouns (need to add to curated_vocabulary.txt):\n")
nouns <- results$word[results$category == "NOUN"]
if (length(nouns) > 0) {
  for (n in nouns) {
    cat("  -", n, "\n")
  }
}

cat("\nOther:\n")
other <- results$word[results$category %in% c("ADJECTIVE", "OTHER")]
if (length(other) > 0) {
  for (o in other) {
    cat("  -", o, "(", results$pos[results$word == o], ")\n")
  }
}
