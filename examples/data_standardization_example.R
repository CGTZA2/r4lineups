# ============================================================================
# Data Standardization in r4lineups
# ============================================================================
#
# This script demonstrates the data standardization functions in r4lineups.
# These functions help convert lineup data from various formats into the
# standard format required for confidence-based analyses.
#
# ============================================================================

library(r4lineups)

# ============================================================================
# Example 1: Validate Existing Data
# ============================================================================

cat("\n=== Example 1: Data Validation ===\n\n")

# Create valid lineup data
valid_data <- data.frame(
  target_present = c(TRUE, TRUE, FALSE, FALSE, TRUE),
  identification = c("suspect", "filler", "reject", "suspect", "suspect"),
  confidence = c(5, 3, 2, 4, 5),
  participant_id = 1:5
)

# Validate the data
validation <- validate_lineup_data(valid_data, strict = FALSE)

if (validation$valid) {
  cat("Data is valid!\n")
} else {
  cat("Data has issues:\n")
  cat(paste(validation$messages, collapse = "\n"), "\n")
}

if (length(validation$warnings) > 0) {
  cat("\nWarnings:\n")
  cat(paste(validation$warnings, collapse = "\n"), "\n")
}


# ============================================================================
# Example 2: Detect Data Problems
# ============================================================================

cat("\n\n=== Example 2: Detecting Data Problems ===\n\n")

# Data with invalid identification value
invalid_data <- data.frame(
  target_present = c(TRUE, FALSE, TRUE),
  identification = c("suspect", "invalid_value", "filler"),
  confidence = c(5, 3, 4)
)

validation <- validate_lineup_data(invalid_data, strict = FALSE)

if (!validation$valid) {
  cat("Problems detected:\n")
  cat(paste(validation$messages, collapse = "\n"), "\n")
}


# ============================================================================
# Example 3: Standardize Data with Non-Standard Column Names
# ============================================================================

cat("\n\n=== Example 3: Standardizing Column Names ===\n\n")

# Data with different column names
raw_data <- data.frame(
  tp = c(1, 1, 0, 0, 1, 0),
  response = c("suspect", "filler", "reject", "suspect", "suspect", "filler"),
  conf = c(5, 3, 2, 4, 5, 3),
  subject = c("S01", "S02", "S03", "S04", "S05", "S06")
)

cat("Original data:\n")
print(head(raw_data, 3))

# Standardize
std_data <- standardize_lineup_data(
  raw_data,
  target_present_col = "tp",
  identification_col = "response",
  confidence_col = "conf",
  participant_id_col = "subject",
  validate = FALSE
)

cat("\nStandardized data:\n")
print(head(std_data, 3))


# ============================================================================
# Example 4: Recode Identification Values
# ============================================================================

cat("\n\n=== Example 4: Recoding Identification Values ===\n\n")

# Data with non-standard identification terms
messy_data <- data.frame(
  target_present = c(TRUE, TRUE, FALSE, FALSE, TRUE),
  identification = c("target", "foil", "none", "target", "distractor"),
  confidence = c(5, 3, 2, 4, 3)
)

cat("Before recoding:\n")
print(unique(messy_data$identification))

# Standardize with custom recoding
std_data <- standardize_lineup_data(
  messy_data,
  recode_identification = c(
    "target" = "suspect",
    "foil" = "filler",
    "none" = "reject"
  ),
  validate = FALSE
)

cat("\nAfter recoding:\n")
print(unique(std_data$identification))


# ============================================================================
# Example 5: Auto-Detection of Common Column Names
# ============================================================================

cat("\n\n=== Example 5: Auto-Detection of Columns ===\n\n")

# Data with various common column names
auto_data <- data.frame(
  culprit_present = c(TRUE, FALSE, TRUE, FALSE),
  choice = c("suspect", "filler", "reject", "suspect"),
  certainty = c(5, 3, 2, 4)
)

cat("r4lineups will automatically detect common column names:\n")
cat("  culprit_present -> target_present\n")
cat("  choice -> identification\n")
cat("  certainty -> confidence\n\n")

# Standardize (will auto-detect)
std_data <- suppressMessages(
  standardize_lineup_data(auto_data, validate = FALSE)
)

cat("Standardized columns:\n")
print(names(std_data))


# ============================================================================
# Example 6: Generate Example Data
# ============================================================================

cat("\n\n=== Example 6: Create Example Data ===\n\n")

# Create example data for testing/demonstration
example_data <- create_example_lineup_data(
  n_trials = 100,
  prop_target_present = 0.5,
  include_confidence = TRUE,
  include_response_time = FALSE,
  seed = 123
)

cat("Example data summary:\n")
print(example_data)


# ============================================================================
# Example 7: Validate Data for Specific Analyses
# ============================================================================

cat("\n\n=== Example 7: Validate for Specific Analyses ===\n\n")

# Some analyses don't require confidence
data_no_conf <- data.frame(
  target_present = c(TRUE, FALSE, TRUE, FALSE),
  identification = c("suspect", "filler", "reject", "suspect")
)

# Validate without requiring confidence
validation <- validate_lineup_data(
  data_no_conf,
  require_confidence = FALSE,
  strict = FALSE
)

cat("Valid for analyses without confidence?", validation$valid, "\n")

# Validate requiring response time
validation_rt <- validate_lineup_data(
  data_no_conf,
  require_response_time = TRUE,
  strict = FALSE
)

cat("Valid for analyses requiring response time?", validation_rt$valid, "\n")


# ============================================================================
# Example 8: Standardize Real-World Messy Data
# ============================================================================

cat("\n\n=== Example 8: Real-World Messy Data ===\n\n")

# Simulate real-world data with various issues
realworld_data <- data.frame(
  # Mixed case, non-standard term
  Culprit_Present = c("Yes", "Yes", "No", "No", "Yes", "No"),
  # Non-standard identification terms
  ParticipantChoice = c("Perpetrator", "Foil", "Not Present", "Target", "Distractor", "None"),
  # Confidence as character
  Rating = c("5", "3", "2", "4", "3", "1"),
  # Subject ID
  SubjID = c("P001", "P002", "P003", "P004", "P005", "P006")
)

cat("Original messy data:\n")
print(head(realworld_data, 3))

# Prepare data step by step
# 1. Convert target_present to logical
realworld_data$Culprit_Present <- realworld_data$Culprit_Present == "Yes"

# 2. Convert confidence to numeric
realworld_data$Rating <- as.numeric(realworld_data$Rating)

# 3. Standardize
clean_data <- suppressMessages(
  standardize_lineup_data(
    realworld_data,
    target_present_col = "Culprit_Present",
    identification_col = "ParticipantChoice",
    confidence_col = "Rating",
    participant_id_col = "SubjID",
    validate = TRUE
  )
)

cat("\nCleaned and standardized data:\n")
print(head(clean_data, 3))

# Verify it works with r4lineups analyses
cat("\nTesting with make_roc()...\n")
roc <- make_roc(clean_data, lineup_size = 6, show_plot = FALSE)
cat(sprintf("  pAUC = %.3f\n", roc$pauc))


# ============================================================================
# Example 9: Integration with Analysis Pipeline
# ============================================================================

cat("\n\n=== Example 9: Full Analysis Pipeline ===\n\n")

# Step 1: Generate or load data
raw_data <- create_example_lineup_data(n_trials = 200, seed = 456)

# Step 2: Validate
validation <- validate_lineup_data(raw_data, strict = FALSE)
if (!validation$valid) {
  stop("Data validation failed: ", paste(validation$messages, collapse = "; "))
}

# Step 3: Run analyses
cat("Running comprehensive analysis pipeline:\n\n")

# ROC analysis
roc <- make_roc(raw_data, lineup_size = 6, show_plot = FALSE)
cat(sprintf("1. ROC Analysis: pAUC = %.3f\n", roc$pauc))

# CAC analysis
cac <- make_cac(raw_data, lineup_size = 6, show_plot = FALSE)
cat(sprintf("2. CAC Analysis: Overall Accuracy = %.3f\n", cac$overall_accuracy))

# SDT parameter estimation
sdt <- fit_sdt_roc(raw_data, lineup_size = 6, bootstrap = FALSE)
cat(sprintf("3. SDT Analysis: d' = %.3f\n", sdt$dprime))

cat("\nAll analyses completed successfully!\n")


# ============================================================================
# Example 10: Error Handling and Troubleshooting
# ============================================================================

cat("\n\n=== Example 10: Error Handling ===\n\n")

# Create problematic data
problem_data <- data.frame(
  target_present = c(TRUE, FALSE),
  # Missing identification column
  conf = c(5, 3)
)

# Try to standardize (will fail)
cat("Attempting to standardize data missing identification column...\n")
tryCatch({
  standardize_lineup_data(problem_data)
}, error = function(e) {
  cat("Error caught:", conditionMessage(e), "\n")
})

# Use validation to diagnose
cat("\nUsing validation to diagnose the problem:\n")
validation <- validate_lineup_data(problem_data, strict = FALSE)
if (!validation$valid) {
  cat("Issues found:\n")
  cat(paste("  -", validation$messages, collapse = "\n"), "\n")
}


# ============================================================================
# Summary and Best Practices
# ============================================================================

cat("\n\n=== Summary and Best Practices ===\n\n")

cat("Data Standardization Workflow:\n\n")

cat("1. VALIDATE your data first:\n")
cat("   validation <- validate_lineup_data(data, strict = FALSE)\n\n")

cat("2. STANDARDIZE if needed:\n")
cat("   std_data <- standardize_lineup_data(\n")
cat("     data,\n")
cat("     target_present_col = 'your_tp_column',\n")
cat("     identification_col = 'your_id_column',\n")
cat("     confidence_col = 'your_conf_column'\n")
cat("   )\n\n")

cat("3. RUN your analyses:\n")
cat("   roc <- make_roc(std_data, lineup_size = 6)\n")
cat("   cac <- make_cac(std_data, lineup_size = 6)\n")
cat("   sdt <- fit_sdt_roc(std_data, lineup_size = 6)\n\n")

cat("Required columns for r4lineups:\n")
cat("  - target_present (logical)\n")
cat("  - identification ('suspect', 'filler', or 'reject')\n")
cat("  - confidence (numeric, for most analyses)\n\n")

cat("Optional columns:\n")
cat("  - participant_id (will be generated if missing)\n")
cat("  - response_time (for RAC analysis)\n\n")

cat("Common issues and solutions:\n")
cat("  1. Non-standard column names → Use standardize_lineup_data()\n")
cat("  2. Invalid identification values → Use recode_identification parameter\n")
cat("  3. Missing confidence → Set require_confidence = FALSE for validation\n")
cat("  4. Character confidence values → Convert to numeric before standardizing\n\n")

cat("=== Examples Complete ===\n")
