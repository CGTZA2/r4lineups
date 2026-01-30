# Example: Full ROC Analysis Using Smith & Yang (2020) Method
# This script demonstrates the implementation of full ROC curves
# that use ALL eyewitness responses (suspect IDs, filler IDs, rejections)

library(r4lineups)
library(ggplot2)
library(dplyr)

# =============================================================================
# Example 1: Simulated Data - Strong vs. Weak Memory
# =============================================================================

set.seed(42)

# Generate data for strong memory condition (clear viewing)
n_trials <- 150

# Target-present lineups (guilty suspect)
tp_strong <- data.frame(
  target_present = TRUE,
  identification = sample(
    c("suspect", "filler", "reject"),
    n_trials,
    replace = TRUE,
    prob = c(0.70, 0.15, 0.15)  # Strong memory: high suspect ID rate
  ),
  confidence = sample(c(20, 40, 60, 80, 100), n_trials, replace = TRUE,
                     prob = c(0.05, 0.10, 0.15, 0.30, 0.40))  # High confidence
)

# Target-absent lineups (innocent suspect)
ta_strong <- data.frame(
  target_present = FALSE,
  identification = sample(
    c("suspect", "filler", "reject"),
    n_trials,
    replace = TRUE,
    prob = c(0.10, 0.30, 0.60)  # Low false ID rate
  ),
  confidence = sample(c(20, 40, 60, 80, 100), n_trials, replace = TRUE,
                     prob = c(0.30, 0.30, 0.20, 0.15, 0.05))  # Lower confidence
)

strong_memory_data <- rbind(tp_strong, ta_strong)

# Generate data for weak memory condition (degraded viewing)
tp_weak <- data.frame(
  target_present = TRUE,
  identification = sample(
    c("suspect", "filler", "reject"),
    n_trials,
    replace = TRUE,
    prob = c(0.35, 0.35, 0.30)  # Weak memory: lower suspect ID rate
  ),
  confidence = sample(c(20, 40, 60, 80, 100), n_trials, replace = TRUE,
                     prob = c(0.25, 0.30, 0.25, 0.15, 0.05))  # Lower confidence
)

ta_weak <- data.frame(
  target_present = FALSE,
  identification = sample(
    c("suspect", "filler", "reject"),
    n_trials,
    replace = TRUE,
    prob = c(0.25, 0.40, 0.35)  # Higher false ID rate
  ),
  confidence = sample(c(20, 40, 60, 80, 100), n_trials, replace = TRUE,
                     prob = c(0.30, 0.30, 0.20, 0.15, 0.05))
)

weak_memory_data <- rbind(tp_weak, ta_weak)

# =============================================================================
# Compute Full ROC Curves
# =============================================================================

cat("\n========================================\n")
cat("STRONG MEMORY CONDITION\n")
cat("========================================\n")

# Using diagnosticity ordering (default)
strong_roc <- make_fullroc(
  strong_memory_data,
  order = "diagnosticity",
  lineup_size = 6,
  show_plot = FALSE
)

print(strong_roc)

cat("\n========================================\n")
cat("WEAK MEMORY CONDITION\n")
cat("========================================\n")

weak_roc <- make_fullroc(
  weak_memory_data,
  order = "diagnosticity",
  lineup_size = 6,
  show_plot = FALSE
)

print(weak_roc)

# =============================================================================
# Compare AUC Values
# =============================================================================

cat("\n========================================\n")
cat("COMPARISON\n")
cat("========================================\n")
cat(sprintf("Strong Memory Full AUC: %.3f\n", strong_roc$auc))
cat(sprintf("Weak Memory Full AUC:   %.3f\n", weak_roc$auc))
cat(sprintf("Difference:             %.3f\n", strong_roc$auc - weak_roc$auc))

if (strong_roc$auc > weak_roc$auc) {
  cat("\nStrong memory condition shows superior discriminability.\n")
  cat("The investigator can better distinguish guilty from innocent suspects\n")
  cat("when eyewitnesses have strong memories.\n")
}

# =============================================================================
# Example 2: Using Custom Confidence Bins
# =============================================================================

cat("\n\n========================================\n")
cat("EXAMPLE WITH CUSTOM CONFIDENCE BINS\n")
cat("========================================\n")

# Create three bins: Low (0-60), Medium (60-80), High (80-100)
strong_roc_binned <- make_fullroc(
  strong_memory_data,
  conf_bins = c(0, 60, 80, 100),
  order = "diagnosticity",
  lineup_size = 6,
  show_plot = FALSE
)

cat("\nUsing confidence bins: Low [0-60), Medium [60-80), High [80-100]\n")
print(strong_roc_binned)

# =============================================================================
# Example 3: Comparing Diagnosticity vs. A-priori Ordering
# =============================================================================

cat("\n\n========================================\n")
cat("COMPARISON: DIAGNOSTICITY vs. A-PRIORI ORDERING\n")
cat("========================================\n")

strong_roc_apriori <- make_fullroc(
  strong_memory_data,
  order = "apriori",
  lineup_size = 6,
  show_plot = FALSE
)

cat(sprintf("Diagnosticity ordering AUC: %.3f\n", strong_roc$auc))
cat(sprintf("A-priori ordering AUC:      %.3f\n", strong_roc_apriori$auc))
cat("\nNote: Both methods should yield similar AUC values if the a-priori ordering\n")
cat("approximates the empirical diagnosticity ordering.\n")

# =============================================================================
# Visualize the Results
# =============================================================================

# Plot both ROC curves for comparison
p1 <- plot_fullroc(strong_roc, title = "Full ROC: Strong Memory")
p2 <- plot_fullroc(weak_roc, title = "Full ROC: Weak Memory")

# Save plots (if needed)
# ggsave("fullroc_strong_memory.png", p1, width = 8, height = 6)
# ggsave("fullroc_weak_memory.png", p2, width = 8, height = 6)

cat("\n\nPlots created successfully!\n")
cat("View with: print(strong_roc$plot) or print(weak_roc$plot)\n")

# =============================================================================
# Example 4: Recreating Smith & Yang (2020) Analysis
# =============================================================================

cat("\n\n========================================\n")
cat("RECREATING SMITH & YANG (2020) ANALYSIS\n")
cat("Example based on their Table 1 data\n")
cat("========================================\n")

# This simulates data similar to their high-similarity lineup example
# (see Table 1 in the paper)

# Create sample data matching their structure
smith_yang_data <- data.frame(
  target_present = c(
    # Suspect IDs, high conf (90-100%)
    rep(TRUE, 13), rep(FALSE, 2),
    # Suspect IDs, medium conf (70-80%)
    rep(TRUE, 13), rep(FALSE, 4),
    # Suspect IDs, low conf (0-60%)
    rep(TRUE, 32), rep(FALSE, 12),
    # Rejections, low conf
    rep(TRUE, 40), rep(FALSE, 49),
    # Filler IDs, medium conf (70-80%)
    rep(TRUE, 17), rep(FALSE, 19),
    # Filler IDs, high conf (90-100%)
    rep(TRUE, 7), rep(FALSE, 8),
    # Filler IDs, low conf
    rep(TRUE, 46), rep(FALSE, 61),
    # Rejections, high conf (90-100%)
    rep(TRUE, 14), rep(FALSE, 20),
    # Rejections, medium conf (70-80%)
    rep(TRUE, 17), rep(FALSE, 25)
  ),
  identification = c(
    rep("suspect", 15),
    rep("suspect", 17),
    rep("suspect", 44),
    rep("reject", 89),
    rep("filler", 36),
    rep("filler", 15),
    rep("filler", 107),
    rep("reject", 34),
    rep("reject", 42)
  ),
  confidence = c(
    rep(95, 15),
    rep(75, 17),
    rep(50, 44),
    rep(50, 89),
    rep(75, 36),
    rep(95, 15),
    rep(50, 107),
    rep(95, 34),
    rep(75, 42)
  )
)

smith_yang_roc <- make_fullroc(
  smith_yang_data,
  conf_bins = c(0, 60, 80, 100),
  order = "diagnosticity",
  lineup_size = 6,
  show_plot = FALSE
)

cat("\nSmith & Yang Style Analysis:\n")
print(smith_yang_roc)

cat("\n\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")
cat("\nKey Insights from Full ROC Analysis:\n")
cat("1. Full ROC uses ALL witness responses (not just suspect IDs)\n")
cat("2. Provides threshold-free measure of discriminability\n")
cat("3. AUC spans full range [0,1] instead of partial range\n")
cat("4. Reflects investigator discriminability, not just witness memory\n")
cat("5. Filler IDs and rejections provide evidence of innocence\n")
cat("\nSee Smith & Yang (2020) for detailed theoretical justification.\n")
