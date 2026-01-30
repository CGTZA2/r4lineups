# pAUC Statistical Comparison Example
# Demonstrates how to statistically compare ROC curves between conditions

library(r4lineups)

# Set seed for reproducibility
set.seed(2026)

# ============================================================================
# Example 1: Compare two simulated conditions with different discriminability
# ============================================================================

cat("Example 1: Comparing high vs. moderate discriminability conditions\n")
cat("====================================================================\n\n")

# Simulate strong discriminability condition
strong_data <- simulate_lineup_data(
  n_tp = 150,
  n_ta = 150,
  d_prime = 2.0,
  c_criterion = 0.5,
  lineup_size = 6,
  conf_levels = 5
)

# Simulate moderate discriminability condition
moderate_data <- simulate_lineup_data(
  n_tp = 150,
  n_ta = 150,
  d_prime = 1.2,
  c_criterion = 0.5,
  lineup_size = 6,
  conf_levels = 5
)

cat("Data generated:\n")
cat("  Strong condition: n =", nrow(strong_data), "\n")
cat("  Moderate condition: n =", nrow(moderate_data), "\n\n")

# Compare pAUC between conditions
cat("Computing pAUC comparison...\n\n")
comparison1 <- compare_pauc(
  strong_data,
  moderate_data,
  lineup_size = 6,
  label1 = "Strong discriminability",
  label2 = "Moderate discriminability",
  n_bootstrap = 1000,  # Use 2000+ for publication
  seed = 123
)

# Print results
print(comparison1)

# Detailed summary
cat("\n")
summary(comparison1)

# Create visualization
cat("\nCreating comparison plot...\n")
plot(comparison1)

# ============================================================================
# Example 2: Compare with specific false ID rate cutoff
# ============================================================================

cat("\n\n")
cat("Example 2: pAUC comparison with specific cutoff\n")
cat("================================================\n\n")

# Use cutoff at 20% false ID rate (common policy threshold)
comparison2 <- compare_pauc(
  strong_data,
  moderate_data,
  lineup_size = 6,
  max_false_id_rate = 0.20,  # 20% false ID rate
  label1 = "Strong",
  label2 = "Moderate",
  n_bootstrap = 1000,
  seed = 456
)

cat("Comparison with 20% false ID rate cutoff:\n")
print(comparison2)

# ============================================================================
# Example 3: Compare conditions with similar discriminability (no difference)
# ============================================================================

cat("\n\n")
cat("Example 3: Comparing conditions with no true difference\n")
cat("========================================================\n\n")

# Simulate two conditions with similar discriminability
condition_a <- simulate_lineup_data(
  n_tp = 120,
  n_ta = 120,
  d_prime = 1.5,
  lineup_size = 6,
  conf_levels = 4
)

condition_b <- simulate_lineup_data(
  n_tp = 120,
  n_ta = 120,
  d_prime = 1.55,  # Very similar to condition A
  lineup_size = 6,
  conf_levels = 4
)

comparison3 <- compare_pauc(
  condition_a,
  condition_b,
  label1 = "Condition A",
  label2 = "Condition B",
  n_bootstrap = 1000,
  seed = 789
)

cat("When conditions are similar, we expect p > 0.05:\n")
print(comparison3)

# ============================================================================
# Example 4: Accessing individual ROC curves
# ============================================================================

cat("\n\n")
cat("Example 4: Accessing individual ROC curves from comparison\n")
cat("===========================================================\n\n")

cat("Condition 1 ROC Data:\n")
print(head(comparison1$roc1$roc_data))

cat("\nCondition 2 ROC Data:\n")
print(head(comparison1$roc2$roc_data))

cat("\nBootstrap distribution summary:\n")
cat("  Condition 1 pAUC: mean =", round(mean(comparison1$bootstrap_results$pauc1_boot), 4),
    ", SD =", round(sd(comparison1$bootstrap_results$pauc1_boot), 4), "\n")
cat("  Condition 2 pAUC: mean =", round(mean(comparison1$bootstrap_results$pauc2_boot), 4),
    ", SD =", round(sd(comparison1$bootstrap_results$pauc2_boot), 4), "\n")
cat("  Difference: mean =", round(mean(comparison1$bootstrap_results$diff_boot), 4),
    ", SD =", round(sd(comparison1$bootstrap_results$diff_boot), 4), "\n")

# ============================================================================
# Example 5: Visualize bootstrap distributions
# ============================================================================

cat("\n\n")
cat("Example 5: Visualizing bootstrap distributions\n")
cat("===============================================\n\n")

# Create histogram of bootstrap differences
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  boot_diff_df <- data.frame(
    diff = comparison1$bootstrap_results$diff_boot
  )

  p_hist <- ggplot(boot_diff_df, aes(x = diff)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = comparison1$pauc_diff, color = "red", size = 1, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "gray50", linetype = "dotted") +
    theme_bw() +
    labs(
      title = "Bootstrap Distribution of pAUC Difference",
      x = "pAUC Difference (Condition 1 - Condition 2)",
      y = "Frequency",
      caption = paste0("Red line = observed difference (", round(comparison1$pauc_diff, 4), ")\n",
                      "Gray line = null hypothesis (difference = 0)")
    )

  print(p_hist)
  cat("\nBootstrap distribution plotted.\n")
}

# ============================================================================
# Example 6: Multiple comparisons with adjustment
# ============================================================================

cat("\n\n")
cat("Example 6: Multiple pairwise comparisons\n")
cat("=========================================\n\n")

# Simulate three conditions
cond1_data <- simulate_lineup_data(n_tp = 100, n_ta = 100, d_prime = 2.0, lineup_size = 6)
cond2_data <- simulate_lineup_data(n_tp = 100, n_ta = 100, d_prime = 1.5, lineup_size = 6)
cond3_data <- simulate_lineup_data(n_tp = 100, n_ta = 100, d_prime = 1.0, lineup_size = 6)

# Perform all pairwise comparisons
cat("Comparing all pairs...\n\n")

comp_1v2 <- compare_pauc(cond1_data, cond2_data,
                         label1 = "High", label2 = "Medium",
                         n_bootstrap = 500, seed = 111)

comp_1v3 <- compare_pauc(cond1_data, cond3_data,
                         label1 = "High", label2 = "Low",
                         n_bootstrap = 500, seed = 222)

comp_2v3 <- compare_pauc(cond2_data, cond3_data,
                         label1 = "Medium", label2 = "Low",
                         n_bootstrap = 500, seed = 333)

# Create summary table
comparison_table <- data.frame(
  Comparison = c("High vs Medium", "High vs Low", "Medium vs Low"),
  pAUC_diff = c(comp_1v2$pauc_diff, comp_1v3$pauc_diff, comp_2v3$pauc_diff),
  Z = c(comp_1v2$z_score, comp_1v3$z_score, comp_2v3$z_score),
  p_value = c(comp_1v2$p_value, comp_1v3$p_value, comp_2v3$p_value)
)

# Apply Bonferroni correction
comparison_table$p_adjusted <- p.adjust(comparison_table$p_value, method = "bonferroni")

cat("Multiple comparison results:\n")
print(comparison_table)
cat("\nNote: p_adjusted uses Bonferroni correction for 3 comparisons\n")

# ============================================================================
# Example 7: Real data example (if nortje2012 dataset available)
# ============================================================================

cat("\n\n")
cat("Example 7: Real data example\n")
cat("=============================\n\n")

# Check if nortje2012 dataset is available
if (exists("nortje2012")) {
  # Split by a condition if available (for demonstration)
  cat("Using nortje2012 dataset...\n")

  # For this example, we'll split randomly to demonstrate the function
  # In real analysis, you'd split by actual experimental conditions
  set.seed(999)
  n_total <- nrow(nortje2012)
  group1_idx <- sample(1:n_total, n_total/2)
  group2_idx <- setdiff(1:n_total, group1_idx)

  group1_data <- nortje2012[group1_idx, ]
  group2_data <- nortje2012[group2_idx, ]

  real_comparison <- compare_pauc(
    group1_data,
    group2_data,
    label1 = "Group 1",
    label2 = "Group 2",
    n_bootstrap = 1000
  )

  print(real_comparison)
  plot(real_comparison)
} else {
  cat("nortje2012 dataset not available. Skipping this example.\n")
}

# ============================================================================
# Example 8: Interpreting results
# ============================================================================

cat("\n\n")
cat("Example 8: Interpretation guide\n")
cat("================================\n\n")

cat("How to interpret pAUC comparison results:\n\n")

cat("1. pAUC Difference:\n")
cat("   - Positive: Condition 1 has better discriminability\n")
cat("   - Negative: Condition 2 has better discriminability\n")
cat("   - Near zero: Similar discriminability\n\n")

cat("2. Z-score:\n")
cat("   - |Z| > 1.96: Significant at α = 0.05 (two-tailed)\n")
cat("   - |Z| > 2.58: Significant at α = 0.01 (two-tailed)\n")
cat("   - |Z| > 3.29: Significant at α = 0.001 (two-tailed)\n\n")

cat("3. P-value:\n")
cat("   - p < 0.05: Significant difference between conditions\n")
cat("   - p ≥ 0.05: No significant difference\n")
cat("   - Always report exact p-value (not just p < 0.05)\n\n")

cat("4. Confidence Interval:\n")
cat("   - If CI does not include 0: Significant difference\n")
cat("   - If CI includes 0: No significant difference\n")
cat("   - Width of CI indicates precision of estimate\n\n")

cat("5. Effect Size:\n")
cat("   - Small effect: d ≈ 0.2\n")
cat("   - Medium effect: d ≈ 0.5\n")
cat("   - Large effect: d ≈ 0.8\n")
cat("   - (Cohen's d equivalent, shown in summary())\n\n")

cat("6. Bootstrap Samples:\n")
cat("   - Use n_bootstrap = 2000+ for publication\n")
cat("   - Use n_bootstrap = 500-1000 for exploratory analysis\n")
cat("   - More samples = more stable standard errors\n\n")

cat("7. False ID Rate Cutoff:\n")
cat("   - Default: uses maximum observed across conditions\n")
cat("   - Policy-relevant: 0.05 (5%), 0.10 (10%), 0.20 (20%)\n")
cat("   - Choose based on acceptable false ID rate in practice\n\n")

# ============================================================================
# Summary
# ============================================================================

cat("\n")
cat("========================================\n")
cat("pAUC Comparison Example Complete\n")
cat("========================================\n\n")

cat("Key functions demonstrated:\n")
cat("  - compare_pauc(): Statistical comparison of ROC curves\n")
cat("  - print.pauc_comparison(): Display comparison results\n")
cat("  - summary.pauc_comparison(): Detailed summary with effect sizes\n")
cat("  - plot.pauc_comparison(): Visualization of ROC curves\n\n")

cat("Key features:\n")
cat("  - Bootstrap-based standard errors\n")
cat("  - Z-test for pAUC differences\n")
cat("  - Confidence intervals for differences\n")
cat("  - Customizable false ID rate cutoffs\n")
cat("  - Publication-ready plots\n\n")

cat("For more information:\n")
cat("  ?compare_pauc\n")
cat("  ?plot.pauc_comparison\n\n")

cat("References:\n")
cat("  - Mickes et al. (2024). pyWitness 1.0. Behavior Research Methods.\n")
cat("  - Wixted & Mickes (2012). ROC analysis. Perspectives on Psych Science.\n\n")
