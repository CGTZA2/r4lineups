# Model Comparison Example
# Demonstrates how to fit and compare multiple eyewitness identification models

library(r4lineups)

# Set seed for reproducibility
set.seed(2026)

# ============================================================================
# Example 1: Compare all models with simulated data
# ============================================================================

cat("Example 1: Comparing all models with simulated data\n")
cat("======================================================\n\n")

# Simulate strong discriminability data
sim_data <- simulate_lineup_data(
  n_tp = 200,
  n_ta = 200,
  d_prime = 2.0,
  c_criterion = 0.5,
  lineup_size = 6,
  conf_levels = 5,
  include_response_time = FALSE
)

cat("Simulated data:\n")
cat("  Target-present:", sum(sim_data$target_present), "\n")
cat("  Target-absent:", sum(!sim_data$target_present), "\n")
cat("  Suspect IDs:", sum(sim_data$identification == "suspect"), "\n")
cat("  Filler IDs:", sum(sim_data$identification == "filler"), "\n")
cat("  Rejections:", sum(sim_data$identification == "reject"), "\n\n")

# Compare all models
cat("Fitting and comparing all models...\n\n")
comparison <- compare_models(
  sim_data,
  models = c("2ht", "eig", "fullroc"),
  lineup_size = 6,
  prior_guilt = 0.5
)

# Print comparison
print(comparison)

# Detailed summary
cat("\n\n")
summary(comparison)

# ============================================================================
# Example 2: Compare specific models with real data
# ============================================================================

cat("\n\n")
cat("Example 2: Comparing 2-HT and EIG models with example data\n")
cat("============================================================\n\n")

# Use the nortje2012 dataset (if available)
data("nortje2012", package = "r4lineups")

cat("Using nortje2012 dataset\n")
cat("  Sample size:", nrow(nortje2012), "\n\n")

# Compare 2-HT and EIG only
comparison2 <- compare_models(
  nortje2012,
  models = c("2ht", "eig"),
  lineup_size = 6,
  prior_guilt = 0.5
)

print(comparison2)

# ============================================================================
# Example 3: Accessing individual model fits
# ============================================================================

cat("\n\n")
cat("Example 3: Accessing individual fitted models\n")
cat("==============================================\n\n")

# Access individual models
if ("2ht" %in% comparison$models_fit) {
  cat("2-HT Model:\n")
  print(comparison$fitted_models$`2ht`)
  cat("\n")
}

if ("eig" %in% comparison$models_fit) {
  cat("\nEIG Model:\n")
  print(comparison$fitted_models$eig)
  cat("\n")
}

if ("fullroc" %in% comparison$models_fit) {
  cat("\nFull ROC Model:\n")
  print(comparison$fitted_models$fullroc)
  cat("\n")
}

# ============================================================================
# Example 4: Visualizing model comparisons
# ============================================================================

cat("\n\n")
cat("Example 4: Creating side-by-side comparison plots\n")
cat("==================================================\n\n")

# Create comparison plot
cat("Creating comparison plots...\n")
plot(comparison, ncol = 2)

cat("\nPlot created successfully!\n")

# Individual plots can also be accessed
if ("eig" %in% comparison$models_fit) {
  cat("\nCreating EIG information gain plot...\n")
  plot_eig(comparison$fitted_models$eig)
}

if ("fullroc" %in% comparison$models_fit) {
  cat("\nCreating Full ROC plot...\n")
  plot_fullroc(comparison$fitted_models$fullroc, show_auc = TRUE)
}

# ============================================================================
# Example 5: Formatted comparison tables
# ============================================================================

cat("\n\n")
cat("Example 5: Creating formatted comparison tables\n")
cat("================================================\n\n")

# Console format
cat("Console format:\n")
print(format_comparison_table(comparison, format = "console"))

# Markdown format (if knitr available)
if (requireNamespace("knitr", quietly = TRUE)) {
  cat("\n\nMarkdown format:\n")
  print(format_comparison_table(comparison, format = "markdown"))
}

# ============================================================================
# Example 6: Comparing conditions with binned confidence
# ============================================================================

cat("\n\n")
cat("Example 6: Model comparison with confidence binning\n")
cat("====================================================\n\n")

# Create confidence bins: low (0-40), medium (40-70), high (70-100)
confidence_bins <- c(0, 40, 70, 100)

comparison_binned <- compare_models(
  sim_data,
  models = c("eig", "fullroc"),
  lineup_size = 6,
  prior_guilt = 0.5,
  confidence_bins = confidence_bins
)

cat("Comparison with binned confidence:\n")
print(comparison_binned)

# ============================================================================
# Example 7: Interpreting model results
# ============================================================================

cat("\n\n")
cat("Example 7: Interpreting model comparison results\n")
cat("=================================================\n\n")

cat("Model Interpretation Guide:\n\n")

cat("1. 2-HT Model (Winter et al., 2022):\n")
cat("   - dP (detection presence): Ability to recognize guilty suspect (0-1)\n")
cat("   - dA (detection absence): Ability to detect innocent suspect (0-1)\n")
cat("   - b (bias): Tendency for suspect to stand out regardless of guilt (0-1)\n")
cat("   - g (guessing): Probability of making a selection vs rejection (0-1)\n")
cat("   - Lower AIC/BIC = better model fit\n\n")

cat("2. EIG (Starns et al., 2023):\n")
cat("   - EIG (bits): Information provided about guilt vs innocence\n")
cat("   - Higher values = more diagnostic procedure\n")
cat("   - Maximum possible = 1 bit (when prior = 0.5)\n")
cat("   - Information efficiency: Percentage of maximum information gained\n\n")

cat("3. Full ROC (Smith & Yang, 2020):\n")
cat("   - AUC: Investigator discriminability (ability to distinguish guilty/innocent)\n")
cat("   - Range: 0.5 (chance) to 1.0 (perfect discrimination)\n")
cat("   - Includes ALL witness responses (suspect, filler, reject)\n")
cat("   - Higher AUC = better investigator performance\n\n")

cat("Model Selection:\n")
cat("  - Each model provides unique insights\n")
cat("  - 2-HT: Process-based model for understanding latent mechanisms\n")
cat("  - EIG: Information-theoretic measure of evidential value\n")
cat("  - Full ROC: Threshold-free measure of discriminability\n")
cat("  - Consider reporting multiple models for comprehensive analysis\n\n")

# ============================================================================
# Example 8: Power analysis with model comparison
# ============================================================================

cat("\n\n")
cat("Example 8: Model comparison across different sample sizes\n")
cat("==========================================================\n\n")

# Compare models with different discriminability levels
cat("Comparing models for weak discriminability (d' = 1.0):\n")
sim_weak <- simulate_lineup_data(
  n_tp = 150, n_ta = 150,
  d_prime = 1.0,
  lineup_size = 6
)
comparison_weak <- compare_models(sim_weak, show_warnings = FALSE)
cat("  EIG:", round(comparison_weak$fitted_models$eig$eig, 4), "bits\n")
cat("  Full AUC:", round(comparison_weak$fitted_models$fullroc$auc, 3), "\n")
cat("  2-HT dP:", round(comparison_weak$fitted_models$`2ht`$parameters["dP"], 3), "\n\n")

cat("Comparing models for strong discriminability (d' = 2.5):\n")
sim_strong <- simulate_lineup_data(
  n_tp = 150, n_ta = 150,
  d_prime = 2.5,
  lineup_size = 6
)
comparison_strong <- compare_models(sim_strong, show_warnings = FALSE)
cat("  EIG:", round(comparison_strong$fitted_models$eig$eig, 4), "bits\n")
cat("  Full AUC:", round(comparison_strong$fitted_models$fullroc$auc, 3), "\n")
cat("  2-HT dP:", round(comparison_strong$fitted_models$`2ht`$parameters["dP"], 3), "\n\n")

cat("As expected, stronger discriminability (d') leads to:\n")
cat("  - Higher EIG (more information)\n")
cat("  - Higher Full ROC AUC (better discriminability)\n")
cat("  - Higher 2-HT dP (better detection)\n\n")

# ============================================================================
# Summary
# ============================================================================

cat("\n")
cat("========================================\n")
cat("Model Comparison Example Complete\n")
cat("========================================\n\n")

cat("Key functions demonstrated:\n")
cat("  - compare_models(): Main function for fitting multiple models\n")
cat("  - print.model_comparison(): Display comparison results\n")
cat("  - summary.model_comparison(): Detailed model summaries\n")
cat("  - plot.model_comparison(): Side-by-side visualizations\n")
cat("  - format_comparison_table(): Create formatted tables\n\n")

cat("For more information:\n")
cat("  ?compare_models\n")
cat("  ?fit_winter_2ht\n")
cat("  ?compute_eig\n")
cat("  ?make_fullroc\n\n")
