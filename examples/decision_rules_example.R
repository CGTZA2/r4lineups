# ============================================================================
# Decision Rules in Lineup Simulation
# ============================================================================
#
# This script demonstrates the four decision rules available in r4lineups
# for simulating eyewitness lineup identification data:
#
#   1. MAX (Independent Observations Model) - Default
#   2. BEST-REST Model
#   3. Ensemble Model
#   4. Integration Model
#
# Based on Wixted et al. (2018) and implemented in pyWitness
# ============================================================================

library(r4lineups)

set.seed(123)

# ============================================================================
# Example 1: Compare All Four Decision Rules
# ============================================================================

cat("\n=== Example 1: Comparing Decision Rules ===\n\n")

# Simulate data with each decision rule
data_max <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.5,
  decision_rule = "max",
  conf_levels = 5,
  seed = 123
)

data_best_rest <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.5,
  decision_rule = "best_rest",
  conf_levels = 5,
  seed = 123
)

data_ensemble <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.5,
  decision_rule = "ensemble",
  conf_levels = 5,
  seed = 123
)

data_integration <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.5,
  decision_rule = "integration",
  conf_levels = 5,
  seed = 123
)

# Compare ROC performance
roc_max <- make_roc(data_max, lineup_size = 6, show_plot = FALSE)
roc_best_rest <- make_roc(data_best_rest, lineup_size = 6, show_plot = FALSE)
roc_ensemble <- make_roc(data_ensemble, lineup_size = 6, show_plot = FALSE)
roc_integration <- make_roc(data_integration, lineup_size = 6, show_plot = FALSE)

cat("ROC Performance by Decision Rule:\n")
cat(sprintf("  MAX:         pAUC = %.3f, d' = %.3f\n",
            roc_max$pauc, roc_max$dprime))
cat(sprintf("  BEST-REST:   pAUC = %.3f, d' = %.3f\n",
            roc_best_rest$pauc, roc_best_rest$dprime))
cat(sprintf("  Ensemble:    pAUC = %.3f, d' = %.3f\n",
            roc_ensemble$pauc, roc_ensemble$dprime))
cat(sprintf("  Integration: pAUC = %.3f, d' = %.3f\n",
            roc_integration$pauc, roc_integration$dprime))


# ============================================================================
# Example 2: MAX Rule (Independent Observations Model)
# ============================================================================

cat("\n\n=== Example 2: MAX Rule (Default) ===\n\n")

# This is the traditional independent observations model
# Witness evaluates each lineup member independently
# Chooses the member with the highest memory strength

max_data <- simulate_lineup_data(
  n_tp = 300, n_ta = 300,
  d_prime = 2.0,
  decision_rule = "max",
  conf_levels = 5,
  seed = 456
)

print(max_data)

cat("\nInterpretation:\n")
cat("  - Each lineup member evaluated independently\n")
cat("  - Highest strength chosen (if above criterion)\n")
cat("  - Most common model in the literature\n")


# ============================================================================
# Example 3: BEST-REST Model
# ============================================================================

cat("\n\n=== Example 3: BEST-REST Model ===\n\n")

# BEST-REST compares the best match against the average of remaining members
# Decision variable = best_strength - mean(other_strengths)
# This represents a relative comparison strategy

best_rest_data <- simulate_lineup_data(
  n_tp = 300, n_ta = 300,
  d_prime = 2.0,
  decision_rule = "best_rest",
  conf_levels = 5,
  seed = 456
)

cat("BEST-REST Model:\n")
cat("  - Compares best match vs average of rest\n")
cat("  - Decision = best - mean(others)\n")
cat("  - Accounts for relative comparisons\n")

# Compare identification rates
max_tp <- sum(max_data$target_present & max_data$identification == "suspect")
best_rest_tp <- sum(best_rest_data$target_present & best_rest_data$identification == "suspect")

cat(sprintf("\nSuspect IDs (Target-Present):\n"))
cat(sprintf("  MAX:       %d / 300 = %.1f%%\n", max_tp, 100 * max_tp / 300))
cat(sprintf("  BEST-REST: %d / 300 = %.1f%%\n", best_rest_tp, 100 * best_rest_tp / 300))


# ============================================================================
# Example 4: Ensemble Model
# ============================================================================

cat("\n\n=== Example 4: Ensemble Model ===\n\n")

# Ensemble model uses ensemble coding - averaging across lineup members
# Based on research showing people automatically compute ensemble statistics
# Decision variable incorporates information from all members

ensemble_data <- simulate_lineup_data(
  n_tp = 300, n_ta = 300,
  d_prime = 2.0,
  decision_rule = "ensemble",
  conf_levels = 5,
  seed = 456
)

cat("Ensemble Model:\n")
cat("  - Averages memory strength across lineup\n")
cat("  - Based on ensemble coding research\n")
cat("  - Holistic evaluation strategy\n")

# Compare with MAX
ensemble_tp <- sum(ensemble_data$target_present & ensemble_data$identification == "suspect")

cat(sprintf("\nSuspect IDs (Target-Present):\n"))
cat(sprintf("  MAX:      %d / 300 = %.1f%%\n", max_tp, 100 * max_tp / 300))
cat(sprintf("  Ensemble: %d / 300 = %.1f%%\n", ensemble_tp, 100 * ensemble_tp / 300))


# ============================================================================
# Example 5: Integration Model
# ============================================================================

cat("\n\n=== Example 5: Integration Model ===\n\n")

# Integration model sums memory strengths across all lineup members
# Represents complete integration of evidence
# Different from MAX which uses only the strongest signal

integration_data <- simulate_lineup_data(
  n_tp = 300, n_ta = 300,
  d_prime = 2.0,
  decision_rule = "integration",
  conf_levels = 5,
  seed = 456
)

cat("Integration Model:\n")
cat("  - Sums memory strength across all members\n")
cat("  - Complete evidence integration\n")
cat("  - Accounts for cumulative signal\n")

# Compare with MAX
integration_tp <- sum(integration_data$target_present & integration_data$identification == "suspect")

cat(sprintf("\nSuspect IDs (Target-Present):\n"))
cat(sprintf("  MAX:         %d / 300 = %.1f%%\n", max_tp, 100 * max_tp / 300))
cat(sprintf("  Integration: %d / 300 = %.1f%%\n", integration_tp, 100 * integration_tp / 300))


# ============================================================================
# Example 6: Decision Rules Across Different d' Values
# ============================================================================

cat("\n\n=== Example 6: Performance Across Memory Strength ===\n\n")

# Test how different decision rules perform with varying memory quality

d_primes <- c(0.5, 1.0, 1.5, 2.0, 2.5)
rules <- c("max", "best_rest", "ensemble", "integration")

results <- expand.grid(
  d_prime = d_primes,
  rule = rules,
  pauc = NA,
  stringsAsFactors = FALSE
)

for (i in 1:nrow(results)) {
  data <- simulate_lineup_data(
    n_tp = 200, n_ta = 200,
    d_prime = results$d_prime[i],
    decision_rule = results$rule[i],
    conf_levels = 5,
    seed = 789
  )

  roc <- make_roc(data, lineup_size = 6, show_plot = FALSE)
  results$pauc[i] <- roc$pauc
}

cat("pAUC by d' and Decision Rule:\n\n")
for (dp in d_primes) {
  cat(sprintf("d' = %.1f:\n", dp))
  for (rule in rules) {
    pauc <- results$pauc[results$d_prime == dp & results$rule == rule]
    cat(sprintf("  %-12s: %.3f\n", rule, pauc))
  }
  cat("\n")
}


# ============================================================================
# Example 7: Comparing CAC Curves
# ============================================================================

cat("\n=== Example 7: Confidence-Accuracy Characteristic (CAC) ===\n\n")

# Compare how decision rules affect the CAC
# CAC shows accuracy at each confidence level

# Use the data from Example 1
cac_max <- make_cac(data_max, lineup_size = 6, show_plot = FALSE)
cac_best_rest <- make_cac(data_best_rest, lineup_size = 6, show_plot = FALSE)
cac_ensemble <- make_cac(data_ensemble, lineup_size = 6, show_plot = FALSE)

cat("CAC Analysis:\n")
cat("  Different decision rules may produce different confidence patterns\n")
cat("  Ensemble/Integration models may show altered calibration\n\n")

cat("Suspect ID Accuracy by Rule:\n")
cat(sprintf("  MAX:       %.3f\n", cac_max$suspect_id_accuracy))
cat(sprintf("  BEST-REST: %.3f\n", cac_best_rest$suspect_id_accuracy))
cat(sprintf("  Ensemble:  %.3f\n", cac_ensemble$suspect_id_accuracy))


# ============================================================================
# Example 8: Power Analysis Across Decision Rules
# ============================================================================

cat("\n\n=== Example 8: Power Analysis ===\n\n")

# How much power do we have to detect differences between decision rules?

cat("Running power analysis (this may take a moment)...\n")

# Simplified power analysis - compare MAX vs BEST-REST
n_sims <- 100  # Reduced for speed; use 1000+ for real analysis

pauc_diffs <- numeric(n_sims)

for (i in 1:n_sims) {
  # Simulate with MAX
  d1 <- simulate_lineup_data(
    n_tp = 100, n_ta = 100,
    d_prime = 1.5,
    decision_rule = "max",
    conf_levels = 5
  )

  # Simulate with BEST-REST
  d2 <- simulate_lineup_data(
    n_tp = 100, n_ta = 100,
    d_prime = 1.5,
    decision_rule = "best_rest",
    conf_levels = 5
  )

  r1 <- make_roc(d1, lineup_size = 6, show_plot = FALSE)
  r2 <- make_roc(d2, lineup_size = 6, show_plot = FALSE)

  pauc_diffs[i] <- r1$pauc - r2$pauc
}

cat(sprintf("\nMAX vs BEST-REST (n=100 per condition, %d simulations):\n", n_sims))
cat(sprintf("  Mean pAUC difference: %.4f\n", mean(pauc_diffs)))
cat(sprintf("  SD of difference:     %.4f\n", sd(pauc_diffs)))
cat(sprintf("  95%% CI: [%.4f, %.4f]\n",
            quantile(pauc_diffs, 0.025),
            quantile(pauc_diffs, 0.975)))


# ============================================================================
# Summary and Recommendations
# ============================================================================

cat("\n\n=== Summary ===\n\n")

cat("Decision Rules Implemented:\n\n")

cat("1. MAX (Independent Observations)\n")
cat("   - Traditional model, most common in literature\n")
cat("   - Each member evaluated independently\n")
cat("   - Choose highest strength\n\n")

cat("2. BEST-REST\n")
cat("   - Compare best vs average of rest\n")
cat("   - Captures relative comparison process\n")
cat("   - May better match witness strategies\n\n")

cat("3. Ensemble\n")
cat("   - Average memory across lineup\n")
cat("   - Based on ensemble coding research\n")
cat("   - Holistic evaluation\n\n")

cat("4. Integration\n")
cat("   - Sum all memory strengths\n")
cat("   - Complete evidence integration\n")
cat("   - Accounts for cumulative signal\n\n")

cat("When to use each:\n")
cat("  - MAX: Default, comparison with literature\n")
cat("  - BEST-REST: Testing relative comparison strategies\n")
cat("  - Ensemble: Exploring holistic processing\n")
cat("  - Integration: Testing evidence accumulation models\n\n")

cat("References:\n")
cat("  Wixted et al. (2018). Models of lineup memory. Cognitive Psychology.\n")
cat("  Mickes et al. (2024). pyWitness 1.0. Behavior Research Methods.\n")
cat("  Clark (2003). WITNESS model. Psychonomic Bulletin & Review.\n")

cat("\n=== Examples Complete ===\n")
