# z-ROC and SDT Parameter Estimation Examples
# ===========================================
#
# This script demonstrates how to use fit_sdt_roc() to extract signal detection
# theory (SDT) parameters from ROC data.

library(r4lineups)

# Example 1: Basic SDT Parameter Estimation
# ------------------------------------------

cat("\n=== Example 1: Basic SDT Fit ===\n\n")

# Simulate data with known d' = 1.5
set.seed(123)
sim_data <- simulate_lineup_data(
  n_tp = 200,
  n_ta = 200,
  d_prime = 1.5,
  conf_levels = 5,
  lineup_size = 6
)

# Fit equal-variance SDT model
sdt_fit <- fit_sdt_roc(sim_data, lineup_size = 6)
print(sdt_fit)

# The estimated d' should be close to the true value (1.5)
cat("\nTrue d' = 1.5, Estimated d' =", round(sdt_fit$dprime, 3), "\n")


# Example 2: Visualizing the z-ROC
# ---------------------------------

cat("\n\n=== Example 2: z-ROC Visualization ===\n\n")

# Plot the z-ROC with fitted line
plot(sdt_fit)

# Points should fall roughly on a straight line
# Red line = fitted model
# Dashed gray line = reference (slope = 1, intercept = 0)


# Example 3: Equal vs Unequal Variance Models
# --------------------------------------------

cat("\n\n=== Example 3: Model Comparison ===\n\n")

# Fit both models
sdt_equal <- fit_sdt_roc(
  sim_data,
  lineup_size = 6,
  model = "equal_variance",
  bootstrap = TRUE,
  n_bootstrap = 500
)

sdt_unequal <- fit_sdt_roc(
  sim_data,
  lineup_size = 6,
  model = "unequal_variance",
  bootstrap = TRUE,
  n_bootstrap = 500
)

cat("Equal Variance Model:\n")
cat("  d' =", round(sdt_equal$dprime, 3), "\n")
cat("  Slope =", round(sdt_equal$slope, 3), "(fixed at 1.0)\n")
cat("  R² =", round(sdt_equal$fit_diagnostics$r_squared, 3), "\n\n")

cat("Unequal Variance Model:\n")
cat("  d' =", round(sdt_unequal$dprime, 3), "\n")
cat("  Slope =", round(sdt_unequal$slope, 3), "\n")
cat("  Variance Ratio =", round(sdt_unequal$variance_ratio, 3), "\n")
cat("  R² =", round(sdt_unequal$fit_diagnostics$r_squared, 3), "\n\n")

# If variance ratio ≈ 1 and R² similar, equal variance model is appropriate
cat("Interpretation:\n")
if (abs(sdt_unequal$variance_ratio - 1) < 0.2) {
  cat("  Variance ratio close to 1 → Equal variance model appropriate\n")
} else {
  cat("  Variance ratio differs from 1 → Consider unequal variance model\n")
}


# Example 4: Parameter Recovery Test
# -----------------------------------

cat("\n\n=== Example 4: Parameter Recovery ===\n\n")

# Test if SDT estimation recovers known parameters
true_dprimes <- c(0.5, 1.0, 1.5, 2.0, 2.5)
estimated_dprimes <- numeric(length(true_dprimes))

for (i in seq_along(true_dprimes)) {
  sim <- simulate_lineup_data(
    n_tp = 300,
    n_ta = 300,
    d_prime = true_dprimes[i],
    conf_levels = 5,
    lineup_size = 6,
    seed = 100 + i
  )

  fit <- fit_sdt_roc(sim, lineup_size = 6, bootstrap = FALSE)
  estimated_dprimes[i] <- fit$dprime
}

recovery_df <- data.frame(
  True_dprime = true_dprimes,
  Estimated_dprime = round(estimated_dprimes, 3),
  Difference = round(estimated_dprimes - true_dprimes, 3)
)

cat("Parameter Recovery Test:\n")
print(recovery_df, row.names = FALSE)

cat("\nCorrelation between true and estimated d':",
    round(cor(true_dprimes, estimated_dprimes), 3), "\n")


# Example 5: Decision Criteria Analysis
# --------------------------------------

cat("\n\n=== Example 5: Decision Criteria ===\n\n")

# Fit model to examine decision criteria
sdt_fit <- fit_sdt_roc(sim_data, lineup_size = 6, bootstrap = FALSE)

cat("Decision Criteria (c values):\n")
print(data.frame(
  Criterion = names(sdt_fit$criteria),
  Value = round(sdt_fit$criteria, 3),
  Interpretation = ifelse(
    sdt_fit$criteria > 0.5, "Conservative",
    ifelse(sdt_fit$criteria < -0.5, "Liberal", "Neutral")
  )
), row.names = FALSE)

cat("\nMean criterion:", round(mean(sdt_fit$criteria), 3), "\n")
cat("  - Positive = conservative (requires more evidence for ID)\n")
cat("  - Negative = liberal (willing to ID with less evidence)\n")
cat("  - Near zero = unbiased\n")


# Example 6: Comparing Conditions
# --------------------------------

cat("\n\n=== Example 6: Comparing Experimental Conditions ===\n\n")

# Simulate two conditions with different discriminability
condition1 <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.8,  # Better memory
  conf_levels = 5,
  lineup_size = 6,
  seed = 201
)

condition2 <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 1.2,  # Worse memory
  conf_levels = 5,
  lineup_size = 6,
  seed = 202
)

# Fit SDT models
fit1 <- fit_sdt_roc(condition1, lineup_size = 6, n_bootstrap = 500)
fit2 <- fit_sdt_roc(condition2, lineup_size = 6, n_bootstrap = 500)

cat("Condition 1 (Strong Memory):\n")
cat("  d' =", round(fit1$dprime, 3),
    "  95% CI: [", round(fit1$bootstrap_ci$dprime_ci[1], 3), ",",
    round(fit1$bootstrap_ci$dprime_ci[2], 3), "]\n\n")

cat("Condition 2 (Weak Memory):\n")
cat("  d' =", round(fit2$dprime, 3),
    "  95% CI: [", round(fit2$bootstrap_ci$dprime_ci[1], 3), ",",
    round(fit2$bootstrap_ci$dprime_ci[2], 3), "]\n\n")

cat("Difference in d':", round(fit1$dprime - fit2$dprime, 3), "\n")

# Check if CIs overlap
if (fit1$bootstrap_ci$dprime_ci[1] > fit2$bootstrap_ci$dprime_ci[2]) {
  cat("CIs do not overlap → Significant difference\n")
} else if (fit2$bootstrap_ci$dprime_ci[1] > fit1$bootstrap_ci$dprime_ci[2]) {
  cat("CIs do not overlap → Significant difference\n")
} else {
  cat("CIs overlap → May not be significantly different\n")
}


# Example 7: Integration with Other r4lineups Analyses
# -----------------------------------------------------

cat("\n\n=== Example 7: Integration with ROC Analysis ===\n\n")

# Generate data
sim <- simulate_lineup_data(n_tp = 150, n_ta = 150, d_prime = 1.5, conf_levels = 5)

# Standard ROC analysis
roc <- make_roc(sim, lineup_size = 6, show_plot = FALSE)

# SDT analysis on same data
sdt <- fit_sdt_roc(sim, lineup_size = 6, bootstrap = FALSE)

cat("ROC Analysis:\n")
cat("  pAUC (0-0.20) =", round(roc$pauc, 3), "\n")
cat("  Overall discriminability\n\n")

cat("SDT Analysis:\n")
cat("  d' =", round(sdt$dprime, 3), "\n")
cat("  Parametric discriminability estimate\n")
cat("  Assumes normal distributions\n\n")

cat("Both metrics indicate discriminability but use different frameworks:\n")
cat("  - pAUC: Non-parametric, policy-relevant (focuses on low FAR region)\n")
cat("  - d': Parametric, theoretical (SDT framework)\n")


# Example 8: Detailed Summary Statistics
# ---------------------------------------

cat("\n\n=== Example 8: Comprehensive Summary ===\n\n")

# Fit model with all options
sdt_full <- fit_sdt_roc(
  sim_data,
  lineup_size = 6,
  model = "equal_variance",
  bootstrap = TRUE,
  n_bootstrap = 1000,
  conf_level = 0.95,
  seed = 999
)

# Get detailed summary
summary(sdt_full)


# Interpretation Guidelines
# -------------------------

cat("\n\n=== Interpretation Guidelines ===\n\n")

cat("d' (Discriminability):\n")
cat("  0.0 - 0.5: Poor discriminability\n")
cat("  0.5 - 1.0: Weak discriminability\n")
cat("  1.0 - 2.0: Moderate discriminability\n")
cat("  2.0 - 3.0: Good discriminability\n")
cat("  > 3.0: Excellent discriminability\n\n")

cat("c (Decision Criterion):\n")
cat("  < -0.5: Liberal (low threshold for IDs)\n")
cat("  -0.5 to 0.5: Neutral/Unbiased\n")
cat("  > 0.5: Conservative (high threshold for IDs)\n\n")

cat("Variance Ratio:\n")
cat("  = 1.0: Equal variance (standard SDT assumption)\n")
cat("  > 1.0: Lure distribution has greater variability\n")
cat("  < 1.0: Target distribution has greater variability\n\n")

cat("R-squared:\n")
cat("  > 0.90: Excellent fit (data well described by SDT)\n")
cat("  0.70 - 0.90: Good fit\n")
cat("  0.50 - 0.70: Moderate fit\n")
cat("  < 0.50: Poor fit (SDT may not be appropriate)\n\n")


cat("\n=== Examples Complete ===\n")
cat("\nKey Functions:\n")
cat("  fit_sdt_roc()      - Fit SDT model to ROC data\n")
cat("  print()            - Display parameter estimates\n")
cat("  plot()             - Visualize z-ROC with fitted line\n")
cat("  summary()          - Detailed model diagnostics\n\n")
