# Testing Script for Winter et al. (2022) 2-HT MPT Model Implementation
#
# This script tests the implementation using data from Table 2 of Winter et al. (2022)

library(r4lineups)

# ==============================================================================
# Experiment 1: Detection of culprit presence (dP manipulation)
# Manipulation: Exposure duration (long vs. short)
# ==============================================================================

cat("\n=== EXPERIMENT 1: CULPRIT PRESENCE DETECTION (dP) ===\n\n")

# Long exposure, simultaneous
exp1_long_sim <- c(
  n_tp_suspect = 147,
  n_tp_filler = 94,
  n_tp_reject = 141,
  n_ta_suspect = 38,
  n_ta_filler = 138,
  n_ta_reject = 206
)

cat("Experiment 1 - Long Exposure, Simultaneous:\n")
fit1_long_sim <- fit_winter_2ht(exp1_long_sim, lineup_size = 6)
print(fit1_long_sim)
cat("\n")

# Short exposure, simultaneous
exp1_short_sim <- c(
  n_tp_suspect = 64,
  n_tp_filler = 152,
  n_tp_reject = 166,
  n_ta_suspect = 44,
  n_ta_filler = 157,
  n_ta_reject = 181
)

cat("Experiment 1 - Short Exposure, Simultaneous:\n")
fit1_short_sim <- fit_winter_2ht(exp1_short_sim, lineup_size = 6)
print(fit1_short_sim)
cat("\n")

# Test: dP should be higher for long vs. short exposure
cat("Validation: dP(long) > dP(short)?\n")
cat(sprintf("  dP(long)  = %.3f\n", fit1_long_sim$parameters["dP"]))
cat(sprintf("  dP(short) = %.3f\n", fit1_short_sim$parameters["dP"]))
cat(sprintf("  Difference = %.3f\n", fit1_long_sim$parameters["dP"] - fit1_short_sim$parameters["dP"]))
cat(sprintf("  Test PASSED: %s\n\n", fit1_long_sim$parameters["dP"] > fit1_short_sim$parameters["dP"]))

# Sequential conditions
exp1_long_seq <- c(
  n_tp_suspect = 140,
  n_tp_filler = 164,
  n_tp_reject = 58,
  n_ta_suspect = 46,
  n_ta_filler = 212,
  n_ta_reject = 104
)

cat("Experiment 1 - Long Exposure, Sequential:\n")
fit1_long_seq <- fit_winter_2ht(exp1_long_seq, lineup_size = 6)
print(fit1_long_seq)
cat("\n")

exp1_short_seq <- c(
  n_tp_suspect = 80,
  n_tp_filler = 193,
  n_tp_reject = 85,
  n_ta_suspect = 57,
  n_ta_filler = 213,
  n_ta_reject = 88
)

cat("Experiment 1 - Short Exposure, Sequential:\n")
fit1_short_seq <- fit_winter_2ht(exp1_short_seq, lineup_size = 6)
print(fit1_short_seq)
cat("\n")

# ==============================================================================
# Experiment 2: Biased selection (b manipulation)
# Manipulation: Lineup fairness (fair vs. unfair)
# ==============================================================================

cat("\n=== EXPERIMENT 2: BIASED SELECTION (b) ===\n\n")

# Fair lineup, simultaneous
exp2_fair_sim <- c(
  n_tp_suspect = 156,
  n_tp_filler = 99,
  n_tp_reject = 129,
  n_ta_suspect = 30,
  n_ta_filler = 140,
  n_ta_reject = 214
)

cat("Experiment 2 - Fair Lineup, Simultaneous:\n")
fit2_fair_sim <- fit_winter_2ht(exp2_fair_sim, lineup_size = 6)
print(fit2_fair_sim)
cat("\n")

# Unfair lineup, simultaneous
exp2_unfair_sim <- c(
  n_tp_suspect = 172,
  n_tp_filler = 71,
  n_tp_reject = 135,
  n_ta_suspect = 60,
  n_ta_filler = 100,
  n_ta_reject = 218
)

cat("Experiment 2 - Unfair Lineup, Simultaneous:\n")
fit2_unfair_sim <- fit_winter_2ht(exp2_unfair_sim, lineup_size = 6)
print(fit2_unfair_sim)
cat("\n")

# Test: b should be higher for unfair vs. fair lineup
cat("Validation: b(unfair) > b(fair)?\n")
cat(sprintf("  b(unfair) = %.3f\n", fit2_unfair_sim$parameters["b"]))
cat(sprintf("  b(fair)   = %.3f\n", fit2_fair_sim$parameters["b"]))
cat(sprintf("  Difference = %.3f\n", fit2_unfair_sim$parameters["b"] - fit2_fair_sim$parameters["b"]))
cat(sprintf("  Test PASSED: %s\n\n", fit2_unfair_sim$parameters["b"] > fit2_fair_sim$parameters["b"]))

# Sequential conditions
exp2_fair_seq <- c(
  n_tp_suspect = 137,
  n_tp_filler = 164,
  n_tp_reject = 65,
  n_ta_suspect = 43,
  n_ta_filler = 213,
  n_ta_reject = 110
)

cat("Experiment 2 - Fair Lineup, Sequential:\n")
fit2_fair_seq <- fit_winter_2ht(exp2_fair_seq, lineup_size = 6)
print(fit2_fair_seq)
cat("\n")

exp2_unfair_seq <- c(
  n_tp_suspect = 177,
  n_tp_filler = 155,
  n_tp_reject = 52,
  n_ta_suspect = 81,
  n_ta_filler = 182,
  n_ta_reject = 121
)

cat("Experiment 2 - Unfair Lineup, Sequential:\n")
fit2_unfair_seq <- fit_winter_2ht(exp2_unfair_seq, lineup_size = 6)
print(fit2_unfair_seq)
cat("\n")

# ==============================================================================
# Experiment 3: Guessing-based selection (g manipulation)
# Manipulation: Pre-lineup instructions (high vs. low culprit probability)
# ==============================================================================

cat("\n=== EXPERIMENT 3: GUESSING-BASED SELECTION (g) ===\n\n")

# Low culprit probability, simultaneous
exp3_low_sim <- c(
  n_tp_suspect = 128,
  n_tp_filler = 78,
  n_tp_reject = 190,
  n_ta_suspect = 28,
  n_ta_filler = 119,
  n_ta_reject = 249
)

cat("Experiment 3 - Low Culprit Probability, Simultaneous:\n")
fit3_low_sim <- fit_winter_2ht(exp3_low_sim, lineup_size = 6)
print(fit3_low_sim)
cat("\n")

# High culprit probability, simultaneous
exp3_high_sim <- c(
  n_tp_suspect = 141,
  n_tp_filler = 128,
  n_tp_reject = 107,
  n_ta_suspect = 44,
  n_ta_filler = 167,
  n_ta_reject = 165
)

cat("Experiment 3 - High Culprit Probability, Simultaneous:\n")
fit3_high_sim <- fit_winter_2ht(exp3_high_sim, lineup_size = 6)
print(fit3_high_sim)
cat("\n")

# Test: g should be higher for high vs. low probability
cat("Validation: g(high) > g(low)?\n")
cat(sprintf("  g(high) = %.3f\n", fit3_high_sim$parameters["g"]))
cat(sprintf("  g(low)  = %.3f\n", fit3_low_sim$parameters["g"]))
cat(sprintf("  Difference = %.3f\n", fit3_high_sim$parameters["g"] - fit3_low_sim$parameters["g"]))
cat(sprintf("  Test PASSED: %s\n\n", fit3_high_sim$parameters["g"] > fit3_low_sim$parameters["g"]))

# Sequential conditions
exp3_low_seq <- c(
  n_tp_suspect = 104,
  n_tp_filler = 119,
  n_tp_reject = 151,
  n_ta_suspect = 38,
  n_ta_filler = 144,
  n_ta_reject = 192
)

cat("Experiment 3 - Low Culprit Probability, Sequential:\n")
fit3_low_seq <- fit_winter_2ht(exp3_low_seq, lineup_size = 6)
print(fit3_low_seq)
cat("\n")

exp3_high_seq <- c(
  n_tp_suspect = 136,
  n_tp_filler = 164,
  n_tp_reject = 62,
  n_ta_suspect = 56,
  n_ta_filler = 199,
  n_ta_reject = 107
)

cat("Experiment 3 - High Culprit Probability, Sequential:\n")
fit3_high_seq <- fit_winter_2ht(exp3_high_seq, lineup_size = 6)
print(fit3_high_seq)
cat("\n")

# ==============================================================================
# Experiment 4: Detection of culprit absence (dA manipulation)
# Manipulation: Ease of rejection (difficult vs. easy)
# ==============================================================================

cat("\n=== EXPERIMENT 4: CULPRIT ABSENCE DETECTION (dA) ===\n\n")

# Difficult to reject, simultaneous
exp4_diff_sim <- c(
  n_tp_suspect = 142,
  n_tp_filler = 121,
  n_tp_reject = 135,
  n_ta_suspect = 40,
  n_ta_filler = 163,
  n_ta_reject = 195
)

cat("Experiment 4 - Difficult to Reject, Simultaneous:\n")
fit4_diff_sim <- fit_winter_2ht(exp4_diff_sim, lineup_size = 6)
print(fit4_diff_sim)
cat("\n")

# Easy to reject, simultaneous
exp4_easy_sim <- c(
  n_tp_suspect = 136,
  n_tp_filler = 103,
  n_tp_reject = 119,
  n_ta_suspect = 19,
  n_ta_filler = 140,
  n_ta_reject = 199
)

cat("Experiment 4 - Easy to Reject, Simultaneous:\n")
fit4_easy_sim <- fit_winter_2ht(exp4_easy_sim, lineup_size = 6)
print(fit4_easy_sim)
cat("\n")

# Test: dA should be higher for easy vs. difficult
cat("Validation: dA(easy) > dA(difficult)?\n")
cat(sprintf("  dA(easy)      = %.3f\n", fit4_easy_sim$parameters["dA"]))
cat(sprintf("  dA(difficult) = %.3f\n", fit4_diff_sim$parameters["dA"]))
cat(sprintf("  Difference = %.3f\n", fit4_easy_sim$parameters["dA"] - fit4_diff_sim$parameters["dA"]))
cat(sprintf("  Test PASSED: %s\n\n", fit4_easy_sim$parameters["dA"] > fit4_diff_sim$parameters["dA"]))

# Sequential conditions
exp4_diff_seq <- c(
  n_tp_suspect = 148,
  n_tp_filler = 165,
  n_tp_reject = 85,
  n_ta_suspect = 52,
  n_ta_filler = 233,
  n_ta_reject = 113
)

cat("Experiment 4 - Difficult to Reject, Sequential:\n")
fit4_diff_seq <- fit_winter_2ht(exp4_diff_seq, lineup_size = 6)
print(fit4_diff_seq)
cat("\n")

exp4_easy_seq <- c(
  n_tp_suspect = 125,
  n_tp_filler = 183,
  n_tp_reject = 82,
  n_ta_suspect = 35,
  n_ta_filler = 191,
  n_ta_reject = 164
)

cat("Experiment 4 - Easy to Reject, Sequential:\n")
fit4_easy_seq <- fit_winter_2ht(exp4_easy_seq, lineup_size = 6)
print(fit4_easy_seq)
cat("\n")

# ==============================================================================
# Detailed Summary and Visualization Examples
# ==============================================================================

cat("\n=== DETAILED SUMMARY EXAMPLE ===\n\n")
summary(fit1_long_sim)

cat("\n=== PLOTTING EXAMPLES ===\n\n")

# Save plots to files
if (require(ggplot2)) {

  # Parameter plot
  cat("Creating parameter plot...\n")
  p1 <- plot_2ht_parameters(fit1_long_sim)
  ggsave("test_winter_2ht_params.png", p1, width = 8, height = 6)

  # Model fit plot
  cat("Creating model fit plot...\n")
  p2 <- plot_2ht_fit(fit1_long_sim)
  ggsave("test_winter_2ht_fit.png", p2, width = 10, height = 6)

  cat("Plots saved successfully.\n\n")
}

# ==============================================================================
# Bootstrap Example
# ==============================================================================

cat("\n=== BOOTSTRAP EXAMPLE ===\n\n")

# Run bootstrap with smaller nboot for testing
cat("Running bootstrap (this may take a minute)...\n")
boot_fit <- boot_winter_2ht(fit1_long_sim, nboot = 200, seed = 123)
print(boot_fit)
cat("\n")
summary(boot_fit)

# Save bootstrap plot
if (require(ggplot2)) {
  cat("\nCreating bootstrap distribution plot...\n")
  p3 <- plot(boot_fit)
  ggsave("test_winter_2ht_bootstrap.png", p3, width = 10, height = 8)
  cat("Bootstrap plot saved successfully.\n\n")
}

# ==============================================================================
# Summary of All Validation Tests
# ==============================================================================

cat("\n=== VALIDATION SUMMARY ===\n\n")

cat("All validation tests:\n")
cat("1. Experiment 1 (dP): Long exposure > Short exposure -",
    ifelse(fit1_long_sim$parameters["dP"] > fit1_short_sim$parameters["dP"], "PASS", "FAIL"), "\n")
cat("2. Experiment 2 (b):  Unfair lineup > Fair lineup -",
    ifelse(fit2_unfair_sim$parameters["b"] > fit2_fair_sim$parameters["b"], "PASS", "FAIL"), "\n")
cat("3. Experiment 3 (g):  High prob > Low prob -",
    ifelse(fit3_high_sim$parameters["g"] > fit3_low_sim$parameters["g"], "PASS", "FAIL"), "\n")
cat("4. Experiment 4 (dA): Easy reject > Difficult reject -",
    ifelse(fit4_easy_sim$parameters["dA"] > fit4_diff_sim$parameters["dA"], "PASS", "FAIL"), "\n")

cat("\n=== TESTING COMPLETE ===\n")
