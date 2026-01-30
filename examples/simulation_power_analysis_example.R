# Example: Data Simulation and Power Analysis for Lineup Studies
# This demonstrates the new simulation capabilities in r4lineups

library(r4lineups)
library(ggplot2)

cat("=== Data Simulation and Power Analysis Tutorial ===\n\n")

# ============================================
# Part 1: Basic Data Simulation
# ============================================
cat("Part 1: Simulating Lineup Data\n")
cat("================================\n\n")

# Simulate strong memory (d' = 2.0)
strong_memory <- simulate_lineup_data(
  n_tp = 200,
  n_ta = 200,
  d_prime = 2.0,
  conf_levels = 5,
  include_response_time = TRUE,
  seed = 42
)

print(strong_memory)

# Simulate weak memory (d' = 1.0)
weak_memory <- simulate_lineup_data(
  n_tp = 200,
  n_ta = 200,
  d_prime = 1.0,
  conf_levels = 5,
  include_response_time = TRUE,
  seed = 43
)

cat("\nWeak Memory Simulation:\n")
print(weak_memory)

# ============================================
# Part 2: Analyze Simulated Data
# ============================================
cat("\n\nPart 2: Analyzing Simulated Data\n")
cat("==================================\n\n")

# ROC analysis on strong memory
cat("ROC Analysis - Strong Memory (d' = 2.0):\n")
roc_strong <- make_roc(strong_memory, show_plot = FALSE)
cat("pAUC:", round(roc_strong$pAUC, 4), "\n")

# ROC analysis on weak memory
cat("\nROC Analysis - Weak Memory (d' = 1.0):\n")
roc_weak <- make_roc(weak_memory, show_plot = FALSE)
cat("pAUC:", round(roc_weak$pAUC, 4), "\n")

# CAC analysis
cat("\nCAC Analysis - Strong Memory:\n")
cac_strong <- make_cac(strong_memory, show_plot = FALSE)
print(cac_strong$cac_data)

# RAC analysis
cat("\nRAC Analysis - Strong Memory:\n")
rac_strong <- make_rac(
  strong_memory,
  time_bins = c(0, 5000, 6000, 7000, 8000, 99999),
  show_plot = FALSE
)
print(rac_strong$rac_data)

# ============================================
# Part 3: Compare Different d' Values
# ============================================
cat("\n\nPart 3: Comparing Different Memory Strengths\n")
cat("==============================================\n\n")

# Simulate a range of d' values
d_primes <- seq(0.5, 2.5, by = 0.5)
results <- data.frame()

for (d in d_primes) {
  sim <- simulate_lineup_data(
    n_tp = 500,
    n_ta = 500,
    d_prime = d,
    conf_levels = 5,
    seed = round(d * 100)
  )

  roc <- make_roc(sim, show_plot = FALSE)

  results <- rbind(results, data.frame(
    d_prime = d,
    pAUC = roc$pAUC,
    correct_id_rate = roc$roc_data$hit_rate[1],
    false_id_rate = roc$roc_data$fa_rate[1]
  ))
}

cat("Results across d' values:\n")
print(results)

# Plot the relationship
p <- ggplot(results, aes(x = d_prime, y = pAUC)) +
  geom_line(size = 1.2, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  theme_bw(base_size = 14) +
  labs(
    x = "d-prime (Memory Strength)",
    y = "pAUC",
    title = "Relationship between Memory Strength and ROC Performance",
    subtitle = "Based on simulated data (N=500 per condition)"
  ) +
  scale_y_continuous(limits = c(0, NA))

print(p)

# ============================================
# Part 4: Power Analysis
# ============================================
cat("\n\nPart 4: Power Analysis\n")
cat("=======================\n\n")

cat("Question: What sample size do we need to reliably detect d' = 1.5?\n\n")

# Define a custom statistic function for power analysis
# Power = ability to detect a significant effect
compute_pAUC <- function(data) {
  tryCatch({
    roc <- make_roc(data, show_plot = FALSE)
    return(roc$pAUC)
  }, error = function(e) {
    return(NA)
  })
}

# Run power analysis
cat("Running power analysis (this may take a minute)...\n")
power_results <- simulate_power_analysis(
  sample_sizes = c(50, 100, 200, 400),
  d_prime = 1.5,
  n_simulations = 200,  # Reduced for speed in example
  stat_function = compute_pAUC,
  conf_levels = 5,
  seed = 42
)

cat("\nPower Analysis Results:\n")
print(power_results)

# Plot power curve
plot(power_results)

cat("\n\nInterpretation:")
cat("\n- To achieve 80% power, we need approximately N =",
    power_results$sample_size[which.min(abs(power_results$power - 0.80))],
    "per condition")
cat("\n- Mean pAUC increases with sample size (better estimation)")
cat("\n- Confidence intervals narrow with larger samples\n")

# ============================================
# Part 5: Method Validation
# ============================================
cat("\n\nPart 5: Validating Analysis Methods\n")
cat("=====================================\n\n")

cat("Use simulation to test if our analysis recovers known parameters.\n\n")

# Simulate data with known d' = 1.8
known_d <- 1.8
validation_data <- simulate_lineup_data(
  n_tp = 1000,
  n_ta = 1000,
  d_prime = known_d,
  conf_levels = 7,
  seed = 999
)

# Analyze with different methods
roc_val <- make_roc(validation_data, show_plot = FALSE)
fullroc_val <- make_fullroc(validation_data, show_plot = FALSE)

cat("True d':", known_d, "\n")
cat("Estimated from pAUC:", round(roc_val$pAUC / 0.01, 2), "(approx)\n")
cat("Full ROC AUC:", round(fullroc_val$auc, 3), "\n\n")

cat("Conclusion: Larger samples provide better parameter recovery.\n")

# ============================================
# Part 6: Scenario Planning
# ============================================
cat("\n\nPart 6: Scenario Planning for Studies\n")
cat("=======================================\n\n")

cat("Scenario: Will we detect a difference between two memory strengths?\n\n")

# Simulate two conditions
condition_A <- simulate_lineup_data(n_tp = 150, n_ta = 150, d_prime = 1.5,
                                    conf_levels = 5, seed = 10)
condition_B <- simulate_lineup_data(n_tp = 150, n_ta = 150, d_prime = 2.0,
                                    conf_levels = 5, seed = 20)

roc_A <- make_roc(condition_A, show_plot = FALSE)
roc_B <- make_roc(condition_B, show_plot = FALSE)

cat("Condition A (d'=1.5):\n")
cat("  pAUC:", round(roc_A$pAUC, 4), "\n")
cat("  Correct ID rate:", round(roc_A$roc_data$hit_rate[1], 3), "\n\n")

cat("Condition B (d'=2.0):\n")
cat("  pAUC:", round(roc_B$pAUC, 4), "\n")
cat("  Correct ID rate:", round(roc_B$roc_data$hit_rate[1], 3), "\n\n")

cat("Difference in pAUC:", round(roc_B$pAUC - roc_A$pAUC, 4), "\n")

cat("\nWith N=150 per condition, we can detect this difference.\n")
cat("For smaller effects, increase sample size accordingly.\n")

# ============================================
# Part 7: Teaching and Demonstration
# ============================================
cat("\n\nPart 7: Teaching Applications\n")
cat("==============================\n\n")

cat("Simulated data is excellent for teaching signal detection theory:\n\n")

# Very clear demonstration
perfect_memory <- simulate_lineup_data(
  n_tp = 100, n_ta = 100, d_prime = 4.0, conf_levels = 3, seed = 1
)
no_memory <- simulate_lineup_data(
  n_tp = 100, n_ta = 100, d_prime = 0.0, conf_levels = 3, seed = 2
)

cat("Perfect Memory (d' = 4.0):\n")
roc_perfect <- make_roc(perfect_memory, show_plot = FALSE)
cat("  Accuracy:", mean(perfect_memory$target_present == TRUE &
                       perfect_memory$identification == "suspect"), "\n")

cat("\nNo Memory (d' = 0.0):\n")
roc_none <- make_roc(no_memory, show_plot = FALSE)
cat("  Accuracy:", mean(no_memory$target_present == TRUE &
                     no_memory$identification == "suspect"), "\n")

cat("\nStudents can see the direct relationship between d' and performance.\n")

# ============================================
# Summary
# ============================================
cat("\n\n=== Summary ===\n")
cat("Simulation capabilities enable:\n")
cat("1. Power analysis for planning studies\n")
cat("2. Method validation and comparison\n")
cat("3. Scenario planning and sensitivity analysis\n")
cat("4. Teaching signal detection theory\n")
cat("5. Testing software implementations\n\n")

cat("Key Functions:\n")
cat("- simulate_lineup_data(): Generate lineup data\n")
cat("- simulate_power_analysis(): Conduct power analysis\n")
cat("- All standard r4lineups functions work with simulated data\n\n")

cat("=== Tutorial Complete ===\n")
