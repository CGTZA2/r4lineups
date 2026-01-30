# Example: RAC (Response Time-Accuracy Characteristic) Analysis
# This demonstrates the new RAC functionality in r4lineups

library(r4lineups)
library(ggplot2)

# Simulate lineup data with response times
# Based on the principle that faster responses often indicate stronger memory
set.seed(42)

n_tp <- 300  # target-present lineups
n_ta <- 300  # target-absent lineups

# Simulate target-present data
# Correct IDs tend to be faster
tp_correct <- data.frame(
  target_present = TRUE,
  identification = "suspect",
  # Faster response times for correct IDs (mean = 8000ms, sd = 3000)
  response_time = rnorm(180, mean = 8000, sd = 3000)
)
tp_correct$response_time <- pmax(1000, tp_correct$response_time)  # minimum 1s

tp_filler <- data.frame(
  target_present = TRUE,
  identification = "filler",
  # Slower for filler picks (mean = 12000ms, sd = 4000)
  response_time = rnorm(60, mean = 12000, sd = 4000)
)
tp_filler$response_time <- pmax(1000, tp_filler$response_time)

tp_reject <- data.frame(
  target_present = TRUE,
  identification = "reject",
  # Variable times for rejections
  response_time = rnorm(60, mean = 15000, sd = 5000)
)
tp_reject$response_time <- pmax(1000, tp_reject$response_time)

# Simulate target-absent data
# False IDs (suspect picks) tend to be slower
ta_suspect <- data.frame(
  target_present = FALSE,
  identification = "suspect",
  # Slower response times for false IDs (mean = 13000ms, sd = 4000)
  response_time = rnorm(60, mean = 13000, sd = 4000)
)
ta_suspect$response_time <- pmax(1000, ta_suspect$response_time)

ta_filler <- data.frame(
  target_present = FALSE,
  identification = "filler",
  # Filler picks also slower
  response_time = rnorm(120, mean = 11000, sd = 4000)
)
ta_filler$response_time <- pmax(1000, ta_filler$response_time)

ta_reject <- data.frame(
  target_present = FALSE,
  identification = "reject",
  # Correct rejections variable
  response_time = rnorm(120, mean = 14000, sd = 5000)
)
ta_reject$response_time <- pmax(1000, ta_reject$response_time)

# Combine all data
lineup_data_rt <- rbind(tp_correct, tp_filler, tp_reject,
                        ta_suspect, ta_filler, ta_reject)

# Shuffle rows
lineup_data_rt <- lineup_data_rt[sample(nrow(lineup_data_rt)), ]

cat("=== Simulated Lineup Data with Response Times ===\n")
cat("Total observations:", nrow(lineup_data_rt), "\n")
cat("Target-present:", sum(lineup_data_rt$target_present), "\n")
cat("Target-absent:", sum(!lineup_data_rt$target_present), "\n")
cat("\nResponse time range:", round(min(lineup_data_rt$response_time)), "-",
    round(max(lineup_data_rt$response_time)), "ms\n\n")

# ============================================
# Example 1: Basic RAC Analysis
# ============================================
cat("Example 1: Basic RAC with Time Bins\n")
cat("=====================================\n\n")

# Define time bins (in milliseconds)
time_bins <- c(0, 5000, 10000, 15000, 20000, 99999)

# Compute RAC
rac_result <- make_rac(
  data = lineup_data_rt,
  lineup_size = 6,
  time_bins = time_bins,
  show_plot = TRUE,
  time_units = "ms"
)

# Print results
print(rac_result)

# Display the plot
print(rac_result$plot)

cat("\n\nInterpretation:")
cat("\n- Faster responses (shorter times) show higher accuracy")
cat("\n- This suggests that strong memory leads to both fast AND accurate IDs")
cat("\n- The speed-accuracy pattern is consistent with the eyewitness literature\n\n")

# ============================================
# Example 2: Comparison with CAC
# ============================================
cat("\nExample 2: RAC vs CAC Comparison\n")
cat("==================================\n\n")

# Add confidence ratings correlated with response time
# Faster = higher confidence (negative correlation)
lineup_data_rt$confidence <- 100 - (lineup_data_rt$response_time / 200) +
                              rnorm(nrow(lineup_data_rt), 0, 10)
lineup_data_rt$confidence <- pmin(100, pmax(0, lineup_data_rt$confidence))

# Compute CAC
cac_result <- make_cac(
  data = lineup_data_rt,
  lineup_size = 6,
  confidence_bins = c(0, 60, 80, 100),
  show_plot = TRUE
)

cat("CAC Analysis:\n")
print(cac_result)

# Compare the patterns
cat("\n\nComparing RAC and CAC:")
cat("\n- RAC shows accuracy by RESPONSE TIME")
cat("\n- CAC shows accuracy by CONFIDENCE")
cat("\n- Both can be diagnostic, but measure different aspects")
cat("\n- Response time may be less susceptible to verbal overshadowing effects\n\n")

# ============================================
# Example 3: Fine-grained Time Bins
# ============================================
cat("\nExample 3: Fine-grained Time Bins\n")
cat("===================================\n\n")

# Use more granular bins
fine_bins <- seq(0, 25000, by = 2500)

rac_fine <- make_rac(
  data = lineup_data_rt,
  lineup_size = 6,
  time_bins = fine_bins,
  show_plot = TRUE,
  time_units = "ms",
  show_errorbars = TRUE,
  show_n = TRUE
)

print(rac_fine$plot)

cat("\n\nWith finer bins:")
cat("\n- More detailed view of the speed-accuracy relationship")
cat("\n- May reveal non-linear patterns")
cat("\n- But: smaller sample sizes per bin (check n values)\n\n")

# ============================================
# Example 4: Accessing RAC Data
# ============================================
cat("\nExample 4: Accessing RAC Data for Custom Analysis\n")
cat("====================================================\n\n")

# Extract the data
rac_data <- rac_result$rac_data
print(rac_data)

cat("\n\nYou can use this data for:")
cat("\n- Custom plotting")
cat("\n- Statistical tests")
cat("\n- Comparing across conditions")
cat("\n- Exporting to other software\n\n")

# Custom plot example
library(ggplot2)
custom_plot <- ggplot(rac_data, aes(x = mean_time, y = accuracy)) +
  geom_point(aes(size = n_total), color = "darkred", alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "darkblue") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Mean Response Time (ms)",
    y = "Accuracy (Proportion Correct)",
    title = "Custom RAC Plot with Smoothing",
    size = "N IDs"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray")

print(custom_plot)

cat("\n\n=== RAC Analysis Complete ===\n")
cat("\nKey References:\n")
cat("- Seale-Carlisle et al. (2019). JARMAC, 8(4), 420-428.\n")
cat("- Mickes et al. (2024). Behavior Research Methods, 56, 1533-1550.\n")
