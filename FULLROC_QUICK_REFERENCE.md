# Full ROC Analysis - Quick Reference Card

## Basic Usage

```r
library(r4lineups)

# Compute full ROC
result <- make_fullroc(data)

# View results
print(result$auc)
print(result$plot)
```

## Data Structure Required

```r
data <- data.frame(
  target_present = c(TRUE, FALSE, ...),    # Logical: guilty suspect present?
  identification = c("suspect", "filler", "reject", ...),  # Character
  confidence = c(90, 70, 80, ...)          # Numeric: 0-100
)
```

## Main Functions

### make_fullroc()
Main user-facing function - computes and plots full ROC

```r
make_fullroc(
  data,                      # Required: your lineup data
  conf_bins = NULL,          # Optional: e.g., c(0, 60, 80, 100)
  order = "diagnosticity",   # or "apriori"
  lineup_size = 6,           # Number of people in lineup
  show_plot = TRUE,          # Create plot?
  epsilon = 0.001            # Small value for FAR=0 cases
)
```

**Returns**: List with `$auc`, `$roc_data`, `$diagnosticity_table`, `$plot`, `$summary`

### make_fullroc_data()
Lower-level function - computation only (no plot)

```r
make_fullroc_data(
  data,
  conf_bins = NULL,
  order = "diagnosticity",
  lineup_size = 6,
  epsilon = 0.001
)
```

**Returns**: List with `$roc_data`, `$auc`, `$diagnosticity_table`, etc.

### plot_fullroc()
Plotting function

```r
plot_fullroc(
  fullroc_obj,               # Output from make_fullroc_data()
  show_auc = TRUE,           # Display AUC on plot?
  point_labels = FALSE,      # Label points? (can be crowded)
  title = "Full ROC Curve (Smith & Yang, 2020)"
)
```

**Returns**: ggplot2 object

## Parameters Explained

### conf_bins
- `NULL` (default): Use each unique confidence value as a bin
- `c(0, 60, 80, 100)`: Create Low, Medium, High bins
- Fewer bins → cleaner ROC curve
- More bins → more operating points

### order
- `"diagnosticity"` (default, recommended): Order by empirical DR = HR/FAR
  - Data-driven
  - Optimal evidence ordering

- `"apriori"`: Use theoretical ordering
  - Suspect IDs: high conf → low conf
  - Filler IDs: low conf → high conf
  - Rejections: low conf → high conf

### lineup_size
- Number of people in lineup (default = 6)
- Not used in full ROC computation (unlike partial ROC)
- Kept for compatibility with package conventions

### epsilon
- Small value to use when FAR = 0 (avoids infinite DR)
- Default = 0.001
- Rarely needs to be changed

## Output Structure

```r
result <- make_fullroc(data)

# Access components:
result$auc                    # Full AUC value (numeric)
result$roc_data              # ROC points (data.frame)
result$diagnosticity_table   # Detailed table (data.frame)
result$plot                  # ggplot2 object
result$summary               # Summary list
```

### ROC Data Columns
- `cumulative_hit_rate`: Y-axis (0 to 1)
- `cumulative_false_alarm_rate`: X-axis (0 to 1)
- `evidence_label`: Response category label

### Diagnosticity Table Columns
- `decision`: "suspect", "filler", or "reject"
- `conf_bin`: Confidence level/bin
- `hit_rate`: P(response | target present)
- `false_alarm_rate`: P(response | target absent)
- `diagnosticity_ratio`: HR / FAR
- `cumulative_hit_rate`: Running sum
- `cumulative_false_alarm_rate`: Running sum
- `evidence_label`: Combined label

## Interpreting AUC

| AUC Range | Interpretation |
|-----------|----------------|
| 0.50 | Chance (no discriminability) |
| 0.50-0.60 | Very weak |
| 0.60-0.70 | Weak |
| 0.70-0.80 | Moderate |
| 0.80-0.90 | Good |
| 0.90-1.00 | Excellent |

## Common Use Cases

### Compare Two Conditions

```r
# Condition A
result_A <- make_fullroc(data_A)

# Condition B
result_B <- make_fullroc(data_B)

# Compare
if (result_A$auc > result_B$auc) {
  cat("Condition A has superior discriminability\n")
}
```

### Use Custom Confidence Bins

```r
# Instead of many confidence values, create 3 bins
result <- make_fullroc(
  data,
  conf_bins = c(0, 60, 80, 100)  # Low, Medium, High
)
```

### Try Both Ordering Methods

```r
# Diagnosticity ordering (data-driven)
result_diag <- make_fullroc(data, order = "diagnosticity")

# A-priori ordering (theoretical)
result_apriori <- make_fullroc(data, order = "apriori")

# Compare (should be similar)
c(result_diag$auc, result_apriori$auc)
```

### Extract Diagnosticity Table

```r
result <- make_fullroc(data)

# View the table
print(result$diagnosticity_table)

# Find most diagnostic of guilt
result$diagnosticity_table[1, ]

# Find most diagnostic of innocence
result$diagnosticity_table[nrow(result$diagnosticity_table), ]
```

### Save Plot

```r
result <- make_fullroc(data)

# Using ggplot2
library(ggplot2)
ggsave("my_fullroc.png", result$plot, width = 8, height = 6)

# Or
pdf("my_fullroc.pdf", width = 8, height = 6)
print(result$plot)
dev.off()
```

## Full ROC vs. Partial ROC

```r
# Traditional partial ROC (suspect IDs only)
partial <- make_roc(data)
partial$pauc  # Partial AUC

# Full ROC (ALL responses)
full <- make_fullroc(data)
full$auc      # Full AUC

# NOT directly comparable!
```

**When to use each**:
- **Full ROC** (`make_fullroc`): New analyses (recommended)
- **Partial ROC** (`make_roc`): Replicating older studies

## Troubleshooting

### Error: "Missing required columns"
```r
# Check column names
names(data)

# Must have: target_present, identification, confidence
# Case-sensitive!
```

### Error: "Data must include both target-present and target-absent lineups"
```r
# Check you have both
table(data$target_present)

# Need TRUE and FALSE
```

### Warning: "Empty categories removed"
```r
# Some decision × confidence bins have zero counts
# This is normal - they're automatically removed
# No action needed
```

### AUC seems wrong
```r
# Check data structure
str(data)

# Check proportions
table(data$target_present, data$identification)

# If AUC < 0.50, might have labels reversed
# Check: TRUE = guilty suspect present?
```

### Too many operating points
```r
# Use confidence bins to reduce
result <- make_fullroc(
  data,
  conf_bins = c(0, 60, 80, 100)  # Creates 3 bins instead of many
)
```

## Examples

### Minimal Example

```r
library(r4lineups)

# Create sample data
set.seed(123)
data <- data.frame(
  target_present = rep(c(TRUE, FALSE), each = 50),
  identification = sample(c("suspect", "filler", "reject"), 100, replace = TRUE),
  confidence = sample(c(20, 40, 60, 80, 100), 100, replace = TRUE)
)

# Analyze
result <- make_fullroc(data)
print(result$auc)
```

### Complete Example

```r
library(r4lineups)
library(ggplot2)

# Load your data
data <- read.csv("my_lineup_data.csv")

# Compute full ROC with custom bins
result <- make_fullroc(
  data,
  conf_bins = c(0, 60, 80, 100),
  order = "diagnosticity",
  lineup_size = 6
)

# Print summary
print(result)

# View diagnosticity table
View(result$diagnosticity_table)

# Customize plot
custom_plot <- result$plot +
  theme_minimal() +
  ggtitle("My Experiment: Full ROC Analysis")

# Save
ggsave("my_roc.png", custom_plot, width = 8, height = 6, dpi = 300)

# Export data for external plotting
write.csv(result$roc_data, "roc_data.csv", row.names = FALSE)
write.csv(result$diagnosticity_table, "diagnosticity.csv", row.names = FALSE)
```

## Getting Help

```r
# Function help
?make_fullroc
?make_fullroc_data
?plot_fullroc

# Examples
example(make_fullroc)

# Vignette (if built)
vignette("fullroc_analysis")
```

## Citation

Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator discriminability and eyewitness discriminability: A method for creating full receiver operating characteristic curves of lineup identification performance. *Perspectives on Psychological Science, 15*(3), 589-607.

---

**For full documentation, see**: `FULLROC_README.md`

**For examples, see**: `examples/fullroc_example.R`

**For tutorial, see**: `vignettes/fullroc_analysis.Rmd`
