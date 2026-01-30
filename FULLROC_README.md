# Full ROC Analysis for Eyewitness Lineups (Smith & Yang, 2020)

## Overview

This implementation adds **full ROC curve analysis** to the r4lineups package, based on the method described in:

> Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator discriminability and eyewitness discriminability: A method for creating full receiver operating characteristic curves of lineup identification performance. *Perspectives on Psychological Science, 15*(3), 589-607.

## What is a Full ROC Curve?

Traditional (partial) ROC curves for lineup data only use **suspect identifications**, ignoring filler identifications and lineup rejections. This is problematic because:

1. **It creates an incomplete picture** - Filler IDs and rejections also provide diagnostic information
2. **pAUC is biased** - Partial AUC favors more conservative procedures
3. **Doesn't span full range** - Cannot provide threshold-free discriminability measure

### Full ROC vs. Partial ROC

| Feature | Partial ROC | Full ROC |
|---------|-------------|----------|
| **Responses used** | Suspect IDs only | Suspect IDs + Filler IDs + Rejections |
| **Range** | Limited (0 to max suspect ID rate) | Full (0 to 1.0) |
| **Measure** | pAUC (partial area) | AUC (full area) |
| **Interpretation** | Confounded with bias | Threshold-free discriminability |
| **Evidence** | Inculpatory only | Both inculpatory and exculpatory |

## Key Concept: Investigator vs. Eyewitness Discriminability

The Smith & Yang method recognizes **two simultaneous signal detection tasks**:

1. **Eyewitness task**: Determine if culprit is present AND identify them
   - 3 × 2 task: (suspect/filler/reject) × (culprit present/absent)

2. **Investigator task**: Determine if suspect is guilty
   - 2 × 2 task: (arrest/release) × (guilty/innocent)
   - The investigator KNOWS which lineup members are fillers
   - Uses witness decisions as evidence to decide about suspect

**The full ROC measures investigator discriminability**, not just eyewitness memory.

## Installation & Usage

### Load the package

```r
library(r4lineups)
```

### Data Structure

Your data should have three columns:

```r
data <- data.frame(
  target_present = c(TRUE, TRUE, FALSE, FALSE),  # Logical: guilty suspect in lineup?
  identification = c("suspect", "filler", "suspect", "reject"),  # Character
  confidence = c(90, 70, 80, 50)  # Numeric: higher = more confident
)
```

### Basic Usage

```r
# Compute full ROC with diagnosticity ordering
result <- make_fullroc(data, lineup_size = 6)

# View results
print(result)
print(result$auc)  # Full AUC value

# Access the plot
print(result$plot)
```

### Custom Confidence Bins

Instead of using each unique confidence value, you can bin confidence ratings:

```r
# Create Low (0-60), Medium (60-80), High (80-100) bins
result <- make_fullroc(
  data,
  conf_bins = c(0, 60, 80, 100),
  lineup_size = 6
)
```

### Ordering Methods

Two methods for ordering response categories:

#### 1. Diagnosticity Ordering (default, recommended)

Orders responses by their empirical diagnosticity ratio (HR/FAR):

```r
result <- make_fullroc(data, order = "diagnosticity")
```

- Most diagnostic of guilt (high DR) appears first
- Most diagnostic of innocence (low DR) appears last
- Data-driven approach

#### 2. A-priori Ordering

Uses theoretical ordering:

```r
result <- make_fullroc(data, order = "apriori")
```

Order:
1. Suspect IDs (high confidence → low confidence)
2. Filler IDs (low confidence → high confidence)
3. Rejections (low confidence → high confidence)

Rationale: High-confidence suspect IDs are strong evidence of guilt. Low-confidence filler IDs/rejections are strong evidence of innocence (rare responses).

## Functions

### Main Functions

| Function | Purpose |
|----------|---------|
| `make_fullroc()` | Main user-facing function - computes and plots full ROC |
| `make_fullroc_data()` | Lower-level function - computes ROC data only |
| `plot_fullroc()` | Plotting function for full ROC curves |

### Parameters

**make_fullroc(data, conf_bins, order, lineup_size, show_plot, epsilon, ...)**

- `data`: Dataframe with target_present, identification, confidence
- `conf_bins`: Numeric vector for binning confidence (default = NULL, uses unique values)
- `order`: "diagnosticity" or "apriori" (default = "diagnosticity")
- `lineup_size`: Number of people in lineup (default = 6)
- `show_plot`: Logical, whether to create plot (default = TRUE)
- `epsilon`: Small value to handle FAR = 0 cases (default = 0.001)

### Return Value

A list with class `lineup_fullroc`:

```r
result <- make_fullroc(data)

# Access components:
result$auc                    # Full AUC value
result$roc_data              # ROC curve points (cumulative)
result$diagnosticity_table   # Non-cumulative rates & DR for each category
result$plot                  # ggplot2 object
result$summary               # Summary statistics
```

## Interpretation

### AUC Values

- **AUC = 0.50**: Chance performance (cannot discriminate)
- **AUC = 0.60-0.70**: Weak discriminability
- **AUC = 0.70-0.80**: Moderate discriminability
- **AUC = 0.80-0.90**: Good discriminability
- **AUC = 0.90-1.00**: Excellent discriminability

### Comparing Procedures

When comparing two lineup procedures:

1. **One curve dominates**: If Procedure A's ROC curve is above Procedure B's ROC across the FULL range, then Procedure A is unambiguously superior

2. **Curves cross**: No clear winner without making assumptions about:
   - Base rate of culprit presence
   - Relative costs of missed IDs vs. false IDs
   - Use utility analysis for these cases

### Full AUC vs. pAUC

```r
# Traditional partial ROC (suspect IDs only)
partial_roc <- make_roc(data)
print(partial_roc$pauc)  # Partial AUC

# Full ROC (all responses)
full_roc <- make_fullroc(data)
print(full_roc$auc)  # Full AUC
```

**Important**: pAUC and full AUC are NOT directly comparable!
- pAUC only uses suspect IDs
- Full AUC uses ALL evidence
- Full AUC provides unbiased discriminability measure

## Example: Strong vs. Weak Memory

```r
# Simulate strong memory condition
strong_data <- data.frame(
  target_present = c(rep(TRUE, 100), rep(FALSE, 100)),
  identification = c(
    # Target-present: high suspect ID rate
    sample(c("suspect", "filler", "reject"), 100, replace = TRUE,
           prob = c(0.70, 0.15, 0.15)),
    # Target-absent: low false ID rate
    sample(c("suspect", "filler", "reject"), 100, replace = TRUE,
           prob = c(0.10, 0.30, 0.60))
  ),
  confidence = sample(seq(0, 100, 10), 200, replace = TRUE)
)

strong_roc <- make_fullroc(strong_data)
print(strong_roc$auc)  # Expect high AUC (e.g., > 0.80)
```

## Advantages of Full ROC

1. **Uses all available evidence**
   - Suspect IDs: evidence of guilt
   - Filler IDs: evidence of innocence
   - Rejections: evidence of innocence

2. **Threshold-free discriminability**
   - Spans full range [0, 1]
   - Not confounded with response bias

3. **Matches real decision-making**
   - Investigators use ALL witness responses
   - Not just suspect IDs

4. **Unbiased comparisons**
   - pAUC is biased toward conservative procedures
   - Full AUC provides fair comparison

5. **Aligns with National Academy of Sciences (2014) recommendations**
   - Consider both inculpatory AND exculpatory evidence

## Technical Details

### Algorithm

1. **Compute non-cumulative rates** for each decision × confidence category:
   ```
   Hit Rate (HR) = P(response | target present)
   False Alarm Rate (FAR) = P(response | target absent)
   Diagnosticity Ratio (DR) = HR / FAR
   ```

2. **Order categories** by DR (diagnosticity) or a-priori

3. **Compute cumulative rates**:
   ```
   Cumulative HR = sum of HRs (ordered by evidence strength)
   Cumulative FAR = sum of FARs (ordered by evidence strength)
   ```

4. **Calculate AUC** using trapezoidal rule:
   ```
   AUC = Σ [(FAR_i - FAR_{i-1}) × (HR_i + HR_{i-1})/2]
   ```

### Handling Edge Cases

- **FAR = 0**: Use small epsilon value (default = 0.001) to avoid infinite DR
- **Both HR and FAR = 0**: Set DR = 1.0 (neutral evidence)
- **Empty categories**: Automatically removed (don't contribute to ROC)

## Comparison with Existing ROC Function

The package includes both methods:

| Feature | `make_roc()` | `make_fullroc()` |
|---------|--------------|------------------|
| Method | Wixted & Mickes (2012) | Smith & Yang (2020) |
| Responses | Suspect IDs only | ALL responses |
| Measure | pAUC (partial) | AUC (full) |
| Range | Limited | Full [0,1] |
| Use case | Traditional analysis | Recommended for new analyses |

**Recommendation**: Use `make_fullroc()` for new analyses. Use `make_roc()` only for:
- Comparing with older literature
- Replicating specific published analyses

## References

### Primary Reference

Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator discriminability and eyewitness discriminability: A method for creating full receiver operating characteristic curves of lineup identification performance. *Perspectives on Psychological Science, 15*(3), 589-607. https://doi.org/10.1177/1745691620902426

### Related Work

- Wells, G. L., & Lindsay, R. C. L. (1980). On estimating the diagnosticity of eyewitness non-identifications. *Psychological Bulletin, 88*(3), 776-784.

- Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness identification: Bayesian information gain, base-rate effect equivalency curves, and reasonable suspicion. *Law and Human Behavior, 39*(1), 99-122.

- National Research Council. (2014). *Identifying the culprit: Assessing eyewitness identification*. Washington, DC: The National Academies Press.

- Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities in eyewitness identification practice: Dissociations between receiver operating characteristic (ROC) analysis and expected utility analysis. *Law and Human Behavior, 43*(1), 26-44.

## Support

For questions or issues:
- Open an issue on GitHub
- Email: colin.tredoux@uct.ac.za

## License

This implementation is part of the r4lineups package (CC0 License).
