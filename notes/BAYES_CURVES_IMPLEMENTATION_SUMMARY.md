# Bayesian Information-Gain Curves Implementation Summary

## Overview
Implemented Bayesian prior-posterior analysis and Base-Rate Effect-Equivalency (BREE) curves following Wells, Yang, & Smalarz (2015).

## Implementation Status: ✅ COMPLETE

## Files Created/Modified

### New Files
- `R/bayes_curves_functions.R` - Main implementation with Bayesian analysis functions
- `notes/test_bayes_curves.R` - Test script validating all functions
- `notes/test_bayes_prior_posterior.png` - Example prior-posterior plot
- `notes/test_bayes_information_gain.png` - Example information gain plot
- `notes/test_bree.png` - Example BREE curve plot

## Functions Implemented

### 1. `entropy()`
**Purpose**: Helper function to compute binary entropy

**Formula**:
```
H(p) = -(p * log2(p) + (1-p) * log2(1-p))
```

**Returns**: Entropy in bits (0 = certainty, 1 = maximum uncertainty at p=0.5)

### 2. `make_bayes_curves()`
**Purpose**: Compute Bayesian prior-posterior and information gain curves for lineup responses

**Parameters**:
- `data`: Standard lineup dataframe (target_present, identification, confidence)
- `response_categories`: "simple" (suspect/filler/reject) or "confidence" (identification × confidence bins)
- `confidence_bins`: Optional binning specification for confidence-based categories
- `prior_grid`: Grid of prior probabilities to evaluate (default: 0.01 to 0.99 by 0.01)

**Returns**:
- **curves**: Dataframe with prior, posterior, and information gain for each response type
- **likelihoods**: p(x|guilty) and p(x|innocent) for each response
- **response_counts**: Distribution of responses by target presence
- Sample sizes

**Key Formulas**:

Likelihoods (estimated from data):
```
p(x|guilty) = count(x & target_present) / N_guilty
p(x|innocent) = count(x & target_absent) / N_innocent
```

Bayesian updating:
```
p(x) = p(guilty) * p(x|guilty) + (1-p(guilty)) * p(x|innocent)
p(guilty|x) = [p(guilty) * p(x|guilty)] / p(x)
```

Information gain:
```
IG(x) = H(prior) - H(posterior)
```

**Note on Information Gain**:
- **Positive IG**: Evidence reduces uncertainty (diagnostic)
- **Negative IG**: Evidence increases uncertainty (can occur with weak evidence against strong priors)
- **Zero IG**: No change in certainty

### 3. `make_bree_curve()`
**Purpose**: Compute Base-Rate Effect-Equivalency curves comparing two procedures

**Parameters**:
- `data_proc_a`: Lineup data for procedure A
- `data_proc_b`: Lineup data for procedure B
- `reference_response`: Which response to compare (e.g., "suspect")
- `confidence_bins`: Optional confidence binning
- `prior_grid`: Prior probabilities for procedure A

**Returns**:
- **bree_curve**: Dataframe with prior_a, posterior_a, prior_b, and delta
- Likelihoods for both procedures
- Reference response used

**Interpretation**:
```
Delta = prior_b - prior_a
```
- **Delta > 0**: Procedure A more diagnostic (B needs higher base rate to match)
- **Delta < 0**: Procedure B more diagnostic (B needs lower base rate)
- **Delta = 0**: Procedures equally diagnostic

**Use Case**: Answers "How much would the base rate need to change for Procedure B to yield the same posterior as Procedure A?"

### 4. `plot_bayes_prior_posterior()`
**Purpose**: Create prior-posterior curve plot

**Features**:
- Shows Bayesian updating for each response type
- Diagonal line = no update (posterior = prior)
- Curves above diagonal = increase belief in guilt
- Curves below diagonal = decrease belief in guilt
- Color-coded by response type

### 5. `plot_bayes_information_gain()`
**Purpose**: Plot information gain curves

**Features**:
- Shows uncertainty reduction for each response
- Higher values = more diagnostic
- Peak typically near prior = 0.5 (maximum initial uncertainty)
- Color-coded by response type

### 6. `plot_bree()`
**Purpose**: Plot BREE curve

**Features**:
- Shows base-rate shift (delta) across prior probabilities
- Reference line at delta = 0 (equal diagnosticity)
- Positive region = Procedure A more diagnostic
- Negative region = Procedure B more diagnostic

### 7. `print.lineup_bayes_curves()`
**Purpose**: Formatted printing for Bayesian analysis results

## Test Results

All tests passed successfully:

### Test 1: Simple Bayesian curves (3 categories)
**Response likelihoods:**
- Suspect ID: p(x|guilty)=0.60, p(x|innocent)=0.15
- Rejection: p(x|guilty)=0.22, p(x|innocent)=0.61
- Filler ID: p(x|guilty)=0.18, p(x|innocent)=0.24

**At prior = 0.5:**
- Suspect ID → posterior = 0.80 (increases belief in guilt ✓)
- Rejection → posterior = 0.265 (decreases belief in guilt ✓)

### Test 2: Confidence-based curves (6 categories)
Successfully computed curves for:
- High-confidence suspect IDs: p(x|guilty)=0.38, p(x|innocent)=0
- Medium-confidence suspect IDs: p(x|guilty)=0.17, p(x|innocent)=0.10
- Low-confidence suspect IDs: p(x|guilty)=0.05, p(x|innocent)=0.05
- Plus filler IDs and rejections by confidence

### Test 3: Information gain properties
- **Range**: [-0.424, 0.424] bits
- Negative values occur correctly when weak evidence meets strong priors
- All posteriors in valid range [0, 1] ✓

### Test 4: BREE curve comparison
**Simulated procedures:**
- Procedure A (more diagnostic):
  - Hit rate = 0.71, False alarm rate = 0.11
- Procedure B (less diagnostic):
  - Hit rate = 0.54, False alarm rate = 0.33

**Result:**
- Delta at prior = 0.5: **+0.298**
- Interpretation: Procedure B needs base rate of 0.798 (vs 0.5 for A) to achieve same posterior
- Confirms Procedure A is more diagnostic ✓

### Test 5: Plotting
All three plot types created successfully:
- Prior-posterior curves ✓
- Information gain curves ✓
- BREE curves ✓

## Integration with r4lineups

### Consistent with Existing Code
- Uses same data structure as ROC/CAC/calibration functions
- Follows roxygen2 documentation style
- Uses tibble, ggplot2, and dplyr (already in DESCRIPTION)
- Returns structured lists with print methods

### No Additional Dependencies Required
All required packages already in DESCRIPTION:
- tibble
- ggplot2
- dplyr

### Ready for Package Build
- All functions have `@export` tags
- Complete roxygen documentation
- NAMESPACE will be auto-generated with `devtools::document()`

## Key Features

1. **Flexibility**:
   - Simple (3 categories) or confidence-based response categories
   - Customizable prior grid
   - Compare any two procedures with BREE

2. **Theoretical Rigor**:
   - Proper Bayesian updating with likelihoods estimated from data
   - Information-theoretic uncertainty quantification
   - Base-rate equivalency for fair procedure comparison

3. **Interpretability**:
   - Clear visualization of how evidence updates beliefs
   - Diagnostic value quantified as information gain
   - BREE curves show practical base-rate shifts

4. **Validation**:
   - Comprehensive test coverage
   - Results match theoretical expectations
   - Handles edge cases (zero likelihoods, extreme priors)

## Practical Applications

### 1. Evaluating Lineup Diagnosticity
```r
# How diagnostic are high-confidence suspect IDs?
bayes <- make_bayes_curves(data,
                           response_categories = "confidence",
                           confidence_bins = c(0, 60, 80, 100))

# Examine likelihood ratios
print(bayes$likelihoods)

# Visualize updating
plot_bayes_prior_posterior(bayes)
```

### 2. Comparing Lineup Procedures
```r
# Compare sequential vs simultaneous lineups
bree <- make_bree_curve(sequential_data,
                        simultaneous_data,
                        reference_response = "suspect")

# Positive delta means sequential is more diagnostic
plot_bree(bree)
```

### 3. Reasonable Suspicion Analysis
```r
# What base rate justifies an arrest?
# If reasonable suspicion requires posterior > 0.5:
bayes <- make_bayes_curves(data, response_categories = "simple")

# Check which priors yield posterior > 0.5 for suspect ID
suspect_curves <- bayes$curves[bayes$curves$response == "suspect", ]
arrest_threshold <- min(suspect_curves$prior[suspect_curves$posterior > 0.5])
```

## Theoretical Background

### Why Bayesian Analysis?

Traditional measures (hit rate, false alarm rate) are **prevalence-dependent** - their interpretation changes with base rates. Bayesian analysis:

1. Makes base-rate dependence **explicit**
2. Quantifies **posterior probability** - what matters for decisions
3. Allows **fair comparison** across different base rates via BREE

### Information Gain vs Traditional Metrics

**Information gain** advantages:
- Proper interval scale (bits)
- Accounts for both hit rate AND false alarm rate
- Sensitive to response distribution, not just one criterion
- Can be aggregated across responses (see EIG implementation for this)

### BREE Curves

BREE curves answer: "If Procedure B is used in a jurisdiction with a different base rate, what base rate makes it equivalent to Procedure A in the original jurisdiction?"

This enables:
- Fair comparison of procedures tested under different conditions
- Policy decisions about procedure choice given local crime rates
- Understanding practical vs statistical significance

## References

Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness identification: Bayesian information gain, base-rate effect-equivalency curves, and reasonable suspicion. *Law and Human Behavior, 39*(2), 99-122.

## Next Steps

To use in the package:
1. Run `devtools::document()` to update NAMESPACE
2. Run `devtools::check()` to verify no issues
3. Install with `devtools::install()`

Example usage:
```r
library(r4lineups)
data(lineup_example)

# Basic Bayesian analysis
bayes <- make_bayes_curves(lineup_example,
                           response_categories = "simple")
print(bayes)
plot_bayes_prior_posterior(bayes)
plot_bayes_information_gain(bayes)

# Compare procedures
bree <- make_bree_curve(data_proc_a, data_proc_b,
                        reference_response = "suspect")
plot_bree(bree)
```

## Connection to Other Methods

- **EIG (Expected Information Gain)**: Aggregate information gain across all responses, weighted by response probability. Bayes curves provide per-response IG; EIG sums them.
- **ROC/CAC**: ROC shows hit rate vs false alarm rate tradeoff; Bayesian analysis converts these to posterior probabilities at different base rates.
- **Calibration**: Calibration assesses confidence-accuracy match; Bayesian analysis shows how confidence should update beliefs.

## Mathematical Notes

### Solving for Prior in BREE

Given target posterior `post` and likelihoods `p(x|g)` and `p(x|i)`, find prior:

```
posterior = prior * p(x|g) / [prior * p(x|g) + (1-prior) * p(x|i)]

Solving for prior:
prior = (posterior * p(x|i)) / [p(x|g) * (1-posterior) + posterior * p(x|i)]
```

This formula is implemented in `make_bree_curve()` to find the equivalent prior for Procedure B.

### Edge Cases

1. **Zero likelihood**: When p(x|g)=0 or p(x|i)=0, posterior becomes 0 or 1 respectively
2. **Equal likelihoods**: When p(x|g)=p(x|i), no update occurs (posterior=prior, IG=0)
3. **Extreme priors**: When prior ≈ 0 or 1, entropy is already low, limiting potential IG
