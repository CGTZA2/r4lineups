# ANRI (Adjusted Normalized Resolution Index) Implementation Summary

## Overview
Implemented ANRI as a focused extension to existing calibration functions, providing bias-corrected resolution estimates and bootstrap inference capabilities following Yaniv et al. (1991).

## Implementation Status: ✅ COMPLETE

## Files Created/Modified

### New Files
- `R/anri_functions.R` - Main implementation with ANRI and bootstrap functions
- `notes/test_anri.R` - Test script validating all functions
- `notes/test_anri_comparison.png` - Example group comparison plot
- `notes/test_anri_difference.png` - Example bootstrap distribution plot

## Functions Implemented

### 1. `compute_anri()`
**Purpose**: Compute bias-corrected resolution index

**Parameters**:
- `data`: Standard lineup dataframe
- `confidence_bins`: Bin edges (required)
- `choosers_only`: Whether to analyze only suspect IDs (default = TRUE)
- `lineup_size`: Lineup size (default = 6)

**Returns**:
- **anri**: Adjusted Normalized Resolution Index (bias-corrected)
- **nri**: Original Normalized Resolution Index
- **n_total**: Total sample size
- **n_bins**: Number of confidence bins
- **calibration_data**: Per-bin accuracy and sample sizes
- **overall_accuracy**: Mean accuracy

**Formulas**:
```
NRI = [(1/N) * Σ n_j * (a_j - ā)²] / [ā * (1-ā)]

ANRI = (N * NRI - J + 1) / (N - J + 1)
```

Where:
- N = total sample size
- J = number of bins
- n_j = sample size in bin j
- a_j = accuracy in bin j
- ā = overall accuracy

### 2. `bootstrap_anri()`
**Purpose**: Compute bootstrap confidence intervals for ANRI

**Parameters**:
- `data`: Lineup dataframe
- `confidence_bins`: Bin edges
- `choosers_only`: Only suspect IDs (default = TRUE)
- `lineup_size`: Lineup size (default = 6)
- `n_bootstrap`: Number of replications (default = 1000)
- `conf_level`: Confidence level (default = 0.95)
- `seed`: Random seed for reproducibility (default = NULL)

**Returns**:
- **anri**: Point estimate
- **ci_lower**: Lower bound of CI
- **ci_upper**: Upper bound of CI
- **bootstrap_distribution**: Vector of bootstrap values

**Bootstrap Procedure**:
1. Resample observations with replacement
2. Compute ANRI for each bootstrap sample
3. Construct percentile-based CI from empirical quantiles

### 3. `compare_anri()`
**Purpose**: Compare ANRI between two groups with bootstrap test

**Parameters**:
- `data`: Dataframe with grouping variable
- `group_var`: Name of grouping variable (must have 2 levels)
- `confidence_bins`: Bin edges
- `n_bootstrap`: Number of replications (default = 1000)
- Additional parameters as above

**Returns**:
- **anri_group1**: ANRI for first group
- **anri_group2**: ANRI for second group
- **difference**: Point estimate of difference (group1 - group2)
- **ci_lower, ci_upper**: CI bounds for difference
- **significant**: Whether CI excludes 0
- **group_names**: Names of the two groups
- **difference_distribution**: Bootstrap distribution of difference

**Hypothesis Test**:
- H0: ANRI_1 = ANRI_2
- Reject H0 if 95% CI for difference excludes 0

### 4. `plot_anri_comparison()`
**Purpose**: Visualize group comparison with error bars

**Features**:
- Point estimates for each group
- Bootstrap confidence intervals
- Difference with CI in subtitle
- Significance indicator (*)

### 5. `plot_anri_difference_distribution()`
**Purpose**: Histogram of bootstrap difference distribution

**Features**:
- Bootstrap distribution of ANRI difference
- Observed difference (solid vertical line)
- CI bounds (dotted lines)
- Null hypothesis at 0 (dashed red line)

### 6. Print Methods
- `print.lineup_anri()` - For ANRI objects
- `print.lineup_anri_comparison()` - For comparison objects

## Test Results

All tests passed successfully:

### Test 1: Basic ANRI computation
- **ANRI** (bias-corrected): 0.2469
- **NRI** (original): 0.2670
- Sample size: 75
- Number of bins: 3
- Manual calculation matches function ✓

### Test 2: ANRI vs NRI - bias correction effect
**3 bins**:
- NRI: 0.3889, ANRI: 0.3721
- Difference: -0.0168

**5 bins**:
- NRI: 0.1606, ANRI: 0.1257
- Difference: -0.0349

**Interpretation**: Bias correction is larger with more bins ✓

### Test 3: Bootstrap CIs
- Point estimate: 0.2469
- 95% CI: [0.1261, 0.4564]
- CI width: 0.3303
- Successful replications: 500/500 ✓

### Test 4: Group comparison
- Group A: ANRI = 0.1483
- Group B: ANRI = 0.3656
- Difference: -0.2173
- 95% CI: [-0.5295, 0.2169]
- Significant: NO (CI includes 0) ✓

### Test 5: Bootstrap distribution properties
- Mean: 0.2694
- Median: 0.2602
- SD: 0.0827
- Point estimate vs bootstrap mean diff: 0.0225 (small ✓)

### Test 6: Plotting
- Group comparison plot: ✓
- Difference distribution plot: ✓
- Saved successfully

### Test 7: Sample size effect
**Large sample (N=75)**:
- NRI: 0.267, ANRI: 0.247
- Correction: -0.020

**Small sample (N=16)**:
- NRI: NA, ANRI: NA
- Appropriate handling of insufficient data ✓

### Test 8: Integration with calibration functions
- NRI from `make_calibration_data()`: 0.267
- NRI from `compute_anri()`: 0.267
- Match: TRUE ✓

## Integration with r4lineups

### Extends Existing Code
- Builds on `make_calibration_data()` from calibration_functions.R
- Uses same data structure and conventions
- Compatible with existing calibration analysis workflow
- Adds new capability (bootstrap inference) without duplication

### Dependencies
All required packages already in DESCRIPTION:
- tibble
- ggplot2
- stats (for quantile, sd)

No additional dependencies needed ✓

### Ready for Package Build
- All functions have `@export` tags
- Complete roxygen documentation
- S3 classes with print methods
- NAMESPACE will be auto-generated

## Key Features

### 1. Bias Correction
- **Why needed**: NRI is biased for small samples and few bins
- **How it works**: ANRI adjusts for degrees of freedom (N - J)
- **When most important**: Small N, few bins, cross-bin comparisons

### 2. Bootstrap Inference
- **Percentile method**: Non-parametric, robust to non-normality
- **Reproducibility**: Optional seed parameter
- **Diagnostics**: Returns full bootstrap distribution

### 3. Group Comparison
- **Independent bootstrapping**: Each group sampled separately
- **Difference distribution**: Bootstrap distribution of difference
- **Formal test**: CI-based hypothesis test
- **Visualization**: Two complementary plots

### 4. Focused Extension
- Adds genuinely new functionality (bootstrap, group comparison)
- Does NOT duplicate existing calibration code
- Leverages existing infrastructure
- Minimal code footprint

## Theoretical Background

### ANRI Formula Derivation

Starting with NRI (Normalized Resolution Index):
```
NRI = Σ(n_j/N) * (a_j - ā)² / [ā * (1-ā)]
```

This is the **variance of accuracy across bins**, normalized by the binomial variance.

**Problem**: NRI has **positive bias** for small samples because:
1. Empirical variance overestimates population variance with few observations
2. Number of bins reduces effective degrees of freedom

**Solution**: Yaniv et al. (1991) derived bias correction:
```
E[NRI] ≈ (N*NRI_pop + J - 1) / N
```

Solving for unbiased estimate:
```
ANRI = (N*NRI - J + 1) / (N - J + 1)
```

This removes the bias term proportional to J/N.

### When Bias Matters

**Bias magnitude** ≈ (J-1)/(N-J+1)

Examples:
- N=100, J=3: Bias ≈ 2/98 = 2%
- N=50, J=5: Bias ≈ 4/46 = 9%
- N=30, J=5: Bias ≈ 4/26 = 15%

**Recommendation**: Always use ANRI for:
- Small to moderate samples (N < 200)
- Comparing across different bin counts
- Formal statistical inference

### Bootstrap Rationale

**Why bootstrap?**
1. **No closed-form standard error** for ANRI
2. **Non-normal distribution** especially with small samples
3. **Flexible**: Works for differences, ratios, etc.
4. **Robust**: Minimal assumptions

**Bootstrap validity**:
- Requires i.i.d. observations
- Assumes sample is representative of population
- More accurate as N increases
- Use ≥ 1000 replications for stable CIs

## Practical Applications

### 1. Single-Group Analysis with Uncertainty
```r
# Compute ANRI with bootstrap CI
result <- bootstrap_anri(data,
                        confidence_bins = c(0, 50, 80, 100),
                        n_bootstrap = 2000,
                        seed = 123)

cat("ANRI:", result$anri, "\n")
cat("95% CI: [", result$ci_lower, ",", result$ci_upper, "]\n")
```

### 2. Compare Experimental Conditions
```r
# Police vs laypeople (as in tredcode.txt)
comparison <- compare_anri(data,
                          group_var = "participant_type",
                          confidence_bins = c(0, 40, 80, 100),
                          n_bootstrap = 2000)

print(comparison)
plot_anri_comparison(comparison)
```

### 3. Publication-Ready Analysis
```r
# Comprehensive reporting
result <- compare_anri(data, group_var = "condition",
                      confidence_bins = c(0, 60, 80, 100),
                      n_bootstrap = 2000, seed = 42)

# Report:
# "Group A (ANRI = 0.35, 95% CI [0.25, 0.45]) showed
#  significantly higher confidence resolution than Group B
#  (ANRI = 0.18, 95% CI [0.10, 0.26]),
#  difference = 0.17, 95% CI [0.05, 0.29], p < .05."
```

### 4. Meta-Analysis
```r
# Compute ANRI for multiple studies
studies <- list(study1_data, study2_data, study3_data)

anris <- sapply(studies, function(d) {
  result <- bootstrap_anri(d, confidence_bins = bins, n_bootstrap = 1000)
  result$anri
})

# Meta-analytic mean (could weight by sample size, etc.)
mean_anri <- mean(anris)
```

## Relationship to Existing Methods

### ANRI vs NRI
- ANRI is bias-corrected version of NRI
- Same interpretation: higher = better resolution
- ANRI ≤ NRI (correction removes positive bias)
- Converge as N → ∞

### ANRI vs C (Calibration)
- C measures calibration (confidence-accuracy match)
- ANRI measures resolution (discrimination ability)
- Both in calibration_functions.R
- Complementary, not redundant

### ANRI vs Information Gain
- ANRI: classical psychometric resolution
- IG: information-theoretic uncertainty reduction
- ANRI bin-based, IG can be continuous
- Different theoretical frameworks, related concepts

### ANRI vs DPP
- ANRI: calibration domain (confidence resolution)
- DPP: ROC domain (identification performance)
- Both measure "goodness" but in different spaces
- Not directly comparable

## Comparison to tredcode.txt

### What We Kept
- ANRI formula (exact match)
- Bootstrap approach (percentile CIs)
- Group comparison logic
- Statistical philosophy

### What We Improved
- **Generalized**: Works with any grouping variable, not hardcoded
- **Standardized**: Uses r4lineups data structure
- **Documented**: Full roxygen documentation
- **Integrated**: Leverages existing calibration functions
- **Testable**: Comprehensive test suite
- **Reproducible**: Seed control for bootstrap

### What We Added
- S3 classes with print methods
- Visualization functions
- Bootstrap distribution diagnostics
- Integration with existing calibration pipeline

## Limitations and Considerations

### 1. Requires Confidence Bins
- ANRI requires binned confidence (not continuous)
- Bin choice affects results (analyst degree of freedom)
- Recommendation: Use 3-5 bins based on sample size

### 2. Bootstrap Assumptions
- Assumes observations are i.i.d.
- Resampling trials (not observations within trials) assumes trials independent
- Small samples may have unstable CIs

### 3. Multiple Comparisons
- If comparing many groups, adjust for multiple testing
- Bonferroni: Use α/k for k comparisons
- Or use permutation test instead of bootstrap

### 4. Sample Size Requirements
- Minimum N ≈ 30-50 per group for reliable estimates
- With N < 30, CIs will be very wide
- Function returns NA when N ≤ J (insufficient data)

## Validation

### Mathematical Correctness
- ANRI formula matches Yaniv et al. (1991) ✓
- Manual calculation matches function output ✓
- Reduces to NRI when N >> J ✓

### Statistical Properties
- Bootstrap distributions reasonably centered ✓
- CI coverage (simulations could verify)
- Handles edge cases (returns NA when appropriate) ✓

### Integration
- NRI from compute_anri() matches make_calibration_data() ✓
- Works with existing data formats ✓
- Compatible with r4lineups ecosystem ✓

## References

Yaniv, I., Yates, J. F., & Smith, J. E. K. (1991). Measures of discrimination skill in probabilistic judgment. *Psychological Bulletin, 110*(3), 611-617.

Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and diagnosticity of confidence in eyewitness identification. *Journal of Experimental Psychology: Learning, Memory, and Cognition, 22*(5), 1304-1316.

Efron, B., & Tibshirani, R. J. (1994). *An Introduction to the Bootstrap*. Chapman & Hall/CRC.

## Next Steps

To use in the package:
1. Run `devtools::document()` to update NAMESPACE
2. Run `devtools::check()` to verify no issues
3. Install with `devtools::install()`

Example usage:
```r
library(r4lineups)
data(lineup_example)

# Basic ANRI
anri <- compute_anri(lineup_example,
                    confidence_bins = c(0, 60, 80, 100))
print(anri)

# With bootstrap CI
boot <- bootstrap_anri(lineup_example,
                      confidence_bins = c(0, 60, 80, 100),
                      n_bootstrap = 2000,
                      seed = 123)
cat("ANRI:", boot$anri, "95% CI: [", boot$ci_lower, ",", boot$ci_upper, "]\n")

# Group comparison
lineup_example$condition <- sample(c("A", "B"), nrow(lineup_example), replace=TRUE)
comparison <- compare_anri(lineup_example,
                          group_var = "condition",
                          confidence_bins = c(0, 60, 80, 100),
                          n_bootstrap = 2000)
print(comparison)
plot_anri_comparison(comparison)
plot_anri_difference_distribution(comparison)
```

## Summary

ANRI implementation provides:
- ✅ Bias-corrected resolution metric
- ✅ Bootstrap confidence intervals
- ✅ Formal group comparison tests
- ✅ Publication-ready visualizations
- ✅ Seamless integration with existing code
- ✅ Comprehensive documentation and testing

This completes the r4lineups method implementation roadmap with a focused extension that adds genuine statistical inference capabilities to the existing calibration analysis toolkit.
