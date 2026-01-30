# Calibration Metrics Implementation Summary

## Overview
Implemented calibration analysis functions following Juslin, Olsson, & Winman (1996) and Brewer & Wells (2006).

## Implementation Status: ✅ COMPLETE

## Files Created/Modified

### New Files
- `R/calibration_functions.R` - Main implementation with all calibration functions
- `notes/test_calibration.R` - Test script validating all functions

## Functions Implemented

### 1. `make_calibration_data()`
**Purpose**: Compute calibration statistics from lineup identification data

**Parameters**:
- `data`: Standard lineup dataframe (target_present, identification, confidence)
- `confidence_bins`: Optional binning specification
- `choosers_only`: Whether to analyze only suspect IDs (default TRUE)
- `lineup_size`: Lineup size for estimating false IDs (default 6)

**Returns**:
- Calibration data by bin (mean confidence, accuracy, sample sizes)
- **C** (Calibration statistic): Weighted mean squared deviation between confidence and accuracy
- **O/U** (Over/underconfidence): Overall bias (mean confidence - mean accuracy)
- **NRI** (Normalized Resolution Index): Standardized discrimination ability
- Overall accuracy and confidence

**Formula for C**:
```
C = Σ (n_j / N) * (conf_j - acc_j)²
```

**Formula for NRI**:
```
NRI = [(1/N) * Σ n_j * (acc_j - overall_acc)²] / [overall_acc * (1 - overall_acc)]
```

### 2. `make_calibration_by_condition()`
**Purpose**: Compute calibration separately for different experimental conditions

**Parameters**:
- `data`: Lineup data with additional condition variables
- `condition_vars`: Vector of column names defining conditions
- All parameters from `make_calibration_data()`

**Returns**:
- List of calibration results for each condition
- Summary table comparing C, O/U, NRI across conditions

**Use Cases**:
- Compare calibration across lineup instructions (biased vs unbiased)
- Examine effect of foil similarity on calibration
- Assess impact of base rates or other system variables

### 3. `make_calibration_gg()`
**Purpose**: Create calibration curve plot

**Features**:
- Plots mean confidence vs accuracy by bin
- Diagonal line shows perfect calibration
- Point size reflects sample size
- Optional statistics annotation (C, O/U, NRI)
- Points above diagonal = underconfidence
- Points below diagonal = overconfidence

### 4. `make_calibration_by_condition_gg()`
**Purpose**: Plot calibration curves across conditions

**Options**:
- Faceted plots (separate panel per condition)
- Overlaid plots (all conditions on same axes)
- Optional statistics per condition

### 5. `make_calibration()`
**Purpose**: Main wrapper function - compute and plot calibration

**Returns**: S3 object of class `lineup_calibration` with:
- Calibration plot
- Calibration data table
- All statistics (C, O/U, NRI)
- Overall performance metrics

### 6. `print.lineup_calibration()`
**Purpose**: Formatted printing for calibration results

## Test Results

All tests passed successfully using the `lineup_example` dataset:

### Test 1: Basic calibration (unbinned)
- **N = 75** suspect IDs
- **C = 0.0276** (good calibration)
- **O/U = +0.0267** (slight overconfidence)
- **NRI = 0.4059** (moderate resolution)
- **Overall accuracy = 0.800**

### Test 2: Binned calibration (0-60, 60-80, 80-100)
- **C = 0.0081** (excellent calibration with binning)
- **NRI = 0.267** (good resolution)
- Accuracy increases monotonically with confidence

### Test 3: All responses (including rejections and filler IDs)
- **C = 0.1124** (poorer calibration when including non-choosers)
- **O/U = +0.287** (substantial overconfidence)
- Demonstrates importance of `choosers_only` parameter

### Test 4: Statistics validation
- All statistics in valid ranges
- C ≥ 0 ✓
- NRI ≥ 0 ✓
- Accuracies and confidences in [0,1] ✓

### Test 5: Calibration by condition
- Successfully stratified by mock "instruction" variable
- Biased: C = 0.0045, O/U = -0.011, NRI = 0.210
- Unbiased: C = 0.015, O/U = 0.063, NRI = 0.326

## Integration with r4lineups

### Consistent with Existing Code
- Uses same data structure as ROC/CAC functions
- Follows roxygen2 documentation style
- Returns S3 objects with print methods
- Uses tibble and ggplot2 (already in DESCRIPTION)

### No Additional Dependencies Required
All required packages already in DESCRIPTION:
- tibble
- ggplot2
- dplyr (for condition filtering)

### Ready for Package Build
- All functions have `@export` tags
- Complete roxygen documentation
- NAMESPACE will be auto-generated with `devtools::document()`

## Key Features

1. **Flexibility**:
   - Works with raw or binned confidence
   - Analyze choosers only or all responses
   - Single condition or stratified analysis

2. **Interpretability**:
   - Clear calibration curve visualization
   - Three complementary metrics (C, O/U, NRI)
   - Comparison to perfect calibration diagonal

3. **Validation**:
   - Input validation matching existing functions
   - Handles edge cases (zero variance, empty bins)
   - Comprehensive test coverage

## References

Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and diagnosticity of confidence in eyewitness identification: Comments on what can be inferred from the low confidence-accuracy correlation. *Journal of Experimental Psychology: Learning, Memory, and Cognition, 22*(5), 1304-1316.

Brewer, N., & Wells, G. L. (2006). The confidence-accuracy relationship in eyewitness identification: Effects of lineup instructions, foil similarity, and target-absent base rates. *Journal of Experimental Psychology: Applied, 12*(1), 11-30.

## Next Steps

To use in the package:
1. Run `devtools::document()` to update NAMESPACE
2. Run `devtools::check()` to verify no issues
3. Install with `devtools::install()`

Example usage:
```r
library(r4lineups)
data(lineup_example)

# Basic calibration
cal <- make_calibration(lineup_example,
                       confidence_bins = c(0, 60, 80, 100))
print(cal)
plot(cal$plot)

# By condition
cal_by_instr <- make_calibration_by_condition(
  my_data,
  condition_vars = "instruction_type",
  confidence_bins = c(0, 60, 80, 100)
)
print(cal_by_instr$condition_summary)
```
