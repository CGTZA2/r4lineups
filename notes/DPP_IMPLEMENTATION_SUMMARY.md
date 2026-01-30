# Deviation from Perfect Performance (DPP) Implementation Summary

## Overview
Implemented DPP metric following Smith et al. (2018/2019). DPP provides a normalized measure of ROC performance that is less affected by curve truncation than pAUC.

## Implementation Status: ✅ COMPLETE

## Files Created/Modified

### New Files
- `R/dpp_functions.R` - Main implementation with DPP analysis functions
- `notes/test_dpp.R` - Test script validating all functions
- `notes/test_dpp.png` - Example DPP plot
- `notes/test_dpp_comparison.png` - Example comparison plot

## Functions Implemented

### 1. `make_dpp()`
**Purpose**: Compute Deviation from Perfect Performance from lineup data

**Parameters**:
- `data`: Standard lineup dataframe OR ROC object from `make_rocdata()`
- `lineup_size`: Number of people in lineup (default = 6)
- `use_roc_obj`: Whether data is already a ROC object (default = FALSE)

**Returns**:
- **dpp**: Deviation from Perfect Performance (0 = perfect, 1 = worst)
- **auc_observed**: Area under observed ROC curve
- **auc_perfect**: Area under perfect ROC curve (for same FA range)
- **roc_data**: ROC curve data points
- **perfect_roc**: Perfect ROC curve data
- **max_fa**: Maximum false alarm rate observed

**DPP Formula**:
```
DPP = 1 - (AUC_observed / AUC_perfect)
```

Where:
- **AUC_observed**: Area under actual ROC curve (trapezoidal rule)
- **AUC_perfect**: Area under perfect ROC curve = max_FA × 1.0
  - Perfect curve: (0,0) → (0,1) → (max_FA, 1)
  - Immediate jump to 100% hit rate, then horizontal

**Interpretation**:
- **DPP = 0**: Perfect performance (all IDs correct at 0% false alarms)
- **DPP close to 0**: Excellent performance
- **DPP = 0.5**: Moderate deviation from perfect
- **DPP close to 1**: Poor performance (near chance)
- **Lower DPP = Better performance**

### 2. `compare_dpp()`
**Purpose**: Compare DPP between two lineup procedures

**Parameters**:
- `data_proc_a`: Dataframe for procedure A
- `data_proc_b`: Dataframe for procedure B
- `lineup_size`: Lineup size

**Returns**:
- **dpp_a**: DPP for procedure A
- **dpp_b**: DPP for procedure B
- **dpp_difference**: DPP_A - DPP_B (negative = A is better)
- Full DPP objects for both procedures

**Interpretation of Difference**:
- **Negative**: Procedure A has better performance (lower DPP)
- **Positive**: Procedure B has better performance
- **Zero**: Procedures equivalent

### 3. `plot_dpp()`
**Purpose**: Visualize DPP with observed and perfect ROC curves

**Features**:
- Observed ROC curve (blue solid line)
- Perfect ROC curve (red dashed line)
- Shaded area between curves (represents deviation)
- DPP value annotation
- AUC values displayed

**Visual Interpretation**:
- Smaller shaded area = better performance (lower DPP)
- Observed curve closer to perfect curve = better discrimination

### 4. `plot_dpp_comparison()`
**Purpose**: Compare DPP between two procedures visually

**Options**:
- **side-by-side**: Separate panels for each procedure (requires `patchwork`)
- **overlay**: Both curves on same axes with color coding

**Features**:
- Shows both observed and perfect curves for each procedure
- Displays DPP values for easy comparison
- Difference calculation in subtitle

### 5. `compute_dpp()`
**Purpose**: Main wrapper function for DPP analysis

**Returns**: S3 object of class `lineup_dpp` with:
- DPP plot
- DPP value
- AUC values
- ROC data

### 6. `print.lineup_dpp()`
**Purpose**: Formatted printing for DPP results

## Test Results

All tests passed successfully:

### Test 1: Basic DPP computation
- **DPP**: 0.5396
- **AUC observed**: 0.0875
- **AUC perfect**: 0.19
- **Max FA rate**: 0.19
- Valid range [0,1]: ✓

### Test 2: Calculation verification
- Manual calculation: 0.539649
- Function result: 0.539649
- Match: TRUE ✓

### Test 3: Procedure comparison
**Simulated procedures**:
- Procedure A (better): DPP = 0.5624
- Procedure B (worse): DPP = 0.6681
- Difference: -0.1057

**Interpretation**: A has lower DPP (better performance) ✓

### Test 4: DPP interpretation
**Perfect performance**:
- All correct IDs at max confidence, no false alarms
- DPP = 0.0 (perfect) ✓

**Chance performance**:
- ~1/6 suspect ID rate for both TP and TA
- DPP = 0.9037 (close to 1, poor) ✓

### Test 5: ROC object input
- DPP from raw data: 0.539649
- DPP from ROC object: 0.539649
- Match: TRUE ✓

### Test 6: Plotting
- DPP plot: ✓
- Comparison plot (overlay): ✓
- Comparison plot (side-by-side): ✓
- Plots saved successfully

### Test 7: Wrapper function
`compute_dpp()` successfully:
- Computed DPP and AUC values
- Generated formatted output
- Created S3 object with print method

### Test 8: Relationship to pAUC
- **pAUC**: 0.0875 (higher = better)
- **DPP**: 0.5396 (lower = better)
- **Relationship**: DPP = 1 - (pAUC / perfect_area)
- Inversely related but not linearly

## Integration with r4lineups

### Consistent with Existing Code
- Uses same data structure as ROC/CAC functions
- Can accept ROC objects from `make_rocdata()`
- Follows roxygen2 documentation style
- Uses tibble and ggplot2 (already in DESCRIPTION)
- S3 class system with custom print methods

### Dependencies
All required packages already in DESCRIPTION:
- tibble
- ggplot2

Optional for side-by-side plots:
- patchwork (gracefully degrades if not available)

### Ready for Package Build
- All functions have `@export` tags
- Complete roxygen documentation
- NAMESPACE will be auto-generated with `devtools::document()`

## Key Features

1. **ROC Truncation Robustness**:
   - DPP normalized to achievable performance within observed FA range
   - Fair comparison even when confidence distributions differ
   - Less confounded than pAUC by truncation

2. **Intuitive Interpretation**:
   - 0 = perfect performance
   - 1 = worst performance
   - Single number summary
   - Visual representation with shaded deviation area

3. **Flexible Input**:
   - Works with raw data or ROC objects
   - Integrates with existing `make_rocdata()` function
   - Consistent interface

4. **Comparison Tools**:
   - Side-by-side and overlay visualizations
   - Automatic difference calculation
   - Clear indication of which procedure is better

## Theoretical Background

### Why DPP?

**Problem with pAUC**:
- Sensitive to ROC truncation points
- Can mis-rank procedures when curves truncate differently
- Not normalized relative to achievable performance

**DPP Solution**:
- Compares to perfect performance within same FA range
- Normalized: always between 0 and 1
- More consistent rankings across datasets

### Perfect ROC Curve

The "perfect" ROC is not the theoretical perfect (instant jump to 100% at FA=0% across all FA ranges) but rather the **best achievable performance given the observed FA range**:

```
Perfect ROC = {(0,0), (0,1), (max_FA, 1)}
```

This represents:
1. Start at origin (0% FA, 0% hit)
2. Immediately jump to 100% hit rate with 0% FA
3. Maintain 100% hit rate as FA increases to observed maximum

### Area Calculations

**AUC Perfect**:
```
AUC_perfect = max_FA × 1.0
```
(Rectangle with width = max_FA, height = 1.0)

**AUC Observed**:
```
AUC_observed = Σ [(FA_i - FA_{i-1}) × (HR_i + HR_{i-1})/2]
```
(Trapezoidal rule integration)

**DPP**:
```
DPP = 1 - (AUC_observed / AUC_perfect)
    = (AUC_perfect - AUC_observed) / AUC_perfect
    = Proportion of perfect area that is lost
```

### Why This Works

Consider two procedures with different confidence distributions:
- **Procedure A**: High confidence overall → ROC extends to FA=0.3
- **Procedure B**: Low confidence overall → ROC extends to FA=0.1

**pAUC comparison**:
- pAUC_A might be higher simply because it covers more FA range
- Misleading if actual discrimination is similar

**DPP comparison**:
- Each normalized to its own achievable perfect area
- DPP_A relative to perfect curve at FA=0.3
- DPP_B relative to perfect curve at FA=0.1
- Fair comparison of relative performance

## Practical Applications

### 1. Procedure Evaluation
```r
# Evaluate lineup procedure quality
dpp <- compute_dpp(lineup_data)
print(dpp)

# Interpretation guidelines:
# DPP < 0.3: Excellent
# DPP 0.3-0.5: Good
# DPP 0.5-0.7: Moderate
# DPP > 0.7: Poor
```

### 2. Procedure Comparison
```r
# Compare two lineup procedures
comparison <- compare_dpp(sequential_data, simultaneous_data)

if (comparison$dpp_difference < -0.05) {
  cat("Sequential significantly better\n")
} else if (comparison$dpp_difference > 0.05) {
  cat("Simultaneous significantly better\n")
} else {
  cat("Procedures roughly equivalent\n")
}
```

### 3. When to Use DPP vs pAUC
Use **DPP** when:
- Confidence distributions differ between procedures
- ROC curves truncate at different points
- You need normalized, interpretable metric
- Comparing across different studies/datasets

Use **pAUC** when:
- ROC curves have similar truncation
- You want traditional ROC summary
- Focusing on specific FA region
- Following established protocols

### 4. Meta-Analysis
```r
# DPP enables fair comparison across studies
# even with different confidence scales/distributions
studies <- list(study1_data, study2_data, study3_data)
dpps <- sapply(studies, function(d) compute_dpp(d)$dpp)
mean_dpp <- mean(dpps)
```

## Advantages and Limitations

### Advantages

1. **Truncation robust**: Fair comparison despite different confidence distributions
2. **Normalized**: Always 0-1 scale, interpretable
3. **Single number**: Easy to report and compare
4. **Visual intuitive**: Shaded area shows deviation clearly
5. **Consistent rankings**: Less affected by arbitrary factors

### Limitations

1. **Summary metric**: Loses information about ROC curve shape
2. **FA range dependent**: Still limited to observed FA range
3. **Not sensitive to specific regions**: Weights all FA values equally
4. **Perfect baseline**: Assumes perfect performance is the right comparison

### When DPP May Be Misleading

1. **Very different FA ranges**: If one procedure goes to FA=0.9 and another to FA=0.1, DPP comparison still problematic
2. **Policy-relevant regions**: If decision-makers only care about low-FA region, full DPP may not capture that
3. **Multimodal ROCs**: DPP doesn't capture multiple crossing points or complex shapes

## Relationship to Other Metrics

### DPP vs pAUC
```
DPP = 1 - (pAUC / perfect_area)
```
- Inversely related
- pAUC higher = better; DPP lower = better
- DPP normalizes pAUC

### DPP vs d' (Sensitivity)
- d' measures discrimination capacity
- DPP measures actual performance deviation
- d' independent of criterion; DPP incorporates criterion placement

### DPP vs Expected Utility
- DPP: discrimination-focused, no costs/benefits
- EU: incorporates base rates and costs
- DPP for comparing discrimination; EU for comparing value

### DPP vs Calibration
- DPP: overall performance deviation
- Calibration: confidence-accuracy match
- Complementary measures

## Mathematical Notes

### Trapezoidal Rule for AUC

```
AUC = Σ_{i=1}^{n-1} [(x_{i+1} - x_i) × (y_{i+1} + y_i)/2]
```

Where points are sorted by x (FA rate).

### Edge Cases

1. **No false alarms** (max_FA = 0):
   - If hit rate = 100%: DPP = 0 (perfect)
   - Otherwise: DPP = NA (can't compute)

2. **All false alarms** (hit rate = 0):
   - DPP → 1 (worst possible)

3. **Chance performance** (ROC on diagonal):
   - AUC_obs ≈ 0.5 × max_FA²
   - AUC_perf = max_FA
   - DPP ≈ 1 - (0.5 × max_FA) = depends on max_FA

## Validation

### Test Coverage
- ✓ Basic computation
- ✓ Manual calculation verification
- ✓ Procedure comparison
- ✓ Perfect performance (DPP=0)
- ✓ Chance performance (DPP~1)
- ✓ ROC object input
- ✓ Plotting functions
- ✓ Wrapper function
- ✓ Edge case handling

### Benchmarking
Tested against known outcomes:
- Perfect procedure → DPP = 0 ✓
- Chance procedure → DPP ≈ 0.9 ✓
- Better procedure → Lower DPP ✓

## References

Smith, A. M., Wilford, M. M., Quigley-McBride, A., & Wells, G. L. (2019). Mistaken eyewitness identification rates increase when either witnessing or testing conditions get worse. *Law and Human Behavior, 43*(4), 358-368.

Smith, A. M., et al. (2018). Deviation from perfect performance measures the diagnostic utility of eyewitness lineups but partial area under the ROC does not. *Journal of Applied Research in Memory and Cognition*.

## Next Steps

To use in the package:
1. Run `devtools::document()` to update NAMESPACE
2. Run `devtools::check()` to verify no issues
3. Install with `devtools::install()`

Example usage:
```r
library(r4lineups)
data(lineup_example)

# Basic DPP analysis
dpp <- compute_dpp(lineup_example)
print(dpp)
plot(dpp$plot)

# Compare procedures
comparison <- compare_dpp(proc_a, proc_b)
plot_dpp_comparison(comparison, layout = "overlay")

# Use with existing ROC object
roc_obj <- make_rocdata(lineup_data)
dpp <- make_dpp(roc_obj, use_roc_obj = TRUE)
```

## Connection to Other Methods

- **ROC/pAUC**: DPP is normalized version of pAUC analysis
- **CAC**: Both measure accuracy, but DPP focuses on ID performance
- **Calibration**: DPP measures discrimination; calibration measures match
- **Bayesian/Utility**: DPP is discrimination metric; others incorporate priors/costs
- **Full ROC**: DPP can be adapted to full ROC (include rejections and filler IDs)

Each method provides complementary insights for comprehensive lineup evaluation.
