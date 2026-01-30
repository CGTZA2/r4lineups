# Expected Utility Analysis Implementation Summary

## Overview
Implemented expected utility analysis for lineup identification following Lampinen, Smith, & Wells (2019).

## Implementation Status: ✅ COMPLETE

## Files Created/Modified

### New Files
- `R/utility_functions.R` - Main implementation with utility analysis functions
- `notes/test_utility.R` - Test script validating all functions
- `notes/test_utility_curve.png` - Example utility curve plot
- `notes/test_utility_difference.png` - Example utility difference plot

## Functions Implemented

### 1. `make_utility_curves()`
**Purpose**: Compute expected utility at different confidence criteria

**Parameters**:
- `data`: Standard lineup dataframe (target_present, identification, confidence)
- `base_rate`: Prior probability that suspect is guilty (0 to 1)
- `utility_matrix`: Named vector with utilities for outcomes (tp, fn, fp, tn)
- `lineup_size`: Number of people in lineup (default = 6)
- `criteria`: How to define decision criteria ("confidence" or "all")

**Default Utility Matrix**:
```r
c(tp = 1,      # True positive: Correct suspect ID
  fn = -0.5,   # False negative: Miss/rejection of guilty
  fp = -2,     # False positive: Innocent suspect ID
  tn = 0.5)    # True negative: Correct rejection of innocent
```

**Returns**:
- **utility_data**: Dataframe with criterion, hit rate, false alarm rate, and expected utility
- **max_utility**: Maximum expected utility and corresponding criterion
- **avg_utility**: Average expected utility across criteria
- **utility_all_ids**: Expected utility if all IDs are accepted
- Base rate and utility matrix used

**Expected Utility Formula**:
```
EU = base_rate * [hit * U_tp + (1-hit) * U_fn] +
     (1-base_rate) * [fa * U_fp + (1-fa) * U_tn]
```

Where:
- `hit` = p(suspect ID | guilty) at criterion
- `fa` = p(suspect ID | innocent) at criterion
- `U_tp`, `U_fn`, `U_fp`, `U_tn` = utilities for each outcome

### 2. `make_utility_difference()`
**Purpose**: Compare expected utility between two procedures across base rates

**Parameters**:
- `data_proc_a`: Lineup data for procedure A
- `data_proc_b`: Lineup data for procedure B
- `base_rate_grid`: Vector of base rates to evaluate (default: 0.01 to 0.99)
- `utility_matrix`: Named vector of utilities
- `utility_type`: Which utility to compare ("max", "avg", or "all")
- `lineup_size`: Lineup size

**Returns**:
- **difference_curve**: Dataframe with base_rate, eu_a, eu_b, and difference
- **utility_type**: Type of utility compared
- **utility_matrix**: Utility matrix used
- **crossover_points**: Base rates where procedures have equal utility

**Interpretation**:
```
Utility Difference = EU_A - EU_B
```
- **Positive**: Procedure A has higher utility (preferred)
- **Negative**: Procedure B has higher utility (preferred)
- **Zero**: Procedures equivalent at that base rate

### 3. `plot_utility_curves()`
**Purpose**: Visualize expected utility across confidence criteria

**Features**:
- Line plot showing expected utility at each criterion
- Highlights maximum utility point
- Optional reject-all baseline
- Displays base rate and utility matrix in caption

### 4. `plot_utility_difference()`
**Purpose**: Plot utility difference between procedures

**Features**:
- Shows difference across base rate range
- Shaded regions indicate which procedure is better:
  - Blue (positive) = Procedure A better
  - Red (negative) = Procedure B better
- Marks crossover points where procedures are equivalent
- Reference line at zero (equal utility)

### 5. `compare_utility()`
**Purpose**: Main wrapper function for procedure comparison

**Returns**: S3 object of class `lineup_utility_comparison` with:
- Utility difference plot
- Difference curve data
- Utility type and matrix
- Crossover points

### 6. `print.lineup_utility_comparison()`
**Purpose**: Formatted printing for utility comparison results

## Test Results

All tests passed successfully:

### Test 1: Basic utility curves
**Sample data** (first 5 criteria):
- Criterion 100: hit=0.17, fa=0.00, EU=0.128
- Criterion 90: hit=0.38, fa=0.00, EU=0.285
- Criterion 80: hit=0.48, fa=0.06, EU=0.285
- Criterion 70: hit=0.55, fa=0.112, EU=0.273
- Criterion 60: hit=0.60, fa=0.133, EU=0.283

**Optimal criterion**: 90 (EU = 0.285, hit = 0.38, fa = 0.00)

**Summary statistics**:
- Maximum utility: 0.285
- Average utility: 0.235
- Utility (all IDs): 0.128

### Test 2: Effect of base rate
**Optimal criterion changes with base rate**:
- Base rate 0.1: Criterion 90 (EU=0.457, hit=0.38, fa=0.00)
- Base rate 0.3: Criterion 90 (EU=0.371, hit=0.38, fa=0.00)
- Base rate 0.5: Criterion 90 (EU=0.285, hit=0.38, fa=0.00)
- Base rate 0.7: Criterion 60 (EU=0.330, hit=0.60, fa=0.13)
- Base rate 0.9: Criterion 60 (EU=0.377, hit=0.60, fa=0.13)

**Interpretation**:
- At low base rates (guilty suspects rare), optimal to be conservative (high criterion)
- At high base rates (guilty suspects common), optimal to be liberal (low criterion)

### Test 3: Effect of cost structure
**Conservative** (FP cost = -5):
- Optimal criterion: 90
- Hit rate: 0.38
- Strategy: Accept only high-confidence IDs to avoid false positives

**Liberal** (FP cost = -0.5):
- Optimal criterion: 60
- Hit rate: 0.60
- Strategy: Accept more IDs since false positive cost is lower

**Validation**: Conservative has higher criterion (lower hit rate) ✓

### Test 4: Utility difference between procedures
**Procedure comparison**:
- Procedure A: Higher hit rate (0.70 vs 0.55), slightly higher FA (0.15 vs 0.08)
- Procedure B: Lower hit rate, lower FA (more conservative)

**Utility difference at different base rates**:
- Base rate 0.1: -0.0045 (Proc B slightly better)
- Base rate 0.3: -0.0225 (Proc B better)
- Base rate 0.5: -0.0408 (Proc B better)
- Base rate 0.7: +0.0573 (Proc A better)
- Base rate 0.9: +0.1391 (Proc A better)

**Crossover point**: 0.581 (procedures equivalent at base rate ~58%)

**Interpretation**:
- At low base rates, Proc B's conservatism is advantageous
- At high base rates, Proc A's higher hit rate is worth the extra false alarms

### Test 5: Calculation verification
- Manual calculation: 0.1275
- Function result: 0.1275
- Match: TRUE ✓

### Test 6: Plotting
- Utility curve plot: ✓
- Utility difference plot: ✓
- Saved to notes/ directory

### Test 7: Wrapper function
`compare_utility()` successfully:
- Computed utility differences across base rates
- Identified crossover point (0.581)
- Generated formatted output
- Created S3 object with print method

## Integration with r4lineups

### Consistent with Existing Code
- Uses same data structure as ROC/CAC/calibration/Bayesian functions
- Follows roxygen2 documentation style
- Uses tibble and ggplot2 (already in DESCRIPTION)
- Returns structured lists with print methods
- S3 class system with custom print methods

### No Additional Dependencies Required
All required packages already in DESCRIPTION:
- tibble
- ggplot2

### Ready for Package Build
- All functions have `@export` tags
- Complete roxygen documentation
- NAMESPACE will be auto-generated with `devtools::document()`

## Key Features

1. **Flexible Cost Structures**:
   - Customizable utility matrix
   - Models different legal/policy contexts
   - Reflects real-world tradeoffs

2. **Base Rate Sensitivity**:
   - Shows how optimal decisions change with prevalence
   - Important for different jurisdictions/contexts
   - Explains why "one size fits all" policies may fail

3. **Fair Procedure Comparison**:
   - Accounts for ROC curve truncation
   - Incorporates practical decision criteria
   - Reveals dominance reversals (one procedure better at some base rates, worse at others)

4. **Four Utility Types** (as per Lampinen et al. 2019):
   - **All IDs**: Utility of accepting all identifications
   - **Max**: Optimal criterion (highest utility)
   - **Avg**: Average across criteria (representative performance)
   - Can extend to high-confidence-only utility

## Practical Applications

### 1. Optimal Decision Thresholds
```r
# What confidence threshold should courts use?
util <- make_utility_curves(data,
                           base_rate = 0.3,  # Local crime base rate
                           utility_matrix = c(tp=1, fn=-0.5, fp=-5, tn=0.5))

cat("Optimal threshold:", util$max_utility$criterion)
```

### 2. Policy Analysis
```r
# How sensitive is the optimal threshold to base rate?
base_rates <- seq(0.1, 0.9, 0.1)
for (br in base_rates) {
  util <- make_utility_curves(data, base_rate = br)
  cat(sprintf("Base rate %.1f: Optimal criterion = %.1f\n",
             br, util$max_utility$criterion))
}
```

### 3. Procedure Selection
```r
# Which lineup procedure should we use?
comparison <- compare_utility(sequential_data, simultaneous_data,
                             base_rate_grid = seq(0.01, 0.99, 0.01),
                             utility_type = "max")

print(comparison)
plot(comparison$plot)
```

### 4. Cost-Benefit Analysis
```r
# How does changing false positive cost affect decisions?
costs_fp <- c(-1, -2, -5, -10)
for (cost in costs_fp) {
  util <- make_utility_curves(data,
                             utility_matrix = c(tp=1, fn=-0.5, fp=cost, tn=0.5))
  cat(sprintf("FP cost %d: Optimal criterion = %.1f\n",
             cost, util$max_utility$criterion))
}
```

## Theoretical Background

### Why Utility Analysis?

Traditional ROC analysis has limitations:

1. **No base rate consideration**: Hit/FA rates alone don't determine optimal decisions
2. **Ignores cost structure**: Not all errors are equally costly
3. **Truncation problems**: Partial ROC curves can mislead (Lampinen et al. 2019)
4. **Decision-agnostic**: Doesn't specify what threshold to use

Utility analysis addresses all these issues by:
- Explicitly incorporating base rates
- Weighting outcomes by their costs/benefits
- Providing optimal decision thresholds
- Enabling fair comparisons under realistic conditions

### Expected Utility Theory

Expected utility is the classical framework for decision-making under uncertainty:

```
Choose action that maximizes: E[U] = Σ p(outcome) * U(outcome)
```

For lineup identification:
- **Actions**: Accept ID at criterion c, or reject all
- **Outcomes**: TP, FN, FP, TN
- **Probabilities**: Determined by base rate, hit rate, FA rate
- **Utilities**: Specified by decision-maker's values

### Connection to Signal Detection Theory

Utility analysis extends SDT by:
- Retaining sensitivity (d') concept
- Adding asymmetric costs (beyond simple β)
- Incorporating base rates explicitly
- Providing optimal criterion placement

### ROC vs Utility

**ROC analysis**: "Which procedure has better discrimination?"
- Answer: Higher pAUC = better discrimination
- Caveat: May depend on criterion placement

**Utility analysis**: "Which procedure is more valuable in practice?"
- Answer: Higher EU = more valuable
- Incorporates: base rates, costs, realistic criteria

**Key insight**: ROC dominance ≠ Utility dominance

A procedure with lower pAUC might have higher utility if:
- Its ROC curve is better in the relevant region
- Base rates favor its particular hit/FA tradeoff
- Cost structure aligns with its characteristics

## Mathematical Notes

### Expected Utility Derivation

For a lineup identification at criterion c:

**Outcomes**:
- True positive: Guilty suspect, ID made
- False negative: Guilty suspect, no ID
- False positive: Innocent suspect, ID made
- True negative: Innocent suspect, no ID

**Probabilities**:
```
p(TP) = base_rate * hit_rate
p(FN) = base_rate * (1 - hit_rate)
p(FP) = (1 - base_rate) * fa_rate
p(TN) = (1 - base_rate) * (1 - fa_rate)
```

**Expected Utility**:
```
EU = p(TP)*U_tp + p(FN)*U_fn + p(FP)*U_fp + p(TN)*U_tn

   = base_rate * [hit * U_tp + (1-hit) * U_fn] +
     (1 - base_rate) * [fa * U_fp + (1-fa) * U_tn]
```

### Optimal Criterion

The optimal criterion c* maximizes EU:

```
c* = argmax_c EU(c)
```

This is found by computing EU at each available criterion and selecting the maximum.

### Crossover Points

Crossover occurs when EU_A(br) = EU_B(br) for some base rate br.

Finding crossovers:
1. Compute EU_A(br) and EU_B(br) for grid of base rates
2. Identify where sign of difference changes
3. Interpolate to estimate exact crossover base rate

## Limitations and Considerations

1. **Utility elicitation**: Determining appropriate utility values is challenging and context-dependent

2. **Base rate estimation**: True base rates often unknown; sensitivity analysis recommended

3. **Aggregation**: Utilities aggregate TP/FN (guilty suspects) with FP/TN (innocent suspects) - assumes commensurability

4. **Dynamic contexts**: Base rates and utilities may change over time or situations

5. **Multiple stakeholders**: Different parties (police, prosecutors, defendants, public) may have different utility functions

## Recommended Workflow

1. **Specify context**:
   - Estimate base rate for your jurisdiction/situation
   - Elicit utility values reflecting policy priorities

2. **Compute utilities**:
   - Use `make_utility_curves()` for single procedure
   - Identify optimal criterion

3. **Compare procedures**:
   - Use `make_utility_difference()` for two procedures
   - Check for crossover points
   - Consider robustness across base rates

4. **Sensitivity analysis**:
   - Vary base rate and utility matrix
   - Assess stability of recommendations
   - Document assumptions

5. **Report**:
   - Present utility curves and difference curves
   - State assumptions clearly
   - Acknowledge limitations

## References

Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities in eyewitness identification practice: Dissociations between receiver operating characteristic analysis and expected utility analysis. *Law and Human Behavior, 43*(1), 26-44.

## Next Steps

To use in the package:
1. Run `devtools::document()` to update NAMESPACE
2. Run `devtools::check()` to verify no issues
3. Install with `devtools::install()`

Example usage:
```r
library(r4lineups)
data(lineup_example)

# Basic utility analysis
util <- make_utility_curves(lineup_example,
                           base_rate = 0.5,
                           utility_matrix = c(tp=1, fn=-0.5, fp=-2, tn=0.5))
print(util)
plot_utility_curves(util)

# Compare procedures
comparison <- compare_utility(proc_a, proc_b,
                             utility_type = "max")
print(comparison)
```

## Connection to Other Methods

- **ROC/CAC**: ROC shows hit/FA tradeoffs; utility analysis selects optimal point
- **Bayesian curves**: Bayesian analysis computes posteriors; utility analysis incorporates decision costs
- **Calibration**: Calibration assesses confidence accuracy; utility incorporates costs of being wrong
- **EIG**: EIG measures information value; utility measures decision value

Each method provides complementary insights for evaluating lineup procedures.
