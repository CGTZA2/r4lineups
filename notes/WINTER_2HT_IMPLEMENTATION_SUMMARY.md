# Winter et al. (2022) 2-HT MPT Model Implementation

## Overview

This document summarizes the implementation of the Winter et al. (2022) Two-High Threshold (2-HT) Multinomial Processing Tree (MPT) model for eyewitness identification data in the `r4lineups` package.

## Model Description

The 2-HT model analyzes the full 2×3 outcome structure of lineup procedures:

**Target-Present Lineups:**
- Suspect identifications (correct culprit IDs)
- Filler identifications (false filler IDs)
- Rejections (false lineup rejections)

**Target-Absent Lineups:**
- Suspect identifications (false innocent-suspect IDs)
- Filler identifications (false filler IDs)
- Rejections (correct lineup rejections)

### Model Parameters

1. **dP**: Detection of culprit presence (0-1)
   - Probability that witness detects the culprit in target-present lineups
   - Higher values = better memory for culprit

2. **dA**: Detection of culprit absence (0-1)
   - Probability that witness detects absence of culprit in target-absent lineups
   - Higher values = better ability to reject lineups without the culprit

3. **b**: Biased suspect selection (0-1)
   - Probability of selecting suspect due to lineup unfairness (suspect stands out)
   - Higher values = more unfair lineups

4. **g**: Guessing-based selection (0-1)
   - Probability of selecting a lineup member based on guessing
   - Higher values = more reliance on guessing

### Model Equations

**Target-Present Lineups:**
```
P(suspect ID) = dP + (1-dP) × [b + (1-b) × g × (1/L)]
P(filler ID)  = (1-dP) × (1-b) × g × ((L-1)/L)
P(reject)     = (1-dP) × (1-b) × (1-g)
```

**Target-Absent Lineups:**
```
P(suspect ID) = (1-dA) × [b + (1-b) × g × (1/L)]
P(filler ID)  = (1-dA) × (1-b) × g × ((L-1)/L)
P(reject)     = dA + (1-dA) × (1-b) × (1-g)
```

Where L is the lineup size (typically 6).

## Implementation

### Core Functions

1. **`fit_winter_2ht()`** - Main fitting function
   - Estimates parameters using maximum likelihood estimation (MLE)
   - Uses constrained optimization (L-BFGS-B) with parameters bounded [0,1]
   - Returns model fit with parameter estimates, SEs, log-likelihood, AIC/BIC

2. **`print.winter_2ht()`** - Print method
   - Displays parameter estimates and standard errors
   - Shows model fit statistics

3. **`summary.winter_2ht()`** - Summary method
   - Extended output with 95% CIs
   - Observed vs. expected counts
   - Goodness-of-fit chi-square test

### Visualization Functions

4. **`plot_2ht_parameters()`** - Parameter plot
   - Bar plot of parameter estimates with error bars
   - Shows 95% confidence intervals

5. **`plot_2ht_fit()`** - Model fit plot
   - Comparison of observed vs. expected counts
   - Separate panels for target-present and target-absent lineups

6. **`plot.winter_2ht()`** - Default plot method
   - Calls `plot_2ht_parameters()`

### Bootstrap Functions

7. **`boot_winter_2ht()`** - Bootstrap confidence intervals
   - Nonparametric bootstrap resampling
   - Supports percentile and BCa intervals
   - Optional parallel processing

8. **`print.winter_2ht_boot()`** - Print bootstrap results

9. **`summary.winter_2ht_boot()`** - Summary of bootstrap distributions

10. **`plot.winter_2ht_boot()`** - Bootstrap distribution plots
    - Histograms of bootstrap parameter estimates
    - Shows original estimates and CIs

## Usage Examples

### Example 1: Basic fitting with count data

```r
# Data from Winter et al. (2022) Experiment 1, simultaneous, long exposure
counts <- c(
  n_tp_suspect = 147,
  n_tp_filler = 94,
  n_tp_reject = 141,
  n_ta_suspect = 38,
  n_ta_filler = 138,
  n_ta_reject = 206
)

# Fit model
fit <- fit_winter_2ht(counts, lineup_size = 6)

# View results
print(fit)
summary(fit)

# Plot parameters
plot(fit)

# Plot model fit
plot_2ht_fit(fit)
```

### Example 2: Using data frame input

```r
# Assume you have a data frame with columns:
# - target_present: TRUE/FALSE
# - identification: "suspect", "filler", or "reject"

fit <- fit_winter_2ht(
  data = lineup_data,
  lineup_size = 6,
  target_present = "target_present",
  identification = "identification"
)
```

### Example 3: Bootstrap confidence intervals

```r
# Compute bootstrap CIs
boot_fit <- boot_winter_2ht(fit, nboot = 1000, seed = 123)

# View results
print(boot_fit)
summary(boot_fit)

# Plot bootstrap distributions
plot(boot_fit)

# Parallel bootstrap (faster for large nboot)
boot_fit_parallel <- boot_winter_2ht(
  fit,
  nboot = 5000,
  parallel = TRUE,
  ncpus = 4,
  seed = 123
)
```

## Validation with Published Data

The implementation has been validated using data from Winter et al. (2022):

### Experiment 1 (Manipulation of dP via exposure duration)

**Long exposure, simultaneous:**
- Expected: dP should be higher for long vs. short exposure
- Result: ✓ Confirmed

**Short exposure, simultaneous:**
- Expected: dP should be lower
- Result: ✓ Confirmed

### Experiment 2 (Manipulation of b via lineup fairness)

**Unfair lineup (birthmarks on fillers):**
- Expected: b should be higher
- Result: ✓ Confirmed

**Fair lineup:**
- Expected: b should be low (near 0)
- Result: ✓ Confirmed

### Experiment 3 (Manipulation of g via pre-lineup instructions)

**High culprit probability instructions:**
- Expected: g should be higher
- Result: ✓ Confirmed

**Low culprit probability instructions:**
- Expected: g should be lower
- Result: ✓ Confirmed

### Experiment 4 (Manipulation of dA via ease of rejection)

**Easy to reject (all lineup members have distinguishing feature):**
- Expected: dA should be higher
- Result: ✓ Confirmed

**Difficult to reject (standard lineup):**
- Expected: dA should be lower
- Result: ✓ Confirmed

## Interpretation Guidelines

### Parameter Ranges

- **dP = 0.0 to 0.3**: Poor culprit detection (short exposure, poor encoding)
- **dP = 0.3 to 0.6**: Moderate culprit detection (typical conditions)
- **dP = 0.6 to 1.0**: Excellent culprit detection (long exposure, good conditions)

- **dA = 0.0 to 0.2**: Poor culprit-absence detection (difficult lineups)
- **dA = 0.2 to 0.5**: Moderate culprit-absence detection
- **dA = 0.5 to 1.0**: Good culprit-absence detection (easy-to-reject lineups)

- **b = 0.0 to 0.05**: Fair lineup
- **b = 0.05 to 0.15**: Moderate unfairness
- **b = 0.15 to 1.0**: Highly unfair lineup

- **g = 0.0 to 0.3**: Low guessing (conservative instructions)
- **g = 0.3 to 0.7**: Moderate guessing
- **g = 0.7 to 1.0**: High guessing (liberal instructions)

### Model Fit Evaluation

1. **Chi-square goodness-of-fit test**
   - p > 0.05: Model fits data adequately
   - p < 0.05: Model may not fit well (inspect residuals)

2. **AIC/BIC**
   - Use for model comparison (lower is better)
   - Can compare 2-HT model to restricted versions

3. **Residuals**
   - Check observed vs. expected counts
   - Large residuals indicate poor fit for specific categories

## Technical Notes

### Optimization

- Uses `optim()` with method = "L-BFGS-B"
- Parameters bounded to [0, 1]
- Starting values: dP=0.3, dA=0.1, b=0.05, g=0.5
- Hessian computed for standard errors

### Identifiability

The model is just-identified (4 parameters, 4 independent data categories after marginal constraints). This means:
- The unrestricted model has 0 degrees of freedom
- Goodness-of-fit is based on df = 2 (6 categories - 4 parameters)
- Can test restricted models (e.g., dP = dA)

### Limitations

1. **Group-level only**: Current implementation estimates parameters at group level, not individual level
2. **No confidence ratings**: Does not currently incorporate confidence judgments
3. **Single lineup size**: Assumes constant lineup size across all lineups
4. **Equal b across conditions**: Assumes biased selection is same for TP and TA lineups

### Future Extensions

Possible extensions to the model:
1. Incorporate confidence ratings (hierarchical model)
2. Individual-level parameter estimation
3. Mixed lineups with varying sizes
4. Integration with face similarity measures
5. Extensions for showup procedures

## References

Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental validation of a multinomial processing tree model for analyzing eyewitness identification decisions. *Scientific Reports*, 12, 15571. https://doi.org/10.1038/s41598-022-19513-w

Moshagen, M. (2010). multiTree: A computer program for the analysis of multinomial processing tree models. *Behavior Research Methods*, 42, 42–54.

Batchelder, W. H., & Riefer, D. M. (1999). Theoretical and empirical review of multinomial process tree modeling. *Psychonomic Bulletin & Review*, 6, 57–86.

## Files Created

1. `R/winter_2ht.R` - Core fitting functions and methods
2. `R/winter_2ht_plots.R` - Plotting functions
3. `R/winter_2ht_boot.R` - Bootstrap functions
4. `notes/WINTER_2HT_IMPLEMENTATION_SUMMARY.md` - This document

## Testing

See `notes/test_winter_2ht.R` for comprehensive testing script with:
- Data from all 4 validation experiments
- Comparison to published parameter estimates
- Bootstrap examples
- Plotting examples
