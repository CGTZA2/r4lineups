# pyWitness Features Implementation in r4lineups

**Date**: 2026-01-30
**Purpose**: Track implementation of useful features from pyWitness into r4lineups

## Overview

This document tracks the implementation of high-priority features from pyWitness (Mickes et al., 2024, *Behavior Research Methods*) into the r4lineups package.

## Priority List

### High Priority ⭐⭐⭐
1. ✅ RAC (Response Time-Accuracy) Analysis - COMPLETED
2. ✅ Data Simulation Framework - COMPLETED
3. ✅ Model Comparison Framework - COMPLETED
4. ⏳ pAUC Statistical Comparison - PENDING

### Medium Priority ⭐⭐
5. ⏳ Additional SDT Models (BEST-REST, Ensemble, Integration)
6. ⏳ z-ROC Parameter Estimation
7. ⏳ Standardized Data Format

### Lower Priority ⭐
8. ⏳ Object-Oriented Pipeline Refactoring
9. ⏳ Showup-Specific Handling

---

## Implementation Details

### 1. RAC Analysis ✅ COMPLETED

**Status**: ✅ Complete
**Files Created**:
- `R/rac_functions.R` - Core RAC implementation
- `examples/rac_example.R` - Comprehensive examples
- `vignettes/rac_analysis.Rmd` - Tutorial vignette
- Man pages: `make_racdata.Rd`, `make_rac.Rd`, `make_rac_gg.Rd`, `print.lineup_rac.Rd`

**Functions**:
```r
make_racdata(data, lineup_size, time_bins)  # Compute RAC data
make_rac_gg(racobj_list, ...)               # Plot RAC curve
make_rac(data, ...)                         # Main user function
print.lineup_rac(x)                         # Print method
```

**Features**:
- Response time-accuracy characteristic analysis
- Flexible time binning
- Standard error computation
- Publication-ready plots
- Follows same pattern as CAC analysis
- Integrates seamlessly with existing r4lineups workflow

**Use Cases**:
- Examining speed-accuracy tradeoffs
- Objective memory strength indicator
- Complementary to CAC (confidence-accuracy) analysis
- Robust to verbal overshadowing effects

**Documentation**:
- ✅ Function documentation (roxygen2)
- ✅ Comprehensive vignette with examples
- ✅ Example script with simulated data
- ✅ Comparison with CAC analysis

**Testing**:
- ✅ Basic functionality tested
- ✅ Works with simulated data
- ✅ Produces expected output format
- ⏳ Needs: Unit tests, edge case testing

**References**:
- Seale-Carlisle et al. (2019). *JARMAC*, 8(4), 420-428
- Mickes et al. (2024). *Behavior Research Methods*, 56, 1533-1550

---

### 2. Data Simulation Framework ✅ COMPLETED

**Status**: ✅ Complete
**Files Created**:
- `R/simulate_lineup_data.R` - Simulation functions
- `examples/simulation_power_analysis_example.R` - Comprehensive tutorial
- Man pages: `simulate_lineup_data.Rd`, `simulate_power_analysis.Rd`, `plot.power_analysis.Rd`

**Functions**:
```r
simulate_lineup_data(n_tp, n_ta, d_prime, ...)  # Generate simulated data
simulate_power_analysis(sample_sizes, ...)      # Conduct power analysis
print.simulated_lineup_data(x)                  # Print method
plot.power_analysis(x)                          # Plot power curves
```

**Features**:
- **Signal Detection Model**: Normal(d', 1) for targets, Normal(0, 1) for lures
- **MAX Decision Rule**: Choose lineup member with highest memory strength
- **Flexible Parameters**:
  - Sample sizes (n_tp, n_ta)
  - Discriminability (d_prime)
  - Decision criterion (c_criterion)
  - Lineup size
  - Confidence levels
  - Response time simulation
- **Power Analysis**: Test multiple sample sizes, compute statistics, estimate power
- **Reproducibility**: Random seed support

**Use Cases**:
1. **Power Analysis**: Determine sample sizes needed for studies
2. **Method Validation**: Test if analyses recover known parameters
3. **Scenario Planning**: Compare different experimental designs
4. **Teaching**: Demonstrate signal detection theory concepts
5. **Software Testing**: Validate r4lineups implementations

**Integration**:
- ✅ Works with all existing r4lineups functions
- ✅ Generates standard data format (target_present, identification, confidence, response_time)
- ✅ Compatible with ROC, CAC, RAC, Full ROC analyses
- ✅ S3 classes for clean printing

**Example Usage**:
```r
# Simulate strong memory
sim_data <- simulate_lineup_data(
  n_tp = 200, n_ta = 200,
  d_prime = 2.0,
  conf_levels = 5,
  include_response_time = TRUE
)

# Analyze with existing functions
roc <- make_roc(sim_data)
cac <- make_cac(sim_data)
rac <- make_rac(sim_data, time_bins = c(0, 5000, 10000, 15000, 20000))

# Power analysis
power_res <- simulate_power_analysis(
  sample_sizes = c(50, 100, 200, 500),
  d_prime = 1.5,
  n_simulations = 1000
)
plot(power_res)
```

**Testing**:
- ✅ Basic functionality verified
- ✅ Produces realistic data patterns
- ✅ Integrates with existing analyses
- ⏳ Needs: More extensive validation, parameter recovery tests

---

### 3. Model Comparison Framework ✅ COMPLETED

**Status**: ✅ Complete
**Files Created**:
- `R/model_comparison.R` - Core comparison framework
- `examples/model_comparison_example.R` - Comprehensive examples
- Man pages: `compare_models.Rd`, `print.model_comparison.Rd`, `summary.model_comparison.Rd`, `plot.model_comparison.Rd`, `format_comparison_table.Rd`

**Functions**:
```r
compare_models(data, models, ...)           # Main function - fit multiple models
print.model_comparison(x)                   # Print method
summary.model_comparison(object)            # Summary method
plot.model_comparison(x, which, ncol)       # Side-by-side plots
format_comparison_table(comparison_obj)     # Formatted tables
```

**Features**:
- Unified interface for fitting multiple models (2-HT, EIG, Full ROC)
- Automatic handling of different data requirements across models
- Comprehensive comparison tables with key statistics
- Side-by-side visualizations for easy comparison
- Model selection recommendations
- Publication-ready formatted tables (console, markdown, latex)

**Models Supported**:
1. **2-HT (Winter et al., 2022)**: Multinomial processing tree model
   - Parameters: dP, dA, b, g
   - Returns: AIC, BIC, log-likelihood
   - Use for: Process-based modeling, understanding latent mechanisms

2. **EIG (Starns et al., 2023)**: Expected Information Gain
   - Returns: EIG (bits), information efficiency
   - Use for: Measuring evidential value of procedures

3. **Full ROC (Smith & Yang, 2020)**: Complete ROC using all responses
   - Returns: AUC, operating points
   - Use for: Threshold-free discriminability assessment

**Use Cases**:
- Compare different lineup procedures
- Evaluate which model best fits your data
- Comprehensive analysis using multiple theoretical frameworks
- Publication-ready model comparison tables and figures

**Integration**:
- ✅ Works seamlessly with existing r4lineups functions
- ✅ Handles different data formats automatically
- ✅ S3 classes for clean printing and plotting
- ✅ Comprehensive error handling and warnings

**Example Usage**:
```r
# Fit all models
comparison <- compare_models(
  lineup_data,
  models = c("2ht", "eig", "fullroc"),
  lineup_size = 6,
  prior_guilt = 0.5
)

# View comparison table
print(comparison)
summary(comparison)

# Access individual models
comparison$fitted_models$`2ht`
comparison$fitted_models$eig
comparison$fitted_models$fullroc

# Create side-by-side plots
plot(comparison, ncol = 2)

# Generate formatted table for publication
format_comparison_table(comparison, format = "markdown")
```

**Documentation**:
- ✅ Function documentation (roxygen2)
- ✅ Comprehensive example script with 8 examples
- ✅ Integration with existing model documentation
- ⏳ Needs: Vignette for model comparison workflow

**Testing**:
- ✅ Basic functionality tested
- ✅ All three models fit successfully
- ✅ Produces expected output format
- ✅ Bug fix applied to winter_2ht.R data extraction
- ⏳ Needs: Unit tests, edge case testing

**Bug Fixes Applied**:
- Fixed `.extract_counts_from_df()` in `winter_2ht.R` to properly handle dataframe inputs
  - Issue: Names were being combined incorrectly (e.g., "n_tp_suspect.suspect")
  - Fix: Added `unname()` to ensure clean names
  - Result: 2-HT model now works correctly with dataframe inputs

**References**:
- Winter et al. (2022). *Scientific Reports*, 12, 15571
- Starns et al. (2023). *Psychological Review*
- Smith & Yang (2020). *Perspectives on Psychological Science*, 15(3), 589-607

---

## Next Steps

### 4. pAUC Statistical Comparison ⏳ PENDING

**Goal**: Make it easy to fit and compare multiple models (e.g., different SDT models, 2-HT variants)

**Planned Features**:
- Fit multiple models to same data with single function call
- Generate comparison table (χ², AIC, BIC, parameter estimates)
- Side-by-side plots
- Model selection recommendations

**Inspiration from pyWitness**:
- Table 6 in pyWitness paper shows excellent model comparison format
- Multiple models (SIO, IO, BR, EN, IN) with EV/UV variants
- Clear presentation of fit quality (χ²/ndf, p-value)
- Number of iterations and fit time

**Implementation Plan**:
```r
compare_models <- function(data, models = c("2ht", "eig", "winter_2ht"), ...)
  # Returns: comparison table, plots, recommendations

fit_multiple_models <- function(data, ...)
  # Fit all available models, return list

plot_model_comparison <- function(comparison_obj, ...)
  # Visualize model fits
```

### 4. pAUC Statistical Comparison ⏳ PENDING

**Goal**: Systematic framework for comparing ROC curves between conditions

**Planned Features**:
- Z-test for pAUC differences (as in pyWitness)
- Automatic handling of different false ID cutoffs
- Bootstrap-based standard errors
- Publication-ready comparison plots

**Formula** (from pyWitness):
$$Z = \frac{pAUC_1 - pAUC_2}{sd(pAUC_1 - pAUC_2)}$$

**Implementation Plan**:
```r
compare_pAUC <- function(data1, data2, ...) {
  # Bootstrap both datasets
  # Compute Z-score
  # Return: Z, p-value, confidence intervals, comparison plot
}

plot_pAUC_comparison <- function(data1, data2, labels, ...) {
  # Overlay ROCs with shaded pAUC regions
  # Show cutoff false ID rate
  # Display test results
}
```

---

## Benefits to r4lineups

### New Capabilities
1. ✅ **RAC Analysis**: Fill gap in response time analysis
2. ✅ **Power Analysis**: Planning tool previously unavailable
3. ✅ **Data Simulation**: Testing and teaching infrastructure
4. ⏳ **Model Comparison**: Systematic model selection
5. ⏳ **Statistical Testing**: Rigorous pAUC comparisons

### Improved Workflow
- More complete toolkit for eyewitness research
- Better integration between analyses
- Clearer documentation and examples
- Reproducible research practices

### Research Impact
- Enable more rigorous study design (power analysis)
- Provide objective memory measures (RAC)
- Facilitate method validation (simulation)
- Support evidence-based model selection

---

## Documentation Strategy

### Vignettes Created
1. ✅ `rac_analysis.Rmd` - Response time-accuracy analysis
2. ⏳ `simulation_power_analysis.Rmd` - Data simulation and power analysis
3. ⏳ `model_comparison.Rmd` - Comparing different models
4. ⏳ `pAUC_comparison.Rmd` - Statistical comparison of ROCs

### Examples Created
1. ✅ `rac_example.R` - RAC analysis with simulated data
2. ✅ `simulation_power_analysis_example.R` - Complete tutorial
3. ⏳ `model_comparison_example.R` - Fitting multiple models
4. ⏳ `pAUC_comparison_example.R` - Comparing conditions

### Integration with Existing Docs
- All new functions follow roxygen2 documentation standards
- Examples use consistent data formats
- Cross-references to existing vignettes
- Citations to original papers

---

## Technical Notes

### Code Quality
- ✅ Follow r4lineups coding style
- ✅ Use existing infrastructure (bootstrap, plotting)
- ✅ S3 methods for clean interfaces
- ✅ Comprehensive parameter validation
- ✅ Informative error messages

### Testing Strategy
- ⏳ Unit tests for all new functions
- ⏳ Integration tests with existing functions
- ⏳ Edge case handling (empty data, extreme parameters)
- ⏳ Comparison with pyWitness outputs (where applicable)

### Performance Considerations
- Simulation functions optimized for speed
- Power analysis can be parallelized (future enhancement)
- Bootstrap procedures leverage existing infrastructure

---

## References

**Primary Source**:
Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024). pyWitness 1.0: A python eyewitness identification analysis toolkit. *Behavior Research Methods, 56*, 1533-1550. https://doi.org/10.3758/s13428-023-02108-2

**Related**:
- Seale-Carlisle et al. (2019). Confidence and response time as indicators... *JARMAC*, 8(4), 420-428
- Wixted et al. (2018). Models of lineup memory. *Cognitive Psychology*, 105, 8-114
- Smith & Yang (2020). Full ROC curves... *Perspectives on Psychological Science*, 15(3), 589-607

---

## Change Log

### 2026-01-30 (Session 1)
- ✅ Implemented RAC analysis (functions, examples, vignette)
- ✅ Implemented data simulation framework (functions, examples)
- ✅ Updated NAMESPACE with new exports
- ✅ Generated roxygen documentation
- ✅ Tested basic functionality
- 📝 Created this tracking document

### 2026-01-30 (Session 2)
- ✅ Implemented model comparison framework (functions, examples)
- ✅ Fixed bug in `winter_2ht.R` data extraction function
- ✅ Updated NAMESPACE with new exports
- ✅ Generated roxygen documentation for model comparison
- ✅ Tested all models (2-HT, EIG, Full ROC) successfully
- ✅ Updated tracking document

### Next Session
- ⏳ Create simulation/power analysis vignette
- ⏳ Create model comparison vignette
- ⏳ Implement pAUC statistical comparison
- ⏳ Add unit tests
- ⏳ Update package version and NEWS

---

## Summary

**Completed**: 3 of 8 features (38%)
**High Priority Completed**: 3 of 4 (75%)

**Impact**: The implemented features significantly enhance r4lineups' capabilities for:
- **RAC Analysis**: Response time-accuracy characteristics for objective memory assessment
- **Data Simulation**: Study planning, power analysis, and method validation
- **Model Comparison**: Unified framework for comparing 2-HT, EIG, and Full ROC models
- Teaching signal detection theory and eyewitness identification methodology

**Next Priorities**:
1. **pAUC Statistical Comparison** - Complete high-priority list (1 remaining)
2. **Vignettes** - Document simulation and model comparison workflows
3. **Unit Tests** - Comprehensive testing for all new features
