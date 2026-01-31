# r4lineups 0.2.0 (2026-01-30)

## Major New Features (pyWitness-inspired)

This release implements high-priority features from pyWitness (Mickes et al., 2024), significantly expanding r4lineups' analytical capabilities for eyewitness identification research.

### Model Comparison Framework

* `compare_models()`: Unified interface for fitting and comparing multiple models
  * 2-HT (Winter et al., 2022): Multinomial processing tree model
  * EIG (Starns et al., 2023): Expected Information Gain
  * Full ROC (Smith & Yang, 2020): Complete ROC using all responses
* S3 methods: `print()`, `summary()`, `plot()` for model comparison objects
* `format_comparison_table()`: Publication-ready comparison tables
* Automatic handling of different data requirements across models
* Model selection recommendations based on AIC/BIC
* Side-by-side visualizations of model results

### pAUC Statistical Comparison

* `compare_pauc()`: Rigorous statistical testing of ROC curve differences
* Z-test framework with bootstrap-based standard errors
* Automatic false ID rate cutoff handling with interpolation
* Confidence intervals and effect size calculation (Cohen's d)
* Multiple comparison support with adjustment examples
* Publication-ready side-by-side ROC plots
* Customizable bootstrap samples and confidence levels

### Data Simulation and Power Analysis

* `simulate_lineup_data()`: Generate lineup identification data
  * Signal Detection Theory (SDT) model with MAX decision rule
  * Flexible parameters: d', criterion, lineup size, confidence levels
  * Optional response time simulation
  * S3 class with print method
* `simulate_power_analysis()`: Determine required sample sizes
  * Multiple sample size evaluation
  * Customizable effect measures and comparison types
  * Power curve visualization
* Full integration with all r4lineups analyses

### Response Time-Accuracy (RAC) Analysis

* `make_rac()`: Response time-accuracy characteristic analysis
* `make_racdata()`: Compute RAC data with time bins
* `make_rac_gg()`: Publication-ready RAC plots
* `print.lineup_rac()`: Print method for RAC objects
* Complementary to CAC analysis for objective memory assessment
* Follows Seale-Carlisle et al. (2019) methodology

## Documentation

### New Vignettes

* `simulation_power_analysis.Rmd`: Complete guide to data simulation and power analysis
* `model_comparison.Rmd`: Comprehensive model comparison workflows
* `pauc_statistical_comparison.Rmd`: Statistical testing of ROC curves
* `rac_analysis.Rmd`: Response time-accuracy analysis

All vignettes include:
* Complete workflows from start to finish
* Real-world examples and use cases
* Best practices and common pitfalls
* Publication-ready guidance
* References to original literature

### Example Scripts

* `model_comparison_example.R`: 8 comprehensive examples
* `pauc_comparison_example.R`: 8 comprehensive examples with interpretation
* `simulation_power_analysis_example.R`: Complete tutorial
* `rac_example.R`: RAC analysis demonstration

## Bug Fixes

* Fixed `.extract_counts_from_df()` in `winter_2ht.R` to properly handle dataframe inputs
  * Issue: Names were being combined incorrectly (e.g., "n_tp_suspect.suspect")
  * Fix: Added `unname()` to ensure clean names
  * Result: 2-HT model now works correctly with dataframe inputs

## Testing

* Added comprehensive testthat unit tests
  * `test-model-comparison.R`: Model comparison framework tests
  * `test-pauc-comparison.R`: pAUC statistical comparison tests
  * `test-simulation.R`: Data simulation and power analysis tests
* Tests cover basic functionality, edge cases, and integration

## Dependencies

* Added `gridExtra` to Imports (used by `plot.model_comparison()`)
* Added `testthat (>= 3.0.0)` to Suggests for unit testing

## References

New features based on:

* Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024). pyWitness 1.0: A python eyewitness identification analysis toolkit. *Behavior Research Methods, 56*, 1533-1550.

* Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental validation of a multinomial processing tree model for analyzing eyewitness identification decisions. *Scientific Reports, 12*, 15571.

* Starns, J. J., Chen, T., & Staub, A. (2023). Assessing theoretical conclusions via the data they should have produced. *Psychological Review*.

* Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator discriminability and eyewitness discriminability. *Perspectives on Psychological Science, 15*(3), 589-607.

* Seale-Carlisle, T. M., Wetmore, S. A., Flowe, H. D., & Mickes, L. (2019). Designing police lineups to maximize memory performance. *Journal of Applied Research in Memory and Cognition, 8*(4), 420-428.

---

# r4lineups 0.1.2

Previous release. See earlier documentation for details.
