# Changelog

## r4lineups 2.0.0 (2026-07-06)

Major release: a substantial expansion of the package from the 0.1.x
fairness-measure toolkit into a full analysis suite for eyewitness
identification research, plus a package-wide statistical audit.

### Major New Features (pyWitness-inspired)

This release implements high-priority features from pyWitness (Mickes et
al., 2024), significantly expanding r4lineups’ analytical capabilities
for eyewitness identification research.

#### Model Comparison Framework

- [`compare_models()`](https://cgtza2.github.io/r4lineups/reference/compare_models.md):
  Unified interface for fitting and comparing multiple models
  - 2-HT (Winter et al., 2022): Multinomial processing tree model
  - EIG (Starns et al., 2023): Expected Information Gain
  - Full ROC (Smith & Yang, 2020): Complete ROC using all responses
- S3 methods: [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for model
  comparison objects
- [`format_comparison_table()`](https://cgtza2.github.io/r4lineups/reference/format_comparison_table.md):
  Publication-ready comparison tables
- Automatic handling of different data requirements across models
- Model selection recommendations based on AIC/BIC
- Side-by-side visualizations of model results

#### pAUC Statistical Comparison

- [`compare_pauc()`](https://cgtza2.github.io/r4lineups/reference/compare_pauc.md):
  Rigorous statistical testing of ROC curve differences
- Z-test framework with bootstrap-based standard errors
- Automatic false ID rate cutoff handling with interpolation
- Confidence intervals and effect size calculation (Cohen’s d)
- Multiple comparison support with adjustment examples
- Publication-ready side-by-side ROC plots
- Customizable bootstrap samples and confidence levels

#### Data Simulation and Power Analysis

- [`simulate_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md):
  Generate lineup identification data
  - Signal Detection Theory (SDT) model with multiple decision rules:
    - **MAX** (Independent Observations Model) - Default
    - **BEST-REST** - Compare best match vs average of rest
    - **Ensemble** - Average memory strength across lineup members
    - **Integration** - Sum memory strengths across all members
  - Based on Wixted et al. (2018) and pyWitness implementations
  - Flexible parameters: d’, criterion, lineup size, confidence levels
  - Optional response time simulation
  - S3 class with print method
- [`simulate_power_analysis()`](https://cgtza2.github.io/r4lineups/reference/simulate_power_analysis.md):
  Determine required sample sizes
  - Multiple sample size evaluation
  - Customizable effect measures and comparison types
  - Power curve visualization
- Full integration with all r4lineups analyses

#### z-ROC and SDT Parameter Estimation

- [`fit_sdt_roc()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_roc.md):
  Extract d’, criteria, and variance ratio from ROC data
  - Equal and unequal variance SDT models
  - z-transformed ROC analysis following Macmillan & Creelman (2005)
  - Log-linear correction for extreme hit/false alarm rates
  - Bootstrap confidence intervals for parameter estimates
  - Model fit diagnostics (R-squared, residuals)
  - S3 class with [`print()`](https://rdrr.io/r/base/print.html),
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
    [`summary()`](https://rdrr.io/r/base/summary.html) methods
- Integration with
  [`make_rocdata()`](https://cgtza2.github.io/r4lineups/reference/make_rocdata.md)
  for seamless workflow
- Comprehensive examples and documentation
- 24 unit tests ensuring reliability

#### Standardized Data Format and Validation

- [`validate_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/validate_lineup_data.md):
  Comprehensive data validation
  - Checks required columns (target_present, identification, confidence)
  - Validates data types and value ranges
  - Detects missing values and invalid identification categories
  - Warns about common data quality issues
  - Strict mode option for error-on-failure behavior
- [`standardize_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/standardize_lineup_data.md):
  Convert data from various formats
  - Auto-detects common column names (tp, culprit_present, choice, etc.)
  - Recodes identification values to standard terms
  - Handles messy real-world data gracefully
  - Generates participant IDs if missing
  - Returns “lineup_data” S3 class with print method
- [`create_example_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/create_example_lineup_data.md):
  Generate example data for testing
  - Configurable sample size and trial distribution
  - Optional confidence ratings and response times
  - Produces realistic identification patterns
  - Useful for demonstrations and method validation
- 76 unit tests covering validation, standardization, and integration
- Comprehensive example script with 10 real-world scenarios

#### Response Time-Accuracy (RAC) Analysis

- [`make_rac()`](https://cgtza2.github.io/r4lineups/reference/make_rac.md):
  Response time-accuracy characteristic analysis
- [`make_racdata()`](https://cgtza2.github.io/r4lineups/reference/make_racdata.md):
  Compute RAC data with time bins
- [`make_rac_gg()`](https://cgtza2.github.io/r4lineups/reference/make_rac_gg.md):
  Publication-ready RAC plots
- [`print.lineup_rac()`](https://cgtza2.github.io/r4lineups/reference/print.lineup_rac.md):
  Print method for RAC objects
- Complementary to CAC analysis for objective memory assessment
- Follows Seale-Carlisle et al. (2019) methodology

#### Full ROC, EIG, and PPV-Range Analysis

- [`make_fullroc()`](https://cgtza2.github.io/r4lineups/reference/make_fullroc.md)
  /
  [`make_fullroc_data()`](https://cgtza2.github.io/r4lineups/reference/make_fullroc_data.md)
  /
  [`plot_fullroc()`](https://cgtza2.github.io/r4lineups/reference/plot_fullroc.md):
  full ROC analysis using all lineup responses (Smith & Yang, 2020),
  with diagnosticity-ratio or a-priori ordering
- [`compute_eig()`](https://cgtza2.github.io/r4lineups/reference/compute_eig.md)
  /
  [`make_eig()`](https://cgtza2.github.io/r4lineups/reference/make_eig.md)
  /
  [`make_eig_data()`](https://cgtza2.github.io/r4lineups/reference/make_eig_data.md)
  /
  [`plot_eig()`](https://cgtza2.github.io/r4lineups/reference/plot_eig.md):
  Expected Information Gain analysis (Starns et al., 2023)
- [`ppv_by_confidence()`](https://cgtza2.github.io/r4lineups/reference/ppv_by_confidence.md)
  /
  [`ppv_range_by_confidence()`](https://cgtza2.github.io/r4lineups/reference/ppv_range_by_confidence.md)
  /
  [`make_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/make_ppv_range.md)
  /
  [`plot_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/plot_ppv_range.md):
  positive predictive value across confidence levels, with nominal- and
  effective-size corrections

#### Winter 2-HT Multinomial Processing Tree Model

- [`fit_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/fit_winter_2ht.md):
  two-high-threshold MPT model for the full 2x3 outcome table (Winter et
  al., 2022), with [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods
- [`boot_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/boot_winter_2ht.md):
  bootstrap confidence intervals for 2-HT parameters

#### Calibration, ANRI, and Decision Analysis

- [`make_calibration()`](https://cgtza2.github.io/r4lineups/reference/make_calibration.md)
  /
  [`make_calibration_by_condition()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_by_condition.md)
  /
  [`make_calibration_data()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md):
  confidence-accuracy calibration (C, O/U, NRI)
- [`compute_anri()`](https://cgtza2.github.io/r4lineups/reference/compute_anri.md)
  /
  [`bootstrap_anri()`](https://cgtza2.github.io/r4lineups/reference/bootstrap_anri.md)
  /
  [`compare_anri()`](https://cgtza2.github.io/r4lineups/reference/compare_anri.md):
  Adjusted Normalized Resolution Index with bootstrap inference
- [`make_bayes_curves()`](https://cgtza2.github.io/r4lineups/reference/make_bayes_curves.md),
  [`make_bree_curve()`](https://cgtza2.github.io/r4lineups/reference/make_bree_curve.md):
  Bayesian prior-posterior curves
- [`make_utility_curves()`](https://cgtza2.github.io/r4lineups/reference/make_utility_curves.md),
  [`make_utility_difference()`](https://cgtza2.github.io/r4lineups/reference/make_utility_difference.md),
  [`compare_utility()`](https://cgtza2.github.io/r4lineups/reference/compare_utility.md):
  expected-utility analysis (Lampinen et al., 2019)
- [`make_dpp()`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md),
  [`compare_dpp()`](https://cgtza2.github.io/r4lineups/reference/compare_dpp.md):
  Deviation from Perfect Performance (Smith et al., 2018)

#### Bayesian Beta-Binomial Inference

- Posterior inference for core fairness measures:
  [`esize_T_bayes()`](https://cgtza2.github.io/r4lineups/reference/esize_T_bayes.md),
  [`func_size_bayes()`](https://cgtza2.github.io/r4lineups/reference/func_size_bayes.md),
  [`diag_ratio_T_bayes()`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_T_bayes.md),
  [`calibration_bayes()`](https://cgtza2.github.io/r4lineups/reference/calibration_bayes.md)
- [`sdt_compare()`](https://cgtza2.github.io/r4lineups/reference/sdt_compare.md):
  Bayesian comparison of SDT parameters between conditions

#### mSDT and MAX SDT Compound-Decision Models

- Multi-item signal detection (mSDT) core functions:
  [`pmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md),
  [`dmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md),
  [`qmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md),
  [`rmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md),
  [`max_filler_moments()`](https://cgtza2.github.io/r4lineups/reference/max_filler_moments.md),
  [`estimate_msdt_params()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
  (method-of-moments from rejection rates)
- [`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
  /
  [`compare_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/compare_max_sdt.md):
  full-information maximum-likelihood fitting of the compound MAX
  decision rule, with bootstrap CIs

#### SDT Summary-Level Comparisons and GLM Estimation

- [`sdt_summary_from_counts()`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_from_counts.md),
  [`sdt_summary_variance()`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_variance.md),
  [`compare_sdt_summary()`](https://cgtza2.github.io/r4lineups/reference/compare_sdt_summary.md):
  SDT comparisons when only summary counts are available
  (Miller/Gourevitch or bootstrap variance)
- [`fit_sdt_glm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glm.md)
  /
  [`fit_sdt_glmm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md)
  /
  [`extract_sdt_metrics()`](https://cgtza2.github.io/r4lineups/reference/extract_sdt_metrics.md):
  SDT parameter estimation via probit/logit (mixed) models

#### Face Similarity via Deep Learning

- [`face_similarity()`](https://cgtza2.github.io/r4lineups/reference/face_similarity.md),
  [`lineup_similarity()`](https://cgtza2.github.io/r4lineups/reference/lineup_similarity.md),
  [`batch_embeddings()`](https://cgtza2.github.io/r4lineups/reference/batch_embeddings.md),
  and related helpers compute facial similarity with deep embeddings
  (ArcFace, FaceNet, …) through Python’s deepface library (optional;
  requires `reticulate`)
- [`install_r4lineups_python()`](https://cgtza2.github.io/r4lineups/reference/install_r4lineups_python.md)
  /
  [`check_python_deps()`](https://cgtza2.github.io/r4lineups/reference/check_python_deps.md)
  for setup

#### Shiny App

- [`run_r4lineups_app()`](https://cgtza2.github.io/r4lineups/reference/run_r4lineups_app.md):
  interactive Shiny interface to the main analyses

#### Core Fairness Measures

- Bootstrap distributions for lineup bias and effective size
  ([`lineup_boot_allprop()`](https://cgtza2.github.io/r4lineups/reference/lineup_boot_allprop.md),
  [`esize_boot_dist()`](https://cgtza2.github.io/r4lineups/reference/esize_boot_dist.md)
  and friends)
- Data standardization is now the recommended entry point for all
  analyses
  ([`standardize_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/standardize_lineup_data.md))

### Statistical Audit (June 2026)

A component-by-component numerical audit of the statistical routines was
completed for this release. Fixes:

- **MAX SDT fitter**
  ([`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)):
  replaced the single boundary-prone optimizer start with a
  deterministic multistart grid plus Nelder-Mead polish; corrected the
  chi-square objective to use a proper outcome partition (correct-ID /
  filler-ID / reject); floored expected counts instead of dropping
  cells; corrected goodness-of-fit degrees of freedom (df = 3 x
  n_conditions - n_params); convergence is now reported honestly
- **Full ROC**: fixed `order = "apriori"` with `conf_bins` (labels were
  coerced to `NA`, silently breaking within-decision ordering)
- **PPV**: `ppv_by_confidence(correction = "effective")` computed the
  wrong effective size in the no-member-data fallback; now uses the
  pseudo-table counts directly
- **Calibration**: added a
  `confidence_scale = c("auto", "0-1", "0-100")` argument to
  [`make_calibration_data()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md)
  and all callers; `auto` now warns when confidence looks like a
  Likert/0-10 scale instead of silently dividing by 100
- **Winter 2-HT**: removed the invalid single-condition goodness-of-fit
  chi-square test (the model is saturated, df = 0);
  [`summary()`](https://rdrr.io/r/base/summary.html) now reports a
  saturation note and the observed-vs-expected discrepancy as a
  convergence check
- **Diagnosticity ratio**: fixed diagnosticity-ratio bugs and documented
  the Haldane correction in
  [`ln_diag_ratio()`](https://cgtza2.github.io/r4lineups/reference/ln_diag_ratio.md)
- Removed a duplicate internal
  [`entropy()`](https://cgtza2.github.io/r4lineups/reference/entropy.md)
  definition that relied on file collation order

### Documentation

#### New Vignettes

- `simulation_power_analysis.Rmd`: Complete guide to data simulation and
  power analysis
- `model_comparison.Rmd`: Comprehensive model comparison workflows
- `pauc_statistical_comparison.Rmd`: Statistical testing of ROC curves
- `rac_analysis.Rmd`: Response time-accuracy analysis
- `fullroc_analysis.Rmd`: Full ROC methodology
- `information_error_rate_analysis.Rmd`: EIG and PPV-range analysis
- `calibration_decision_analysis.Rmd`: Calibration, utility, DPP, and
  ANRI
- `winter_2ht_model.Rmd`: Winter 2-HT model
- `bayesian_inference.Rmd`: Bayesian posterior inference for lineup
  measures
- `msdt_model.Rmd`: Multi-item signal detection theory
- `sdt_summary_comparisons.Rmd`: SDT comparisons from summary counts
- `sdt_glm_analysis.Rmd`: SDT via GLM/GLMM
- `face_similarity.Rmd`: Deep-learning face similarity

All vignettes include: \* Complete workflows from start to finish \*
Real-world examples and use cases \* Best practices and common pitfalls
\* Publication-ready guidance \* References to original literature

#### Example Scripts

- `model_comparison_example.R`: 8 comprehensive examples
- `pauc_comparison_example.R`: 8 comprehensive examples with
  interpretation
- `simulation_power_analysis_example.R`: Complete tutorial
- `rac_example.R`: RAC analysis demonstration

### Bug Fixes

- Fixed
  [`.extract_counts_from_df()`](https://cgtza2.github.io/r4lineups/reference/dot-extract_counts_from_df.md)
  in `winter_2ht.R` to properly handle dataframe inputs
  - Issue: Names were being combined incorrectly (e.g.,
    “n_tp_suspect.suspect”)
  - Fix: Added [`unname()`](https://rdrr.io/r/base/unname.html) to
    ensure clean names
  - Result: 2-HT model now works correctly with dataframe inputs

### Testing

- Added comprehensive testthat unit tests
  - `test-model-comparison.R`: Model comparison framework tests
  - `test-pauc-comparison.R`: pAUC statistical comparison tests
  - `test-simulation.R`: Data simulation and power analysis tests
- Tests cover basic functionality, edge cases, and integration

### Dependencies

- Added `gridExtra` to Imports (used by
  [`plot.model_comparison()`](https://cgtza2.github.io/r4lineups/reference/plot.model_comparison.md))
- Added `testthat (>= 3.0.0)` to Suggests for unit testing

### References

New features based on:

- Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
  pyWitness 1.0: A python eyewitness identification analysis toolkit.
  *Behavior Research Methods, 56*, 1533-1550.

- Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental
  validation of a multinomial processing tree model for analyzing
  eyewitness identification decisions. *Scientific Reports, 12*, 15571.

- Starns, J. J., Chen, T., & Staub, A. (2023). Assessing theoretical
  conclusions via the data they should have produced. *Psychological
  Review*.

- Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between
  investigator discriminability and eyewitness discriminability.
  *Perspectives on Psychological Science, 15*(3), 589-607.

- Seale-Carlisle, T. M., Wetmore, S. A., Flowe, H. D., & Mickes, L.
  (2019). Designing police lineups to maximize memory performance.
  *Journal of Applied Research in Memory and Cognition, 8*(4), 420-428.

------------------------------------------------------------------------

## r4lineups 0.1.2

Previous release. See earlier documentation for details.
