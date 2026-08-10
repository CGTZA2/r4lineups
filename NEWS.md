# r4lineups 2.1.0 (2026-08-10)

* Added an explicit authorship and acknowledgements statement recognizing
  Colin Tredoux and Tamsyn Naylor as authors of the original package; the use
  of Codex and Claude for code-review, drafting, and debugging support in the
  latest development cycle; and the published work on which the package's
  computational methods rely. Suspected implementation errors should be
  reported through the GitHub issue tracker.
* Relicensed the package under the permissive MIT license and added canonical
  citation metadata for the CRAN and JOSS release.

## Wixted lineup-memory model interface

* Added `install_pywitness()` and `check_pywitness_deps()` for an explicit,
  isolated `r4lineups-pywitness` virtual environment. Installation uses the
  immutable archive for audited pyWitness revision
  `e726dcfc09423d0e0ff7f46c8e3a711040293eba`; package loading and ordinary CRAN
  checks never install or initialize it.
* Added `fit_lineup_models()` for fair simultaneous lineups, with Independent
  Observations, Ensemble, Integration, and optional BEST-Rest fits; equal- and
  unequal-variance paths; zero or estimated shared variance where identified;
  starting values; optimizer controls; and explicit convergence diagnostics.
* Results are durable R-native `lineup_model_comparison` objects with extracted
  parameters, observed and expected response cells, Pearson statistics,
  degrees of freedom, p-values, confidence binning, engine metadata, and
  comparison/fit/ROC plots. No live Python object is retained.
* The audited engine minimizes Pearson chi-squared. The reported multinomial
  log likelihood is evaluated at that estimate rather than maximized, so AIC
  and BIC are deliberately `NA` rather than invalidly treating it as an MLE.
* BEST-Rest and Ensemble are reported as scale-equivalent parameterizations and
  are never counted as independent model-selection evidence. Integration is a
  historical comparator rather than a recommended default.
* Added opt-in, three-platform Python tests for wrapper/direct-engine parity,
  parameter and generating-model recovery, probability partitions,
  BEST-Rest/Ensemble rescaling, equal/unequal variance, zero/estimated shared
  variance, sparse confidence cells, and optimizer failure.

# r4lineups 2.0.0 (2026-08-08)

Major release: a substantial expansion of the package from the 0.1.x
fairness-measure toolkit into a full analysis suite for eyewitness
identification research, plus a package-wide statistical audit.

## CRAN-readiness and statistical corrections

* Corrected the legacy fairness estimators and bootstrap helpers: unchosen
  lineup positions are retained, named count tables are matched by member
  label, Malpass and Tredoux effective sizes use the documented definitions,
  Tredoux bootstrap resampling occurs at the witness level, and bootstrap
  standard errors are the standard deviation of the replicate statistics.
* Corrected diagnosticity pairing, log-ratio variance, inverse-variance
  weighting, homogeneity degrees of freedom, and independent TP/TA resampling.
  `pos_list` now denotes the actual suspect position for each TP/TA pair (or a
  two-element TP/TA position pair). Homogeneity inference requires positive
  suspect-ID cells, as in Tredoux's uncorrected log-risk procedure.
* Confidence-based methods now distinguish observed designated innocent-suspect
  IDs from estimates based on target-absent filler IDs. ROC/CAC/RAC,
  calibration, PPV, utility, full ROC, EIG, and Bayesian curves no longer add
  both quantities. With no designated suspect, full ROC/EIG/Bayesian response
  probabilities split target-absent filler choices into `1 / lineup_size`
  suspect and `(lineup_size - 1) / lineup_size` filler components.
* Full-ROC documentation now identifies empirical diagnosticity ordering as an
  optimistic, sample-dependent ordering. PPV/effective-size, utility
  false-alarm, fractional calibration/ANRI, saturated single-condition 2-HT,
  and method-of-moments mSDT assumptions are stated explicitly.
* Corrected pAUC sorting/interpolation and stratified its bootstrap; corrected
  unequal-variance z-ROC discriminability and raw-data resampling; fixed binary
  SDT-GLM validation and the covariance term in summary-level log-beta
  comparisons.
* Seed-taking simulation and bootstrap functions now restore the caller's RNG
  state. Bootstrap count and confidence arguments are validated and honored,
  optimizer/bootstrap failures are checked, and numerical/vector edge cases
  receive explicit errors.
* Optional Python dependency checks no longer initialize or download a managed
  Python installation. Face-embedding distances reject missing, non-finite,
  unequal-length, or zero-norm vectors as appropriate.
* Added package-wide export-contract and hand-calculation regression tests.
  All 156 exported functions are referenced by the test suite; functions that
  require Python, Shiny, `lme4`, images, or other optional environments have
  explicit dependency/error-path coverage and opt-in smoke-test exemptions.
* Reduced evaluated vignette bootstrap/simulation sizes while retaining
  publication-grade settings in unevaluated examples. Source builds no longer
  include the audit article PDFs.
* Corrected the EIG reference to Starns, Cohen, and Rotello (2023).
* Corrected the lineup-memory simulator: the supplied lowest criterion now
  controls identification versus rejection, subsequent ordered criteria
  control confidence, and explicit criteria are preserved. The simulator now
  implements Independent Observations/MAX, Ensemble, and Integration exactly
  under its documented independent equal-variance assumptions. `"best_rest"`
  remains accepted for compatibility but is identified as a scale-equivalent
  parameterization of Ensemble.
* Reframed `fit_max_sdt()` as a restricted, aggregate, single-criterion,
  equal-variance Independent-Observations fit estimated by minimum Pearson
  chi-squared. It is not a maximum-likelihood fit and is not the complete
  confidence-based Wixted et al. (2018) model.

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
* Side-by-side reporting of non-comparable estimands; AIC/BIC are reported only
  for the 2-HT likelihood fit and are not used to rank EIG or full-ROC AUC
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
  * Signal Detection Theory (SDT) model with three distinct decision families:
    * **MAX** (Independent Observations Model) - Default
    * **Ensemble** - Best match relative to the lineup mean
    * **Integration** - Sum memory strengths across all members
  * **BEST-REST** remains a compatibility parameterization. Its decision
    variable is `k / (k - 1)` times Ensemble, so the two fit identically when
    criteria are rescaled and must not be treated as independent model evidence
  * Based on Wixted et al. (2018) and pyWitness implementations
  * Flexible parameters: d', criterion, lineup size, confidence levels
  * Optional response time simulation
  * S3 class with print method
* `simulate_power_analysis()`: Determine required sample sizes
  * Multiple sample size evaluation
  * Customizable effect measures and comparison types
  * Power curve visualization
* Full integration with all r4lineups analyses

### z-ROC and SDT Parameter Estimation

* `fit_sdt_roc()`: Extract d', criteria, and variance ratio from ROC data
  * Equal and unequal variance SDT models
  * z-transformed ROC analysis following Macmillan & Creelman (2005)
  * Log-linear correction for extreme hit/false alarm rates
  * Bootstrap confidence intervals for parameter estimates
  * Model fit diagnostics (R-squared, residuals)
  * S3 class with `print()`, `plot()`, `summary()` methods
* Integration with `make_rocdata()` for seamless workflow
* Comprehensive examples and documentation
* 24 unit tests ensuring reliability

### Standardized Data Format and Validation

* `validate_lineup_data()`: Comprehensive data validation
  * Checks required columns (target_present, identification, confidence)
  * Validates data types and value ranges
  * Detects missing values and invalid identification categories
  * Warns about common data quality issues
  * Strict mode option for error-on-failure behavior
* `standardize_lineup_data()`: Convert data from various formats
  * Auto-detects common column names (tp, culprit_present, choice, etc.)
  * Recodes identification values to standard terms
  * Handles messy real-world data gracefully
  * Generates participant IDs if missing
  * Returns "lineup_data" S3 class with print method
* `create_example_lineup_data()`: Generate example data for testing
  * Configurable sample size and trial distribution
  * Optional confidence ratings and response times
  * Produces realistic identification patterns
  * Useful for demonstrations and method validation
* 76 unit tests covering validation, standardization, and integration
* Comprehensive example script with 10 real-world scenarios

### Response Time-Accuracy (RAC) Analysis

* `make_rac()`: Response time-accuracy characteristic analysis
* `make_racdata()`: Compute RAC data with time bins
* `make_rac_gg()`: Publication-ready RAC plots
* `print.lineup_rac()`: Print method for RAC objects
* Complementary to CAC analysis for objective memory assessment
* Follows Seale-Carlisle et al. (2019) methodology

### Full ROC, EIG, and PPV-Range Analysis

* `make_fullroc()` / `make_fullroc_data()` / `plot_fullroc()`: full ROC analysis
  using all lineup responses (Smith & Yang, 2020), with diagnosticity-ratio or
  a-priori ordering
* `compute_eig()` / `make_eig()` / `make_eig_data()` / `plot_eig()`: Expected
  Information Gain analysis (Starns et al., 2023)
* `ppv_by_confidence()` / `ppv_range_by_confidence()` / `make_ppv_range()` /
  `plot_ppv_range()`: positive predictive value across confidence levels, with
  nominal- and effective-size corrections

### Winter 2-HT Multinomial Processing Tree Model

* `fit_winter_2ht()`: two-high-threshold MPT model for the full 2x3 outcome
  table (Winter et al., 2022), with `print()`, `summary()`, and `plot()` methods
* `boot_winter_2ht()`: bootstrap confidence intervals for 2-HT parameters

### Calibration, ANRI, and Decision Analysis

* `make_calibration()` / `make_calibration_by_condition()` /
  `make_calibration_data()`: confidence-accuracy calibration (C, O/U, NRI)
* `compute_anri()` / `bootstrap_anri()` / `compare_anri()`: Adjusted Normalized
  Resolution Index with bootstrap inference
* `make_bayes_curves()`, `make_bree_curve()`: Bayesian prior-posterior curves
* `make_utility_curves()`, `make_utility_difference()`, `compare_utility()`:
  expected-utility analysis (Lampinen et al., 2019)
* `make_dpp()`, `compare_dpp()`: Deviation from Perfect Performance
  (Smith et al., 2018)

### Bayesian Beta-Binomial Inference

* Posterior inference for core fairness measures: `esize_T_bayes()`,
  `func_size_bayes()`, `diag_ratio_T_bayes()`, `calibration_bayes()`
* `sdt_compare()`: Bayesian comparison of SDT parameters between conditions

### mSDT and MAX SDT Compound-Decision Models

* Multi-item signal detection (mSDT) core functions: `pmax_filler()`,
  `dmax_filler()`, `qmax_filler()`, `rmax_filler()`, `max_filler_moments()`,
  `estimate_msdt_params()` (method-of-moments from rejection rates)
* `fit_max_sdt()` / `compare_max_sdt()`: restricted aggregate
  Independent-Observations/MAX fitting by minimum Pearson chi-squared, with
  bootstrap CIs and nested-constraint comparisons

### SDT Summary-Level Comparisons and GLM Estimation

* `sdt_summary_from_counts()`, `sdt_summary_variance()`, `compare_sdt_summary()`:
  SDT comparisons when only summary counts are available (Miller/Gourevitch or
  bootstrap variance)
* `fit_sdt_glm()` / `fit_sdt_glmm()` / `extract_sdt_metrics()`: SDT parameter
  estimation via probit/logit (mixed) models

### Face Similarity via Deep Learning

* `face_similarity()`, `lineup_similarity()`, `batch_embeddings()`, and related
  helpers compute facial similarity with deep embeddings (ArcFace, FaceNet, ...)
  through Python's deepface library (optional; requires `reticulate`)
* `install_r4lineups_python()` / `check_python_deps()` for setup

### Shiny App

* `run_r4lineups_app()`: interactive Shiny interface to the main analyses

### Core Fairness Measures

* Bootstrap distributions for lineup bias and effective size
  (`lineup_boot_allprop()`, `esize_boot_dist()` and friends)
* Data standardization is now the recommended entry point for all analyses
  (`standardize_lineup_data()`)

## Statistical Audit (June 2026)

A component-by-component numerical audit of the statistical routines was
completed for this release. Fixes:

* **MAX SDT fitter** (`fit_max_sdt()`): replaced the single boundary-prone
  optimizer start with a deterministic multistart grid plus Nelder-Mead polish;
  corrected the chi-square objective to use a proper outcome partition
  (correct-ID / filler-ID / reject); floored expected counts instead of dropping
  cells; corrected goodness-of-fit degrees of freedom (df = 3 x n_conditions -
  n_params); convergence is now reported honestly
* **Full ROC**: fixed `order = "apriori"` with `conf_bins` (labels were coerced
  to `NA`, silently breaking within-decision ordering)
* **PPV**: `ppv_by_confidence(correction = "effective")` computed the wrong
  effective size in the no-member-data fallback; now uses the pseudo-table
  counts directly
* **Calibration**: added a `confidence_scale = c("auto", "0-1", "0-100")`
  argument to `make_calibration_data()` and all callers; `auto` now warns when
  confidence looks like a Likert/0-10 scale instead of silently dividing by 100
* **Winter 2-HT**: removed the invalid single-condition goodness-of-fit
  chi-square test (the model is saturated, df = 0); `summary()` now reports a
  saturation note and the observed-vs-expected discrepancy as a convergence
  check
* **Diagnosticity ratio**: fixed diagnosticity-ratio bugs and documented the
  Haldane correction in `ln_diag_ratio()`
* Removed a duplicate internal `entropy()` definition that relied on file
  collation order

## Documentation

### New Vignettes

* `simulation_power_analysis.Rmd`: Complete guide to data simulation and power analysis
* `model_comparison.Rmd`: Comprehensive model comparison workflows
* `pauc_statistical_comparison.Rmd`: Statistical testing of ROC curves
* `rac_analysis.Rmd`: Response time-accuracy analysis
* `fullroc_analysis.Rmd`: Full ROC methodology
* `information_error_rate_analysis.Rmd`: EIG and PPV-range analysis
* `calibration_decision_analysis.Rmd`: Calibration, utility, DPP, and ANRI
* `winter_2ht_model.Rmd`: Winter 2-HT model
* `bayesian_inference.Rmd`: Bayesian posterior inference for lineup measures
* `msdt_model.Rmd`: Multi-item signal detection theory
* `sdt_summary_comparisons.Rmd`: SDT comparisons from summary counts
* `sdt_glm_analysis.Rmd`: SDT via GLM/GLMM
* `face_similarity.Rmd`: Deep-learning face similarity

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

* Starns, J. J., Cohen, A. L., & Rotello, C. M. (2023). A complete method for assessing the effectiveness of eyewitness identification procedures: Expected information gain. *Psychological Review, 130*(3), 677–719.

* Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator discriminability and eyewitness discriminability. *Perspectives on Psychological Science, 15*(3), 589-607.

* Seale-Carlisle, T. M., Wetmore, S. A., Flowe, H. D., & Mickes, L. (2019). Designing police lineups to maximize memory performance. *Journal of Applied Research in Memory and Cognition, 8*(4), 420-428.

---

# r4lineups 0.1.2

Previous release. See earlier documentation for details.
