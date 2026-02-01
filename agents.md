# r4lineups Agent Summary

This document gives a concise, systematic overview of the **r4lineups** codebase: folder structure, exported functions (grouped by feature area), and composite workflows that combine multiple functions.

## Repository structure (high‑level)

- `R/` — all function implementations (exported + internal helpers)
- `man/` — Rd documentation (auto‑generated from roxygen in `R/`)
- `data/` — bundled datasets (`line73`, `lineup_example`, `mickwick`, `mockdata`, `nortje2012`)
- `vignettes/` — Rmd vignettes (core + full ROC + calibration/decision + EIG/PPV + 2‑HT + SDT GLM)
- `notes/` — working notes, PDFs, and test scripts (not part of package build)
- `inst/` — packaged docs (vignette outputs in `inst/doc` after build), plus Shiny app in `inst/shiny/r4lineups_app`

## Exported functions (grouped, concise definitions)

### Core lineup proportion and bias
- `lineup_prop_vec()` — proportion choosing a specific lineup position from a vector.
- `lineup_prop_tab()` — same as above, from a table.
- `lineup_prop_boot()` — boot statistic for lineup proportion.
- `allprop()` — proportions for all lineup members.
- `lineup_boot_allprop()` — bootstrap CIs for all member proportions.
- `allfoilbias()` — foil bias across all members.
- `allfoil_cihigh()` — CI‑high values for foil bias.
- `makevec_prop()` — helper to compute proportion vectors.
- `gen_lineup_prop()` — proportions across bootstrap samples.
- `gen_boot_propmean_se()` — mean/SE for bootstrap proportions.
- `gen_boot_propci()` — percentile CI for bootstrap proportions.
- `gen_boot_samples()` — bootstrap resamples for a lineup vector.
- `gen_boot_samples_list()` — bootstrap resamples for list input.
- `gen_linevec()` — generate synthetic lineup vectors.

### Effective size and functional size
- `esize_m()` — Malpass effective size (incl. Tredoux adjustment option).
- `esize_m_boot()` — bootstrap statistic for Malpass effective size.
- `gen_esize_m()` — bootstrap effective‑size estimates.
- `gen_esize_m_ci()` — CI for Malpass effective size.
- `esize_T()` — Tredoux effective size (diversity index formulation).
- `i_esize_T()` — intermediate diversity index.
- `esize_T_boot()` — bootstrap statistic for Tredoux effective size.
- `esize_T_ci_n()` — normal‑theory CI for Tredoux effective size.
- `func_size()` — functional size (N / suspect IDs).
- `func_size.boot()` — bootstrap statistic for functional size.
- `func_size_report()` — functional size + boot CIs.
- `effsize_compare()` — compare effective sizes across lineups.
- `compare_eff_sizes.boot()` — bootstrap stat for effective size differences.
- `eff_size_per_foils()` — effective size contribution per foil.

### Diagnosticity (TP/TA lineups)
- `diag_ratio_W()` — Wells & Lindsay diagnosticity ratio.
- `diag_ratio_T()` — Tredoux adjusted diagnosticity ratio.
- `diag_param()` — counts for diagnosticity parameters across lineup pairs.
- `var_diag_ratio()` — variance for diagnosticity ratio.
- `ln_diag_ratio()` — log diagnosticity ratio.
- `var_lnd()` — variance of log diagnosticity.
- `d_bar()` — aggregated diagnosticity.
- `d_weights()` — weights for diagnosticity aggregation.
- `chi_diag()` — chi‑square test for diagnosticity.
- `homog_diag()` — homogeneity test across diagnosticity ratios.
- `homog_diag_boot()` — bootstrap homogeneity test.
- `rep_index()` — representation index helper.

### ROC, CAC, RAC (confidence‑based)
- `make_rocdata()` — ROC data (suspect IDs only).
- `make_roc()` — ROC analysis + plot wrapper.
- `make_roc_gg()` — ROC ggplot.
- `make_cacdata()` — CAC data.
- `make_cac()` — CAC analysis + plot wrapper.
- `make_cac_gg()` — CAC ggplot.
- `make_racdata()` — RAC data.
- `make_rac()` — RAC analysis + plot wrapper.
- `make_rac_gg()` — RAC ggplot.
- `compare_pauc()` — compare pAUC across procedures.
- `compare_models()` — compare ROC‑style models.
- `format_comparison_table()` — summary table for model comparison.

### Full ROC (Smith & Yang)
- `make_fullroc_data()` — full ROC data (suspect/filler/reject + confidence).
- `make_fullroc()` — full ROC analysis + plot wrapper.
- `plot_fullroc()` — full ROC ggplot.

### EIG (Starns et al.)
- `make_eig_data()` — EIG data structure by response/decision.
- `compute_eig()` — core EIG calculations.
- `make_eig()` — EIG analysis + plots wrapper.
- `plot_eig()` — information‑gain plot.
- `plot_eig_posteriors()` — posterior plot.

### PPV range & error‑rate estimation (Fitzgerald et al.)
- `make_ppv_range()` — PPV‑range analysis (nominal/effective/none).
- `ppv_by_confidence()` — PPV by confidence.
- `ppv_range_by_confidence()` — PPV range by confidence.
- `innocent_id_rate_nominal()` — false ID rate corrected by nominal size.
- `innocent_id_rate_effective()` — false ID rate corrected by effective size.
- `innocent_id_rate_uncorrected()` — uncorrected false ID rate.
- `plot_ppv_range()` — PPV‑range plot.
- `plot_effective_size_conf()` — effective size by confidence.
- `plot_error_rate_conf()` — error rate by confidence.

### Calibration + ANRI
- `make_calibration_data()` — calibration metrics (C, O/U, NRI).
- `make_calibration()` — calibration analysis + plot wrapper.
- `make_calibration_gg()` — calibration plot.
- `make_calibration_by_condition()` — calibration by condition.
- `make_calibration_by_condition_gg()` — calibration comparison plot.
- `compute_anri()` — ANRI bias‑corrected resolution.
- `bootstrap_anri()` — bootstrap CI for ANRI.
- `compare_anri()` — ANRI group comparison.
- `plot_anri_comparison()` — ANRI comparison plot.
- `plot_anri_difference_distribution()` — ANRI difference bootstrap plot.

### Bayesian curves & BREE
- `make_bayes_curves()` — prior‑posterior & info‑gain curves.
- `plot_bayes_prior_posterior()` — Bayesian prior‑posterior plot.
- `plot_bayes_information_gain()` — Bayesian info‑gain plot.
- `make_bree_curve()` — base‑rate effect equivalency (BREE) curves.
- `plot_bree()` — BREE plot.

### Expected utility analysis
- `make_utility_curves()` — expected utility across criteria.
- `plot_utility_curves()` — utility curve plot.
- `make_utility_difference()` — utility difference across base rates.
- `plot_utility_difference()` — utility difference plot.
- `compare_utility()` — wrapper for comparison output.

### DPP (Deviation from Perfect Performance)
- `make_dpp()` — compute DPP from data or ROC object.
- `compute_dpp()` — DPP analysis + plot wrapper.
- `compare_dpp()` — compare DPP across procedures.
- `plot_dpp()` — DPP plot.
- `plot_dpp_comparison()` — DPP comparison plot.

### Winter 2‑HT model
- `fit_winter_2ht()` — fit 2‑HT model from counts/data.
- `boot_winter_2ht()` — bootstrap CIs for 2‑HT parameters.
- `plot_2ht_parameters()` — parameter plot.
- `plot_2ht_fit()` — observed vs expected counts.

### Face similarity + embeddings (deepface)
- `install_r4lineups_python()` — install Python deps.
- `check_python_deps()` — verify Python deps.
- `available_models()` — supported embedding models.
- `available_detectors()` — supported face detectors.
- `get_embedding()` — extract embedding from an image.
- `batch_embeddings()` — batch embeddings.
- `embedding_distance()` — distance between embeddings.
- `cosine_to_similarity()` — cosine distance → similarity.
- `face_similarity()` — similarity between two faces.
- `lineup_similarity()` — target vs multiple foils; ranks.
- `lineup_pairwise_matrix()` — full similarity matrix.
- `plot_lineup_similarity()` — target‑foil bar plot.
- `plot_similarity_distribution()` — similarity distribution.
- `plot_embedding_space()` — embedding projection plot.
- `display_lineup()` — grid display of lineup faces.
- `detect_faces()` — detect faces in an image.
- `get_face_landmarks()` — 468‑point landmark extraction.
- `get_key_landmarks()` — 5‑point landmark extraction.
- `has_valid_face()` — quick face‑detection check.
- `rot_vector()` — helper for landmark rotation.

### SDT via GLM / GLMM (Wright et al.)
- `fit_sdt_glm()` — SDT GLM for old/new recognition (probit → d′, logit → lnOR).
- `fit_sdt_glmm()` — multilevel SDT GLMM with subject/item random effects (requires `lme4`).
- `extract_sdt_metrics()` — extract d′/lnOR and criterion from model fits.

### Simulation & power
- `simulate_lineup_data()` — synthetic lineup data generator.
- `simulate_power_analysis()` — power simulation (ROC/lineup comparisons).

### Validation & internal helpers
- `datacheck1()`, `datacheck2()`, `datacheck3()` — input validation helpers.

## Composite workflows (functions that aggregate multiple steps)

These wrappers combine data creation, analysis, and plotting:

- **ROC/CAC/RAC**: `make_roc()`, `make_cac()`, `make_rac()`
- **Full ROC**: `make_fullroc()` + `plot_fullroc()`
- **EIG**: `make_eig()` + `plot_eig()` + `plot_eig_posteriors()`
- **PPV range**: `make_ppv_range()` + `plot_ppv_range()` + `plot_effective_size_conf()`
- **Calibration**: `make_calibration()` + `make_calibration_gg()`
- **Calibration by condition**: `make_calibration_by_condition()` + `make_calibration_by_condition_gg()`
- **ANRI**: `compute_anri()` + `bootstrap_anri()` + `compare_anri()` + plots
- **Bayesian**: `make_bayes_curves()` + plots; `make_bree_curve()` + `plot_bree()`
- **Utility**: `make_utility_curves()` + `plot_utility_curves()`; `compare_utility()` + `plot_utility_difference()`
- **DPP**: `make_dpp()`/`compute_dpp()` + `plot_dpp()`; `compare_dpp()` + `plot_dpp_comparison()`
- **Winter 2‑HT**: `fit_winter_2ht()` + `plot_2ht_parameters()` + `plot_2ht_fit()`; `boot_winter_2ht()` for CIs
- **Face similarity**: `get_embedding()`/`batch_embeddings()` + `lineup_similarity()` + plots (`plot_lineup_similarity()`, `plot_embedding_space()`)
- **SDT GLM/GLMM**: `fit_sdt_glm()`/`fit_sdt_glmm()` + `extract_sdt_metrics()`

## Vignettes (current)

- `Vignette.Rmd` — core lineup fairness measures + ROC basics
- `fullroc_analysis.Rmd` — full ROC (Smith & Yang)
- `calibration_decision_analysis.Rmd` — calibration, Bayesian curves, utility, DPP, ANRI
- `information_error_rate_analysis.Rmd` — EIG + PPV‑range
- `winter_2ht_model.Rmd` — Winter 2‑HT model
- `face_similarity.Rmd` — pending face similarity walkthrough
- `sdt_glm_analysis.Rmd` — SDT via GLM/GLMM (old/new recognition)

## Current status and TODO (high‑level)

**Implemented & stable**
- Core lineup fairness metrics (bias, effective size, functional size, diagnosticity).
- ROC/CAC/RAC + full ROC + pAUC comparison.
- EIG, PPV range, calibration, ANRI, DPP, utility, Bayesian curves, Winter 2‑HT.
- Face similarity tooling + Shiny app integration.
- SDT via GLM/GLMM (Wright et al.) + vignette.

**Remaining / future work**
- Multilevel multinomial lineup model (Wright & Sparks) with system vs estimator covariates.
- MUD (mixed unknown distributions) nonparametric mixture test (Wright & Skagerberg, 2008).
- Additional SDT models (BEST‑REST, ensemble/integration) + z‑ROC estimation.
- Standardized data‑format helper for confidence‑based analyses.
- Showup‑specific handling.
- Expanded unit tests + edge‑case validation for recent additions.

## Notes for agents

- Most analyses expect either a **lineup vector** (mock‑witness choices) or a **data frame** with `target_present`, `identification`, and `confidence` for confidence‑based methods.
- `make_*()` functions typically return structured S3 objects with `print()`, `summary()`, and `plot()` methods.
- For face similarity, ensure Python deps are installed (`install_r4lineups_python()`), and set `MPLCONFIGDIR` if matplotlib cache issues appear.
