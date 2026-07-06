# r4lineups Agent Summary

This document gives a concise, systematic overview of the **r4lineups**
codebase: folder structure, exported functions (grouped by feature
area), and composite workflows that combine multiple functions.

## Repository structure (high‑level)

- `R/` — all function implementations (exported + internal helpers)
- `man/` — Rd documentation (auto‑generated from roxygen in `R/`)
- `data/` — bundled datasets (`line73`, `lineup_example`, `mickwick`,
  `mockdata`, `nortje2012`)
- `vignettes/` — Rmd vignettes (core + full ROC + calibration/decision +
  EIG/PPV + 2‑HT + SDT GLM)
- `notes/` — working notes, PDFs, and test scripts (not part of package
  build)
- `inst/` — packaged docs (vignette outputs in `inst/doc` after build),
  plus Shiny app in `inst/shiny/r4lineups_app`

## Exported functions (grouped, concise definitions)

### Core lineup proportion and bias

- [`lineup_prop_vec()`](https://cgtza2.github.io/r4lineups/reference/lineup_prop_vec.md)
  — proportion choosing a specific lineup position from a vector.
- [`lineup_prop_tab()`](https://cgtza2.github.io/r4lineups/reference/lineup_prop_tab.md)
  — same as above, from a table.
- [`lineup_prop_boot()`](https://cgtza2.github.io/r4lineups/reference/lineup_prop_boot.md)
  — boot statistic for lineup proportion.
- [`allprop()`](https://cgtza2.github.io/r4lineups/reference/allprop.md)
  — proportions for all lineup members.
- [`lineup_boot_allprop()`](https://cgtza2.github.io/r4lineups/reference/lineup_boot_allprop.md)
  — bootstrap CIs for all member proportions.
- [`allfoilbias()`](https://cgtza2.github.io/r4lineups/reference/allfoilbias.md)
  — foil bias across all members.
- [`allfoil_cihigh()`](https://cgtza2.github.io/r4lineups/reference/allfoil_cihigh.md)
  — CI‑high values for foil bias.
- [`makevec_prop()`](https://cgtza2.github.io/r4lineups/reference/makevec_prop.md)
  — helper to compute proportion vectors.
- [`gen_lineup_prop()`](https://cgtza2.github.io/r4lineups/reference/gen_lineup_prop.md)
  — proportions across bootstrap samples.
- [`gen_boot_propmean_se()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_propmean_se.md)
  — mean/SE for bootstrap proportions.
- [`gen_boot_propci()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_propci.md)
  — percentile CI for bootstrap proportions.
- [`gen_boot_samples()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_samples.md)
  — bootstrap resamples for a lineup vector.
- [`gen_boot_samples_list()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_samples_list.md)
  — bootstrap resamples for list input.
- [`gen_linevec()`](https://cgtza2.github.io/r4lineups/reference/gen_linevec.md)
  — generate synthetic lineup vectors.

### Effective size and functional size

- [`esize_m()`](https://cgtza2.github.io/r4lineups/reference/esize_m.md)
  — Malpass effective size (incl. Tredoux adjustment option).
- [`esize_m_boot()`](https://cgtza2.github.io/r4lineups/reference/esize_m_boot.md)
  — bootstrap statistic for Malpass effective size.
- [`gen_esize_m()`](https://cgtza2.github.io/r4lineups/reference/gen_esize_m.md)
  — bootstrap effective‑size estimates.
- [`gen_esize_m_ci()`](https://cgtza2.github.io/r4lineups/reference/gen_esize_m_ci.md)
  — CI for Malpass effective size.
- [`esize_T()`](https://cgtza2.github.io/r4lineups/reference/esize_T.md)
  — Tredoux effective size (diversity index formulation).
- [`i_esize_T()`](https://cgtza2.github.io/r4lineups/reference/i_esize_T.md)
  — intermediate diversity index.
- [`esize_T_boot()`](https://cgtza2.github.io/r4lineups/reference/esize_T_boot.md)
  — bootstrap statistic for Tredoux effective size.
- [`esize_T_ci_n()`](https://cgtza2.github.io/r4lineups/reference/esize_T_ci_n.md)
  — normal‑theory CI for Tredoux effective size.
- [`func_size()`](https://cgtza2.github.io/r4lineups/reference/func_size.md)
  — functional size (N / suspect IDs).
- [`func_size.boot()`](https://cgtza2.github.io/r4lineups/reference/func_size.boot.md)
  — bootstrap statistic for functional size.
- [`func_size_report()`](https://cgtza2.github.io/r4lineups/reference/func_size_report.md)
  — functional size + boot CIs.
- [`effsize_compare()`](https://cgtza2.github.io/r4lineups/reference/effsize_compare.md)
  — compare effective sizes across lineups.
- [`compare_eff_sizes.boot()`](https://cgtza2.github.io/r4lineups/reference/compare_eff_sizes.boot.md)
  — bootstrap stat for effective size differences.
- [`eff_size_per_foils()`](https://cgtza2.github.io/r4lineups/reference/eff_size_per_foils.md)
  — effective size contribution per foil.

### Diagnosticity (TP/TA lineups)

- [`diag_ratio_W()`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_W.md)
  — Wells & Lindsay diagnosticity ratio.
- [`diag_ratio_T()`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_T.md)
  — Tredoux adjusted diagnosticity ratio.
- [`diag_param()`](https://cgtza2.github.io/r4lineups/reference/diag_param.md)
  — counts for diagnosticity parameters across lineup pairs.
- [`var_diag_ratio()`](https://cgtza2.github.io/r4lineups/reference/var_diag_ratio.md)
  — variance for diagnosticity ratio.
- [`ln_diag_ratio()`](https://cgtza2.github.io/r4lineups/reference/ln_diag_ratio.md)
  — log diagnosticity ratio.
- [`var_lnd()`](https://cgtza2.github.io/r4lineups/reference/var_lnd.md)
  — variance of log diagnosticity.
- [`d_bar()`](https://cgtza2.github.io/r4lineups/reference/d_bar.md) —
  aggregated diagnosticity.
- [`d_weights()`](https://cgtza2.github.io/r4lineups/reference/d_weights.md)
  — weights for diagnosticity aggregation.
- [`chi_diag()`](https://cgtza2.github.io/r4lineups/reference/chi_diag.md)
  — chi‑square test for diagnosticity.
- [`homog_diag()`](https://cgtza2.github.io/r4lineups/reference/homog_diag.md)
  — homogeneity test across diagnosticity ratios.
- [`homog_diag_boot()`](https://cgtza2.github.io/r4lineups/reference/homog_diag_boot.md)
  — bootstrap homogeneity test.
- [`rep_index()`](https://cgtza2.github.io/r4lineups/reference/rep_index.md)
  — representation index helper.

### ROC, CAC, RAC (confidence‑based)

- [`make_rocdata()`](https://cgtza2.github.io/r4lineups/reference/make_rocdata.md)
  — ROC data (suspect IDs only).
- [`make_roc()`](https://cgtza2.github.io/r4lineups/reference/make_roc.md)
  — ROC analysis + plot wrapper.
- [`make_roc_gg()`](https://cgtza2.github.io/r4lineups/reference/make_roc_gg.md)
  — ROC ggplot.
- [`make_cacdata()`](https://cgtza2.github.io/r4lineups/reference/make_cacdata.md)
  — CAC data.
- [`make_cac()`](https://cgtza2.github.io/r4lineups/reference/make_cac.md)
  — CAC analysis + plot wrapper.
- [`make_cac_gg()`](https://cgtza2.github.io/r4lineups/reference/make_cac_gg.md)
  — CAC ggplot.
- [`make_racdata()`](https://cgtza2.github.io/r4lineups/reference/make_racdata.md)
  — RAC data.
- [`make_rac()`](https://cgtza2.github.io/r4lineups/reference/make_rac.md)
  — RAC analysis + plot wrapper.
- [`make_rac_gg()`](https://cgtza2.github.io/r4lineups/reference/make_rac_gg.md)
  — RAC ggplot.
- [`compare_pauc()`](https://cgtza2.github.io/r4lineups/reference/compare_pauc.md)
  — compare pAUC across procedures.
- [`compare_models()`](https://cgtza2.github.io/r4lineups/reference/compare_models.md)
  — compare ROC‑style models.
- [`format_comparison_table()`](https://cgtza2.github.io/r4lineups/reference/format_comparison_table.md)
  — summary table for model comparison.

### Full ROC (Smith & Yang)

- [`make_fullroc_data()`](https://cgtza2.github.io/r4lineups/reference/make_fullroc_data.md)
  — full ROC data (suspect/filler/reject + confidence).
- [`make_fullroc()`](https://cgtza2.github.io/r4lineups/reference/make_fullroc.md)
  — full ROC analysis + plot wrapper.
- [`plot_fullroc()`](https://cgtza2.github.io/r4lineups/reference/plot_fullroc.md)
  — full ROC ggplot.

### EIG (Starns et al.)

- [`make_eig_data()`](https://cgtza2.github.io/r4lineups/reference/make_eig_data.md)
  — EIG data structure by response/decision.
- [`compute_eig()`](https://cgtza2.github.io/r4lineups/reference/compute_eig.md)
  — core EIG calculations.
- [`make_eig()`](https://cgtza2.github.io/r4lineups/reference/make_eig.md)
  — EIG analysis + plots wrapper.
- [`plot_eig()`](https://cgtza2.github.io/r4lineups/reference/plot_eig.md)
  — information‑gain plot.
- [`plot_eig_posteriors()`](https://cgtza2.github.io/r4lineups/reference/plot_eig_posteriors.md)
  — posterior plot.

### PPV range & error‑rate estimation (Fitzgerald et al.)

- [`make_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/make_ppv_range.md)
  — PPV‑range analysis (nominal/effective/none).
- [`ppv_by_confidence()`](https://cgtza2.github.io/r4lineups/reference/ppv_by_confidence.md)
  — PPV by confidence.
- [`ppv_range_by_confidence()`](https://cgtza2.github.io/r4lineups/reference/ppv_range_by_confidence.md)
  — PPV range by confidence.
- [`innocent_id_rate_nominal()`](https://cgtza2.github.io/r4lineups/reference/innocent_id_rate_nominal.md)
  — false ID rate corrected by nominal size.
- [`innocent_id_rate_effective()`](https://cgtza2.github.io/r4lineups/reference/innocent_id_rate_effective.md)
  — false ID rate corrected by effective size.
- [`innocent_id_rate_uncorrected()`](https://cgtza2.github.io/r4lineups/reference/innocent_id_rate_uncorrected.md)
  — uncorrected false ID rate.
- [`plot_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/plot_ppv_range.md)
  — PPV‑range plot.
- [`plot_effective_size_conf()`](https://cgtza2.github.io/r4lineups/reference/plot_effective_size_conf.md)
  — effective size by confidence.
- [`plot_error_rate_conf()`](https://cgtza2.github.io/r4lineups/reference/plot_error_rate_conf.md)
  — error rate by confidence.

### Calibration + ANRI

- [`make_calibration_data()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md)
  — calibration metrics (C, O/U, NRI).
- [`make_calibration()`](https://cgtza2.github.io/r4lineups/reference/make_calibration.md)
  — calibration analysis + plot wrapper.
- [`make_calibration_gg()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_gg.md)
  — calibration plot.
- [`make_calibration_by_condition()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_by_condition.md)
  — calibration by condition.
- [`make_calibration_by_condition_gg()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_by_condition_gg.md)
  — calibration comparison plot.
- [`compute_anri()`](https://cgtza2.github.io/r4lineups/reference/compute_anri.md)
  — ANRI bias‑corrected resolution.
- [`bootstrap_anri()`](https://cgtza2.github.io/r4lineups/reference/bootstrap_anri.md)
  — bootstrap CI for ANRI.
- [`compare_anri()`](https://cgtza2.github.io/r4lineups/reference/compare_anri.md)
  — ANRI group comparison.
- [`plot_anri_comparison()`](https://cgtza2.github.io/r4lineups/reference/plot_anri_comparison.md)
  — ANRI comparison plot.
- [`plot_anri_difference_distribution()`](https://cgtza2.github.io/r4lineups/reference/plot_anri_difference_distribution.md)
  — ANRI difference bootstrap plot.

### Bayesian curves & BREE

- [`make_bayes_curves()`](https://cgtza2.github.io/r4lineups/reference/make_bayes_curves.md)
  — prior‑posterior & info‑gain curves.
- [`plot_bayes_prior_posterior()`](https://cgtza2.github.io/r4lineups/reference/plot_bayes_prior_posterior.md)
  — Bayesian prior‑posterior plot.
- [`plot_bayes_information_gain()`](https://cgtza2.github.io/r4lineups/reference/plot_bayes_information_gain.md)
  — Bayesian info‑gain plot.
- [`make_bree_curve()`](https://cgtza2.github.io/r4lineups/reference/make_bree_curve.md)
  — base‑rate effect equivalency (BREE) curves.
- [`plot_bree()`](https://cgtza2.github.io/r4lineups/reference/plot_bree.md)
  — BREE plot.

### Expected utility analysis

- [`make_utility_curves()`](https://cgtza2.github.io/r4lineups/reference/make_utility_curves.md)
  — expected utility across criteria.
- [`plot_utility_curves()`](https://cgtza2.github.io/r4lineups/reference/plot_utility_curves.md)
  — utility curve plot.
- [`make_utility_difference()`](https://cgtza2.github.io/r4lineups/reference/make_utility_difference.md)
  — utility difference across base rates.
- [`plot_utility_difference()`](https://cgtza2.github.io/r4lineups/reference/plot_utility_difference.md)
  — utility difference plot.
- [`compare_utility()`](https://cgtza2.github.io/r4lineups/reference/compare_utility.md)
  — wrapper for comparison output.

### DPP (Deviation from Perfect Performance)

- [`make_dpp()`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md)
  — compute DPP from data or ROC object.
- [`compute_dpp()`](https://cgtza2.github.io/r4lineups/reference/compute_dpp.md)
  — DPP analysis + plot wrapper.
- [`compare_dpp()`](https://cgtza2.github.io/r4lineups/reference/compare_dpp.md)
  — compare DPP across procedures.
- [`plot_dpp()`](https://cgtza2.github.io/r4lineups/reference/plot_dpp.md)
  — DPP plot.
- [`plot_dpp_comparison()`](https://cgtza2.github.io/r4lineups/reference/plot_dpp_comparison.md)
  — DPP comparison plot.

### Winter 2‑HT model

- [`fit_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/fit_winter_2ht.md)
  — fit 2‑HT model from counts/data.
- [`boot_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/boot_winter_2ht.md)
  — bootstrap CIs for 2‑HT parameters.
- [`plot_2ht_parameters()`](https://cgtza2.github.io/r4lineups/reference/plot_2ht_parameters.md)
  — parameter plot.
- [`plot_2ht_fit()`](https://cgtza2.github.io/r4lineups/reference/plot_2ht_fit.md)
  — observed vs expected counts.

### Face similarity + embeddings (deepface)

- [`install_r4lineups_python()`](https://cgtza2.github.io/r4lineups/reference/install_r4lineups_python.md)
  — install Python deps.
- [`check_python_deps()`](https://cgtza2.github.io/r4lineups/reference/check_python_deps.md)
  — verify Python deps.
- [`available_models()`](https://cgtza2.github.io/r4lineups/reference/available_models.md)
  — supported embedding models.
- [`available_detectors()`](https://cgtza2.github.io/r4lineups/reference/available_detectors.md)
  — supported face detectors.
- [`get_embedding()`](https://cgtza2.github.io/r4lineups/reference/get_embedding.md)
  — extract embedding from an image.
- [`batch_embeddings()`](https://cgtza2.github.io/r4lineups/reference/batch_embeddings.md)
  — batch embeddings.
- [`embedding_distance()`](https://cgtza2.github.io/r4lineups/reference/embedding_distance.md)
  — distance between embeddings.
- [`cosine_to_similarity()`](https://cgtza2.github.io/r4lineups/reference/cosine_to_similarity.md)
  — cosine distance → similarity.
- [`face_similarity()`](https://cgtza2.github.io/r4lineups/reference/face_similarity.md)
  — similarity between two faces.
- [`lineup_similarity()`](https://cgtza2.github.io/r4lineups/reference/lineup_similarity.md)
  — target vs multiple foils; ranks.
- [`lineup_pairwise_matrix()`](https://cgtza2.github.io/r4lineups/reference/lineup_pairwise_matrix.md)
  — full similarity matrix.
- [`plot_lineup_similarity()`](https://cgtza2.github.io/r4lineups/reference/plot_lineup_similarity.md)
  — target‑foil bar plot.
- [`plot_similarity_distribution()`](https://cgtza2.github.io/r4lineups/reference/plot_similarity_distribution.md)
  — similarity distribution.
- [`plot_embedding_space()`](https://cgtza2.github.io/r4lineups/reference/plot_embedding_space.md)
  — embedding projection plot.
- [`display_lineup()`](https://cgtza2.github.io/r4lineups/reference/display_lineup.md)
  — grid display of lineup faces.
- [`detect_faces()`](https://cgtza2.github.io/r4lineups/reference/detect_faces.md)
  — detect faces in an image.
- [`get_face_landmarks()`](https://cgtza2.github.io/r4lineups/reference/get_face_landmarks.md)
  — 468‑point landmark extraction.
- [`get_key_landmarks()`](https://cgtza2.github.io/r4lineups/reference/get_key_landmarks.md)
  — 5‑point landmark extraction.
- [`has_valid_face()`](https://cgtza2.github.io/r4lineups/reference/has_valid_face.md)
  — quick face‑detection check.
- [`rot_vector()`](https://cgtza2.github.io/r4lineups/reference/rot_vector.md)
  — helper for landmark rotation.

### SDT via GLM / GLMM (Wright et al.)

- [`fit_sdt_glm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glm.md)
  — SDT GLM for old/new recognition (probit → d′, logit → lnOR).
- [`fit_sdt_glmm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md)
  — multilevel SDT GLMM with subject/item random effects (requires
  `lme4`).
- [`extract_sdt_metrics()`](https://cgtza2.github.io/r4lineups/reference/extract_sdt_metrics.md)
  — extract d′/lnOR and criterion from model fits.

### SDT summary‑level comparisons (Gourevitch/Miller/Suero)

- [`sdt_summary_from_counts()`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_from_counts.md)
  — d′, c, β from 2×2 counts with extreme‑rate correction.
- [`sdt_summary_variance()`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_variance.md)
  — variance estimates for d′/c (Miller exact‑binomial, Gourevitch
  delta, or bootstrap).
- [`compare_sdt_summary()`](https://cgtza2.github.io/r4lineups/reference/compare_sdt_summary.md)
  — z‑test for d′/c/lnβ between two conditions using summary counts.

### mSDT (multi‑item SDT; Yang et al. 2025)

- [`dmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  /
  [`pmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  /
  [`qmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  /
  [`rmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  — max filler distribution (IID normal fillers).
- [`max_filler_moments()`](https://cgtza2.github.io/r4lineups/reference/max_filler_moments.md)
  — mean/var/skewness of max filler distribution (numeric).
- [`msdt_gamma_from_rej()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
  — estimate decision criterion from TA rejection rate.
- [`msdt_dprime_from_rej()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
  — estimate d′ from TP rejection rate + gamma.
- [`estimate_msdt_params()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
  — convenience wrapper for gamma + d′.
- [`plot_msdt_joint()`](https://cgtza2.github.io/r4lineups/reference/plot_msdt_joint.md)
  — joint density contour plot (suspect vs max filler).

### Simulation & power

- [`simulate_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md)
  — synthetic lineup data generator.
- [`simulate_power_analysis()`](https://cgtza2.github.io/r4lineups/reference/simulate_power_analysis.md)
  — power simulation (ROC/lineup comparisons).

### Validation & internal helpers

- [`datacheck1()`](https://cgtza2.github.io/r4lineups/reference/datacheck1.md),
  [`datacheck2()`](https://cgtza2.github.io/r4lineups/reference/datacheck2.md),
  [`datacheck3()`](https://cgtza2.github.io/r4lineups/reference/datacheck3.md)
  — input validation helpers.

## Composite workflows (functions that aggregate multiple steps)

These wrappers combine data creation, analysis, and plotting:

- **ROC/CAC/RAC**:
  [`make_roc()`](https://cgtza2.github.io/r4lineups/reference/make_roc.md),
  [`make_cac()`](https://cgtza2.github.io/r4lineups/reference/make_cac.md),
  [`make_rac()`](https://cgtza2.github.io/r4lineups/reference/make_rac.md)
- **Full ROC**:
  [`make_fullroc()`](https://cgtza2.github.io/r4lineups/reference/make_fullroc.md) +
  [`plot_fullroc()`](https://cgtza2.github.io/r4lineups/reference/plot_fullroc.md)
- **EIG**:
  [`make_eig()`](https://cgtza2.github.io/r4lineups/reference/make_eig.md) +
  [`plot_eig()`](https://cgtza2.github.io/r4lineups/reference/plot_eig.md) +
  [`plot_eig_posteriors()`](https://cgtza2.github.io/r4lineups/reference/plot_eig_posteriors.md)
- **PPV range**:
  [`make_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/make_ppv_range.md) +
  [`plot_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/plot_ppv_range.md) +
  [`plot_effective_size_conf()`](https://cgtza2.github.io/r4lineups/reference/plot_effective_size_conf.md)
- **Calibration**:
  [`make_calibration()`](https://cgtza2.github.io/r4lineups/reference/make_calibration.md) +
  [`make_calibration_gg()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_gg.md)
- **Calibration by condition**:
  [`make_calibration_by_condition()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_by_condition.md) +
  [`make_calibration_by_condition_gg()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_by_condition_gg.md)
- **ANRI**:
  [`compute_anri()`](https://cgtza2.github.io/r4lineups/reference/compute_anri.md) +
  [`bootstrap_anri()`](https://cgtza2.github.io/r4lineups/reference/bootstrap_anri.md) +
  [`compare_anri()`](https://cgtza2.github.io/r4lineups/reference/compare_anri.md) +
  plots
- **Bayesian**:
  [`make_bayes_curves()`](https://cgtza2.github.io/r4lineups/reference/make_bayes_curves.md) +
  plots;
  [`make_bree_curve()`](https://cgtza2.github.io/r4lineups/reference/make_bree_curve.md) +
  [`plot_bree()`](https://cgtza2.github.io/r4lineups/reference/plot_bree.md)
- **Utility**:
  [`make_utility_curves()`](https://cgtza2.github.io/r4lineups/reference/make_utility_curves.md) +
  [`plot_utility_curves()`](https://cgtza2.github.io/r4lineups/reference/plot_utility_curves.md);
  [`compare_utility()`](https://cgtza2.github.io/r4lineups/reference/compare_utility.md) +
  [`plot_utility_difference()`](https://cgtza2.github.io/r4lineups/reference/plot_utility_difference.md)
- **DPP**:
  [`make_dpp()`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md)/[`compute_dpp()`](https://cgtza2.github.io/r4lineups/reference/compute_dpp.md) +
  [`plot_dpp()`](https://cgtza2.github.io/r4lineups/reference/plot_dpp.md);
  [`compare_dpp()`](https://cgtza2.github.io/r4lineups/reference/compare_dpp.md) +
  [`plot_dpp_comparison()`](https://cgtza2.github.io/r4lineups/reference/plot_dpp_comparison.md)
- **Winter 2‑HT**:
  [`fit_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/fit_winter_2ht.md) +
  [`plot_2ht_parameters()`](https://cgtza2.github.io/r4lineups/reference/plot_2ht_parameters.md) +
  [`plot_2ht_fit()`](https://cgtza2.github.io/r4lineups/reference/plot_2ht_fit.md);
  [`boot_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/boot_winter_2ht.md)
  for CIs
- **Face similarity**:
  [`get_embedding()`](https://cgtza2.github.io/r4lineups/reference/get_embedding.md)/[`batch_embeddings()`](https://cgtza2.github.io/r4lineups/reference/batch_embeddings.md) +
  [`lineup_similarity()`](https://cgtza2.github.io/r4lineups/reference/lineup_similarity.md) +
  plots
  ([`plot_lineup_similarity()`](https://cgtza2.github.io/r4lineups/reference/plot_lineup_similarity.md),
  [`plot_embedding_space()`](https://cgtza2.github.io/r4lineups/reference/plot_embedding_space.md))
- **SDT GLM/GLMM**:
  [`fit_sdt_glm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glm.md)/[`fit_sdt_glmm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md) +
  [`extract_sdt_metrics()`](https://cgtza2.github.io/r4lineups/reference/extract_sdt_metrics.md)

## Vignettes (current)

- `Vignette.Rmd` — core lineup fairness measures + ROC basics
- `fullroc_analysis.Rmd` — full ROC (Smith & Yang)
- `calibration_decision_analysis.Rmd` — calibration, Bayesian curves,
  utility, DPP, ANRI
- `information_error_rate_analysis.Rmd` — EIG + PPV‑range
- `winter_2ht_model.Rmd` — Winter 2‑HT model
- `face_similarity.Rmd` — pending face similarity walkthrough
- `sdt_glm_analysis.Rmd` — SDT via GLM/GLMM (old/new recognition)
- `sdt_summary_comparisons.Rmd` — SDT summary‑level comparisons from
  counts
- `msdt_model.Rmd` — multi‑item SDT (mSDT) core functions

## Current status and TODO (high‑level)

**Implemented & stable** - Core lineup fairness metrics (bias, effective
size, functional size, diagnosticity). - ROC/CAC/RAC + full ROC + pAUC
comparison. - EIG, PPV range, calibration, ANRI, DPP, utility, Bayesian
curves, Winter 2‑HT. - Face similarity tooling + Shiny app
integration. - SDT via GLM/GLMM (Wright et al.) + vignette.

**Remaining / future work** - Multilevel multinomial lineup model
(Wright & Sparks) with system vs estimator covariates. - MUD (mixed
unknown distributions) nonparametric mixture test (Wright & Skagerberg,
2008). - Additional SDT models (BEST‑REST, ensemble/integration) + z‑ROC
estimation. - Standardized data‑format helper for confidence‑based
analyses. - Showup‑specific handling. - Expanded unit tests + edge‑case
validation for recent additions.

## Notes for agents

- Most analyses expect either a **lineup vector** (mock‑witness choices)
  or a **data frame** with `target_present`, `identification`, and
  `confidence` for confidence‑based methods.
- `make_*()` functions typically return structured S3 objects with
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
- For face similarity, ensure Python deps are installed
  ([`install_r4lineups_python()`](https://cgtza2.github.io/r4lineups/reference/install_r4lineups_python.md)),
  and set `MPLCONFIGDIR` if matplotlib cache issues appear.
