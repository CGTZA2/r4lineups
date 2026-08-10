# Package index

## Data preparation and validation

Validate, standardize, and simulate lineup data.

- [`create_example_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/create_example_lineup_data.md)
  : Create Example Lineup Data
- [`datacheck1()`](https://cgtza2.github.io/r4lineups/reference/datacheck1.md)
  : Helper function
- [`datacheck2()`](https://cgtza2.github.io/r4lineups/reference/datacheck2.md)
  : Helper function
- [`datacheck3()`](https://cgtza2.github.io/r4lineups/reference/datacheck3.md)
  : Helper function
- [`line73`](https://cgtza2.github.io/r4lineups/reference/line73.md) :
  line73
- [`lineup_example`](https://cgtza2.github.io/r4lineups/reference/lineup_example.md)
  : Example Lineup Identification Data
- [`mickwick`](https://cgtza2.github.io/r4lineups/reference/mickwick.md)
  : Confidence & Accuracy data (Mickes & Wixted)
- [`mockdata`](https://cgtza2.github.io/r4lineups/reference/mockdata.md)
  : mockdata
- [`nortje2012`](https://cgtza2.github.io/r4lineups/reference/nortje2012.md)
  : nortje2012
- [`plot(`*`<power_analysis>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.power_analysis.md)
  : Plot Power Analysis Results
- [`print(`*`<lineup_data>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_data.md)
  : Print Method for Lineup Data
- [`simulate_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md)
  : Simulate Lineup Identification Data
- [`simulate_power_analysis()`](https://cgtza2.github.io/r4lineups/reference/simulate_power_analysis.md)
  : Simulate Power Analysis for Lineup Studies
- [`standardize_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/standardize_lineup_data.md)
  : Standardize Lineup Identification Data
- [`validate_lineup_data()`](https://cgtza2.github.io/r4lineups/reference/validate_lineup_data.md)
  : Validate Lineup Identification Data

## Lineup bias and member proportions

Proportions choosing each lineup member, foil bias, and bootstrap CIs.

- [`allfoil_cihigh()`](https://cgtza2.github.io/r4lineups/reference/allfoil_cihigh.md)
  : Confidence Intervals for Proportion
- [`allfoilbias()`](https://cgtza2.github.io/r4lineups/reference/allfoilbias.md)
  : Bias for each lineup member
- [`allprop()`](https://cgtza2.github.io/r4lineups/reference/allprop.md)
  : Lineup proportion for all lineup members
- [`gen_boot_propci()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_propci.md)
  : Percentile of Bootstrapped Lineup Proportion
- [`gen_boot_propmean_se()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_propmean_se.md)
  : Descriptive statistics for bootstrapped lineup proportion
- [`gen_boot_samples()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_samples.md)
  : Bootstrap resampling
- [`gen_boot_samples_list()`](https://cgtza2.github.io/r4lineups/reference/gen_boot_samples_list.md)
  : Bootstrapped resampling
- [`lineup_boot_allprop()`](https://cgtza2.github.io/r4lineups/reference/lineup_boot_allprop.md)
  : Confidence intervals for lineup proportion
- [`lineup_prop_boot()`](https://cgtza2.github.io/r4lineups/reference/lineup_prop_boot.md)
  : Bootstrapped lineup proportion
- [`lineup_prop_tab()`](https://cgtza2.github.io/r4lineups/reference/lineup_prop_tab.md)
  : Lineup proportion
- [`lineup_prop_vec()`](https://cgtza2.github.io/r4lineups/reference/lineup_prop_vec.md)
  : Lineup proportion
- [`makevec_prop()`](https://cgtza2.github.io/r4lineups/reference/makevec_prop.md)
  : Helper functions
- [`rot_vector()`](https://cgtza2.github.io/r4lineups/reference/rot_vector.md)
  : Rotate vector

## Effective size

Tredoux’s E’, Malpass’s effective size, and inference on them.

- [`compare_eff_sizes.boot()`](https://cgtza2.github.io/r4lineups/reference/compare_eff_sizes.boot.md)
  : Comparing Effective Size: Base function for bootstrapping
- [`esize_boot_dist()`](https://cgtza2.github.io/r4lineups/reference/esize_boot_dist.md)
  : Bootstrap Distribution of Effective Size
- [`esize_m()`](https://cgtza2.github.io/r4lineups/reference/esize_m.md)
  : Effective Size
- [`esize_m_boot()`](https://cgtza2.github.io/r4lineups/reference/esize_m_boot.md)
  : Bootstrapped Effective Size
- [`esize_T()`](https://cgtza2.github.io/r4lineups/reference/esize_T.md)
  : Tredoux Effective Size (Tredoux, 1998)
- [`esize_T_bayes()`](https://cgtza2.github.io/r4lineups/reference/esize_T_bayes.md)
  : Bayesian Posterior Distribution of Tredoux Effective Size
- [`esize_T_bayes_compare()`](https://cgtza2.github.io/r4lineups/reference/esize_T_bayes_compare.md)
  : Bayesian Comparison of Tredoux Effective Size Across Two Lineups
- [`esize_T_boot()`](https://cgtza2.github.io/r4lineups/reference/esize_T_boot.md)
  : Bootstrapped Effective Size (Tredoux, 1998)
- [`esize_T_ci_n()`](https://cgtza2.github.io/r4lineups/reference/esize_T_ci_n.md)
  : Effective Size with Confidence Intervals from Normal Theory
  (Tredoux, 1998)
- [`i_esize_T()`](https://cgtza2.github.io/r4lineups/reference/i_esize_T.md)
  : I Component of Tredoux Effective Size (Tredoux, 1998)

## Functional size

Functional size estimation and bootstrap/Bayesian inference.

- [`func_size()`](https://cgtza2.github.io/r4lineups/reference/func_size.md)
  : Functional Size
- [`func_size_bayes()`](https://cgtza2.github.io/r4lineups/reference/func_size_bayes.md)
  : Bayesian Functional Size (Beta-Binomial Model)
- [`func_size_report()`](https://cgtza2.github.io/r4lineups/reference/func_size_report.md)
  : Functional Size with Bootstrapped Confidence Intervals
- [`func_size.boot()`](https://cgtza2.github.io/r4lineups/reference/func_size.boot.md)
  : Bootstrapped Functional Size

## Diagnosticity

Diagnosticity ratios and homogeneity tests.

- [`chi_diag()`](https://cgtza2.github.io/r4lineups/reference/chi_diag.md)
  : Chi-squared estimate of homogeneity of diagnosticity ratio
- [`d_bar()`](https://cgtza2.github.io/r4lineups/reference/d_bar.md) :
  Mean diagnosticity ratio for k lineup pairs
- [`d_weights()`](https://cgtza2.github.io/r4lineups/reference/d_weights.md)
  : Diagnosticity ratio weights
- [`diag_param()`](https://cgtza2.github.io/r4lineups/reference/diag_param.md)
  : Parameters for diagnosticity ratio
- [`diag_ratio_T()`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_T.md)
  : Diagnosticity Ratio (Tredoux, 1998)
- [`diag_ratio_T_bayes()`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_T_bayes.md)
  : Bayesian Diagnosticity Ratio (Tredoux, Beta-Binomial Model)
- [`diag_ratio_T_bayes_compare()`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_T_bayes_compare.md)
  : Bayesian Comparison of Two Diagnosticity Ratios
- [`diag_ratio_W()`](https://cgtza2.github.io/r4lineups/reference/diag_ratio_W.md)
  : Diagnosticity Ratio (Wells & Lindsay, 1980; Wells & Turtle, 1986)
- [`homog_diag()`](https://cgtza2.github.io/r4lineups/reference/homog_diag.md)
  : Master function: Homogeneity of diagnosticity ratio
- [`homog_diag_boot()`](https://cgtza2.github.io/r4lineups/reference/homog_diag_boot.md)
  : Homogeneity of diagnosticity ratio with bootstrapped CIs
- [`ln_diag_ratio()`](https://cgtza2.github.io/r4lineups/reference/ln_diag_ratio.md)
  : Ln of Diagnosticity Ratio
- [`var_diag_ratio()`](https://cgtza2.github.io/r4lineups/reference/var_diag_ratio.md)
  : Variance of diagnosticity ratio (Tredoux)
- [`var_lnd()`](https://cgtza2.github.io/r4lineups/reference/var_lnd.md)
  : Variance of ln of diagnosticity ratio

## ROC, CAC, and RAC analysis

Receiver operating characteristic, confidence-accuracy, and response
time-accuracy analysis.

- [`make_cac()`](https://cgtza2.github.io/r4lineups/reference/make_cac.md)
  : Compute and Plot CAC for Lineup Identification
- [`make_cac_gg()`](https://cgtza2.github.io/r4lineups/reference/make_cac_gg.md)
  : Plot CAC Curve
- [`make_cacdata()`](https://cgtza2.github.io/r4lineups/reference/make_cacdata.md)
  : Compute CAC (Confidence-Accuracy Characteristic) Data
- [`make_rac()`](https://cgtza2.github.io/r4lineups/reference/make_rac.md)
  : Compute and Plot RAC for Lineup Identification
- [`make_rac_gg()`](https://cgtza2.github.io/r4lineups/reference/make_rac_gg.md)
  : Plot RAC Curve
- [`make_racdata()`](https://cgtza2.github.io/r4lineups/reference/make_racdata.md)
  : Compute RAC (Response Time-Accuracy Characteristic) Data
- [`make_roc()`](https://cgtza2.github.io/r4lineups/reference/make_roc.md)
  : Compute and Plot ROC Curve for Lineup Identification
- [`make_roc_gg()`](https://cgtza2.github.io/r4lineups/reference/make_roc_gg.md)
  : Plot ROC Curve for Lineup Identification
- [`make_rocdata()`](https://cgtza2.github.io/r4lineups/reference/make_rocdata.md)
  : Compute ROC Data for Lineup Identification
- [`print(`*`<lineup_cac>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_cac.md)
  : Print Method for lineup_cac Objects
- [`print(`*`<lineup_rac>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_rac.md)
  : Print Method for lineup_rac Objects
- [`print(`*`<lineup_roc>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_roc.md)
  : Print Method for lineup_roc Objects

## Full ROC (Smith & Yang, 2020)

Full ROC curves using all response categories.

- [`make_fullroc()`](https://cgtza2.github.io/r4lineups/reference/make_fullroc.md)
  : Compute and Plot Full ROC Curve (Smith & Yang, 2020)
- [`make_fullroc_data()`](https://cgtza2.github.io/r4lineups/reference/make_fullroc_data.md)
  : Compute Full ROC Data for Lineup Identification (Smith & Yang, 2020)
- [`plot_fullroc()`](https://cgtza2.github.io/r4lineups/reference/plot_fullroc.md)
  : Plot Full ROC Curve
- [`print(`*`<lineup_fullroc>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_fullroc.md)
  : Print Method for lineup_fullroc Objects

## pAUC and DPP comparisons

Statistical comparison of ROC curves and deviation from perfect
performance.

- [`compare_dpp()`](https://cgtza2.github.io/r4lineups/reference/compare_dpp.md)
  : Compare DPP Between Two Procedures
- [`compare_pauc()`](https://cgtza2.github.io/r4lineups/reference/compare_pauc.md)
  : Compare pAUC Between Two ROC Curves
- [`compute_dpp()`](https://cgtza2.github.io/r4lineups/reference/compute_dpp.md)
  : Compute and Plot DPP
- [`make_dpp()`](https://cgtza2.github.io/r4lineups/reference/make_dpp.md)
  : Compute Deviation from Perfect Performance (DPP)
- [`plot_dpp()`](https://cgtza2.github.io/r4lineups/reference/plot_dpp.md)
  : Plot DPP with Observed and Perfect ROC Curves
- [`plot_dpp_comparison()`](https://cgtza2.github.io/r4lineups/reference/plot_dpp_comparison.md)
  : Plot DPP Comparison Between Two Procedures
- [`plot(`*`<pauc_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.pauc_comparison.md)
  : Plot pAUC Comparison
- [`print(`*`<lineup_dpp>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_dpp.md)
  : Print Method for lineup_dpp Objects
- [`print(`*`<pauc_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.pauc_comparison.md)
  : Print Method for pauc_comparison Objects
- [`summary(`*`<pauc_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/summary.pauc_comparison.md)
  : Summary Method for pauc_comparison Objects

## Expected information gain and PPV

EIG (Starns et al., 2023) and positive predictive value ranges.

- [`compute_eig()`](https://cgtza2.github.io/r4lineups/reference/compute_eig.md)
  : Compute Expected Information Gain (EIG)
- [`innocent_id_rate_effective()`](https://cgtza2.github.io/r4lineups/reference/innocent_id_rate_effective.md)
  : Compute Innocent Suspect ID Rate with Effective Size Correction
- [`innocent_id_rate_nominal()`](https://cgtza2.github.io/r4lineups/reference/innocent_id_rate_nominal.md)
  : Compute Innocent Suspect ID Rate with Nominal Size Correction
- [`innocent_id_rate_uncorrected()`](https://cgtza2.github.io/r4lineups/reference/innocent_id_rate_uncorrected.md)
  : Compute Innocent Suspect ID Rate with No Correction
- [`make_eig()`](https://cgtza2.github.io/r4lineups/reference/make_eig.md)
  : Main Function to Compute and Visualize EIG
- [`make_eig_data()`](https://cgtza2.github.io/r4lineups/reference/make_eig_data.md)
  : Prepare EIG Data from Lineup Identification
- [`make_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/make_ppv_range.md)
  : Main Function to Compute and Visualize PPV Range
- [`plot_eig()`](https://cgtza2.github.io/r4lineups/reference/plot_eig.md)
  : Plot Information Gain by Response Category
- [`plot_eig_posteriors()`](https://cgtza2.github.io/r4lineups/reference/plot_eig_posteriors.md)
  : Plot Posterior Probabilities for Response Categories
- [`plot_ppv_range()`](https://cgtza2.github.io/r4lineups/reference/plot_ppv_range.md)
  : Plot PPV Range Across Confidence Levels
- [`ppv_by_confidence()`](https://cgtza2.github.io/r4lineups/reference/ppv_by_confidence.md)
  : Compute PPV by Confidence with Lineup Size Correction
- [`ppv_range_by_confidence()`](https://cgtza2.github.io/r4lineups/reference/ppv_range_by_confidence.md)
  : Compute PPV Range by Confidence (All Three Corrections)
- [`print(`*`<lineup_eig>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_eig.md)
  : Print Method for lineup_eig Objects
- [`print(`*`<lineup_ppv>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_ppv.md)
  : Print Method for lineup_ppv Objects
- [`print(`*`<lineup_ppv_range>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_ppv_range.md)
  : Print Method for lineup_ppv_range Objects
- [`summary(`*`<lineup_eig>`*`)`](https://cgtza2.github.io/r4lineups/reference/summary.lineup_eig.md)
  : Summary Method for lineup_eig Objects

## Calibration and ANRI

Confidence-accuracy calibration, over/underconfidence, and adjusted
normalized resolution index.

- [`bootstrap_anri()`](https://cgtza2.github.io/r4lineups/reference/bootstrap_anri.md)
  : Bootstrap Confidence Intervals for ANRI
- [`calibration_bayes()`](https://cgtza2.github.io/r4lineups/reference/calibration_bayes.md)
  : Bayesian Calibration Analysis (Beta-Binomial Model)
- [`compare_anri()`](https://cgtza2.github.io/r4lineups/reference/compare_anri.md)
  : Compare ANRI Between Groups with Bootstrap
- [`compute_anri()`](https://cgtza2.github.io/r4lineups/reference/compute_anri.md)
  : Compute ANRI (Adjusted Normalized Resolution Index)
- [`make_calibration()`](https://cgtza2.github.io/r4lineups/reference/make_calibration.md)
  : Compute and Plot Calibration for Lineup Identification
- [`make_calibration_by_condition()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_by_condition.md)
  : Compute Calibration Statistics by Condition
- [`make_calibration_by_condition_gg()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_by_condition_gg.md)
  : Plot Calibration Curves by Condition
- [`make_calibration_data()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_data.md)
  : Compute Calibration Statistics for Eyewitness Identification
- [`make_calibration_gg()`](https://cgtza2.github.io/r4lineups/reference/make_calibration_gg.md)
  : Plot Calibration Curve
- [`plot_anri_comparison()`](https://cgtza2.github.io/r4lineups/reference/plot_anri_comparison.md)
  : Plot ANRI Comparison Between Groups
- [`plot_anri_difference_distribution()`](https://cgtza2.github.io/r4lineups/reference/plot_anri_difference_distribution.md)
  : Plot Bootstrap Distribution of ANRI Difference
- [`print(`*`<lineup_anri>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_anri.md)
  : Print Method for ANRI Objects
- [`print(`*`<lineup_anri_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_anri_comparison.md)
  : Print Method for ANRI Comparison Objects
- [`print(`*`<lineup_calibration>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_calibration.md)
  : Print Method for lineup_calibration Objects

## Bayesian curves, utility, and BREE

Prior-posterior curves, expected utility analysis, and base-rate
effect-equivalency.

- [`compare_utility()`](https://cgtza2.github.io/r4lineups/reference/compare_utility.md)
  : Compare Utilities for Two Procedures
- [`make_bayes_curves()`](https://cgtza2.github.io/r4lineups/reference/make_bayes_curves.md)
  : Compute Bayesian Prior-Posterior and Information Gain Curves
- [`make_bree_curve()`](https://cgtza2.github.io/r4lineups/reference/make_bree_curve.md)
  : Compute Base-Rate Effect-Equivalency (BREE) Curves
- [`make_utility_curves()`](https://cgtza2.github.io/r4lineups/reference/make_utility_curves.md)
  : Compute Expected Utility Curves for Lineup Identification
- [`make_utility_difference()`](https://cgtza2.github.io/r4lineups/reference/make_utility_difference.md)
  : Compute Utility Difference Curves Comparing Two Procedures
- [`plot_bayes_information_gain()`](https://cgtza2.github.io/r4lineups/reference/plot_bayes_information_gain.md)
  : Plot Information Gain Curves
- [`plot_bayes_prior_posterior()`](https://cgtza2.github.io/r4lineups/reference/plot_bayes_prior_posterior.md)
  : Plot Prior-Posterior Curves
- [`plot_bree()`](https://cgtza2.github.io/r4lineups/reference/plot_bree.md)
  : Plot BREE Curve
- [`plot_utility_curves()`](https://cgtza2.github.io/r4lineups/reference/plot_utility_curves.md)
  : Plot Expected Utility Curves
- [`plot_utility_difference()`](https://cgtza2.github.io/r4lineups/reference/plot_utility_difference.md)
  : Plot Utility Difference Curve
- [`print(`*`<lineup_bayes_curves>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_bayes_curves.md)
  : Print Method for Bayes Curves Objects
- [`print(`*`<lineup_utility_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_utility_comparison.md)
  : Print Method for lineup_utility_comparison Objects

## Winter 2-HT model

Two-high-threshold multinomial processing tree model (Winter et al.,
2022).

- [`boot_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/boot_winter_2ht.md)
  : Bootstrap Confidence Intervals for 2-HT Model Parameters
- [`fit_winter_2ht()`](https://cgtza2.github.io/r4lineups/reference/fit_winter_2ht.md)
  : Winter et al. (2022) Two-High Threshold (2-HT) MPT Model for
  Eyewitness Identification
- [`plot_2ht_fit()`](https://cgtza2.github.io/r4lineups/reference/plot_2ht_fit.md)
  : Plot Observed vs. Expected Counts
- [`plot_2ht_parameters()`](https://cgtza2.github.io/r4lineups/reference/plot_2ht_parameters.md)
  : Plot Parameter Estimates from 2-HT Model
- [`plot(`*`<winter_2ht>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.winter_2ht.md)
  : Plot Method for winter_2ht Objects
- [`plot(`*`<winter_2ht_boot>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.winter_2ht_boot.md)
  : Plot Bootstrap Distributions
- [`print(`*`<winter_2ht>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.winter_2ht.md)
  : Print Method for winter_2ht Objects
- [`print(`*`<winter_2ht_boot>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.winter_2ht_boot.md)
  : Print Method for winter_2ht_boot Objects
- [`summary(`*`<winter_2ht>`*`)`](https://cgtza2.github.io/r4lineups/reference/summary.winter_2ht.md)
  : Summary Method for winter_2ht Objects
- [`summary(`*`<winter_2ht_boot>`*`)`](https://cgtza2.github.io/r4lineups/reference/summary.winter_2ht_boot.md)
  : Summary Method for winter_2ht_boot Objects

## mSDT and MAX SDT models

Multi-item signal detection theory and compound-decision MAX SDT
fitting.

- [`compare_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/compare_max_sdt.md)
  : Compare Two Nested MAX SDT Models
- [`dmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  [`pmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  [`qmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  [`rmax_filler()`](https://cgtza2.github.io/r4lineups/reference/dmax_filler.md)
  : mSDT: Max Filler Distribution (IID Normal Fillers)
- [`estimate_msdt_params()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
  [`msdt_gamma_from_rej()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
  [`msdt_dprime_from_rej()`](https://cgtza2.github.io/r4lineups/reference/estimate_msdt_params.md)
  : mSDT Parameter Estimation from Rejection Rates
- [`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
  : Restricted Independent-Observations/MAX Model for Lineup Counts
- [`max_filler_moments()`](https://cgtza2.github.io/r4lineups/reference/max_filler_moments.md)
  : Max Filler Distribution Moments
- [`plot_msdt_joint()`](https://cgtza2.github.io/r4lineups/reference/plot_msdt_joint.md)
  : Plot mSDT Joint Distributions

## Wixted lineup-memory models (optional pyWitness engine)

Fit and compare Independent Observations, Ensemble, and Integration
models for fair simultaneous lineups.

- [`check_pywitness_deps()`](https://cgtza2.github.io/r4lineups/reference/check_pywitness_deps.md)
  : Check Availability of the pyWitness Engine
- [`fit_lineup_models()`](https://cgtza2.github.io/r4lineups/reference/fit_lineup_models.md)
  : Fit Competing Wixted Lineup-Memory Models
- [`install_pywitness()`](https://cgtza2.github.io/r4lineups/reference/install_pywitness.md)
  : Install the Audited pyWitness Engine
- [`plot(`*`<lineup_model_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.lineup_model_comparison.md)
  : Plot a Wixted Lineup-Memory Model Comparison

## SDT estimation and comparison

SDT parameters from ROC data, GLM/GLMM estimation, and summary-count
comparisons.

- [`compare_sdt_summary()`](https://cgtza2.github.io/r4lineups/reference/compare_sdt_summary.md)
  : Compare SDT Summary Metrics Between Two Conditions
- [`extract_sdt_metrics()`](https://cgtza2.github.io/r4lineups/reference/extract_sdt_metrics.md)
  : Extract SDT Metrics from a GLM/GLMM
- [`fit_sdt_glm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glm.md)
  : Fit an SDT GLM for Old/New Recognition
- [`fit_sdt_glmm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md)
  : Fit an SDT GLMM for Old/New Recognition
- [`fit_sdt_roc()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_roc.md)
  : Fit Signal Detection Theory Model to ROC Data
- [`plot(`*`<sdt_roc_fit>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.sdt_roc_fit.md)
  : Plot Method for SDT ROC Fit
- [`print(`*`<sdt_roc_fit>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.sdt_roc_fit.md)
  : Print Method for SDT ROC Fit
- [`sdt_compare()`](https://cgtza2.github.io/r4lineups/reference/sdt_compare.md)
  : Bayesian Comparison of SDT Sensitivity (d') Between Two Conditions
- [`sdt_summary_from_counts()`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_from_counts.md)
  : SDT Summary Statistics from Counts
- [`sdt_summary_variance()`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_variance.md)
  : Variance Estimates for SDT Summary Metrics
- [`summary(`*`<sdt_roc_fit>`*`)`](https://cgtza2.github.io/r4lineups/reference/summary.sdt_roc_fit.md)
  : Summary Method for SDT ROC Fit

## Model comparison

Unified comparison of 2-HT, EIG, and full ROC models.

- [`compare_models()`](https://cgtza2.github.io/r4lineups/reference/compare_models.md)
  : Compare Multiple Models for Lineup Identification Data
- [`format_comparison_table()`](https://cgtza2.github.io/r4lineups/reference/format_comparison_table.md)
  : Create Comprehensive Model Comparison Table
- [`plot(`*`<model_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.model_comparison.md)
  : Plot Side-by-Side Model Comparisons
- [`print(`*`<model_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.model_comparison.md)
  : Print Method for model_comparison Objects
- [`summary(`*`<model_comparison>`*`)`](https://cgtza2.github.io/r4lineups/reference/summary.model_comparison.md)
  : Summary Method for model_comparison Objects

## Face similarity (Python/deepface)

Deep-learning face similarity, embeddings, and landmarks (requires
optional Python setup).

- [`available_detectors()`](https://cgtza2.github.io/r4lineups/reference/available_detectors.md)
  : List Available Face Detectors
- [`available_models()`](https://cgtza2.github.io/r4lineups/reference/available_models.md)
  : List Available Face Recognition Models
- [`batch_embeddings()`](https://cgtza2.github.io/r4lineups/reference/batch_embeddings.md)
  : Extract Embeddings for Multiple Images
- [`check_python_deps()`](https://cgtza2.github.io/r4lineups/reference/check_python_deps.md)
  : Check Python Dependencies
- [`cosine_to_similarity()`](https://cgtza2.github.io/r4lineups/reference/cosine_to_similarity.md)
  : Convert Cosine Distance to Similarity Score
- [`detect_faces()`](https://cgtza2.github.io/r4lineups/reference/detect_faces.md)
  : Detect Faces in an Image
- [`display_lineup()`](https://cgtza2.github.io/r4lineups/reference/display_lineup.md)
  : Display Lineup as Image Grid
- [`embedding_distance()`](https://cgtza2.github.io/r4lineups/reference/embedding_distance.md)
  : Compute Distance Between Two Embeddings
- [`face_similarity()`](https://cgtza2.github.io/r4lineups/reference/face_similarity.md)
  : Compute Face Similarity Between Two Images
- [`get_embedding()`](https://cgtza2.github.io/r4lineups/reference/get_embedding.md)
  : Extract Face Embedding from an Image
- [`get_face_landmarks()`](https://cgtza2.github.io/r4lineups/reference/get_face_landmarks.md)
  : Get 468-Point Face Mesh Landmarks
- [`get_key_landmarks()`](https://cgtza2.github.io/r4lineups/reference/get_key_landmarks.md)
  : Get Key Facial Landmarks (5-Point)
- [`has_valid_face()`](https://cgtza2.github.io/r4lineups/reference/has_valid_face.md)
  : Check if Image Contains a Valid Face
- [`install_r4lineups_python()`](https://cgtza2.github.io/r4lineups/reference/install_r4lineups_python.md)
  : Install Python Dependencies for r4lineups
- [`lineup_similarity()`](https://cgtza2.github.io/r4lineups/reference/lineup_similarity.md)
  : Compute Lineup Similarity Matrix
- [`plot_embedding_space()`](https://cgtza2.github.io/r4lineups/reference/plot_embedding_space.md)
  : Plot Embedding Space
- [`plot_lineup_bias_distribution()`](https://cgtza2.github.io/r4lineups/reference/plot_lineup_bias_distribution.md)
  : Plot Bootstrap Distribution of Lineup Bias
- [`plot_lineup_similarity()`](https://cgtza2.github.io/r4lineups/reference/plot_lineup_similarity.md)
  : Plot Lineup Similarity Results
- [`plot_similarity_distribution()`](https://cgtza2.github.io/r4lineups/reference/plot_similarity_distribution.md)
  : Plot Similarity Distribution
- [`print(`*`<lineup_similarity>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.lineup_similarity.md)
  : Print Method for lineup_similarity
- [`summary(`*`<lineup_similarity>`*`)`](https://cgtza2.github.io/r4lineups/reference/summary.lineup_similarity.md)
  : Summary Method for lineup_similarity

## Shiny app

Interactive Shiny interface.

- [`run_r4lineups_app()`](https://cgtza2.github.io/r4lineups/reference/run_r4lineups_app.md)
  : Run the r4lineups Shiny app

## Other helpers

Additional helper functions.

- [`eff_size_per_foils()`](https://cgtza2.github.io/r4lineups/reference/eff_size_per_foils.md)
  : Effective Size per Foils
- [`effsize_compare()`](https://cgtza2.github.io/r4lineups/reference/effsize_compare.md)
  : Master Function: Comparing Effective Size
- [`gen_esize_m()`](https://cgtza2.github.io/r4lineups/reference/gen_esize_m.md)
  : Effective Size (across a dataframe)
- [`gen_esize_m_ci()`](https://cgtza2.github.io/r4lineups/reference/gen_esize_m_ci.md)
  : Bootstrap Quantile for Effective Size
- [`gen_lineup_prop()`](https://cgtza2.github.io/r4lineups/reference/gen_lineup_prop.md)
  : Lineup proportion over dataframe
- [`gen_linevec()`](https://cgtza2.github.io/r4lineups/reference/gen_linevec.md)
  : Lineup vector
- [`lineup_bias_boot_dist()`](https://cgtza2.github.io/r4lineups/reference/lineup_bias_boot_dist.md)
  : Bootstrap Distribution of Lineup Bias (Target Proportion)
- [`lineup_pairwise_matrix()`](https://cgtza2.github.io/r4lineups/reference/lineup_pairwise_matrix.md)
  : Compute Pairwise Similarity Matrix for All Lineup Members
- [`plot_effective_size_conf()`](https://cgtza2.github.io/r4lineups/reference/plot_effective_size_conf.md)
  : Plot Effective Size by Confidence Level
- [`plot_error_rate_conf()`](https://cgtza2.github.io/r4lineups/reference/plot_error_rate_conf.md)
  : Plot Error Rate by Confidence Level
- [`plot_esize_distribution()`](https://cgtza2.github.io/r4lineups/reference/plot_esize_distribution.md)
  : Plot Bootstrap Distribution of Effective Size
- [`plot(`*`<esize_T_bayes>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.esize_T_bayes.md)
  : Plot method for esize_T_bayes objects
- [`plot(`*`<esize_T_bayes_compare>`*`)`](https://cgtza2.github.io/r4lineups/reference/plot.esize_T_bayes_compare.md)
  : Plot method for esize_T_bayes_compare objects
- [`print(`*`<esize_T_bayes>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.esize_T_bayes.md)
  : Print method for esize_T_bayes objects
- [`print(`*`<esize_T_bayes_compare>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.esize_T_bayes_compare.md)
  : Print method for esize_T_bayes_compare objects
- [`print(`*`<simulated_lineup_data>`*`)`](https://cgtza2.github.io/r4lineups/reference/print.simulated_lineup_data.md)
  : Print Method for Simulated Lineup Data
- [`rep_index()`](https://cgtza2.github.io/r4lineups/reference/rep_index.md)
  : Rep index
