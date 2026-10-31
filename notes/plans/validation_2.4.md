# Plan: statistical validation for r4lineups 2.4

The independent statistical audit of 2.3.0 (`notes/statistical-qa/final_report.md`, 31 October 2026) found and repaired 71 reproducible defects. It also left a set of methods whose inferential behaviour has not yet been independently established. None of these is a known error. Colin decided (31 Oct 2026) to release 2.3.0 with the repairs and the limitations stated in the help and vignettes, and to work through the items below for 2.4. Each item names the evidence that would close it.

## 1. Multi-d′ (Lee & Penrod, 2019)

Obtain the original paper and reproduce its tables from `make_multid()`. The audit checked only the author supplement (14 checks). Then assess interval coverage by simulation for every contrast returned by `compare_multid()` (only GI has been checked, coverage .948), and for the confidence-accumulated indices.

## 2. z-ROC and diagnosticity inference

The audit checked two common-ratio regimes. Extend this to heterogeneous diagnosticity across lineups, to sparse cells (the sparse homogeneity scenario showed coverage that depends on positive-cell restrictions) and to unequal arm sizes, and report Q rejection rates and pooled-ratio coverage.

## 3. Calibration and ANRI designs

One two-bin fixed-design ANRI scenario has been checked (coverage .950). Cover more bins, other bin rules, sparse bins, the all-response calibration variant and estimated (filler-derived) designs. For Bayesian calibration, decide whether to add a fixed-arm likelihood, since the current conditional-binomial working model is stated as a limitation.

## 4. Winter 2-HT model

Run simulation studies at parameter boundaries (b = 0, dA near 0), under deliberate misspecification (data from a MAX model), for bootstrap percentile and BCa coverage, and for model comparisons across constrained conditions. The interior Hessian intervals have been checked (one tree, 10,000 fits).

## 5. pyWitness lineup-memory models

Derive and integrate the unequal-variance and correlated-filler predictions independently, rather than relying on bridge parity alone. Report the rejection-cell denominator issue to the pyWitness maintainers (Mickes and colleagues); a draft note is in `notes/statistical-qa/maintainer_response.md`. Revisit the per-fit adapter if upstream changes.

## 6. SDT GLM/GLMM

Random slopes and crossed random effects: compare the coefficients with independently integrated likelihoods beyond the single random-intercept case, and state when conditional coefficients differ from marginal d′.

## 7. Power analysis

Assess `simulate_power_analysis()` calibration beyond the pAUC test: other `test_function` choices, sparse designs, and the null rejection rate across sample sizes.

## 8. Effective size intervals

The plug-in E′ bootstrap cannot reach k, so it cannot test exact fairness. Investigate an interval designed for the boundary (for example inverting an exact multinomial test of uniformity, or a likelihood-ratio interval), and a Benjamini–Hochberg option for the k member intervals (see the future-work list in agents.md).

## 9. Optional components

Face similarity across models, detectors and thresholds; the dense MediaPipe mesh on a machine with GPU access; and the live browser app (Shinylive) version identity after each deployment.

## 10. EIG inference and a standardised pAUC

Implement 95% credible intervals for EIG and for the difference between two procedures, using a Bayesian multinomial (Dirichlet) model of the response proportions as in Tuttle, Starns and Cohen (2025, *Journal of Memory and Language*, 140, 104581), and check their calibration by simulation. Add a McClish (1989) standardised pAUC, on the .5–1 scale, as an option in `make_rocdata()` and `compare_pauc()`.

## 11. Process

Each study follows the audit's ADEMP layout (Morris, White & Crowther, 2019): an independent reference implementation, at least 5,000 replicates, MCSE of .003 or less for coverage, and all attempts and failures retained. Finished studies become regression tests where practical, and the limitation text in the help and vignettes is removed once the evidence supports it.
