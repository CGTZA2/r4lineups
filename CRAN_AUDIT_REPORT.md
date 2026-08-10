# r4lineups 2.1.0 CRAN Readiness and Mathematical Audit

The mathematical audit began on 2026-08-08 and the consolidated 2.1.0 release
was finalized on 2026-08-10. This report describes the source tree that
produced `r4lineups_2.1.0.tar.gz`; it is excluded from the package build so that
CRAN receives only user-facing package material.

## Release verdict

The package is CRAN-ready: the built source tarball installs, runs its tests and
both classes of examples, rebuilds every evaluated vignette, and passes
`R CMD check --as-cran --no-manual` locally with **0 errors, 0 warnings, and 0
notes**. GitHub Actions run `31405897556` also passed on the final `main`
revision for Linux R devel/release/oldrel-1, macOS release, and Windows release,
and produced the PDF reference manual on a TeX-equipped runner.

The audit covered 159 exports, 54 registered S3 methods, 96 implementation
files, 216 help topics, 15 vignettes, and 23 test files. The final tests contain
734 syntactic `expect_*()` calls and produce 815 runtime passes. Every export is
named in a behavioral contract test or an explicit optional-environment
exemption; S3 registrations are checked through their public generic workflows
and namespace/static checks.

## Reproducible local evidence

| Item | Result |
|---|---|
| Host | macOS Tahoe 26.6 (Darwin 25.6.0), arm64 |
| R | R 4.6.1 (2026-06-24), `aarch64-apple-darwin23` |
| Dependency setup | Imports and Suggests installed in isolated library `/private/tmp/r4lineups-Rlib` |
| Test suite | PASS: 815 expectations; four documented skips (one deliberately slow power simulation and three separately passing opt-in pyWitness tests) |
| Ordinary examples | PASS |
| `\\donttest{}` examples | PASS |
| Vignettes | PASS from source tarball |
| Namespace/Rd/static checks | PASS |
| URLs and DOIs | PASS (`urlchecker::url_check()` returned zero failures with live network access) |
| JOSS references | PASS: all 16 bibliography DOIs resolved through `doi.org`; Pandoc/citeproc rendered all citations without error |
| Spelling | Reviewed; genuine typographical errors corrected; remaining candidates are names, abbreviations, and domain/API terms |
| Final tarball | `r4lineups_2.1.0.tar.gz`; 2,155,732 bytes; SHA-256 `b83bec26b8ab0aaca3180b874b76ee2e1aedc7f5b5ce2b040492d2194ac7689d` |
| Check | `R CMD check --as-cran --no-manual`: 0 errors, 0 warnings, 0 notes |
| PDF manual | PASS on TeX-equipped GitHub runner (run `31405897556`) |
| Cross-platform final revision | PASS: Linux devel/release/oldrel-1, macOS release, and Windows release (run `31405897556`) |

The final check ran with live network access and completed CRAN incoming
feasibility checks. A separate live `urlchecker` run resolved all declared URLs
and DOIs with zero failures.

The local opt-in suite also passed against pyWitness revision
`e726dcfc09423d0e0ff7f46c8e3a711040293eba`, including direct-engine parity,
parameter recovery, equal- and unequal-variance fits, zero and estimated shared
variance, sparse cells, BEST-Rest/Ensemble rescaling, probability partitions,
and an intentional non-convergence path.

The same pinned-pyWitness opt-in suite passed on Linux, macOS, and Windows in
GitHub Actions run `31405924004` against final `main` commit
`fbf30bf6a2bb14f054ba4b034bf71edd93ff08bb`.

RNG-taking functions were tested for reproducibility and for restoration of the
caller's `.Random.seed`. Vignette bootstrap and simulation counts were reduced
only for evaluated demonstrations; larger research settings remain in
unevaluated examples. No check-time path initializes Python, downloads a model,
opens a GUI, starts Shiny, installs software, or accesses the network.

## Mathematical traceability matrix

“Hand test” below means a benchmark whose expected value is specified directly
from a small count table, probability identity, or independent numerical
identity rather than by calling the implementation under test. The principal
regressions are in `tests/testthat/test-cran-audit-regressions.R`; broader API
contracts are in `tests/testthat/test-export-contracts.R` and the specialist
test files named below.

| Estimand/family | Authoritative definition used | Implementation and independent benchmark | Status |
|---|---|---|---|
| Lineup member proportions and foil bias | Counts for a named position divided by all mock-witness choices | `lineup_prop_*()`, `allprop()`, `allfoilbias()`; sparse labelled table and missing-member hand tests | Source-defined; corrected and verified |
| Malpass effective size | Malpass (1981): deviation of foil frequencies from equal selection, excluding a null foil; Tredoux (1998) adjustment uses declared lineup size | `esize_m()`/bootstrap; complete four-member count-table hand calculation, raw/table invariance | Primary-source verified |
| Tredoux effective size | Tredoux (1998), E' = 1/(1-I), I = 1-sum(p_i^2) | `esize_T()`, `i_esize_T()`, CI/bootstrap; exact uniform and concentrated-distribution identities, witness-level bootstrap | Primary-source verified |
| Functional size | lineup size divided by suspect-identification count | `func_size()` and report/bootstrap; count identity, zero-target and invalid-position boundaries | Definition verified; zero gives documented infinity |
| Diagnosticity and homogeneity | Wells-Lindsay relative risk; Tredoux (1998) Eq. 12 correction and Eqs. 15-18 uncorrected log-risk homogeneity | `diag_ratio_*()`, `diag_param()`, `var_lnd()`, `d_weights()`, `homog_diag*()`; two-table log-risk variance and df=k-1 hand tests | Primary-source verified; corrected |
| ROC/CAC/RAC | Suspect-ID hit rate and innocent-suspect false-ID rate; RAC/CAC accuracy CID/(CID+FID/m) when no designated innocent suspect | `make_rocdata()`, `make_cacdata()`, `make_racdata()`; designated-suspect and filler-estimated count tables | RAC checked against Seale-Carlisle et al. (2019); corrected |
| Partial AUC | Trapezoidal area up to a fixed false-ID cutoff | `compare_pauc()`; unsorted three-point curve with independently calculated interpolated triangle; stratified raw-data bootstrap | Numerically verified |
| Full ROC | Smith, Yang, and Wells method: all response categories; no-designated TA filler split 1/m suspect and (m-1)/m filler | `make_fullroc_data()`; probability-sum and count-split tests in `test-fullroc.R` | Primary-source and implementation-article verified; empirical diagnosticity ordering is explicitly labelled optimistic |
| Calibration and ANRI | C, O/U and NRI definitions; Yaniv-Yates-Smith ANRI=(N*NRI-J+1)/(N-J+1) | `make_calibration_data()`, `compute_anri()`; perfect-calibration/bounds and formula substitution tests | Primary formula verified; filler-derived fractional-count use labelled approximate |
| EIG | Starns, Cohen, and Rotello (2023): posterior entropy reduction averaged over responses | `make_eig_data()`, `compute_eig()`; response probabilities sum to one, Bayes posterior and entropy identities | Primary-source verified; citation corrected |
| PPV and innocent-ID ranges | Bayes PPV with nominal/effective/uncorrected innocent-ID alternatives | `make_ppv_range()` family; designated-suspect equality and no-designated correction tests | Fitzgerald source in repository; effective-size variants explicitly approximate |
| Bayesian curves and BREE | Bayes theorem over guilty/innocent response likelihoods; base-rate equivalency | `make_bayes_curves()`/`make_bree_curve()`; posterior bounds, endpoint, likelihood-sum, and response-split tests | Algebraically verified |
| Expected utility | TP/FN/FP/TN expected payoff over confidence criteria | `make_utility_curves()` family; all-IDs endpoint and direct-vs-estimated false-alarm tests | Algebraically verified; false-alarm interpretation documented |
| DPP | Geometric deviation of an empirical ROC from perfect performance | `make_dpp()`/comparison; perfect and diagonal curve identities and bounds | Source checked from repository article; verified |
| Winter 2-HT | Winter et al. (2022) six multinomial cell probabilities | `fit_winter_2ht()`; each TP/TA probability triplet sums to one, known-count fit, bounds/convergence/bootstrap checks | Source-verified; single-condition aggregate fit documented as saturated |
| SDT GLM/GLMM | Probit coefficients map to d' and criterion; logit coefficients map to log odds ratio | `fit_sdt_glm()`/`fit_sdt_glmm()`/extractor; simulated coefficient identities and binary-input rejection | Formula verified; GLMM is optional-`lme4` path |
| Summary SDT | Corrected hit/false-alarm transforms; Miller/Gourevitch/bootstrap variance | `sdt_summary_*()` and comparison; 2x2 count transform, variance, symmetry, and covariance benchmarks | Repository sources verified; corrected |
| z-ROC | z(H)=intercept+slope*z(FA); unequal-variance d_a=sqrt(2)*intercept/sqrt(1+slope^2) | `fit_sdt_roc()`; direct coefficient identity, raw stratified bootstrap and extreme-rate boundaries | Corrected and algebraically verified |
| mSDT rejection estimator | IID normal maximum-filler distribution; two rejection moments identify gamma and d' | `d/p/q/rmax_filler()`, moments, `estimate_msdt_params()`; density integration, CDF/quantile inversion, probability bounds and simulated recovery | Yang et al. source verified; method-of-moments limitation documented |
| Restricted MAX-SDT fitter | Equal-variance Independent-Observations/MAX probabilities over aggregate suspect/filler/reject outcomes with one criterion | `fit_max_sdt()`/comparison; mutually exclusive probability sums, boundary/convergence and recovery tests | Source and supplied-script review completed; estimator is explicitly minimum-Pearson, not likelihood, and is not presented as the full Wixted model |
| Confidence-based Wixted models | Wixted et al. (2018) Independent Observations, Ensemble/BEST-Rest, and Integration models as implemented by audited pyWitness revision `e726dcfc09423d0e0ff7f46c8e3a711040293eba` | `fit_lineup_models()`; direct-wrapper parity, probability partitions, BEST-Rest/Ensemble criterion rescaling, equal/unequal variance, zero/estimated shared variance, sparse cells, generating-model and parameter recovery | Reference-engine verified locally and on Linux, macOS, and Windows |
| Model comparison | Method-specific outputs; AIC/BIC only for compatible likelihoods on identical outcomes | `compare_models()`; contract tests prohibit a cross-estimand “best model” | Corrected; no invalid common ranking claim |
| Simulation and power | Equal-variance independent-signal Independent Observations/MAX, Ensemble, and Integration rules plus repeated-analysis power estimates | `simulate_lineup_data()`/`simulate_power_analysis()`; algebraic BEST-Rest/Ensemble rescaling, criterion, probability-partition, reproducibility, and recovery tests | Corrected and source-verified; BEST-Rest is documented as an equivalent Ensemble parameterization |
| Face embeddings/similarity | Euclidean distance; cosine distance=1-cos(theta); reported similarity=1-distance | `embedding_distance()`, `cosine_to_similarity()`, lineup helpers; identical/opposite/zero-vector identities and missing-file/dependency errors | Transformation verified; external neural models and their forensic validity are explicitly outside package validation |

## Confirmed corrections and severity

### Blockers resolved

* Source PDFs and a non-portable PDF filename were entering the source tarball;
  `pdfs/`, the raw manuscript workspace, and the canonical JOSS sources are now
  build-ignored. The final archive scan found no PDF, manuscript, correspondence,
  Python/cache, audit, or other development artifact.
* Confidence-method false-ID counts were double counted when designated
  innocent-suspect IDs were present. All affected analyses now choose exactly
  one documented estimation method.
* Full-ROC/EIG/Bayesian target-absent response probabilities did not implement
  the published filler split in no-designated-suspect data. Counts and
  probabilities are corrected and regression-tested.
* Several effective-size, diagnosticity variance/weight/df, z-ROC, SDT, and
  bootstrap calculations were inconsistent with their documented definitions.
  These are corrected with hand-calculation tests.

### Pre-submission corrections resolved

* Invalid/sparse/empty/non-finite inputs now fail clearly across legacy and new
  modules; bootstrap and optimizer failures are checked rather than silently
  propagated.
* Bootstrap counts and confidence levels are honored; seed-taking functions do
  not mutate caller RNG state.
* Return objects, print/summary/plot methods, documentation defaults, examples,
  and registered namespace methods were reconciled.
* The EIG authorship, full-ROC ordering caveat, PPV base-rate/effective-size
  approximation, utility interpretation, 2-HT saturation, and mSDT estimator
  type are documented.
* Vignettes now complete within the source-build budget on the audit host.
* Optional Python checks are isolated and non-installing.

### Non-blocking limitations retained

* `fit_max_sdt()` remains deliberately limited to an aggregate,
  single-criterion, equal-variance Independent-Observations/MAX fit. The full
  confidence-based workflow is provided separately by `fit_lineup_models()`
  through an optional pinned pyWitness engine.
* Neural embedding providers are external software/model artifacts; this
  package validates vector transformations and error handling, not model bias,
  accuracy, or forensic validity.
* Perfect-separation and boundary fits can be intrinsically non-regular. The
  package now reports/errors on convergence and boundary conditions where
  inference would otherwise be misleading; no universal finite-sample coverage
  claim is made.
* Full-ROC empirical diagnosticity ordering is sample-dependent and optimistic;
  a priori ordering should be used for confirmatory work.

## Sources used

The audit used the repository copies of Tredoux (1998), Malpass (1981), Wells
and Lindsay (1980), Yaniv, Yates, and Smith (1991), Seale-Carlisle et al.
(2019), Starns, Cohen, and Rotello (2023), Smith, Yang, and Wells (2020),
Wixted et al. (2018), Duncan (2006), Kaesler et al. (2020), Yang's 2023
full-ROC package article, Winter 2-HT, Yang et al. mSDT, Fitzgerald
PPV/error-rate, calibration, BREE, utility, DPP, SDT variance, Wright GLM, and
the supplied model correspondence/scripts. The article PDFs, correspondence,
and scripts are audit inputs only and are excluded from Git and the CRAN source
package.

## Final submission checklist

- [x] Built source tarball installs from an isolated library
- [x] 159 exports and 54 registered S3 methods accounted for
- [x] Tests, ordinary examples, and `\\donttest{}` examples pass
- [x] Evaluated vignettes rebuild from the tarball
- [x] Roxygen, Rd, and NAMESPACE regenerated together
- [x] No check-time network/Python/GUI/install action
- [x] MIT package and repository license metadata are present
- [x] JOSS sources and local article PDFs are excluded from the R source tarball
- [x] URL/DOI audit has zero failures
- [x] All 16 JOSS bibliography DOIs resolve and the citation processor completes
- [x] Local `--as-cran --no-manual`: 0 errors, 0 warnings, 0 notes
- [x] Exact checked tarball size and SHA-256 recorded
- [x] Local pinned-pyWitness direct parity and recovery suite passes
- [x] MIT-relicensing consent recorded for both authors: Colin Tredoux
  confirmed on 2026-08-10 that Tamsyn Naylor agreed on 2026-08-09
- [x] PDF manual build on a TeX-equipped runner
- [x] Final GitHub Actions matrix: Linux devel/release/oldrel-1, macOS release, Windows release
- [x] Three-platform opt-in pyWitness workflow
- [x] Official JOSS/Open Journals draft PDF build
- [x] Replace the pending CI statements in `cran-comments.md` with verified results
- [ ] Submit the exact checked tarball; do not rebuild after CI without rechecking
