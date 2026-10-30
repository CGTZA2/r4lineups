# cran-comments

## Release summary

r4lineups 2.3.0 is a major update from the 0.1.x series on CRAN. It expands
the package from a lineup-fairness toolkit into a full analysis suite for
eyewitness identification research: ROC/CAC/RAC analysis, full ROC
(Smith & Yang, 2020), expected information gain, PPV ranges, calibration and
ANRI, expected-utility analysis and deviation from perfect performance, the
Winter et al. (2022) 2-HT model, multi-item signal detection, Lee and
Penrod's (2019) Multi-d' model, Bayesian beta-binomial inference for the core
fairness measures, model and pAUC comparison, data simulation and power
analysis, a Shiny application, optional deep-learning face similarity via
reticulate/Python, and an optional interface to a pinned pyWitness revision
for fitting lineup-memory models. Versions 2.1.0 to 2.2.0 were never
submitted; 2.3.0 supersedes them and adds the corrections from two
independent statistical reviews. See NEWS.md for the full changelog.

## Test environments

* local macOS 27.0.1 (Darwin 27.0.0, arm64), R 4.6.1
* GitHub Actions macOS, R release
* GitHub Actions Windows, R release
* GitHub Actions Ubuntu, R devel, release, and oldrel-1

All five jobs passed on the release revision in GitHub Actions run
`40967586729`, and the TeX-equipped job in that run built the PDF reference
manual. The separate pinned-pyWitness parity suite passed on Linux, macOS, and
Windows in run `40962145807`.

## R CMD check results

Local source-tarball check (`R CMD check --as-cran --no-manual --run-donttest`),
with network access, including the CRAN incoming feasibility checks:

0 errors | 0 warnings | 0 notes

The tests, ordinary and `\donttest{}` examples, installation, and all 16
vignettes completed successfully. A PDF manual was not built locally because
a TeX installation is unavailable; it was built by the TeX-equipped GitHub
Actions job above.

The submitted archive is `r4lineups_2.3.0.tar.gz` (2,080,831 bytes), SHA-256
`b44dbc9badea79cb0b5508c135f35fced55abadbe3b88640fdfb7819cf1227ec`.

## purrr::rerun() notice

On 27 October 2026 the purrr maintainers warned that `rerun()` becomes defunct
in the next purrr release, and that r4lineups 0.1.1 on CRAN imports it. This
release no longer uses or imports `rerun()` (it was replaced by
`purrr::map()`).

## Notes for CRAN reviewers

* The face-similarity functions require an optional Python environment
  (deepface via reticulate). Their examples are wrapped in \dontrun{} and the
  corresponding vignette does not evaluate its chunks, so no Python is needed
  to build or check the package.
* The lineup-memory fitting interface also uses an optional, explicitly
  installed Python environment containing a pinned pyWitness revision. Package
  loading and ordinary CRAN checks neither initialize Python nor install that
  environment. Its live parity tests run only in a separate opt-in workflow.
* The Shiny application in inst/shiny is launched only on request; shiny and
  bslib are suggested, not imported.
* Evaluated vignette bootstrap/simulation settings are deliberately modest for
  check-time reliability; publication-grade settings are shown in unevaluated
  examples.
* This is a resubmission of a package previously on CRAN as r4lineups 0.1.x.
