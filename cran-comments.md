# cran-comments

## Release summary

r4lineups 2.1.0 is a major update from the 0.1.x series. It expands the
package from a lineup-fairness toolkit into a full analysis suite for
eyewitness identification research: ROC/CAC/RAC analysis, full ROC
(Smith & Yang, 2020), expected information gain, PPV ranges, calibration and
ANRI, expected-utility analysis, the Winter et al. (2022) 2-HT model,
multi-item signal detection (mSDT and a restricted aggregate MAX fit), Bayesian beta-binomial inference
for the core fairness measures, model comparison, pAUC comparison, data
simulation and power analysis, optional deep-learning face similarity via
reticulate/Python, and an optional interface to a pinned pyWitness revision for
fitting Independent Observations, Ensemble, and Integration lineup-memory
models. See NEWS.md for the full changelog, including fixes from a
component-by-component statistical audit.

## Test environments

* local macOS Tahoe 26.6 (Darwin 25.6.0, arm64), R 4.6.1
* GitHub Actions macOS, R release
* GitHub Actions Windows, R release
* GitHub Actions Ubuntu, R devel, release, and oldrel-1

All jobs passed on the final `main` revision in GitHub Actions run
`31405897556`. The TeX-equipped job in that run also built the PDF reference
manual successfully. The separate pinned-pyWitness parity suite passed on
Linux, macOS, and Windows in run `31405924004`.

## R CMD check results

Local source-tarball check (`R CMD check --as-cran --no-manual`):

0 errors | 0 warnings | 0 notes

The built tarball's tests, ordinary examples, `\donttest{}` examples, package
installation, and all vignettes completed successfully. A PDF manual was not
built locally because a TeX installation is unavailable; it was built
successfully by the TeX-equipped GitHub Actions job described above.

The final network-enabled run completed CRAN incoming feasibility checks. A
separate URL/DOI audit returned zero failures.

The checked archive is `r4lineups_2.1.0.tar.gz` (2,155,732 bytes), SHA-256
`b83bec26b8ab0aaca3180b874b76ee2e1aedc7f5b5ce2b040492d2194ac7689d`.

## Notes for CRAN reviewers

* The face-similarity functions require an optional Python environment
  (deepface via reticulate). Their examples are wrapped in \dontrun{} and the
  corresponding vignette does not evaluate its chunks, so no Python is needed
  to build or check the package.
* The lineup-memory fitting interface also uses an optional, explicitly
  installed Python environment containing a pinned pyWitness revision. Package
  loading and ordinary CRAN checks neither initialize Python nor install that
  environment. Its live parity tests run only in a separate opt-in workflow.
* Evaluated vignette bootstrap/simulation settings are deliberately modest for
  check-time reliability; publication-grade settings are shown in unevaluated
  examples.
* This is a resubmission of a package previously on CRAN as r4lineups 0.1.x.
