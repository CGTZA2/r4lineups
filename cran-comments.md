# cran-comments

## Release summary

r4lineups 2.0.0 is a major update from the 0.1.x series. It expands the
package from a lineup-fairness toolkit into a full analysis suite for
eyewitness identification research: ROC/CAC/RAC analysis, full ROC
(Smith & Yang, 2020), expected information gain, PPV ranges, calibration and
ANRI, expected-utility analysis, the Winter et al. (2022) 2-HT model,
multi-item signal detection (mSDT/MAX-SDT), Bayesian beta-binomial inference
for the core fairness measures, model comparison, pAUC comparison, data
simulation and power analysis, and optional deep-learning face similarity via
reticulate/Python. See NEWS.md for the full changelog, including fixes from a
component-by-component statistical audit.

## Test environments

* local macOS (Darwin 25.5), R release
* GitHub Actions: ubuntu-latest (R devel, release, oldrel-1),
  macos-latest (release), windows-latest (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for CRAN reviewers

* The face-similarity functions require an optional Python environment
  (deepface via reticulate). Their examples are wrapped in \dontrun{} and the
  corresponding vignette does not evaluate its chunks, so no Python is needed
  to build or check the package.
* Longer-running bootstrap/simulation examples are wrapped in \donttest{}.
* This is a resubmission of a package previously on CRAN as r4lineups 0.1.x.

## Downstream dependencies

There are no reverse dependencies on CRAN.
