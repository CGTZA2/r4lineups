# cran-comments

## r4lineups 2.3.1

This update follows 2.3.0 closely because it corrects results that were wrong,
found while checking the package against the primary sources:

* `make_bayes_curves()` computed entropy reduction while documenting and citing
  the Wells, Yang and Smalarz (2015) information gain (posterior minus prior).
  The cited measure is now the default; the entropy measure is an option.
* `make_fullroc(order = "apriori")` did not use the published a priori order
  (Smith, Yang & Wells, 2020; Yang & Smith, 2023).
* Calibration with `choosers_only = FALSE` scored correct rejections as errors.
* `homog_diag()` reported homogeneity from non-finite results, and
  `fit_max_sdt()` could return estimates outside its bounds.

## R CMD check results

0 errors | 0 warnings | 0 notes (local, macOS, R release).


## Resubmission

### Check time (second submission, 1 November 2026)

Uwe Ligges reported "Overall checktime 14 min > 10 min", mainly from tests
(297 s) and re-building of vignette outputs (328 s) on Windows. Both are
reduced:

* Tests. Tests that take more than about 0.3 s (two-condition MAX model fits,
  bootstrap variants, a power simulation) and the Shiny module tests now call
  `skip_on_cran()`, so they run only where `NOT_CRAN=true` (locally and on
  GitHub Actions, where all 1,334 expectations pass). The 1,188 expectations
  that run on CRAN take 16 s locally instead of about 70 s.
* Vignettes. The five slowest chunks (a power simulation, three bootstrap
  comparisons and a two-condition MAX fit with bootstrap) show their code and
  load results precomputed with that code from `vignettes/precomputed/`
  (39 KB). The rendered output is identical to a full computation. Rendering
  all 16 vignettes takes 26 s locally instead of 79 s.

On win-builder (R-devel, Windows) the resubmitted archive checks with Status
OK in 400 s: tests 62 s and vignette re-building 116 s.

### Pretest NOTEs (first submission, 31 October 2026)

The automatic pretest of the first 2.3.0 submission reported two NOTEs on
Windows and Debian. Both are fixed:

* "Possibly misspelled words in DESCRIPTION: embeddings". The word was correct,
  but the Description now says "numeric face representations" instead.
* Examples with elapsed time over 5 s (`fit_max_sdt`, `compare_max_sdt`,
  `compare_dpp`). The two-condition MAX model fits, which take several seconds
  each, are now in `\donttest{}`; the single-condition fit still runs. The
  `compare_dpp()` example uses 50 bootstrap resamples instead of 200.

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
submitted; 2.3.0 supersedes them and adds the corrections from three
independent reviews and an independent statistical audit. See NEWS.md for the
full changelog.

## Test environments

* local macOS 27.0.1 (Darwin 27.0.0, arm64), R 4.6.1
* GitHub Actions macOS, R release
* GitHub Actions Windows, R release
* GitHub Actions Ubuntu, R devel, release, and oldrel-1

All five jobs passed on the resubmitted revision in GitHub Actions run
`41420279002` on the submitted merge commit `94fcb14`, and the TeX-equipped job in that run built the PDF reference
manual. The opt-in pinned-pyWitness parity tests passed locally on macOS
(24 expectations).

## R CMD check results

Local source-tarball check (`R CMD check --as-cran`), with network access,
including the CRAN incoming feasibility checks and the PDF manual:

0 errors | 0 warnings | 0 notes

The tests, examples, installation, and all 16 vignettes completed
successfully.

The submitted archive is `r4lineups_2.3.0.tar.gz` (2,347,222 bytes), SHA-256
`441cabddae64f4841629659f75a45ed609d4eafb603436cc7e337049b65d1273`, built
from commit `94fcb14`.

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
* The license changes from CC0 (0.1.1) to MIT + file LICENSE. Both authors
  agreed to the change, and NEWS.md records it.
