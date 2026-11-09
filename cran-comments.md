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

It also adds tests that reproduce published values from the primary sources,
stricter input checks, and corrections to the vignettes and help pages. See
NEWS.md for the full list.

## Test environments

* local macOS 27.2 (arm64), R 4.6.1
* win-builder, R-devel (2026-11-07 r90850 ucrt)
* win-builder, R-release (4.6.2)
* GitHub Actions: macOS and Windows (R release), Ubuntu (R devel, release and
  oldrel-1), all passing with the full test suite (`NOT_CRAN=true`)

## R CMD check results

0 errors | 0 warnings | 0 notes

Local `R CMD check --as-cran`, with the CRAN incoming feasibility checks, and
win-builder R-devel and R-release all report Status OK.

## Check time

The 2.3.0 resubmission reduced check time below the 10-minute limit by
skipping slow tests on CRAN and precomputing the slowest vignette chunks. For
2.3.1, the new tests that take more than 0.25 s also call `skip_on_cran()`;
they run locally and on GitHub Actions. On win-builder R-devel the submitted
revision checks in 534 s: tests 58 s, re-building the vignettes 181 s and
examples 72 s. Win-builder was slower on the day of this check than
on 1 November, when 2.3.0 checked in 400 s; locally, the tests and vignettes
take as long as they did for 2.3.0.

## Reverse dependencies

There are no reverse dependencies on CRAN.

## Notes for CRAN reviewers

* The face-similarity functions require an optional Python environment
  (deepface via reticulate). Their examples are wrapped in \dontrun{} and the
  corresponding vignette does not evaluate its chunks, so no Python is needed
  to build or check the package.
* The lineup-memory fitting interface also uses an optional, explicitly
  installed Python environment containing a pinned pyWitness revision. Package
  loading and ordinary CRAN checks neither initialize Python nor install that
  environment.
* The Shiny application in inst/shiny is launched only on request; shiny and
  bslib are suggested, not imported.
