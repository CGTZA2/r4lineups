# Contributing to r4lineups

`r4lineups` is developed as research software for eyewitness-identification and lineup-fairness analysis. Contributions that improve correctness, documentation, tests, examples, or support for published methods are welcome.

## Reporting Issues

Please use the project issue tracker to report bugs, unclear documentation, reproducibility problems, or feature requests. Include:

- the version of `r4lineups` you are using;
- a small reproducible example, if possible;
- the expected result and the observed result;
- any relevant output from `sessionInfo()`.

## Development Workflow

Before proposing a change, run the relevant tests locally:

```r
testthat::test_dir("tests/testthat")
```

For broader changes, also run:

```sh
R CMD build .
R CMD check --no-manual r4lineups_*.tar.gz
```

Changes that add or alter statistical methods should include tests that compare the implementation with a hand calculation, published example, simulation expectation, or another transparent reference.

## Documentation

Public functions should be documented with roxygen comments and should include examples that can run without network access or private files. Vignette examples that require large computation, local face images, or optional Python dependencies should be clearly marked as non-evaluated.

## Scope

The package aims to keep a stable public API for existing lineup-fairness and confidence-based analysis functions. Breaking interface changes should be avoided unless they correct a serious error or substantially improve consistency across the package.
