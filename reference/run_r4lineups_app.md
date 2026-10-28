# Run the r4lineups Shiny app

Launches the bundled Shiny app. It covers lineup fairness from
mock-witness choices (bias, effective and functional size,
diagnosticity), Bayesian posteriors, Lee and Penrod's Multi-d' model,
ROC/CAC/full ROC with pAUC comparison, calibration and decision analyses
(ANRI, DPP, expected utility, Bayesian and BREE curves, RAC), expected
information gain and PPV ranges, and, when Python is available, face
similarity. Every result is computed by an exported r4lineups function.

## Usage

``` r
run_r4lineups_app()
```

## Value

None. Launches a Shiny app.

## Details

The app needs the suggested packages shiny, bslib and DT.

## Examples

``` r
if (FALSE) { # \dontrun{
run_r4lineups_app()
} # }
```
