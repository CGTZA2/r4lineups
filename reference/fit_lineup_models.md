# Fit Competing Wixted Lineup-Memory Models

Fits confidence-binned Independent Observations, Ensemble/BEST-Rest, and
Integration models through an explicitly installed pyWitness engine.

## Usage

``` r
fit_lineup_models(
  data,
  models = c("independent", "ensemble", "integration"),
  lineup_size = 6L,
  confidence_bins = NULL,
  variance = c("equal", "unequal"),
  shared_variance = c("estimated", "zero"),
  starts = NULL,
  control = NULL,
  envname = .pywitness_default_env
)
```

## Arguments

- data:

  A data frame with `target_present`, `identification`, and numeric
  `confidence` columns.

- models:

  Character vector containing `"independent"`, `"ensemble"`,
  `"integration"`, and optionally `"best_rest"`. `"max"` aliases
  `"independent"`.

- lineup_size:

  Integer lineup size.

- confidence_bins:

  Optional strictly increasing numeric bin edges. When `NULL`, each
  observed confidence value is an ordered bin.

- variance:

  Either `"equal"` or `"unequal"`.

- shared_variance:

  Either `"estimated"` or `"zero"`. Shared variance cancels from
  Ensemble and BEST-Rest and is fixed at zero there.

- starts:

  Optional named numeric starting vector applied to all models, or a
  named list of such vectors indexed by model.

- control:

  Named list controlling `maxiter`, optimizer `method`, numerical
  `integration_sigma`, `chi2_variance`, and `verbose`.

- envname:

  Name or path of the isolated pyWitness virtual environment.

## Value

An R-native `"lineup_model_comparison"` object containing comparison
statistics, parameters, observed and expected cells, diagnostics,
specifications, and engine metadata. It contains no live Python objects.

## Details

This interface is restricted to fair simultaneous lineups. Explicit
target-absent suspect IDs are combined with other target-absent choices
because all innocent members share one lure distribution. The upstream
processor requires at least one observation in each aggregate TP/TA
response category; zero cells within particular confidence bins remain
supported. If the input includes a `condition` or `procedure` column, it
must contain only one non-missing value; otherwise the data must be
subset before fitting.

The audited pyWitness revision estimates parameters by minimizing
Pearson chi-squared. Thus, `loglik_at_estimate` is a multinomial
likelihood evaluated at the minimum-Pearson estimate, not a maximized
likelihood. AIC and BIC are returned as `NA`; computing them from a
non-likelihood optimum would be invalid.

BEST-Rest and Ensemble obey \$\$DV\_{ensemble} = (k -
1)DV\_{best-rest}/k.\$\$ They fit identically after rescaling criteria
and must not be counted as independent model-selection evidence.
Integration is included as a historically important comparator, not a
recommended default.

## References

Wixted, J. T., Vul, E., Mickes, L., & Wilson, B. M. (2018). Models of
lineup memory. *Cognitive Psychology, 105*, 81–114.
[doi:10.1016/j.cogpsych.2018.06.001](https://doi.org/10.1016/j.cogpsych.2018.06.001)

Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
pyWitness 1.0: A Python eyewitness identification analysis toolkit.
*Behavior Research Methods, 56*, 1533–1550.

## Examples

``` r
if (FALSE) { # \dontrun{
install_pywitness()
fits <- fit_lineup_models(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100)
)
print(fits)
plot(fits, type = "fit")
} # }
```
