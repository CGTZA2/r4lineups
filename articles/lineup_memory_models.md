# Fitting Wixted Lineup-Memory Models

## Scope

[`fit_lineup_models()`](https://cgtza2.github.io/r4lineups/reference/fit_lineup_models.md)
fits the fair simultaneous-lineup models considered by Wixted et
al. (2018): Independent Observations, Ensemble, and Integration.
BEST-Rest is available for exact replication work, but it is a linear
rescaling of Ensemble and does not constitute independent
model-selection evidence.

The initial interface requires trial-level data with `target_present`,
`identification`, and ordered numeric `confidence`. Fit one procedure or
experimental condition per call. Designated-suspect distributions,
unfair lineups, sequential lineups, reaction-time models, and
hierarchical participant/item effects are outside this interface.

## Install the audited engine

pyWitness is optional and remains in an isolated virtual environment.
Neither package loading nor CRAN checks install Python software.

``` r

library(r4lineups)
install_pywitness()
check_pywitness_deps(initialize = TRUE)
```

The installer pins the audited upstream commit, and every fitted result
records that full revision and the imported engine version. The bridge
refuses an environment whose version does not match the audited
revision.

## Prepare one condition

``` r

condition_a <- subset(my_lineup_data, condition == "A")

str(condition_a[c("target_present", "identification", "confidence")])
```

`target_present` must be logical (or 0/1), `identification` must be
`"suspect"`, `"filler"`, or `"reject"`, and confidence must be finite
numeric data whose larger values mean greater confidence. Under the
supported fair-lineup design, target-absent suspect choices are pooled
with target-absent filler choices. The upstream processor requires at
least one observation in every aggregate TP/TA response category,
although cells within individual confidence bins may be zero.

## Fit the models

``` r

fits <- fit_lineup_models(
  condition_a,
  models = c("independent", "ensemble", "integration"),
  lineup_size = 6,
  confidence_bins = c(0, 60, 80, 100),
  variance = "equal",
  shared_variance = "estimated",
  control = list(maxiter = 5000L, method = "Nelder-Mead")
)

print(fits)
summary(fits)
```

For an Independent-Observations model with zero shared variance, use
`shared_variance = "zero"`; the bridge selects pyWitness’s dedicated
simple class for this exact limit. Shared variance cancels from Ensemble
and BEST-Rest and is fixed at zero for those models. Unequal signal
variance is requested with `variance = "unequal"`.

Model-specific starting values can be supplied as a named list:

``` r

fit_started <- fit_lineup_models(
  condition_a,
  models = c("independent", "ensemble"),
  starts = list(
    independent = c(targetMean = 1.5, c1 = 0.8, c2 = 1.4, c3 = 2.0),
    ensemble = c(targetMean = 1.5, c1 = 0.7, c2 = 1.2, c3 = 1.7)
  )
)
```

## Interpret the result

``` r

fits$comparison
fits$parameters
fits$observed
fits$expected
fits$diagnostics
fits$engine

plot(fits, type = "comparison")
plot(fits, type = "fit")
plot(fits, type = "roc")
```

The audited pyWitness engine estimates parameters by minimizing Pearson
chi-squared. `loglik_at_estimate` is the multinomial log likelihood
evaluated at that minimum-Pearson estimate, not a maximized likelihood.
Consequently AIC and BIC are `NA`; calculating them from this estimate
would be invalid. Inspect convergence and expected cells before
interpreting model differences.

For lineup size (k), the decision variables obey

``` math
DV_{Ensemble} = \frac{k-1}{k} DV_{BEST-Rest}.
```

Their criteria rescale by the same factor and their fits are identical.
Integration is included as a historically important comparator, not as a
recommended default. Wixted et al. (2018) found it generally inferior,
whereas Independent Observations and Ensemble both remained plausible,
with an overall advantage for Ensemble.

## References

Mickes, L., Seale-Carlisle, T. M., Chen, X., & Boogert, S. (2024).
pyWitness 1.0: A Python eyewitness identification analysis toolkit.
*Behavior Research Methods, 56*, 1533–1550.

Wixted, J. T., Vul, E., Mickes, L., & Wilson, B. M. (2018). Models of
lineup memory. *Cognitive Psychology, 105*, 81–114.
<https://doi.org/10.1016/j.cogpsych.2018.06.001>
