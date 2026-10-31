# SDT Summary-Level Comparisons

> **Acknowledgement.** Large language models supplied by Anthropic and
> OpenAI were used in preparing this vignette, to assist with checking
> code, testing the veracity of the approaches described, and on
> occasion editing the text. Responsibility for the content rests with
> the authors.

## Overview

This vignette shows how to compare **conditions using only summary SDT
counts** (hits, false alarms, misses, correct rejections).

Use these tools when **trial-level data are unavailable** and you have
only summary counts per condition — the usual situation when working
from published tables, or when synthesising results across studies for a
meta-analysis.

### Where the methods come from

The variance estimators follow Gourevitch and Galanter (1967), Miller
(1996), and Suero, Privado and Botella (2017). The bootstrap option
follows the procedure used in the meta-analysis of Tredoux, Sporer,
Vredeveldt, Kempen and Nortje (2021), where summary counts extracted
from published studies had to be compared without access to the
underlying trial data — which is exactly the problem these functions
address.

### A caution about what d′ means for a lineup

These functions treat the data as a standard **yes/no detection**
problem: hits, misses, false alarms and correct rejections, from which
d′ follows as $`z(\text{hit rate}) - z(\text{false-alarm rate})`$.

A lineup is not that. A witness viewing a lineup performs a **compound**
task — deciding whether the culprit is present at all, *and* which
member it is. Duncan (2006) set this out formally as a compound
signal-detection model, in which the lineup is a 1-of-*m* detection task
combined with an *m*-alternative forced-choice identification task, with
a single set of latent memory parameters driving both. Estimating d′
from collapsed 2 × 2 counts ignores that structure, and in particular it
discards which lineup member was chosen.

That does not make the summary-level comparison useless — it is often
the only thing available, and it is what published tables report — but
the d′ it yields is a yes/no analogue and should be labelled as such
rather than presented as the lineup’s discriminability. Where
trial-level data exist, prefer the lineup models in
[`vignette("lineup_memory_models")`](https://cgtza2.github.io/r4lineups/articles/lineup_memory_models.md)
or
[`vignette("model_comparison")`](https://cgtza2.github.io/r4lineups/articles/model_comparison.md),
which respect the compound structure.

## Summary metrics from counts

``` r

library(r4lineups)

# Condition A counts
hits_a <- 34
fas_a <- 6
misses_a <- 16
cr_a <- 44

summ_a <- sdt_summary_from_counts(hits_a, fas_a, misses_a, cr_a)
summ_a
#> $hits
#> [1] 34
#> 
#> $fas
#> [1] 6
#> 
#> $misses
#> [1] 16
#> 
#> $cr
#> [1] 44
#> 
#> $ns
#> [1] 50
#> 
#> $nn
#> [1] 50
#> 
#> $hit_rate
#> [1] 0.6764706
#> 
#> $fa_rate
#> [1] 0.127451
#> 
#> $zH
#> [1] 0.4578519
#> 
#> $zF
#> [1] -1.138523
#> 
#> $dprime
#> [1] 1.596375
#> 
#> $c
#> [1] 0.3403358
#> 
#> $ln_beta
#> [1] 0.5433037
#> 
#> $beta
#> [1] 1.721685
#> 
#> $correction
#> [1] "loglinear"
```

## Variance estimation

Miller’s exact‑binomial method is recommended for most cases:

``` r

var_a_miller <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a, method = "miller")
var_a_miller
#> $var_zH
#> [1] 0.03372994
#> 
#> $var_zF
#> [1] 0.05230691
#> 
#> $var_dprime
#> [1] 0.08603685
#> 
#> $var_c
#> [1] 0.02150921
#> 
#> $cov_dprime_c
#> [1] 0.009288488
#> 
#> $var_ln_beta
#> [1] 0.07487284
#> 
#> $nboot
#> [1] 1000
#> 
#> $method
#> [1] "miller"
#> 
#> $correction
#> [1] "loglinear"
```

Gourevitch & Galanter’s delta method is faster but more approximate:

``` r

var_a_gouv <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a, method = "gourevitch")
var_a_gouv
#> $var_zH
#> [1] 0.03391665
#> 
#> $var_zF
#> [1] 0.05108471
#> 
#> $var_dprime
#> [1] 0.08500136
#> 
#> $var_c
#> [1] 0.02125034
#> 
#> $cov_dprime_c
#> [1] 0.008584026
#> 
#> $var_ln_beta
#> [1] 0.07332771
#> 
#> $nboot
#> [1] 1000
#> 
#> $method
#> [1] "gourevitch"
#> 
#> $correction
#> [1] "loglinear"
```

Bootstrap variance can also be used:

``` r

set.seed(1)
var_a_boot <- sdt_summary_variance(hits_a, fas_a, misses_a, cr_a,
                                   method = "bootstrap", nboot = 1000)
var_a_boot
#> $var_zH
#> [1] 0.03600002
#> 
#> $var_zF
#> [1] 0.05148473
#> 
#> $var_dprime
#> [1] 0.08391206
#> 
#> $var_c
#> [1] 0.02276436
#> 
#> $cov_dprime_c
#> [1] 0.007742356
#> 
#> $var_ln_beta
#> [1] 0.08607643
#> 
#> $nboot
#> [1] 1000
#> 
#> $method
#> [1] "bootstrap"
#> 
#> $correction
#> [1] "loglinear"
```

## Comparing two conditions

``` r

# Condition B counts
hits_b <- 28
fas_b <- 10
misses_b <- 22
cr_b <- 40

compare_sdt_summary(hits_a, fas_a, misses_a, cr_a,
                    hits_b, fas_b, misses_b, cr_b,
                    metric = "dprime",
                    method = "miller")
#> $metric
#> [1] "dprime"
#> 
#> $estimate_a
#> [1] 1.596375
#> 
#> $estimate_b
#> [1] 0.9687792
#> 
#> $diff
#> [1] 0.6275962
#> 
#> $se
#> [1] 0.3973377
#> 
#> $z
#> [1] 1.579503
#> 
#> $p_value
#> [1] 0.1142207
#> 
#> $method
#> [1] "miller"
#> 
#> $correction
#> [1] "loglinear"
```

You can also compare **criterion** or **lnβ**:

``` r

compare_sdt_summary(hits_a, fas_a, misses_a, cr_a,
                    hits_b, fas_b, misses_b, cr_b,
                    metric = "c",
                    method = "bootstrap",
                    nboot = 2000)
#> $metric
#> [1] "c"
#> 
#> $estimate_a
#> [1] 0.3403358
#> 
#> $estimate_b
#> [1] 0.3364025
#> 
#> $diff
#> [1] 0.003933284
#> 
#> $se
#> [1] 0.1999384
#> 
#> $z
#> [1] 0.01967248
#> 
#> $p_value
#> [1] 0.9843046
#> 
#> $method
#> [1] "bootstrap"
#> 
#> $correction
#> [1] "loglinear"
```

## Notes

- Extreme rates (0 or 1) are corrected by default (loglinear).
- If only **published d′ or β** are available, you need a reported
  **SE/variance** to compare conditions. Without counts or SEs, z-tests
  are not identifiable.

## The compound-model alternative

[`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
fits the compound (MAX SDT) model of Duncan (2006) directly to lineup
counts, estimating d’ and a criterion together, and gives the sampling
distribution by parametric bootstrap. It is the lineup-appropriate
counterpart to everything above.

### What it needs, and why you may not have it

The yes/no functions need four numbers per condition. The compound model
needs **five**, because it distinguishes outcomes the 2 x 2 framing
merges:

|  | Yes/no summary | Compound model |
|----|----|----|
| Target-present | hits, misses | correct IDs, **filler IDs**, rejections |
| Target-absent | false alarms, correct rejections | suspect/filler choices, rejections |

The extra cell is the filler identification, and this is the practical
constraint: **you cannot convert 2 x 2 counts into compound-model
input**, because collapsing to “miss” has already merged filler
identifications with rejections. If a published table reports only hits
and false alarms, the yes/no route above is the only one available. If
the suspect/filler/rejection breakdown survives, prefer the compound
model.

### A worked example

These are the Profile-lineup counts from Reisberg and Pezdek (2024), the
data on which the reference implementation of this model was written:
two conditions, each with 96 target-present and 96 or 106 target-absent
lineups of size 6.

``` r

fit_free <- fit_max_sdt(
  n_hit = 69, n_tp_choose = 82, n_fa = 64, N_tp = 96, N_ta = 106,
  n_hit_2 = 68, n_tp_choose_2 = 78, n_fa_2 = 42, N_tp_2 = 96, N_ta_2 = 96,
  n = 6, nboot = 300, seed = 11
)

knitr::kable(
  data.frame(
    Condition = c("Condition 1", "Condition 2"),
    `d'`      = round(c(fit_free$dprime_1, fit_free$dprime_2), 3),
    `95% CI`  = c(sprintf("[%.2f, %.2f]", fit_free$boot_ci["lower", "d1"],
                                          fit_free$boot_ci["upper", "d1"]),
                  sprintf("[%.2f, %.2f]", fit_free$boot_ci["lower", "d2"],
                                          fit_free$boot_ci["upper", "d2"])),
    `Criterion` = round(c(fit_free$lambda_1, fit_free$lambda_2), 3),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Compound (MAX SDT) model fitted to the Reisberg & Pezdek Profile-lineup counts, with parametric bootstrap intervals from 300 replicates."
)
```

| Condition   |    d’ | 95% CI         | Criterion |
|:------------|------:|:---------------|----------:|
| Condition 1 | 1.944 | \[1.63, 2.21\] |     1.146 |
| Condition 2 | 2.083 | \[1.80, 2.41\] |     1.393 |

Compound (MAX SDT) model fitted to the Reisberg & Pezdek Profile-lineup
counts, with parametric bootstrap intervals from 300 replicates.
{.table}

The bootstrap intervals **are** the sampling distribution: replicates
are simulated from the fitted model and refitted, so the spread reflects
the estimator’s behaviour rather than an asymptotic approximation.

``` r

cat("Goodness of fit: chi-square =", round(fit_free$chisq, 3),
    "on", fit_free$df, "df, p =", round(fit_free$p_value, 4), "
")
#> Goodness of fit: chi-square = 6.619 on 2 df, p = 0.0365
cat("Bootstrap replicates used:", fit_free$boot_n_used,
    " failed:", fit_free$boot_n_failed, "
")
#> Bootstrap replicates used: 298  failed: 2
```

Unlike the exactly-identified summary estimators, this model has
residual degrees of freedom, so it **can** be found to fit badly — which
is informative in itself.

### Comparing conditions

The comparison is a nested test rather than a Wald test on a difference:
fit the model with d’ free across conditions, refit with d’ constrained
equal, and compare the chi-square change.

``` r

fit_equal_d <- fit_max_sdt(
  n_hit = 69, n_tp_choose = 82, n_fa = 64, N_tp = 96, N_ta = 106,
  n_hit_2 = 68, n_tp_choose_2 = 78, n_fa_2 = 42, N_tp_2 = 96, N_ta_2 = 96,
  n = 6, constrain_d = TRUE
)

compare_max_sdt(fit_free, fit_equal_d)
#> MAX SDT Model Comparison (chi-squared difference test)
#>   Free model:       chi-sq = 6.619 (4 free params)
#>   Constrained model: chi-sq = 7.039 (3 free params)
#>   Constraint: d' equal
#>   Delta chi-sq(1) = 0.420, p = 0.5170
#>   Interpretation: constraint does not significantly worsen fit.
```

Constraining d’ to be equal costs 0.42 in chi-square for one degree of
freedom, which is nothing: these two conditions do not differ detectably
in discriminability. `constrain_c = TRUE` tests the criterion in the
same way.

### Which to use

| Situation | Use |
|----|----|
| Only hits and false alarms survive, e.g. from a published table | The yes/no summary functions above, stating that d’ is a yes/no analogue |
| Full suspect/filler/rejection breakdown available | [`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md) — it respects the compound structure and gives a goodness-of-fit test |
| Trial-level data with confidence | The lineup models in [`vignette("model_comparison")`](https://cgtza2.github.io/r4lineups/articles/model_comparison.md) and [`vignette("lineup_memory_models")`](https://cgtza2.github.io/r4lineups/articles/lineup_memory_models.md) |

A note for anyone reproducing published analyses:
[`fit_max_sdt()`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md)
uses mutually exclusive response cells, whereas some reference scripts
for this model fit total target-present choices alongside culprit
identifications, which counts the latter twice. Estimates therefore
differ — see the “Relation to Smith’s scripts” section of
[`?fit_max_sdt`](https://cgtza2.github.io/r4lineups/reference/fit_max_sdt.md).

## Technical Details

### The estimators

From hits $`H`$, false alarms $`F`$, signal trials $`N_s`$ and noise
trials $`N_n`$:

``` math
d' = z(p_H) - z(p_F), \qquad
c = -\tfrac{1}{2}\bigl[z(p_H) + z(p_F)\bigr], \qquad
\ln\beta = d'c
```

with $`p_H = H/N_s`$ and $`p_F = F/N_n`$. The $`\ln\beta = d'c`$
identity holds under equal variance only.

### Variance, and why there are three methods

Comparing two conditions needs a variance for the difference. The three
options differ in what they assume:

| Method | Basis | When to prefer it |
|----|----|----|
| `"gourevitch"` | Delta-method approximation (Gourevitch & Galanter, 1967) | Quick, and adequate with large counts away from the extremes |
| `"miller"` | Binomial moment sums evaluated at estimated rates (Miller, 1996) | The package default; its plug-in Wald test is approximate and can be liberal with sparse false alarms |
| `"bootstrap"` | Parametric resampling of independent binomial counts at estimated rates | Avoids a normal approximation for the transformed draws, but still assumes the binomial sampling model; boundary behaviour needs assessment |

Exact binomial moment summation does not make the resulting plug-in
comparison an exact test. In an independent audit scenario with 200
signal and 200 noise trials per condition, true hit rate .15 and
false-alarm rate .01, nominal 5% Miller Wald tests rejected equal
conditions about 8% of the time for d’ and 9% for ln-beta (12,000
datasets). These are limits of the approximation, rather than evidence
that one variance option is uniformly preferable.

The comparison is a Wald test, with
$`\mathrm{Var}(\Delta) = \mathrm{Var}_A + \mathrm{Var}_B`$, valid
because the two conditions are independent samples. It would be wrong
for a within-subjects design, where the two estimates are correlated;
the package provides no within-subjects version.

### The extreme-rate correction

A hit rate of exactly 1 or a false-alarm rate of exactly 0 gives an
infinite $`z`$, so a correction is unavoidable. Three are available via
`correction`:

| Option | Rate computed as | Applied |
|----|----|----|
| `"loglinear"` (default) | $`(x + 0.5)/(n + 1)`$ | To every case, extreme or not |
| `"half"` | $`0.5/n`$ or $`(n - 0.5)/n`$ | Only when the count is 0 or $`n`$ |
| `"none"` | $`x/n`$ | Never (rates are clamped away from 0 and 1 to keep $`z`$ finite) |

The default applies unconditionally rather than only to offending cells,
which is the form that behaves better in simulation. The choice is
recorded in the returned object rather than being applied silently, and
it should be stated in any writeup: with small counts it moves d′
appreciably.

### Assumptions

1.  Equal-variance signal detection. Unequal variance requires the full
    ROC, not a single point; see
    [`fit_sdt_roc()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_roc.md).
2.  The two conditions are independent samples.
3.  Every trial contributes one binary response, so the four counts
    partition the sample.
4.  The task is treated as yes/no detection. See the caution above on
    compound decision tasks (Duncan, 2006) for what this costs when the
    data are lineups.

## References

Duncan, M. (2006). *A signal detection model of compound decision tasks*
(Technical Note DRDC TR 2006-256). Defence R&D Canada, Toronto.

Gourevitch, V., & Galanter, E. (1967). A significance test for one
parameter isosensitivity functions. *Psychometrika, 32*(1), 25-33.

Miller, J. (1996). The sampling distribution of d’. *Perception &
Psychophysics, 58*(1), 65-72.

Reisberg, D., & Pezdek, K. (2024). Data analysed with the MAX SDT model
in scripts supplied by A. M. Smith (personal communication, 4 April
2024).

Suero, M., Privado, J., & Botella, J. (2017). Estimating the variances
and covariance of the parameters of signal detection theory.
*Psicothema, 29*(3), 432-437.

Tredoux, C. G., Sporer, S. L., Vredeveldt, A., Kempen, K., & Nortje, A.
(2021). Does constructing a facial composite affect eyewitness memory? A
research synthesis and meta-analysis. *Journal of Experimental
Criminology, 17*(4), 713-741.
<https://doi.org/10.1007/s11292-020-09432-z>

## Further Reading

- [`?sdt_summary_from_counts`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_from_counts.md),
  [`?sdt_summary_variance`](https://cgtza2.github.io/r4lineups/reference/sdt_summary_variance.md),
  [`?compare_sdt_summary`](https://cgtza2.github.io/r4lineups/reference/compare_sdt_summary.md)
- [`vignette("sdt_glm_analysis")`](https://cgtza2.github.io/r4lineups/articles/sdt_glm_analysis.md)
  for trial-level SDT with covariates and random effects
- [`vignette("model_comparison")`](https://cgtza2.github.io/r4lineups/articles/model_comparison.md)
  and
  [`vignette("lineup_memory_models")`](https://cgtza2.github.io/r4lineups/articles/lineup_memory_models.md)
  for models that respect the compound structure of a lineup decision
