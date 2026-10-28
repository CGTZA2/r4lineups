# SDT via GLM/GLMM

> **Acknowledgement.** Large language models supplied by Anthropic and
> OpenAI were used in preparing this vignette, to assist with checking
> code, testing the veracity of the approaches described, and on
> occasion editing the text. Responsibility for the content rests with
> the authors.

## Overview

This vignette estimates signal-detection metrics from **old/new
recognition** data using generalized linear models.

**Attribution.** The approach is that of Wright, Horry and Skagerberg
(2009), who set out both the traditional per-participant computation and
the multilevel alternative, and released it as the R package **sdtalt**.
The implementation here —
[`fit_sdt_glm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glm.md),
[`fit_sdt_glmm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md)
and
[`extract_sdt_metrics()`](https://cgtza2.github.io/r4lineups/reference/extract_sdt_metrics.md)
— was written from that paper and adapted into this package’s
conventions; it is not derived from their code. The multilevel fitting
is done by **lme4** (Bates, Mächler, Bolker & Walker, 2015).

``` r

library(r4lineups)
```

## What changes when you move from classic SDT to a GLM

Classic signal detection analysis computes, for each participant
separately, a hit rate and a false-alarm rate, converts both to
z-scores, and takes d′ as their difference:

``` math
d' = z(\text{hit rate}) - z(\text{false alarm rate})
```

Those per-participant values are then averaged, and a t-test is run on
them. This works, and for a simple design it gives sensible answers. But
it has four awkward properties:

1.  **Extreme values break it.** A participant with a hit rate of 1 or a
    false-alarm rate of 0 gives an infinite z-score. The usual remedies
    — adding 0.5 to each cell, or replacing 0 and 1 with 1/(2N) — are
    arbitrary adjustments applied to make the arithmetic work.
2.  **Every participant is weighted equally**, whether they contributed
    20 trials or 200.
3.  **Covariates are awkward.** Testing whether d′ depends on a
    continuous predictor means computing d′ per participant and then
    regressing on it, which is a two-stage procedure that loses the
    trial-level uncertainty.
4.  **Items are ignored entirely.** More on this below, since it is the
    most consequential.

The GLM formulation avoids all four by modelling the individual response
rather than the per-participant summary. It turns out that d′ falls
straight out of a probit regression, so nothing is lost by doing it this
way.

### Why a probit regression gives you d′

The connection is exact rather than approximate, and worth seeing.

Signal detection theory says the witness’s evidence is normally
distributed with mean 0 for new items and mean d′ for old ones, with a
criterion at some point $`c`$. The probability of saying “old” is the
area of that distribution above the criterion:

``` math
P(\text{said old} \mid \text{new}) = \Phi(-c), \qquad
P(\text{said old} \mid \text{old}) = \Phi(d' - c)
```

A probit regression models exactly this form:

``` math
\Phi^{-1}\bigl(P(\text{said old})\bigr) = \beta_0 + \beta_1 \cdot \text{is old}
```

Matching the two, $`\beta_1 = d'`$ and $`\beta_0 = -c`$. **The
coefficient on item status *is* d′** — not an analogue of it.

The intercept needs one further step. With `is_old` coded 0/1,
$`\beta_0`$ is the value when `is_old = 0`, so it is the criterion
measured from the new-item distribution. Coding `is_old` as −0.5/+0.5
instead centres it, and the intercept becomes the criterion measured
from the midpoint of the two distributions — which is the conventional
definition of $`c`$.
[`fit_sdt_glm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glm.md)
centres by default, which is why the criterion is only reported when
`center_isold = TRUE`.

With a **logit** link instead, the same coefficient is a log odds ratio
rather than d′. It answers the same question about discriminability but
on a different scale, and should not be reported as d′.

## Simulated data

``` r

set.seed(123)
n_subj  <- 40
n_items <- 60

df <- data.frame(
  subject_id = rep(1:n_subj, each = n_items),
  item_id    = rep(1:n_items, times = n_subj)
)
df$is_old <- rbinom(nrow(df), 1, 0.5)

# True d' = 1.2, with both participants and items varying in difficulty
subj_shift <- rnorm(n_subj,  0, 0.4)
item_shift <- rnorm(n_items, 0, 0.4)

df$said_old <- rbinom(
  nrow(df), 1,
  stats::pnorm(1.2 * df$is_old + subj_shift[df$subject_id] +
                 item_shift[df$item_id])
)
```

Note that both participants **and items** vary. That is realistic — some
faces are simply more memorable than others — and it is what makes the
next section matter.

## Single-level GLM

``` r

glm_fit <- fit_sdt_glm(df, is_old = "is_old", said_old = "said_old",
                       link = "probit")
extract_sdt_metrics(glm_fit)
#> $metric
#> [1] "dprime"
#> 
#> $estimate
#> [1] 1.15806
#> 
#> $criterion
#> [1] -0.4869419
#> 
#> $link
#> [1] "probit"
#> 
#> $centered
#> [1] TRUE
```

## Multilevel GLMM

[`fit_sdt_glmm()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md)
fits the same model with random effects, using
[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) internally:

``` r

glmm_fit <- fit_sdt_glmm(df,
                         is_old       = "is_old",
                         said_old     = "said_old",
                         subject_id   = "subject_id",
                         item_id      = "item_id",
                         link         = "probit",
                         random_slope = FALSE)
extract_sdt_metrics(glmm_fit)
#> $metric
#> [1] "dprime"
#> 
#> $estimate
#> [1] 1.312359
#> 
#> $criterion
#> [1] -0.5736078
#> 
#> $link
#> [1] "probit"
#> 
#> $centered
#> [1] TRUE
```

## Participants, items, or both

This is the substantive reason to prefer the multilevel formulation, and
it is an old argument: Clark (1973) pointed out that treating stimulus
items as fixed means any conclusion strictly applies only to the
particular items used. In recognition memory the items are faces or
words, and they differ in memorability. If your analysis treats them as
fixed, you are entitled to conclude something about *those sixty faces*,
not about faces in general.

The traditional workaround was to run the analysis twice — once
averaging over items to get a by-participants test, once averaging over
participants to get a by-items test — and require both to be
significant. Crossed random effects do the job properly in one model.

The consequence is visible in the standard errors:

``` r

structures <- list(
  "GLM: no clustering"        = fit_sdt_glm(df, "is_old", "said_old",
                                            link = "probit"),
  "GLMM: subject only"        = fit_sdt_glmm(df, "is_old", "said_old",
                                             subject_id = "subject_id",
                                             link = "probit",
                                             random_slope = FALSE),
  "GLMM: subject and item"    = fit_sdt_glmm(df, "is_old", "said_old",
                                             subject_id = "subject_id",
                                             item_id = "item_id",
                                             link = "probit",
                                             random_slope = FALSE)
)

comparison <- do.call(rbind, lapply(names(structures), function(nm) {
  m  <- structures[[nm]]
  co <- summary(m)$coefficients
  term <- attr(m, "sdt_is_old_term")
  if (is.null(term)) term <- ".is_old_c"
  data.frame(
    `Random effects` = nm,
    `d'`             = round(co[term, "Estimate"], 3),
    `SE`             = round(co[term, "Std. Error"], 4),
    check.names = FALSE
  )
}))

knitr::kable(
  comparison, row.names = FALSE,
  caption = "Estimated d' and its standard error under three random-effect structures. Simulated data, 40 participants by 60 items, true d' = 1.2."
)
```

| Random effects         |    d’ |     SE |
|:-----------------------|------:|-------:|
| GLM: no clustering     | 1.158 | 0.0576 |
| GLMM: subject only     | 1.245 | 0.0616 |
| GLMM: subject and item | 1.312 | 0.0654 |

Estimated d’ and its standard error under three random-effect
structures. Simulated data, 40 participants by 60 items, true d’ = 1.2.
{.table}

The standard error grows as clustering is acknowledged. Ignoring it
entirely treats every one of the 2400 trials as independent, which they
are not: trials from the same participant share that participant’s
ability, and trials on the same item share that item’s memorability. **A
GLM that ignores this reports a standard error that is too small, and a
p-value that is too generous.**

### The estimates move too, and predictably

The d′ estimates also differ across the three rows, and this is not
noise. The GLM estimate is **marginal** (population-averaged): it
describes the average response probability across all participants and
items. The GLMM estimate is **conditional** (subject- and
item-specific): it describes the effect for a participant and item at
the average of the random effects.

For a probit link the two are related by a known attenuation:

``` math
d'_{\text{marginal}} \approx \frac{d'_{\text{conditional}}}{\sqrt{1 + \sigma^2_{\text{subject}} + \sigma^2_{\text{item}}}}
```

``` r

vc <- as.data.frame(lme4::VarCorr(structures[["GLMM: subject and item"]]))
total_var <- sum(vc$vcov)

cond <- lme4::fixef(structures[["GLMM: subject and item"]])[[".is_old_c"]]
marg <- stats::coef(structures[["GLM: no clustering"]])[[".is_old_c"]]

knitr::kable(
  data.frame(
    Quantity = c("Conditional d' (GLMM)",
                 "Random-effect variance, total",
                 "Attenuation factor",
                 "Conditional / attenuation factor",
                 "Marginal d' (GLM), observed"),
    Value = round(c(cond, total_var, sqrt(1 + total_var),
                    cond / sqrt(1 + total_var), marg), 3)
  ),
  row.names = FALSE,
  caption = "Marginal and conditional d' are different quantities related by a known factor. Simulated data."
)
```

| Quantity                         | Value |
|:---------------------------------|------:|
| Conditional d’ (GLMM)            | 1.312 |
| Random-effect variance, total    | 0.336 |
| Attenuation factor               | 1.156 |
| Conditional / attenuation factor | 1.135 |
| Marginal d’ (GLM), observed      | 1.158 |

Marginal and conditional d’ are different quantities related by a known
factor. Simulated data. {.table}

The predicted marginal value and the observed one agree closely. So the
two models are not disagreeing about the data; they are answering
slightly different questions. Neither is wrong, but they should not be
compared as though interchangeable, and a paper should say which is
being reported. The conditional estimate is usually the one meant by
“d′” in a cognitive model, since it refers to a typical participant
rather than to an average over heterogeneity.

## Including covariates

``` r

df$delay <- rnorm(nrow(df), 0, 1)

glm_fit_cov <- fit_sdt_glm(df,
                           is_old       = "is_old",
                           said_old     = "said_old",
                           covariates   = "delay",
                           link         = "probit",
                           interactions = TRUE)
summary(glm_fit_cov)
#> 
#> Call:
#> stats::glm(formula = form, family = stats::binomial(link = link), 
#>     data = df)
#> 
#> Coefficients:
#>                 Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)      0.48750    0.02886  16.893   <2e-16 ***
#> .is_old_c        1.15905    0.05772  20.082   <2e-16 ***
#> delay           -0.04253    0.02907  -1.463    0.143    
#> .is_old_c:delay -0.06977    0.05813  -1.200    0.230    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 3083.6  on 2399  degrees of freedom
#> Residual deviance: 2647.3  on 2396  degrees of freedom
#> AIC: 2655.3
#> 
#> Number of Fisher Scoring iterations: 4
```

With `interactions = TRUE`, the `is_old × delay` term tests whether
discriminability itself depends on the covariate — which is the question
usually intended, and which the two-stage classic approach cannot ask
directly.

## Technical Details

### What is estimated

| Link   | Coefficient on `is_old` | Intercept (centred coding)    |
|--------|-------------------------|-------------------------------|
| probit | d′                      | −criterion                    |
| logit  | log odds ratio          | −criterion on the logit scale |

Standard errors for the GLM come from the usual Fisher information; for
the GLMM they come from `lme4`’s Laplace approximation to the marginal
likelihood.

### Coding conventions

`center_isold = TRUE` (the default) codes item status as −0.5/+0.5
rather than 0/1. This does not change the coefficient on `is_old`, but
it changes the intercept from “criterion relative to the new-item
distribution” to “criterion relative to the midpoint”, which is the
conventional $`c`$.
[`extract_sdt_metrics()`](https://cgtza2.github.io/r4lineups/reference/extract_sdt_metrics.md)
returns the criterion only under centred coding, since under 0/1 coding
the intercept is not $`c`$.

### Assumptions

1.  **Equal variance.** The probit model as fitted assumes the old- and
    new-item evidence distributions have the same variance. The
    unequal-variance case — which is the empirically supported one in
    recognition memory — needs a model with a scale parameter, for which
    see
    [`fit_sdt_roc()`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_roc.md)
    in `R/zroc_sdt.R`, or the ordinal approach discussed below.
2.  **Random effects are normally distributed** with mean zero, and
    independent of the predictors.
3.  **Crossed, not nested.** Participants and items are crossed: every
    participant sees every item, or nearly so.
    `(1|subject_id) + (1|item_id)` is correct for this. If items were
    nested within lists, the specification would differ.
4.  Random intercepts only, unless `random_slope = TRUE`. Allowing d′
    itself to vary across participants is often the more realistic model
    and requires more data to fit.

### A limitation worth naming

This treats the response as binary — “old” or “new”. Recognition data
usually carry confidence as well, and reducing a confidence rating to a
binary response discards it. The natural extension is an **ordinal**
(cumulative link) model with confidence as the ordered outcome, in which
the coefficient on item status is still d′ and the thresholds are the
criteria; allowing the scale to depend on condition then gives the
unequal-variance model. That is not implemented here.

## References

Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear
mixed-effects models using lme4. *Journal of Statistical Software,
67*(1), 1-48.

Clark, H. H. (1973). The language-as-fixed-effect fallacy: A critique of
language statistics in psychological research. *Journal of Verbal
Learning and Verbal Behavior, 12*(4), 335-359.

Wright, D. B., Horry, R., & Skagerberg, E. M. (2009). Functions for
traditional and multilevel approaches to signal detection theory.
*Behavior Research Methods, 41*(2), 257-267.

## Further Reading

- [`?fit_sdt_glm`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glm.md),
  [`?fit_sdt_glmm`](https://cgtza2.github.io/r4lineups/reference/fit_sdt_glmm.md),
  [`?extract_sdt_metrics`](https://cgtza2.github.io/r4lineups/reference/extract_sdt_metrics.md)
- [`vignette("sdt_summary_comparisons")`](https://cgtza2.github.io/r4lineups/articles/sdt_summary_comparisons.md)
  for comparing SDT parameters between groups
- [`vignette("model_comparison")`](https://cgtza2.github.io/r4lineups/articles/model_comparison.md)
  for lineup-specific models
