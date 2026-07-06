# Fit an SDT GLMM for Old/New Recognition

Fits a multilevel GLMM (binomial link) to estimate SDT metrics from
old/new recognition responses, using random effects for participants and
optionally items.

## Usage

``` r
fit_sdt_glmm(
  data,
  is_old,
  said_old,
  subject_id,
  covariates = NULL,
  item_id = NULL,
  link = "probit",
  center_isold = TRUE,
  random_slope = TRUE,
  interactions = FALSE
)
```

## Arguments

- data:

  Data frame containing trial-level responses.

- is_old:

  Column name indicating whether the item is old (1) or new (0).

- said_old:

  Column name indicating whether the participant said "old" (1) or "new"
  (0).

- subject_id:

  Column name identifying participants.

- covariates:

  Optional character vector of covariate column names.

- item_id:

  Optional column name for item-level random intercepts.

- link:

  Link function for the binomial model ("probit" or "logit").

- center_isold:

  Logical. If TRUE, recodes is_old to -0.5 / +0.5.

- random_slope:

  Logical. If TRUE, include random slope for is_old by subject.

- interactions:

  Logical. If TRUE, include is_old interactions with covariates.

## Value

A `glmerMod` object with SDT attributes.

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  set.seed(123)
  n_subj <- 20
  n_trial <- 20
  df <- expand.grid(subject = seq_len(n_subj), trial = seq_len(n_trial))
  df$is_old <- rep(c(0, 1), length.out = nrow(df))
  df$said_old <- rbinom(nrow(df), 1, ifelse(df$is_old == 1, 0.7, 0.3))
  fit <- fit_sdt_glmm(df, is_old = "is_old", said_old = "said_old",
                      subject_id = "subject", random_slope = FALSE)
  extract_sdt_metrics(fit)
}
#> boundary (singular) fit: see help('isSingular')
#> $metric
#> [1] "dprime"
#> 
#> $estimate
#> [1] 1.123856
#> 
#> $criterion
#> [1] 0.06607783
#> 
#> $link
#> [1] "probit"
#> 
#> $centered
#> [1] TRUE
#> 
# }
```
