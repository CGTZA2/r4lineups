# Extract SDT Metrics from a GLM/GLMM

Extracts d' (probit) or ln(OR) (logit) from SDT GLM/GLMM fits. Criterion
is reported only when is_old was centered to -0.5/+0.5.

## Usage

``` r
extract_sdt_metrics(model, link = NULL)
```

## Arguments

- model:

  A glm or glmerMod object produced by fit_sdt_glm/fit_sdt_glmm.

- link:

  Optional. If NULL, inferred from the model family.

## Value

A list containing d' or ln(OR), criterion (if available), and metadata.

## Examples

``` r
set.seed(123)
n <- 200
is_old <- rep(c(0, 1), each = n / 2)
said_old <- rbinom(n, 1, ifelse(is_old == 1, 0.7, 0.3))
df <- data.frame(is_old = is_old, said_old = said_old)
fit <- fit_sdt_glm(df, is_old = "is_old", said_old = "said_old")
metrics <- extract_sdt_metrics(fit)
metrics$estimate  # d' under the probit link
#> [1] 1.19673
```
