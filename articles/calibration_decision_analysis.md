# Calibration and Decision Analysis

> **Acknowledgement.** Large language models supplied by Anthropic and
> OpenAI were used in preparing this vignette, to assist with checking
> code, testing the veracity of the approaches described, and on
> occasion editing the text. Responsibility for the content rests with
> the authors.

## Introduction

This vignette demonstrates five statistical methods for analyzing
eyewitness identification data in **r4lineups**:

1.  **Calibration Analysis** - Assessing confidence-accuracy
    correspondence
2.  **Bayesian Information-Gain Curves** - Evaluating diagnostic value
    of evidence
3.  **Expected Utility Analysis** - Decision-theoretic evaluation with
    cost/benefit tradeoffs
4.  **Deviation from Perfect Performance (DPP)** - ROC-based
    truncation-robust metric
5.  **ANRI** - Bias-corrected resolution index with bootstrap inference

These methods complement the traditional lineup fairness measures by
providing sophisticated analyses of identification accuracy, confidence
calibration, and decision-making utility.

They answer different questions, and it is worth being clear which is
which before working through them.

**Calibration analysis** asks whether a witness’s confidence *means*
what it appears to mean: when witnesses say they are 90% sure, are they
right 90% of the time? It yields a calibration curve and three summary
statistics, and it is the oldest of the five in eyewitness work (Juslin,
Olsson & Winman, 1996).

**Bayesian information-gain analysis** asks a different question: given
a particular identification response, how much should a rational
observer revise their belief that the suspect is guilty? This is the
framework of Wells, Yang and Smalarz (2015), and it makes explicit
something the other methods leave implicit — that the evidential value
of an identification depends on the prior probability of guilt, which is
a property of the investigation and not of the witness.

**Expected utility analysis** goes one step further and asks what a
policy *should* be. Given the costs of a wrongful identification and the
benefits of a correct one, at what confidence level should an
identification be acted upon? Lampinen, Smith and Wells (2019) set out
the framework and, importantly, show that it can disagree with ROC-based
conclusions about which procedure is better.

**Deviation from perfect performance (DPP)** is a discriminability
measure, proposed by Smith, Lampinen, Wells, Smalarz and Mackovichova
(2019) as an alternative to partial area under the ROC curve. Their
argument is that pAUC does not track the diagnostic utility of a lineup,
and DPP does.

**ANRI** is a small-sample correction to the normalized resolution
index. NRI (Yaniv, Yates & Smith, 1991) is positively biased when the
number of confidence bins is large relative to sample size; ANRI removes
that bias so that resolution can be compared across studies and tested
formally.

## Data Format

All methods in this vignette require a dataframe with at least the
following columns:

- `target_present`: Logical. TRUE if the guilty suspect is in the lineup
- `identification`: Character. “suspect”, “filler”, or “reject”
- `confidence`: Numeric. Confidence rating (any scale)

We use the built-in `lineup_example` dataset throughout. It contains 700
target-present and 700 target-absent lineups of size 6.

``` r
library(r4lineups)
data(lineup_example)

head(lineup_example)
  target_present lineup_member suspect_pos identification confidence
1          FALSE             2           1         filler         40
2          FALSE             3           1         filler         50
3           TRUE             2           1         filler         40
4           TRUE             1           1        suspect         80
5           TRUE             1           1        suspect         60
6          FALSE             0           1         reject         20
```

### Why there is a `lineup_member` column

`lineup_example` records two further columns, and the reason is worth
stating because it applies to any lineup data you collect or simulate
yourself.

`identification` collapses a witness’s decision into three categories.
That is what the analysis functions consume, but it is *not* what should
be recorded. The raw datum is **which lineup member the witness chose**
— `lineup_member`, with `0` for a rejection — together with the
suspect’s position, `suspect_pos`. The label is then derived:

``` r
head(classify_lineup_choices(
  lineup_example[, c("lineup_member", "suspect_pos")],
  k = 6
))
  lineup_member suspect_pos identification
1             2           1         filler
2             3           1         filler
3             2           1         filler
4             1           1        suspect
5             1           1        suspect
6             0           1         reject
```

This matters because **foils are not interchangeable**. They differ in
how plausible they are, and a foil that nobody ever chooses is not a
functioning alternative to the suspect at all — it is precisely what
makes a lineup unfair. Recording only “filler” throws that information
away. In `lineup_example` the five foils attract very different numbers
of choices:

``` r
lineup_table(
  lineup_example$lineup_member[lineup_example$lineup_member > 0],
  k = 6, suspect_pos = 1
)
Lineup choices (k = 6, N = 1132)

 pos:  1*    2    3    4   5   6
 cnt: 503  248  203  115  53  10

 * = nominated suspect
```

`lineup_example` is simulated, and it is simulated at the level of
individual foils: each is given its own mean memory strength rather than
being drawn from a common distribution. See
[`?simulate_lineup_data`](https://cgtza2.github.io/r4lineups/reference/simulate_lineup_data.md)
and its `foil_bias` argument if you want to generate data of your own on
the same basis.

## 1. Calibration Analysis

Calibration measures how well confidence judgments correspond to
accuracy. Perfect calibration means that when witnesses express X%
confidence, they are correct X% of the time.

### Basic Calibration

``` r

# Compute calibration with binned confidence
cal_result <- make_calibration(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100),
  choosers_only = TRUE  # Analyze only suspect identifications
)
```

![](calibration_decision_analysis_files/figure-html/calibration-basic-1.png)

``` r

print(cal_result)

=== Lineup Calibration Analysis ===

Analysis type: Choosers only (suspect IDs)
Total N: 503 

Calibration Statistics:
  C (Calibration):         0.0727 
  O/U (Over/Under):        -0.2290 
  NRI (Resolution):        0.0453 

Overall Performance:
  Mean Accuracy:      0.825 
  Mean Confidence:    0.596 

Calibration Data by Bin:
 [38;5;246m# A tibble: 3 × 7 [39m
  bin          n mean_confidence accuracy n_correct n_incorrect
   [3m [38;5;246m<chr> [39m [23m     [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m      [3m [38;5;246m<dbl> [39m [23m        [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m [0,60]     305            42.6    0.764       233          72
 [38;5;250m2 [39m (60,80]     94            74.0    0.872        82          12
 [38;5;250m3 [39m (80,100]   104            96.5    0.962       100           4
 [38;5;246m# ℹ 1 more variable: mean_confidence_prop <dbl> [39m

Plot: save this object first, then extract it with x$plot
```

The output provides three key statistics:

- **C statistic**: Overall calibration quality (0 = perfect, higher =
  worse)
- **O/U (Over/Under confidence)**: Positive = overconfident, Negative =
  underconfident
- **NRI (Normalized Resolution Index)**: Ability to discriminate
  accuracy levels with confidence (higher = better)

### Interpreting the Calibration Plot

The calibration plot shows:

- **Diagonal line**: Perfect calibration
- **Points above diagonal**: Underconfident (accuracy exceeds
  confidence)
- **Points below diagonal**: Overconfident (confidence exceeds accuracy)
- **Point size**: Sample size in each bin

``` r

cal_result$plot
```

![](calibration_decision_analysis_files/figure-html/calibration-plot-1.png)

Accuracy rises steadily across the confidence bins, from about 76% in
the lowest to about 96% in the highest, so confidence here is genuinely
informative. The points sit *above* the diagonal, however, which means
these witnesses are **under**confident: they are more accurate than
their stated confidence implies. That is the direction the O/U statistic
reports.

The plot is an ordinary **ggplot2** object, so it can be modified and
saved in the usual way.
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) picks
its output format from the file extension alone, so the same call writes
PNG, PDF, SVG or TIFF depending on what you ask for:

``` r

outfile <- file.path(tempdir(), "calibration_plot.png")
ggplot2::ggsave(outfile, cal_result$plot, width = 8, height = 6, dpi = 300)

# Change only the extension to change the format:
#   ggsave("calibration_plot.pdf", cal_result$plot)   # vector, for print
#   ggsave("calibration_plot.svg", cal_result$plot)   # vector, for the web
```

### Calibration by Condition

The question here is whether calibration differs between two
identification procedures. To answer it we need data from two
procedures, and `lineup_example` records only one, so we simulate them.

The comparison is only meaningful if the two conditions genuinely
differ. We give Procedure A the stronger memory evidence
(`d_prime = 1.8` against `1.1`), as might arise from better viewing
conditions or a better-constructed lineup, and give the two lineups
different foil quality as well. Note that both are seeded, so the
results below are reproducible rather than changing on every rebuild.

``` r

# Procedure A: stronger evidence, and foils that are reasonable alternatives
data_proc_a <- as.data.frame(simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 1.8, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 9,
  foil_bias = c(0.4, 0.2, 0.0, -0.3, -0.8), seed = 101
))

# Procedure B: weaker evidence, and one near-useless foil
data_proc_b <- as.data.frame(simulate_lineup_data(
  n_tp = 400, n_ta = 400, d_prime = 1.1, c_criterion = 0.9,
  lineup_size = 6, conf_levels = 9,
  foil_bias = c(0.5, 0.1, -0.2, -0.9, -2.0), seed = 102
))

# Put confidence on the same 20-100 scale used by lineup_example
conf_scale <- round(seq(20, 100, length.out = 9))
data_proc_a$confidence <- conf_scale[data_proc_a$confidence]
data_proc_b$confidence <- conf_scale[data_proc_b$confidence]

# Combine into one frame with a condition variable
procedures <- rbind(
  transform(data_proc_a, procedure = "Procedure A"),
  transform(data_proc_b, procedure = "Procedure B")
)
```

``` r
cal_by_cond <- make_calibration_by_condition(
  procedures,
  condition_var = "procedure",
  confidence_bins = c(0, 60, 80, 100)
)

print(cal_by_cond)
$by_condition
$by_condition$`Procedure A`
$by_condition$`Procedure A`$calibration_data
 [38;5;246m# A tibble: 3 × 7 [39m
  bin          n mean_confidence accuracy n_correct n_incorrect
   [3m [38;5;246m<chr> [39m [23m     [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m      [3m [38;5;246m<dbl> [39m [23m        [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m [0,60]     184            41.5    0.745       137          47
 [38;5;250m2 [39m (60,80]     71            75.4    0.887        63           8
 [38;5;250m3 [39m (80,100]    88            97.3    0.977        86           2
 [38;5;246m# ℹ 1 more variable: mean_confidence_prop <dbl> [39m

$by_condition$`Procedure A`$C
[1] 0.06189919

$by_condition$`Procedure A`$OU
[1] -0.2055394

$by_condition$`Procedure A`$NRI
[1] 0.07322035

$by_condition$`Procedure A`$overall_accuracy
[1] 0.8338192

$by_condition$`Procedure A`$overall_confidence
[1] 0.6282799

$by_condition$`Procedure A`$n_total
[1] 343

$by_condition$`Procedure A`$choosers_only
[1] TRUE

$by_condition$`Procedure A`$confidence_scale
[1] 100

$by_condition$`Procedure A`$innocent_suspect_method
[1] "designated"


$by_condition$`Procedure B`
$by_condition$`Procedure B`$calibration_data
 [38;5;246m# A tibble: 3 × 7 [39m
  bin          n mean_confidence accuracy n_correct n_incorrect
   [3m [38;5;246m<chr> [39m [23m     [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m      [3m [38;5;246m<dbl> [39m [23m        [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m [0,60]     162            41.0    0.747       121          41
 [38;5;250m2 [39m (60,80]     35            74      0.829        29           6
 [38;5;250m3 [39m (80,100]    22            96.8    1            22           0
 [38;5;246m# ℹ 1 more variable: mean_confidence_prop <dbl> [39m

$by_condition$`Procedure B`$C
[1] 0.08538386

$by_condition$`Procedure B`$OU
[1] -0.2666667

$by_condition$`Procedure B`$NRI
[1] 0.03571501

$by_condition$`Procedure B`$overall_accuracy
[1] 0.7853881

$by_condition$`Procedure B`$overall_confidence
[1] 0.5187215

$by_condition$`Procedure B`$n_total
[1] 219

$by_condition$`Procedure B`$choosers_only
[1] TRUE

$by_condition$`Procedure B`$confidence_scale
[1] 100

$by_condition$`Procedure B`$innocent_suspect_method
[1] "designated"



$condition_summary
 [38;5;246m# A tibble: 2 × 7 [39m
  condition       n      C     OU    NRI overall_accuracy overall_confidence
 [38;5;250m* [39m  [3m [38;5;246m<chr> [39m [23m        [3m [38;5;246m<dbl> [39m [23m   [3m [38;5;246m<dbl> [39m [23m   [3m [38;5;246m<dbl> [39m [23m   [3m [38;5;246m<dbl> [39m [23m             [3m [38;5;246m<dbl> [39m [23m               [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m Procedure A   343 0.061 [4m9 [24m - [31m0 [39m [31m. [39m [31m206 [39m 0.073 [4m2 [24m            0.834              0.628
 [38;5;250m2 [39m Procedure B   219 0.085 [4m4 [24m - [31m0 [39m [31m. [39m [31m267 [39m 0.035 [4m7 [24m            0.785              0.519

$condition_vars
[1] "procedure"

cal_by_cond$plot
NULL
```

## 2. Bayesian Information-Gain Analysis

Bayesian analysis evaluates how much an identification response changes
our belief about guilt, quantified as information gain (reduction in
uncertainty). The framework is that of Wells, Yang and Smalarz (2015).

### The arithmetic, briefly

Let $`G`$ denote the event that the suspect is guilty and $`x`$ a
particular identification response — say, a suspect identification made
with confidence above 80. Bayes’ theorem gives the posterior probability
of guilt as

``` math
P(G \mid x) \;=\;
\frac{P(x \mid G)\,P(G)}{P(x \mid G)\,P(G) + P(x \mid \neg G)\,\bigl(1 - P(G)\bigr)}
```

The two conditional probabilities are estimated directly from the data:
$`P(x \mid G)`$ is the proportion of target-present witnesses who gave
response $`x`$, and $`P(x \mid \neg G)`$ the proportion of target-absent
witnesses who did. Their ratio is the **likelihood ratio**,

``` math
\mathrm{LR}(x) \;=\; \frac{P(x \mid G)}{P(x \mid \neg G)}
```

and it is the whole of what the identification contributes. An
$`\mathrm{LR}`$ of 1 means the response is uninformative; larger values
point towards guilt, smaller towards innocence.
[`make_bayes_curves()`](https://cgtza2.github.io/r4lineups/reference/make_bayes_curves.md)
returns these quantities in `$likelihoods`:

``` r

lik <- make_bayes_curves(
  lineup_example,
  response_categories = "confidence",
  confidence_bins = c(0, 60, 80, 100)
)$likelihoods

knitr::kable(
  transform(
    as.data.frame(lik)[, c("response", "p_x_given_guilty",
                           "p_x_given_innocent", "n_guilty", "n_innocent")],
    LR = round(lik$p_x_given_guilty / lik$p_x_given_innocent, 2)
  ),
  digits = 3, row.names = FALSE,
  caption = "Response likelihoods and likelihood ratios, lineup_example."
)
```

| response | p_x_given_guilty | p_x_given_innocent | n_guilty | n_innocent | LR |
|:---|---:|---:|---:|---:|---:|
| filler\_(60,80\] | 0.057 | 0.084 | 40 | 59 | 0.68 |
| filler\_(80,100\] | 0.019 | 0.043 | 13 | 30 | 0.43 |
| filler\_\[0,60\] | 0.236 | 0.460 | 165 | 322 | 0.51 |
| reject\_\[0,60\] | 0.096 | 0.287 | 67 | 201 | 0.33 |
| suspect\_(60,80\] | 0.117 | 0.017 | 82 | 12 | 6.83 |
| suspect\_(80,100\] | 0.143 | 0.006 | 100 | 4 | 25.00 |
| suspect\_\[0,60\] | 0.333 | 0.103 | 233 | 72 | 3.24 |

Response likelihoods and likelihood ratios, lineup_example. {.table
style="width:100%;"}

A high-confidence suspect identification carries a likelihood ratio of
roughly 25 here, while a rejection carries one of about 0.3 — that is,
it is evidence *against* guilt. Note also that the likelihood ratio, not
the posterior, is the portable quantity: it belongs to the
identification procedure, whereas the posterior additionally depends on
the prior, which belongs to the investigation.

One practical warning. If no target-absent witness ever produces a given
response, the estimated $`P(x \mid \neg G)`$ is zero, the likelihood
ratio is infinite, and the posterior curve is pinned at 1.0 for every
prior. That is almost always a sign of too few observations or too
narrow a bin rather than of perfectly diagnostic evidence, and
[`make_bayes_curves()`](https://cgtza2.github.io/r4lineups/reference/make_bayes_curves.md)
warns when it happens.

### Prior-Posterior Curves

``` r
# Compute Bayesian curves for simple responses
bayes_result <- make_bayes_curves(
  lineup_example,
  response_categories = "simple",  # "suspect", "filler", "reject"
  prior_grid = seq(0.01, 0.99, 0.01)
)

print(bayes_result)
$curves
 [38;5;246m# A tibble: 297 × 4 [39m
   response prior posterior information_gain
    [3m [38;5;246m<chr> [39m [23m     [3m [38;5;246m<dbl> [39m [23m      [3m [38;5;246m<dbl> [39m [23m             [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m 1 [39m filler    0.01   0.005 [4m3 [24m [4m3 [24m           0.032 [4m9 [24m
 [38;5;250m 2 [39m filler    0.02   0.010 [4m7 [24m            0.056 [4m0 [24m
 [38;5;250m 3 [39m filler    0.03   0.016 [4m1 [24m            0.075 [4m2 [24m
 [38;5;250m 4 [39m filler    0.04   0.021 [4m6 [24m            0.091 [4m8 [24m
 [38;5;250m 5 [39m filler    0.05   0.027 [4m2 [24m            0.106 
 [38;5;250m 6 [39m filler    0.06   0.032 [4m7 [24m            0.119 
 [38;5;250m 7 [39m filler    0.07   0.038 [4m4 [24m            0.131 
 [38;5;250m 8 [39m filler    0.08   0.044 [4m1 [24m            0.141 
 [38;5;250m 9 [39m filler    0.09   0.049 [4m8 [24m            0.151 
 [38;5;250m10 [39m filler    0.1    0.055 [4m7 [24m            0.159 
 [38;5;246m# ℹ 287 more rows [39m

$likelihoods
 [38;5;246m# A tibble: 3 × 5 [39m
  response p_x_given_guilty p_x_given_innocent n_guilty n_innocent
   [3m [38;5;246m<chr> [39m [23m                [3m [38;5;246m<dbl> [39m [23m               [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<int> [39m [23m       [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m filler             0.311               0.587      218        411
 [38;5;250m2 [39m reject             0.095 [4m7 [24m              0.287       67        201
 [38;5;250m3 [39m suspect            0.593               0.126      415         88

$response_counts
 [38;5;246m# A tibble: 3 × 3 [39m
  response n_guilty n_innocent
   [3m [38;5;246m<chr> [39m [23m        [3m [38;5;246m<int> [39m [23m       [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m filler        218        411
 [38;5;250m2 [39m reject         67        201
 [38;5;250m3 [39m suspect       415         88

$n_guilty
[1] 700

$n_innocent
[1] 700

$response_categories
[1] "simple"

$lineup_size
[1] 6

$innocent_suspect_method
[1] "designated"

# Plot prior-posterior relationships
plot_bayes_prior_posterior(bayes_result)
```

![](calibration_decision_analysis_files/figure-html/bayes-curves-1.png)

The plot shows how each response type updates prior belief:

- **Lines above diagonal**: Evidence increases guilt belief (diagnostic
  of guilt)
- **Lines below diagonal**: Evidence decreases guilt belief (diagnostic
  of innocence)
- **Steeper slopes**: More diagnostic evidence

The prior-posterior curve is due to Wells, Yang and Smalarz (2015), who
introduced it along with the base-rate effect-equivalency curve used
later in this vignette. The implementation here is written directly from
that paper; it is not adapted from another package.

**On the shape of these curves.** They are strongly curved, and that
curvature is not an artefact of any smoothing — no loess or spline is
fitted anywhere in these plots. The curve is the exact plot of the
equation above as $`P(G)`$ varies from 0 to 1, and Bayes’ theorem is
nonlinear in the prior. The characteristic S-shape is the substantive
point of the display: the same identification moves belief a great deal
when the prior is middling, and very little when the prior is already
near 0 or near 1. Evidence that would transform a genuinely uncertain
case can do almost nothing to a case that is already close to settled.

### Information Gain

Information gain quantifies uncertainty reduction, measured in bits.
Where the posterior curve says *where belief ends up*, information gain
says *how much was learned*, which is greatest when the prior leaves
most room for revision:

``` r

plot_bayes_information_gain(bayes_result)
```

![](calibration_decision_analysis_files/figure-html/bayes-information-gain-1.png)

Interpretation:

- **Positive values**: Response reduces uncertainty (diagnostic)
- **Negative values**: Response increases uncertainty (misleading)
- **Higher magnitude**: Stronger diagnostic value

### Confidence-Based Bayesian Analysis

Analyze by confidence level:

``` r

bayes_conf <- make_bayes_curves(
  lineup_example,
  response_categories = "confidence",
  confidence_bins = c(0, 60, 80, 100)
)

# Get suspect responses only
suspect_responses <- grep("^suspect_", unique(bayes_conf$curves$response), value = TRUE)
plot_bayes_prior_posterior(bayes_conf, selected_responses = suspect_responses)
```

![](calibration_decision_analysis_files/figure-html/bayes-confidence-1.png)

Splitting suspect identifications by confidence separates three quite
different kinds of evidence. The highest-confidence band sits well above
the others across the whole range of priors, which is the Bayesian
restatement of the calibration result found earlier: high-confidence
identifications from this procedure are worth considerably more than
low-confidence ones.

The three curves stay clearly apart and none is pinned to the top of the
plot. A curve that *is* pinned flat at 1.0 signals the zero-cell problem
described above, not evidence of unusual strength.

### Base-Rate Equivalency of Evidence (BREE)

The base-rate effect-equivalency curve, also from Wells, Yang and
Smalarz (2015), answers a question that comes up whenever two procedures
are compared and neither dominates: *how much better is one than the
other, expressed in units anyone can interpret?*

The idea is to express the difference as an exchange rate between base
rates. Suppose Procedure A, applied to a case with a 20% prior
probability of guilt, yields a certain posterior after a suspect
identification. We can then ask what prior Procedure B would need in
order to reach that same posterior. If B needs a 40% prior to get where
A got from 20%, then A is worth as much as a doubling of the base rate —
and a doubling of the base rate is something an investigator has an
intuition about, in a way that a difference in pAUC is not.

Plotting that correspondence for every prior gives the BREE curve. The
diagonal is the null result of no difference; the further the curve
departs from it, the larger the advantage.

``` r

bree_result <- make_bree_curve(
  data_proc_a,
  data_proc_b,
  reference_response = "suspect"
)

plot_bree(bree_result)
```

![](calibration_decision_analysis_files/figure-html/bree-1.png)

The curve lies below the diagonal throughout, meaning Procedure B
requires a *higher* prior probability of guilt to reach any given
posterior — which is simply to say that a suspect identification from
Procedure A is worth more. The gap is widest at low base rates, which is
the practically important region: it is exactly where cases are most
uncertain that the difference between the two procedures matters most.

## 3. Expected Utility Analysis

The methods so far describe how good a procedure is. Expected utility
analysis asks what should be *done* about it, and it is the framework of
Lampinen, Smith and Wells (2019).

The motivating observation is that discriminability measures are silent
about consequences. An ROC curve treats a missed identification of a
guilty suspect and the identification of an innocent one as two kinds of
error, without saying how much worse one is than the other. But everyone
agrees they are not equally bad, and once that is admitted, the question
of which procedure is preferable cannot be settled without saying by how
much. Lampinen et al. show that ROC analysis and expected utility
analysis can rank the same two procedures differently — the dissociation
their title refers to — so the choice of framework is not a
technicality.

### Define Utility Matrix

The costs and benefits must be stated explicitly. That is often felt to
be the uncomfortable part of the exercise, but the values are being
assumed either way; utility analysis only makes them visible and open to
argument.

Four outcomes need a value, on any common scale:

``` r

utility_matrix <- c(
  tp =  1.0,   # Correct identification of a guilty suspect: a benefit
  fn = -0.5,   # Failing to identify a guilty suspect: a cost
  fp = -2.0,   # Identifying an innocent suspect: the most serious cost
  tn =  0.5    # Correctly declining to identify an innocent suspect: a benefit
)
```

The relative magnitudes carry the argument. Here a wrongful
identification is set at four times the cost of a missed one (`-2.0`
against `-0.5`), and twice the absolute value of a correct
identification’s benefit. That is a moral and legal judgement, not a
statistical one, and it is the natural place for disagreement. Someone
who set `fp = -10` would be expressing Blackstone’s ratio; the analysis
below would then recommend a considerably stricter confidence threshold.
Because the choice matters this much, it is worth reporting the
sensitivity of the conclusion to it, which is what the base-rate section
does for the other key assumption.

### Compute Utility Curves

``` r
# Compute expected utility across confidence criteria
util_result <- make_utility_curves(
  lineup_example,
  base_rate = 0.5,  # Assume 50% guilty suspects
  utility_matrix = utility_matrix,
  lineup_size = 6
)

print(util_result)
$utility_data
 [38;5;246m# A tibble: 10 × 6 [39m
   criterion hit_rate false_alarm_rate expected_utility n_ids_tp n_ids_ta
        [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m             [3m [38;5;246m<dbl> [39m [23m             [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m 1 [39m       100   0.097 [4m1 [24m          0                 0.072 [4m9 [24m       68        0
 [38;5;250m 2 [39m        90   0.143           0.005 [4m7 [24m [4m1 [24m           0.1         100        4
 [38;5;250m 3 [39m        80   0.189           0.014 [4m3 [24m            0.124       132       10
 [38;5;250m 4 [39m        70   0.26            0.022 [4m9 [24m            0.166       182       16
 [38;5;250m 5 [39m        60   0.351           0.03              0.226       246       21
 [38;5;250m 6 [39m        50   0.436           0.054 [4m3 [24m            0.259       305       38
 [38;5;250m 7 [39m        40   0.507           0.077 [4m1 [24m            0.284       355       54
 [38;5;250m 8 [39m        30   0.556           0.103             0.288       389       72
 [38;5;250m 9 [39m        20   0.593           0.126             0.288       415       88
 [38;5;250m10 [39m        19   0               0                 0             0        0

$max_utility
$max_utility$expected_utility
       tp 
0.2882143 

$max_utility$criterion
[1] 30

$max_utility$hit_rate
[1] 0.5557143

$max_utility$false_alarm_rate
[1] 0.1028571


$avg_utility
[1] 0.2008333

$utility_all_ids
    tp 
0.2875 

$base_rate
[1] 0.5

$utility_matrix
  tp   fn   fp   tn 
 1.0 -0.5 -2.0  0.5 

$n_target_present
[1] 700

$n_target_absent
[1] 700

$innocent_suspect_method
[1] "designated"

$criteria
[1] "confidence"

# Plot utility curves
plot_utility_curves(util_result)
```

![](calibration_decision_analysis_files/figure-html/utility-curves-1.png)

The plot shows:

- **Expected utility** at each confidence threshold
- **Optimal threshold**: Maximum utility point
- **Comparison with “all identifications”** strategy

Reading it from right to left: a very strict threshold acts on almost
nothing, so little is gained and little is risked. Relaxing the
threshold admits identifications that are mostly correct, and utility
climbs. Past the optimum, each further relaxation admits more wrongful
identifications than correct ones, and because those carry four times
the weight, utility falls away again. The maximum is the criterion at
which the marginal identification is exactly worth acting on.

This is the shape Lampinen et al. (2019) describe. Note that the curve
is usually rather flat near its peak, which is reassuring for practice:
thresholds somewhat either side of the optimum lose very little, so the
recommendation does not depend on locating the maximum precisely.

### Compare Procedures

`max_utility` is a list rather than a single number: it reports the best
expected utility together with the criterion that achieves it, and the
hit and false-alarm rates at that criterion. Ask for the element you
want.

``` r

# Compare the two procedures simulated in the BREE section above
util_proc_a <- make_utility_curves(data_proc_a, base_rate = 0.5,
                                   utility_matrix = utility_matrix)
util_proc_b <- make_utility_curves(data_proc_b, base_rate = 0.5,
                                   utility_matrix = utility_matrix)

knitr::kable(
  data.frame(
    Procedure = c("A (d' = 1.8)", "B (d' = 1.1)"),
    `Max expected utility` = round(c(util_proc_a$max_utility$expected_utility,
                                     util_proc_b$max_utility$expected_utility), 3),
    `Optimal criterion` = c(util_proc_a$max_utility$criterion,
                            util_proc_b$max_utility$criterion),
    check.names = FALSE
  ),
  caption = "Expected utility at the optimal confidence criterion, simulated data (200 target-present and 200 target-absent lineups per procedure)."
)
```

| Procedure    | Max expected utility | Optimal criterion |
|:-------------|---------------------:|------------------:|
| A (d’ = 1.8) |                0.367 |                40 |
| B (d’ = 1.1) |                0.185 |                30 |

Expected utility at the optimal confidence criterion, simulated data
(200 target-present and 200 target-absent lineups per procedure).
{.table}

The procedure with the stronger evidence supports both a higher expected
utility and a stricter criterion: it can afford to wait for
high-confidence identifications because it gets more of them.

### Sensitivity to Base Rate

Examine how utility depends on base rate:

``` r

# Test multiple base rates
base_rates <- c(0.2, 0.5, 0.8)

sensitivity <- do.call(rbind, lapply(base_rates, function(br) {
  util <- make_utility_curves(lineup_example, base_rate = br,
                              utility_matrix = utility_matrix,
                              lineup_size = 6)
  data.frame(
    `Base rate` = br,
    `Max expected utility` = round(util$max_utility$expected_utility, 3),
    `Optimal criterion` = util$max_utility$criterion,
    check.names = FALSE
  )
}))

knitr::kable(
  sensitivity,
  row.names = FALSE,
  caption = "Sensitivity of expected utility to the assumed base rate, lineup_example data."
)
```

| Base rate | Max expected utility | Optimal criterion |
|----------:|---------------------:|------------------:|
|       0.2 |                0.345 |                60 |
|       0.5 |                0.288 |                30 |
|       0.8 |                0.349 |                20 |

Sensitivity of expected utility to the assumed base rate, lineup_example
data. {.table}

Base rate matters a great deal. When guilty suspects are rare, a false
identification is both more likely and more costly relative to the
benefit of a correct one, so the optimal criterion becomes stricter and
the attainable utility falls.

## 4. Deviation from Perfect Performance (DPP)

DPP quantifies how far an ROC curve deviates from perfect performance,
and it is robust to ROC truncation — that is, to witnesses not using the
full confidence scale.

It was proposed by Smith, Lampinen, Wells, Smalarz and Mackovichova
(2019) as a replacement for partial area under the ROC curve, and the
reason is worth stating because it is contested. Lineup ROC curves
cannot be traced over their full range: the observed false-alarm rate
only extends as far as witnesses’ willingness to choose takes it, and
that endpoint differs between procedures. pAUC handles this by
integrating over a restricted region, but the choice of region is then
itself a problem. Take the region from the procedure with the shorter
curve and information from the other is discarded; take a common region
and the comparison is made somewhere neither procedure was actually
operating. Smith et al. argue that pAUC consequently fails to track
which procedure has greater diagnostic utility, and that DPP does.

DPP takes a different approach: rather than integrating over a region,
it measures how far the observed ROC falls short of the best curve
attainable given the false-alarm rate the procedure actually reached.
Because the comparison is made against a benchmark defined by the data
themselves, curves of different lengths remain comparable.

This is an active disagreement rather than settled ground, and the pAUC
vignette takes up the other side of it.

### Basic DPP

``` r
# Compute DPP
dpp_result <- make_dpp(
  lineup_example,
  lineup_size = 6
)

print(dpp_result)
$dpp
[1] 0.5757224

$auc_observed
[1] 0.05333776

$auc_perfect
[1] 0.1257143

$auc_gap
[1] 0.07237653

$roc_data
 [38;5;246m# A tibble: 10 × 5 [39m
   confidence correct_id_rate false_id_rate n_correct_ids n_false_ids
         [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m          [3m [38;5;246m<dbl> [39m [23m          [3m [38;5;246m<dbl> [39m [23m        [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m 1 [39m         19          0            0                   0           0
 [38;5;250m 2 [39m        100          0.097 [4m1 [24m       0                  68           0
 [38;5;250m 3 [39m         90          0.143        0.005 [4m7 [24m [4m1 [24m           100           4
 [38;5;250m 4 [39m         80          0.189        0.014 [4m3 [24m            132          10
 [38;5;250m 5 [39m         70          0.26         0.022 [4m9 [24m            182          16
 [38;5;250m 6 [39m         60          0.351        0.03              246          21
 [38;5;250m 7 [39m         50          0.436        0.054 [4m3 [24m            305          38
 [38;5;250m 8 [39m         40          0.507        0.077 [4m1 [24m            355          54
 [38;5;250m 9 [39m         30          0.556        0.103             389          72
 [38;5;250m10 [39m         20          0.593        0.126             415          88

$perfect_roc
 [38;5;246m# A tibble: 3 × 2 [39m
  false_id_rate correct_id_rate
           [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m         0                   0
 [38;5;250m2 [39m         0                   1
 [38;5;250m3 [39m         0.126               1

$max_fa
[1] 0.1257143

$n_target_present
[1] 700

$n_target_absent
[1] 700

# Plot ROC with perfect performance comparison
plot_dpp(dpp_result)
```

![](calibration_decision_analysis_files/figure-html/dpp-1.png)

Interpretation:

- **DPP = 0**: Perfect performance
- **DPP = 1**: Chance performance
- **DPP \< 0**: Better than perfect (impossible, indicates error)

The plot shows:

- **Black curve**: Observed ROC
- **Red dashed line**: Perfect ROC given the maximum false alarm rate
- **Shaded area**: Deviation from perfect performance

The shaded area is the quantity of interest, and it has a concrete
reading: it is the improvement that would be available if this procedure
extracted everything its witnesses had to give, without their choosing
any more often than they already do. A large shaded area means the
procedure is leaving discriminability unclaimed; a small one means the
procedure is close to the best that could be done at that rate of
choosing.

### Compare DPP Between Groups

The comparison is the usual use. Here the two simulated procedures from
earlier differ in the strength of the evidence they generate, so
Procedure A should show the smaller deviation:

``` r
dpp_comparison <- compare_dpp(
  data_proc_a,
  data_proc_b,
  lineup_size = 6
)

print(dpp_comparison)
$dpp_a
[1] 0.4591228

$dpp_b
[1] 0.7065426

$dpp_difference
[1] -0.2474197

$dpp_obj_a
$dpp_obj_a$dpp
[1] 0.4591228

$dpp_obj_a$auc_observed
[1] 0.077075

$dpp_obj_a$auc_perfect
[1] 0.1425

$dpp_obj_a$auc_gap
[1] 0.065425

$dpp_obj_a$roc_data
 [38;5;246m# A tibble: 10 × 5 [39m
   confidence correct_id_rate false_id_rate n_correct_ids n_false_ids
         [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m          [3m [38;5;246m<dbl> [39m [23m          [3m [38;5;246m<dbl> [39m [23m        [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m 1 [39m         19           0            0                  0           0
 [38;5;250m 2 [39m        100           0.16         0                 64           0
 [38;5;250m 3 [39m         90           0.215        0.005             86           2
 [38;5;250m 4 [39m         80           0.302        0.012 [4m5 [24m           121           5
 [38;5;250m 5 [39m         70           0.372        0.025            149          10
 [38;5;250m 6 [39m         60           0.462        0.032 [4m5 [24m           185          13
 [38;5;250m 7 [39m         50           0.52         0.06             208          24
 [38;5;250m 8 [39m         40           0.628        0.082 [4m5 [24m           251          33
 [38;5;250m 9 [39m         30           0.68         0.115            272          46
 [38;5;250m10 [39m         20           0.715        0.142            286          57

$dpp_obj_a$perfect_roc
 [38;5;246m# A tibble: 3 × 2 [39m
  false_id_rate correct_id_rate
           [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m         0                   0
 [38;5;250m2 [39m         0                   1
 [38;5;250m3 [39m         0.142               1

$dpp_obj_a$max_fa
[1] 0.1425

$dpp_obj_a$n_target_present
[1] 400

$dpp_obj_a$n_target_absent
[1] 400


$dpp_obj_b
$dpp_obj_b$dpp
[1] 0.7065426

$dpp_obj_b$auc_observed
[1] 0.03448125

$dpp_obj_b$auc_perfect
[1] 0.1175

$dpp_obj_b$auc_gap
[1] 0.08301875

$dpp_obj_b$roc_data
 [38;5;246m# A tibble: 10 × 5 [39m
   confidence correct_id_rate false_id_rate n_correct_ids n_false_ids
         [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m          [3m [38;5;246m<dbl> [39m [23m          [3m [38;5;246m<dbl> [39m [23m        [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m 1 [39m         19          0             0                  0           0
 [38;5;250m 2 [39m        100          0.037 [4m5 [24m        0                 15           0
 [38;5;250m 3 [39m         90          0.055         0                 22           0
 [38;5;250m 4 [39m         80          0.085         0.005             34           2
 [38;5;250m 5 [39m         70          0.128         0.015             51           6
 [38;5;250m 6 [39m         60          0.198         0.027 [4m5 [24m            79          11
 [38;5;250m 7 [39m         50          0.272         0.047 [4m5 [24m           109          19
 [38;5;250m 8 [39m         40          0.338         0.057 [4m5 [24m           135          23
 [38;5;250m 9 [39m         30          0.392         0.087 [4m5 [24m           157          35
 [38;5;250m10 [39m         20          0.43          0.118            172          47

$dpp_obj_b$perfect_roc
 [38;5;246m# A tibble: 3 × 2 [39m
  false_id_rate correct_id_rate
           [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m         0                   0
 [38;5;250m2 [39m         0                   1
 [38;5;250m3 [39m         0.118               1

$dpp_obj_b$max_fa
[1] 0.1175

$dpp_obj_b$n_target_present
[1] 400

$dpp_obj_b$n_target_absent
[1] 400

# Plot comparison side-by-side
plot_dpp_comparison(dpp_comparison)
```

![](calibration_decision_analysis_files/figure-html/dpp-comparison-1.png)

### When to Use DPP

Use DPP when:

- You have confidence-based ROC data
- ROC curves are truncated (witnesses may not use full scale)
- You want a single performance metric comparable across studies
- You need to compare procedures with different confidence distributions

## 5. ANRI: Adjusted Normalized Resolution Index

ANRI corrects NRI for small-sample bias, making it more appropriate for
hypothesis testing and group comparisons.

Resolution is the third of the properties confidence can have, alongside
calibration and over/underconfidence. A witness could be poorly
calibrated — systematically over- or underconfident — and yet have
excellent resolution, if their confidence still ranked correct
identifications above incorrect ones. Resolution is arguably the
property that matters most in practice, since a systematic bias can be
corrected for once known, whereas an inability to discriminate cannot.

NRI (Yaniv, Yates & Smith, 1991) measures this, but it is **positively
biased**: some apparent resolution arises from sampling noise alone.
Splitting a sample into more confidence bins gives noise more
opportunity to look like signal, so the bias grows with the number of
bins and shrinks with sample size. Two studies using different numbers
of bins therefore cannot have their NRIs compared directly. ANRI
estimates and removes that bias.

### Basic ANRI

``` r
# Compute ANRI
anri_result <- compute_anri(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100)
)

print(anri_result)
$anri
[1] 0.04145948

$nri
[1] 0.04527077

$n_total
[1] 503

$n_bins
[1] 3

$calibration_data
 [38;5;246m# A tibble: 3 × 7 [39m
  bin          n mean_confidence accuracy n_correct n_incorrect
   [3m [38;5;246m<chr> [39m [23m     [3m [38;5;246m<dbl> [39m [23m            [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m      [3m [38;5;246m<dbl> [39m [23m        [3m [38;5;246m<dbl> [39m [23m
 [38;5;250m1 [39m [0,60]     305            42.6    0.764       233          72
 [38;5;250m2 [39m (60,80]     94            74.0    0.872        82          12
 [38;5;250m3 [39m (80,100]   104            96.5    0.962       100           4
 [38;5;246m# ℹ 1 more variable: mean_confidence_prop <dbl> [39m

$overall_accuracy
[1] 0.8250497

$overall_confidence
[1] 0.5960239

$choosers_only
[1] TRUE
```

ANRI is always ≤ NRI because it removes positive bias. The bias
correction is larger when:

- Sample size N is small
- Number of bins J is large
- The ratio J/N is high

### Bootstrap Confidence Intervals

A point estimate is not much use on its own, so the interval comes from
a bootstrap. `n_bootstrap = 100` keeps this vignette quick; use 2000 or
more for anything you intend to report.

``` r

anri_boot <- bootstrap_anri(
  lineup_example,
  confidence_bins = c(0, 60, 80, 100),
  n_bootstrap = 100,
  conf_level = 0.95,
  seed = 123
)

knitr::kable(
  data.frame(
    Statistic = c("NRI (uncorrected)", "ANRI (bias-corrected)"),
    Estimate  = round(c(anri_boot$nri, anri_boot$anri), 3),
    `95% CI`  = c("—", sprintf("[%.3f, %.3f]",
                               anri_boot$ci_lower, anri_boot$ci_upper)),
    check.names = FALSE
  ),
  row.names = FALSE,
  caption = "Resolution with bootstrap interval, lineup_example, 3 confidence bins."
)
```

| Statistic             | Estimate | 95% CI           |
|:----------------------|---------:|:-----------------|
| NRI (uncorrected)     |    0.045 | —                |
| ANRI (bias-corrected) |    0.041 | \[0.021, 0.076\] |

Resolution with bootstrap interval, lineup_example, 3 confidence bins.
{.table}

### Compare Groups

The comparison uses the two simulated procedures, which differ in the
strength of the evidence they generate. Because they were simulated with
genuinely different parameters, any difference found here reflects that
design rather than chance group assignment.

``` r
anri_comparison <- compare_anri(
  procedures,
  group_var = "procedure",
  confidence_bins = c(0, 60, 80, 100),
  n_bootstrap = 100,
  seed = 456
)

print(anri_comparison)
$anri_group1
[1] 0.06778469

$anri_group2
[1] 0.02682759

$nri_group1
[1] 0.07322035

$nri_group2
[1] 0.03571501

$difference
[1] 0.04095711

$ci_lower
[1] -0.008108478

$ci_upper
[1] 0.08953339

$conf_level
[1] 0.95

$significant
 2.5% 
FALSE 

$group_names
[1] "Procedure A" "Procedure B"

$n_bootstrap
[1] 100

$bootstrap_results_group1
$bootstrap_results_group1$anri
[1] 0.06778469

$bootstrap_results_group1$nri
[1] 0.07322035

$bootstrap_results_group1$ci_lower
[1] 0.0336087

$bootstrap_results_group1$ci_upper
[1] 0.1164634

$bootstrap_results_group1$conf_level
[1] 0.95

$bootstrap_results_group1$n_bootstrap
[1] 100

$bootstrap_results_group1$n_total
[1] 343

$bootstrap_results_group1$n_bins
[1] 3

$bootstrap_results_group1$bootstrap_distribution
  [1] 0.03792035 0.10230640 0.07128750 0.09972424 0.03213718 0.06769488
  [7] 0.05146193 0.08428818 0.05697483 0.06450267 0.04643998 0.05204378
 [13] 0.04052060 0.05626850 0.08046377 0.05508151 0.07542900 0.05429258
 [19] 0.03339433 0.03949982 0.03314501 0.12294947 0.06149616 0.08899741
 [25] 0.08887571 0.05678356 0.08559018 0.08993446 0.03384564 0.07501329
 [31] 0.10901750 0.03843067 0.07237962 0.04681311 0.08234473 0.07169593
 [37] 0.05393085 0.04605020 0.04416228 0.08616716 0.05979818 0.07163163
 [43] 0.06592243 0.06227943 0.05380252 0.08467802 0.05825533 0.03794652
 [49] 0.04897432 0.05309413 0.05712630 0.11953708 0.10506389 0.09455008
 [55] 0.07545982 0.07001821 0.07813977 0.07755299 0.07582407 0.06920125
 [61] 0.05581948 0.09070718 0.11306627 0.08195092 0.05373707 0.07570843
 [67] 0.05920084 0.04019088 0.06619352 0.06693720 0.10473087 0.05404773
 [73] 0.07227918 0.05755365 0.06346656 0.05302063 0.06614226 0.07395793
 [79] 0.08212438 0.04497331 0.11215098 0.07525709 0.08608872 0.09973898
 [85] 0.12193547 0.05719782 0.07908794 0.05332117 0.05546346 0.07763277
 [91] 0.05515692 0.09137111 0.05421248 0.05186790 0.06845404 0.08821952
 [97] 0.08745584 0.07771619 0.06039380 0.09771786


$bootstrap_results_group2
$bootstrap_results_group2$anri
[1] 0.02682759

$bootstrap_results_group2$nri
[1] 0.03571501

$bootstrap_results_group2$ci_lower
[1] 0.01183448

$bootstrap_results_group2$ci_upper
[1] 0.0666507

$bootstrap_results_group2$conf_level
[1] 0.95

$bootstrap_results_group2$n_bootstrap
[1] 100

$bootstrap_results_group2$n_total
[1] 219

$bootstrap_results_group2$n_bins
[1] 3

$bootstrap_results_group2$bootstrap_distribution
  [1] 0.02827003 0.04079840 0.02517238 0.03062012 0.02181972 0.01049838
  [7] 0.01899978 0.03036700 0.02552104 0.02271060 0.05713917 0.02578298
 [13] 0.03016016 0.02054713 0.03918945 0.02557476 0.06779379 0.03874987
 [19] 0.01051332 0.03233665 0.01733051 0.03296402 0.02803529 0.03895519
 [25] 0.03858131 0.01528382 0.03298276 0.01736324 0.03464778 0.02914002
 [31] 0.03573929 0.01157663 0.04527195 0.05739101 0.04756280 0.04921251
 [37] 0.04395061 0.02516586 0.01504147 0.02946546 0.03826375 0.01386205
 [43] 0.05332499 0.01690167 0.01447765 0.05670686 0.02829661 0.03629397
 [49] 0.05534675 0.02370688 0.01755271 0.02891908 0.01502363 0.02507785
 [55] 0.03040495 0.02453349 0.04079696 0.02576999 0.05562779 0.01936541
 [61] 0.03889947 0.01721450 0.04392177 0.02245940 0.03280615 0.03121139
 [67] 0.03159972 0.04362042 0.01838025 0.05005197 0.04072979 0.02646716
 [73] 0.02649673 0.05504240 0.01767153 0.02731134 0.02837761 0.03833251
 [79] 0.07664124 0.01767752 0.04196244 0.06601501 0.04125118 0.03145147
 [85] 0.03290173 0.03288050 0.02903352 0.02191484 0.03724168 0.02525612
 [91] 0.06442027 0.02237895 0.02130252 0.01918565 0.01211948 0.01694000
 [97] 0.05328292 0.02129131 0.06722584 0.01440498


$difference_distribution
  [1]  0.009650324  0.061508003  0.046115121  0.069104122  0.010317458
  [6]  0.057196503  0.032462146  0.053921182  0.031453794  0.041792070
 [11] -0.010699198  0.026260792  0.010360434  0.035721373  0.041274323
 [16]  0.029506748  0.007635216  0.015542702  0.022881012  0.007163168
 [21]  0.015814504  0.089985455  0.033460873  0.050042216  0.050294396
 [26]  0.041499746  0.052607411  0.072571219 -0.000802140  0.045873268
 [31]  0.073278205  0.026854038  0.027107675 -0.010577897  0.034781935
 [36]  0.022483411  0.009980238  0.020884337  0.029120813  0.056701703
 [41]  0.021534433  0.057769577  0.012597441  0.045377767  0.039324873
 [46]  0.027971162  0.029958720  0.001652550 -0.006372430  0.029387246
 [51]  0.039573589  0.090618006  0.090040259  0.069472226  0.045054866
 [56]  0.045484712  0.037342808  0.051783007  0.020196282  0.049835837
 [61]  0.016920011  0.073492679  0.069144496  0.059491524  0.020930921
 [66]  0.044497036  0.027601119 -0.003429540  0.047813274  0.016885229
 [71]  0.064001073  0.027580575  0.045782455  0.002511241  0.045795035
 [76]  0.025709292  0.037764650  0.035625420  0.005483140  0.027295784
 [81]  0.070188537  0.009242086  0.044837538  0.068287508  0.089033735
 [86]  0.024317320  0.050054417  0.031406323  0.018221782  0.052376654
 [91] -0.009263345  0.068992157  0.032909964  0.032682244  0.056334568
 [96]  0.071279519  0.034172918  0.056424889 -0.006832047  0.083312884

plot_anri_comparison(anri_comparison)
```

![](calibration_decision_analysis_files/figure-html/anri-comparison-1.png)

The comparison provides:

- **Point estimates** for each group
- **Difference with CI**: Tests whether groups differ
- **Significance test**: CI excludes 0 = significant difference

The bootstrap difference distribution is available as
`plot_anri_difference_distribution(anri_comparison)` if you want to
inspect its shape — worth doing when an interval looks surprising, since
a heavily skewed or multimodal distribution is a sign the bins are too
thin somewhere.

### When to Use ANRI vs NRI

The considerations, rather than rules:

ANRI is the safer default whenever the bias it corrects could be
material — modest samples (say a few hundred or fewer), several
confidence bins, or any comparison across studies that binned confidence
differently. Because the bias grows with the bin-to-sample ratio, it is
exactly the studies most likely to be compared informally that are least
comparable on raw NRI. Formal tests should use ANRI, since testing a
biased estimate against zero tests the wrong thing.

NRI remains reasonable when the correction is negligible — large samples
with few bins, where ANRI and NRI will agree closely anyway — or when
the point is to compare with older published work that reported NRI. In
the analysis above the two differ by very little, which is what a sample
of this size should produce.

Reporting both, as the table above does, costs nothing and lets a reader
see the size of the correction for themselves.

## Integrating Multiple Methods

For a comprehensive analysis, combine these methods:

Each method answers a different question, so a full analysis runs
several. The five calls below use `lineup_example`; substitute your own
data frame.

``` r

bins <- c(0, 60, 80, 100)

# 1. Calibration: does confidence mean what it says?
cal <- make_calibration(lineup_example, confidence_bins = bins,
                        show_plot = FALSE)

# 2. DPP: overall discriminability
dpp <- make_dpp(lineup_example, lineup_size = 6)

# 3. ANRI: resolution, with inference
anri <- bootstrap_anri(lineup_example, confidence_bins = bins,
                       n_bootstrap = 100, seed = 789)

# 4. Utility: what should be done about it?
util <- make_utility_curves(lineup_example, base_rate = 0.5,
                            utility_matrix = utility_matrix,
                            lineup_size = 6)

# 5. Bayesian: what is an identification worth as evidence?
bayes <- make_bayes_curves(lineup_example, response_categories = "confidence",
                           confidence_bins = bins)

knitr::kable(
  data.frame(
    Method   = c("Calibration (C)", "Over/underconfidence (O/U)",
                 "DPP", "ANRI", "Max expected utility"),
    Estimate = round(c(cal$C, cal$OU, dpp$dpp, anri$anri,
                       util$max_utility$expected_utility), 3)
  ),
  row.names = FALSE,
  caption = "Summary across all five methods, lineup_example."
)
```

| Method                     | Estimate |
|:---------------------------|---------:|
| Calibration (C)            |    0.073 |
| Over/underconfidence (O/U) |   -0.229 |
| DPP                        |    0.576 |
| ANRI                       |    0.041 |
| Max expected utility       |    0.288 |

Summary across all five methods, lineup_example. {.table}

## Reporting Results

### Example Write-Up

Every number below is computed from the objects created above rather
than typed in, so this passage cannot drift out of step with the
analysis it describes. The same discipline is worth adopting in a
manuscript.

> We analysed identification performance using calibration analysis,
> deviation from perfect performance (DPP), and the adjusted normalized
> resolution index (ANRI) with bootstrap inference, using 1400 lineups
> of size 6 (700 target-present, 700 target-absent).
>
> Witnesses were well calibrated overall (C = 0.073), but systematically
> **underconfident** (O/U = -0.229): accuracy in the lowest confidence
> band was substantially higher than stated confidence implied. DPP was
> 0.576, indicating performance above chance but well short of what was
> attainable at the observed rate of choosing. Resolution, measured by
> ANRI, was 0.041 (95% CI \[0.02, 0.077\]), against an uncorrected NRI
> of 0.045.
>
> Expected utility analysis assumed a base rate of 50% and the cost
> matrix u(correct suspect ID) = 1, u(missed guilty suspect) = -0.5,
> u(innocent suspect ID) = -2, u(correct rejection) = 0.5; that is, a
> wrongful identification was weighted four times as heavily as a missed
> one. Under those assumptions a confidence criterion of 30 maximised
> expected utility (EU = 0.288).
>
> Bayesian analysis showed that high-confidence suspect identifications
> were the most informative response, with a likelihood ratio of 25
> (based on 100 guilty-suspect and 4 innocent-suspect identifications in
> that band).

Note the last parenthesis. The likelihood ratio there rests on very few
target-absent observations, and reporting the counts alongside the ratio
is what allows a reader to judge how much weight it will bear.

## References

**Calibration:**

- Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and
  diagnosticity of confidence in eyewitness identification: Comments on
  what can be inferred from the low confidence-accuracy correlation.
  *Journal of Experimental Psychology: Learning, Memory, and Cognition,
  22*(5), 1304-1316.
- Brewer, N., & Wells, G. L. (2006). The confidence-accuracy
  relationship in eyewitness identification: Effects of lineup
  instructions, foil similarity, and target-absent base rates. *Journal
  of Experimental Psychology: Applied, 12*(1), 11-30.

**Bayesian information gain and BREE:**

- Wells, G. L., Yang, Y., & Smalarz, L. (2015). Eyewitness
  identification: Bayesian information gain, base-rate
  effect-equivalency curves, and reasonable suspicion. *Law and Human
  Behavior, 39*(2), 99-122.

**Expected Utility:**

- Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities
  in eyewitness identification practice: Dissociations between receiver
  operating characteristic (ROC) analysis and expected utility analysis.
  *Law and Human Behavior, 43*(1), 26-44.

**DPP:**

- Smith, A. M., Lampinen, J. M., Wells, G. L., Smalarz, L., &
  Mackovichova, S. (2019). Deviation from perfect performance measures
  the diagnostic utility of eyewitness lineups but partial area under
  the ROC curve does not. *Journal of Applied Research in Memory and
  Cognition, 8*(1), 50-59.

**ANRI:**

- Yaniv, I., Yates, J. F., & Smith, J. E. K. (1991). Measures of
  discrimination skill in probabilistic judgment. *Psychological
  Bulletin, 110*(3), 611-617.

**Related, on CAC analysis:**

- Mickes, L. (2015). Receiver operating characteristic analysis and
  confidence-accuracy characteristic analysis in investigations of
  system variables and estimator variables that affect eyewitness
  memory. *Journal of Applied Research in Memory and Cognition, 4*(2),
  93-102.

## Session Info

``` r
sessionInfo()
R version 4.6.1 (2026-06-24)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.5 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

locale:
 [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
 [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
 [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
[10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   

time zone: UTC
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] r4lineups_2.2.0

loaded via a namespace (and not attached):
 [1] Matrix_1.7-5       gtable_0.3.6       jsonlite_2.0.0     dplyr_1.2.1       
 [5] compiler_4.6.1     tidyselect_1.2.1   Rcpp_1.1.2         gridExtra_2.3.1   
 [9] jquerylib_0.1.4    png_0.1-9          systemfonts_1.3.2  scales_1.4.0      
[13] textshaping_1.0.5  boot_1.3-32        yaml_2.3.12        fastmap_1.2.0     
[17] lattice_0.22-9     reticulate_1.47.0  ggplot2_4.0.3      R6_2.6.1          
[21] patchwork_1.3.2    labeling_0.4.3     generics_0.1.4     knitr_1.52        
[25] htmlwidgets_1.6.4  ggrepel_0.9.8      tibble_3.3.1       desc_1.4.3        
[29] bslib_0.12.0       pillar_1.11.1      RColorBrewer_1.1-3 rlang_1.3.0       
[33] utf8_1.2.6         cachem_1.1.0       xfun_0.62          fs_2.1.0          
[37] sass_0.4.10        S7_0.2.2           otel_0.2.0         cli_3.6.6         
[41] withr_3.0.3        pkgdown_2.2.1      magrittr_2.0.5     digest_0.6.39     
[45] grid_4.6.1         lifecycle_1.0.5    vctrs_0.7.3        evaluate_1.0.5    
[49] glue_1.8.1         farver_2.1.2       ragg_1.5.2         purrr_1.2.2       
[53] rmarkdown_2.32     tools_4.6.1        pkgconfig_2.0.3    htmltools_0.5.9   
```
