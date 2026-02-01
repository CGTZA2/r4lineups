# r4lineups

Statistical tools for assessing lineup fairness, confidence‑accuracy analyses, and face similarity in eyewitness identification research.

## What this package includes

- **Lineup bias & proportions**: target‑position proportions, all‑member proportions, bootstrap CIs
- **Effective size**: E (Tredoux, 1998) and E (Malpass, 1981, adjusted)
- **Functional size & diagnosticity**: functional size, diagnosticity ratios, homogeneity tests
- **ROC / CAC / RAC**: confidence‑accuracy methods for lineup data
- **SDT summary comparisons**: d′/c/β from counts, variance estimates (Miller/Gourevitch/bootstrap), z‑tests
- **Full ROC (Smith & Yang)**: full ROC data and plots
- **EIG & PPV range**: information‑gain and PPV‑range analyses
- **Calibration & ANRI**: calibration metrics, ANRI and bootstrap comparisons
- **Bayesian curves & utility**: prior‑posterior curves, expected utility, DPP
- **Winter 2‑HT model**: model fitting and bootstrap CIs
- **Face similarity**: deep‑learning embeddings, target‑foil similarity, pairwise matrices, plots
- **Simulation**: synthetic lineup data and power analysis

## Installation

This repository is an R package. From R:

```r
# from local source
devtools::install()
```

If you prefer base R:

```r
R CMD INSTALL .
```

## Core data format (confidence‑based analyses)

Functions like ROC/CAC/RAC/EIG/PPV expect a data frame with:

- `target_present` (logical or 0/1)
- `identification` (e.g., "suspect", "filler", "reject")
- `confidence` (numeric)

See the built‑in example `lineup_example` and vignette tables for concrete layouts.

## Quick starts

### Lineup bias and effective size

```r
library(r4lineups)
data(nortje2012)

lineup_vec <- nortje2012$lineup_1
lineup_table <- table(lineup_vec)

# Target proportion (position 3)
lineup_prop_vec(lineup_vec, target_pos = 3, k = 6)

# Effective size (Tredoux)
esize_T(lineup_table)
```

### ROC / CAC

```r
data(lineup_example)
roc <- make_rocdata(lineup_example, lineup_size = 6)
cac <- make_cac(lineup_example, lineup_size = 6, confidence_bins = c(0, 60, 80, 100))
```

### Full ROC

```r
fullroc <- make_fullroc(lineup_example, conf_bins = c(0, 60, 80, 100), lineup_size = 6)
plot_fullroc(fullroc)
```

### Face similarity

```r
install_r4lineups_python()
lineup <- lineup_similarity("target.jpg", c("foil1.jpg", "foil2.jpg"))
plot_lineup_similarity(lineup)
```

## Shiny app

The app bundles key analyses (bias, effective size, ROC/CAC, EIG/PPV, face similarity).

```r
library(r4lineups)
run_r4lineups_app()
```

For deployment, the app lives at `inst/shiny/r4lineups_app`.

## Vignettes

- `Vignette.Rmd` — core lineup fairness measures + ROC basics
- `fullroc_analysis.Rmd` — full ROC (Smith & Yang)
- `calibration_decision_analysis.Rmd` — calibration, Bayesian curves, utility, DPP, ANRI
- `information_error_rate_analysis.Rmd` — EIG + PPV‑range
- `winter_2ht_model.Rmd` — Winter 2‑HT model
- `face_similarity.Rmd` — face similarity walkthrough (in progress)
- `sdt_glm_analysis.Rmd` — SDT via GLM/GLMM (old/new recognition)
- `sdt_summary_comparisons.Rmd` — SDT summary‑level comparisons from counts

## Notes

- `notes/` contains working papers and test scripts and is **not** part of the package build.
- For face similarity, Python dependencies are managed via `install_r4lineups_python()`.
