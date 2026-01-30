# r4lineups codebase summary (for new agents)

## Purpose and scope
r4lineups is an R package for lineup fairness and eyewitness identification analysis. It includes classic lineup fairness metrics (bias, functional size, effective size, diagnosticity), ROC/CAC/full-ROC analyses, PPV-range estimation (nominal/effective/no correction), bootstrapping helpers, and optional face-similarity/embedding tools via Python deepface.

## High-level layout
- `R/` core functions (most logic lives here)
- `data/` example datasets: `line73.rda`, `lineup_example.rda`, `mickwick.rda`, `mockdata.rda`, `nortje2012.rda`
- `man/` roxygen-generated docs
- `vignettes/` a single vignette and images
- `notes/` papers, agent summaries, implementation notes, and working plots/scripts
  - Includes PPV-range, EIG, and Fitzgerald/Winter summaries

## Common data structures used
- **Lineup choice vectors**: numeric vector of lineup member positions (e.g., `lineup_vec`), plus `target_pos` and lineup size `k`.
- **ROC/CAC data frame**: columns
  - `target_present` (TRUE/FALSE)
  - `identification` ("suspect" | "filler" | "reject")
  - `confidence` (numeric)
- **Face similarity inputs**: image paths (strings). Functions assume files exist on disk.

## Main function families (by file)

### Lineup fairness + diagnosticity metrics
- `diag_ratio_T.R`, `diag_ratio_W.R`, `ln_diag_ratio.R`, `diag_param*.R`, `homog_diag*.R`, `chi_diag*.R`, `var_d.R`, `var_lnd.R`
- `func_size*.R`, `effsize_*`, `d_bar.R`, `d_weights.R`, `allprop.R`, `allfoilbias.R`, `allfoil_cis.R`, `eff_size_per_foils.R`, `effsize_compare.R`
- These are classic lineup metrics (diagnosticity ratio, functional size, effective size, d measures) plus bootstrapped CIs.

### ROC / CAC (confidence-accuracy)
- `roc_functions.R` + `make_roc.R`:
  - **Partial ROC** (Wixted & Mickes style), uses suspect IDs and estimates false IDs from fillers.
  - Primary functions: `make_rocdata()`, `make_roc()`, `make_roc_gg()`.
- `cac_functions.R`:
  - **CAC** analysis: `make_cacdata()`, `make_cac()`, `make_cac_gg()`.
  - Accuracy computed per confidence bin from suspect IDs.
- **Full ROC (Smith & Yang 2020)** is already implemented in `fullroc_functions.R`:
  - `make_fullroc_data()`, `make_fullroc()`, `plot_fullroc()`, `print.lineup_fullroc()`
  - Orders decision x confidence categories by diagnosticity or an a-priori order, then cumulative HR/FAR to AUC.

### Bootstrapping utilities
- `boot_helper.R`, `gen_boot_*`, `lineup_*_boot.R`, `compare_eff_sizes.boot.R`, `func_size.boot.R`, `esize_*_boot.R`
- These generate bootstrap samples, CIs, and summary tables for lineup metrics.

### Data validation + utilities
- `typecheck.R`, `datacheck1.R` ... `datacheck5.R`, `tab_to_vec.R`, `lineup_prop_*`.
- Mostly input validation and helper transforms.

### Face similarity / embeddings (Python deepface via reticulate)
- `setup.R`: `install_r4lineups_python()`, `check_python_deps()`, `available_models()`, `available_detectors()`.
- `zzz.R`: reticulate init, module import, accessors.
- `embeddings.R`, `similarity.R`, `landmarks.R`, `visualization_faces.R`: extraction, similarity, landmarks, plots.
- Key functions exported: `get_embedding()`, `face_similarity()`, `lineup_similarity()`, `plot_lineup_similarity()`, `batch_embeddings()`, `embedding_distance()`, `detect_faces()`, `get_face_landmarks()`.

## Existing Smith & Yang (2020) full ROC implementation
- File: `R/fullroc_functions.R`
- Functions: `make_fullroc_data()`, `make_fullroc()`, `plot_fullroc()`
- Inputs: same as ROC/CAC (`target_present`, `identification`, `confidence`), optional confidence binning and ordering method.
- Diagnosticity ordering is default; a-priori ordering is also supported.

---

# Paper summaries for quick onboarding

## Starns et al. (2023) - Expected Information Gain (EIG)
**Goal**: Provide a single, comprehensive measure of the evidentiary value of an identification procedure (lineup/showup), using information theory rather than ROC or diagnosticity ratios.

**Key ideas**
- Treat witness responses as evidence about guilt/innocence.
- Use **entropy** as a proper, equal-interval measure of uncertainty.
- For each response category, compute the **information gain** (IG) as the reduction in entropy from prior to posterior.
- **Expected Information Gain (EIG)** is the average IG across responses, weighted by response probabilities.

**Core quantities**
- Prior probability of guilt: `p(g)` (often 0.5 for lab data; can be set by user).
- Response categories `x` can be decision x confidence bins (e.g., suspect-high, suspect-mid, filler-low, reject-high, etc.).
- Likelihoods: `p(x|g)` and `p(x|i)` from observed frequencies.
- Posterior via Bayes: `p(g|x) = p(x|g)*p(g) / (p(x|g)*p(g) + p(x|i)*(1-p(g)))`.
- Entropy: `H(p) = -[p*log2(p) + (1-p)*log2(1-p)]`.
- Information gain for response `x`: `IG(x) = H(prior) - H(p(g|x))`.
- EIG: `sum_x p(x) * IG(x)` where `p(x) = p(g)*p(x|g) + (1-p(g))*p(x|i)`.

**Implementation notes**
- Handles response sets that differ across procedures (e.g., lineups vs showups).
- Works with confidence bins, continuous predictors (can be discretized), or other response categories.
- Optional: map EIG back to a probability-gain scale (PGEIG) for interpretability (not strictly required).

## Winter et al. (2022) - Two-High Threshold (2-HT) MPT model
**Goal**: Model lineup outcomes with a multinomial processing tree (MPT) that separates detection from guessing/bias processes, using the full 2 x 3 outcome table (culprit-present/absent x suspect/filler/reject).

**Parameters**
- `dP`: detection of culprit presence (culprit-present lineups)
- `dA`: detection of culprit absence (culprit-absent lineups)
- `b`: biased suspect selection (suspect stands out)
- `g`: guessing-based selection among lineup members
- `L`: lineup size (fixed, not estimated)

**Model structure (key outcomes)**
- Culprit-present: suspect IDs can arise from detection (`dP`) or biased/guessing selection when detection fails.
- Culprit-absent: correct rejects arise from `dA`; otherwise biased/guessing processes produce suspect/filler IDs.

**Why it matters**
- Provides latent-process estimates rather than a single accuracy metric.
- Uses fillers and rejections as informative categories rather than discarding them.
- Can be fit via MLE or with MPT tools (MPTinR / multiTree).

---

# Pseudocode for new functions

## EIG for lineup procedures (Starns et al.)

```
function eig_lineup(data, prior_guilt=0.5, conf_bins=NULL):
    # data columns: target_present, identification, confidence

    # 1) Bin confidence if requested
    if conf_bins is not NULL:
        conf_bin = cut(confidence, conf_bins)
    else:
        conf_bin = confidence

    # 2) Define response categories
    # response = (identification, conf_bin)
    categories = all combinations of identification x conf_bin

    # 3) Count outcomes separately for TP and TA
    for each category x:
        n_g[x] = count(target_present==TRUE & response==x)
        n_i[x] = count(target_present==FALSE & response==x)

    # 4) Convert to likelihoods
    p_x_given_g = n_g[x] / sum(n_g)
    p_x_given_i = n_i[x] / sum(n_i)

    # 5) Prior entropy
    H_prior = entropy(prior_guilt)

    # 6) For each response category
    for each category x:
        p_x = prior_guilt * p_x_given_g + (1 - prior_guilt) * p_x_given_i
        if p_x == 0: continue

        # posterior
        p_g_given_x = (prior_guilt * p_x_given_g) / p_x
        H_post = entropy(p_g_given_x)

        IG[x] = H_prior - H_post

    # 7) Expected information gain
    EIG = sum_x p_x * IG[x]

    return list(EIG=EIG, IG_by_response=IG, response_probs=p_x, prior=prior_guilt)
```

Helper:
```
entropy(p):
    if p in {0,1}: return 0
    return -(p*log2(p) + (1-p)*log2(1-p))
```

## 2-HT model fit (Winter et al.)

```
function fit_2ht(counts, lineup_size L, method='ML'):
    # counts contain TP and TA outcome totals
    # TP: n_tp_suspect, n_tp_filler, n_tp_reject
    # TA: n_ta_suspect, n_ta_filler, n_ta_reject

    # Define model-predicted probabilities
    # Culprit-present
    P_tp_suspect = dP + (1-dP) * ( b + (1-b) * g * (1/L) )
    P_tp_filler  = (1-dP) * (1-b) * g * ((L-1)/L)
    P_tp_reject  = (1-dP) * (1-b) * (1-g)

    # Culprit-absent
    P_ta_suspect = (1-dA) * ( b + (1-b) * g * (1/L) )
    P_ta_filler  = (1-dA) * (1-b) * g * ((L-1)/L)
    P_ta_reject  = dA + (1-dA) * (1-b) * (1-g)

    # Log-likelihood
    loglik = sum( n * log(P) ) across the 6 categories

    # Optimize parameters in [0,1]
    use constrained optimizer (e.g., optim with logit transform or L-BFGS-B bounds)

    return estimates (dP, dA, b, g), loglik, fitted_probs
```

Optional extensions:
- Add confidence-binned categories by fitting separate parameter sets per bin (or a hierarchical model).
- Use MPTinR/multiTree for standard goodness-of-fit tests.

