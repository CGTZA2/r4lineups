# Method papers in notes/ — summaries + implementation pseudocode

This file summarizes the PDFs currently in `notes/` and outlines function‑level pseudocode for methods that are not yet fully integrated into r4lineups. It is meant as a handoff for another agent.

---

## 1) Juslin, Olsson, & Winman (1996)
**File**: `notes/1996-05780-017.pdf`  
**Title**: *Calibration and Diagnosticity of Confidence in Eyewitness Identification* (JEP:LMC, 1996)

**Core contribution**
- Argues that **confidence–accuracy correlation** is a poor indicator of reliability; **calibration analysis** is the correct tool.
- Distinguishes **calibration** (match between confidence and accuracy), **resolution** (ability to discriminate correct vs incorrect), and **diagnosticity** (likelihood ratios / posterior odds).
- Shows that **low correlation can coexist with good calibration**.

**Implementation targets**
- Calibration curves + indices: **C** (calibration statistic), **O/U** (over/underconfidence), **NRI** (normalized resolution index), plus Brier‑style accuracy metrics.
- Optional: diagnosticity ratio by confidence bin.

**Pseudocode (calibration metrics)**
```
function calibration_stats(data, conf_var, correct_var, bins):
    # Bin confidence
    conf_bin = cut(conf_var, bins)
    for each bin j:
        n_j = count(bin j)
        acc_j = mean(correct_var in bin j)
        conf_j = mean(conf_var in bin j)
    overall_acc = mean(correct_var)
    overall_conf = mean(conf_var)

    # Calibration (C) — Brier-style reliability component
    C = sum_j (n_j / N) * (conf_j - acc_j)^2

    # Over/underconfidence (O/U)
    OU = overall_conf - overall_acc

    # Resolution (NRI)
    NRI = [ (1/N) * sum_j n_j * (acc_j - overall_acc)^2 ] / [ overall_acc * (1 - overall_acc) ]

    return list(C=C, OU=OU, NRI=NRI, per_bin=table(conf_j, acc_j, n_j))
```

---

## 2) Brewer & Wells (2006)
**File**: `notes/2006-02910-002.pdf`  
**Title**: *The Confidence–Accuracy Relationship in Eyewitness Identification* (JEP:Applied, 2006)

**Core contribution**
- Uses **calibration analysis** (not correlation) to show confidence can be informative.
- Examines **lineup instructions**, **foil similarity**, and **target‑absent base rates**.
- Finds lower overconfidence when base rates are lower and foil similarity higher; confidence collected **immediately** is more diagnostic.

**Implementation targets**
- Calibration curves and statistics for **choosers** vs **all responses**.
- Stratified plots by condition (instructions, foil similarity, base rate).
- Optional: incorporate decision time as a covariate.

**Pseudocode (calibration by condition)**
```
function calibration_by_condition(data, conf_var, correct_var, condition_vars, bins, choosers_only=TRUE):
    if choosers_only:
        data = filter(identification == 'suspect')
    for each condition combo:
        stats = calibration_stats(subset, conf_var, correct_var, bins)
        store stats
    return table of stats + per-condition calibration curves
```

---

## 3) Wixted & Mickes (2012)
**File**: `notes/Wixted-2012-The Field of Eyewitness Memory Sho.pdf`

**Core contribution**
- Argues for **ROC analysis** as the primary tool over diagnosticity ratios.
- Shows how to compute ROC curves from confidence thresholds.

**Status in r4lineups**
- Already implemented (`make_rocdata()`, `make_roc()`, `make_roc_gg()`).

---

## 4) Wells, Yang, & Smalarz (2015)
**File**: `notes/2015-14642-001.pdf`  
**Title**: *Eyewitness Identification: Bayesian Information Gain, Base‑Rate Effect–Equivalency Curves, and Reasonable Suspicion* (LHB, 2015)

**Core contribution**
- Treats lineup outcomes as a **Bayesian updating problem**.
- Introduces:
  - **Prior‑by‑posterior curves**
  - **Information‑gain curves**
  - **Base‑rate effect–equivalency (BREE) curves**
- Explicitly connects results to **reasonable suspicion** thresholds.

**Implementation targets**
- `make_bayes_curves()` returning prior‑posterior and information‑gain curves.
- `make_bree_curve()` to compute base‑rate shift required to match a system‑variable effect.

**Pseudocode (prior‑posterior + info gain)**
```
function bayes_curves(data, response_types, prior_grid):
    # response_types can be suspect ID, filler ID, reject (optionally by confidence bins)
    # Compute p(x|g) and p(x|i) from observed frequencies
    for each x:
        p_x_given_g = count(x | target_present) / n_g
        p_x_given_i = count(x | target_absent) / n_i

    for each prior in prior_grid:
        for each response x:
            p_x = prior*p_x_given_g + (1-prior)*p_x_given_i
            p_g_given_x = (prior*p_x_given_g) / p_x
            posterior[prior, x] = p_g_given_x
            info_gain[prior, x] = entropy(prior) - entropy(p_g_given_x)

    return curves for each response type
```

**Pseudocode (BREE)**
```
function bree_curve(procA, procB, prior_grid):
    # For each prior, compute posterior for a reference response (e.g., suspect ID)
    # Then find base-rate shift delta for procB that yields same posterior as procA
    for each prior in prior_grid:
        postA = posterior(procA, prior)
        find priorB such that posterior(procB, priorB) == postA
        delta = priorB - prior
        store delta
    return prior vs delta curve
```

---

## 5) Lampinen, Smith, & Wells (2019)
**File**: `notes/2018-53589-001.pdf`  
**Title**: *Four Utilities in Eyewitness Identification Practice* (LHB, 2019)

**Core contribution**
- Argues ROC comparisons can be misleading when curves are truncated or tradeoffs differ.
- Defines **four utility types**:
  1) Utility of **all identifications**
  2) Utility of **high‑confidence IDs** only
  3) **Average** utility across criteria
  4) **Maximum** utility (optimal criterion)
- Introduces **utility‑difference curves** to compare procedures under varying base‑rate/cost assumptions.

**Implementation targets**
- `make_utility_curves()` + `plot_utility_difference()`.

**Pseudocode (expected utility)**
```
function expected_utility(data, base_rate, cost_matrix, criteria):
    # compute p(IDS|g), p(IDS|i) at each confidence criterion
    for each criterion c:
        hit = p(IDS | guilty, conf>=c)
        fa  = p(IDS | innocent, conf>=c)
        # expected utility = base_rate*(hit*U_tp + (1-hit)*U_fn)
        #                    + (1-base_rate)*(fa*U_fp + (1-fa)*U_tn)
        EU[c] = ...
    return EU curve
```

**Utility difference curves**
```
function utility_difference(procA, procB, base_rate_grid, cost_matrix):
    for each base_rate:
        EU_A = max_or_avg(EU(procA))
        EU_B = max_or_avg(EU(procB))
        diff = EU_A - EU_B
    return base_rate vs diff
```

---

## 6) Smith et al. — Deviation from Perfect Performance (DPP)
**File**: `notes/2019-09429-001.pdf`  
**Title**: *Deviation from Perfect Performance Measures the Diagnostic Utility of Eyewitness Lineups but Partial Area Under the ROC* (JARMAC, 2018/2019)

**Core contribution**
- Shows **pAUC can mis-rank procedures** when ROC curves are differentially truncated.
- Introduces **DPP**, which compares observed ROC performance to a perfect-performance benchmark and is less confounded by confidence distributions.

**Implementation targets**
- `make_dpp()` + `plot_dpp()`.

**Pseudocode (DPP – placeholder until formula confirmed)**
```
function dpp_from_roc(roc_points):
    # roc_points: ordered by false-alarm rate
    # perfect ROC: y=1 for x>0 (or boundary of achievable performance within same x-range)
    # DPP often defined as: area_between(perfect, observed) / area_under(perfect)
    A_obs = trapezoid_area(roc_points)
    A_perf = trapezoid_area(perfect_curve_over_same_x_range)
    DPP = 1 - (A_obs / A_perf)
    return DPP
```
**Note**: confirm exact formula and treatment of truncated ROC in the paper before finalizing.

---

## 7) Winter et al. (2022)
**File**: `notes/Winter - 2022 - Experimental validation of a m.pdf`  
**Title**: *Experimental validation of a multinomial processing tree model for analyzing eyewitness identification decisions* (Scientific Reports, 2022)

**Core contribution**
- Introduces **2‑HT model** with parameters dP, dA, b, g.
- Uses full 2×3 outcome structure (suspect/filler/reject × culprit present/absent).

**Implementation targets**
- `fit_2ht()` and `plot_2ht_params()` (already drafted elsewhere in notes).

**Pseudocode** (see `WINTER_2HT_IMPLEMENTATION_SUMMARY.md`).

---

## 8) Fitzgerald, Tredoux, & Juncu (2023)
**File**: `notes/fitzgerald_et_al_2023_LHB.pdf`  
**Title**: *Estimation of Eyewitness Error Rates in Fair and Biased Lineups* (LHB, 2023)

**Core contribution**
- Compares **nominal size correction** vs **effective size correction** for estimating innocent‑suspect misidentification risk.
- Recommends reporting the **PPV range** (nominal ↔ effective ↔ no correction).

**Implementation targets**
- `ppv_range_by_confidence()` + `plot_ppv_range()` (see PPV notes).

---

## 9) Calibration code in `notes/tredcode.txt`
Your script implements **ANRI/ANDI** (Yaniv et al., 1991) and bootstrap CIs for differences between groups.

**Suggested extraction**
- Convert to `anri()` + `anri_boot()` functions with standardized input schema.
- Provide `plot_anri()` for group comparisons.

**Pseudocode (ANRI)**
```
function anri(data, conf_var, group_var, bins):
    conf_bin = cut(conf_var, bins)
    per_bin = count(conf_bin, accuracy)
    acc_p = acc_1 / (acc_0 + acc_1)
    N = sum(n)
    J = number of bins
    total_acc = sum(acc_1)/N
    NRI = ((1/N) * sum(n * (acc_p - total_acc)^2)) / (total_acc*(1-total_acc))
    ANRI = (N*NRI - J + 1) / (N - J + 1)
    return ANRI
```

---

## 10) External R package for calibration
**Package**: `legalPsych` (GitHub / CRAN mirror) provides calibration metrics:
- `CA.rel()` (C, O/U, NRI, etc.)
- `CA.plotCI()`

**Wrap in r4lineups**
- `calibration_rel()` to adapt r4lineups schema and return tidy tables.

---

## Actionable next steps for implementation
1. Confirm DPP formula from the Smith et al. (JARMAC) paper.
2. Add calibration functions (C, O/U, NRI) and plotting; use `legalPsych` if available.
3. Add Bayesian information‑gain curves + BREE.
4. Add expected utility curves + utility difference plot.
5. Add ANRI/bootstrapping from `tredcode.txt`.

