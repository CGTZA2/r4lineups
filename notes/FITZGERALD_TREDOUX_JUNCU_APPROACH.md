# Fitzgerald, Tredoux & Juncu (2023) approach — summary + implementation ideas

**Paper**: *Estimation of Eyewitness Error Rates in Fair and Biased Lineups* (Law & Human Behavior, 2023)

## Core idea (one‑page summary)
The paper focuses on **estimating innocent‑suspect misidentification risk** in lineup studies when the lineup may be **fair or biased**. Traditional estimates often assume perfect fairness by dividing the overall mistaken‑ID rate by the **nominal lineup size** (e.g., 6). Fitzgerald et al. show that this **nominal size correction** is blind to lineup bias and implausible fillers. They compare it with a newer **effective size correction** (Smith et al., 2021, based on Tredoux 1998), which uses the **distribution of mistaken IDs across lineup members** to estimate the number of *plausible* lineup members.

Key consequences:
- **Nominal size correction** → insensitive to bias (assumes every member is equally plausible).
- **Effective size correction** → sensitive to bias (implausible fillers reduce effective size and increase estimated innocent‑suspect risk).
- **No correction** (treat all mistaken IDs as innocent‑suspect IDs) → represents a “worst case” bound.

The paper recommends **reporting both nominal and effective size corrections**, and in practice showing a **PPV range** (best‑case to worst‑case) across confidence levels:
- **Best‑case**: CAC‑style PPV (nominal correction; assumes perfectly fair lineups).
- **Worst‑case**: calibration‑style PPV (no correction; assumes every mistaken ID is an innocent‑suspect ID).
- **Middle**: effective‑size‑corrected PPV (lineup fairness between the two extremes).

## Computation logic (what to implement)

### 1) Effective size (Tredoux, 1998)
Compute from the distribution of *culprit‑absent* mistaken IDs across lineup members:
- Let `n_i` be counts for each lineup member, `N = sum(n_i)`, `p_i = n_i / N`.
- Effective size:
  - `eff_size = 1 / (1 - sum(p_i^2))`
  - (equivalently as in `R/esize_T.R`)

This is already implemented in r4lineups:
- `esize_T()` (Tredoux)
- `esize_m()` (Malpass + Tredoux adjustment)
- Bootstrapped CIs: `esize_T_boot()`, `esize_T_ci_n()`, etc.

### 2) Innocent‑suspect ID estimate from a culprit‑absent lineup
Let `error_rate` be the *overall mistaken ID rate* in culprit‑absent lineups.
- **Nominal correction**: `innocent_ID_rate = error_rate / lineup_size`
- **Effective size correction**: `innocent_ID_rate = error_rate / effective_size`
- **No correction**: `innocent_ID_rate = error_rate`

### 3) PPV (positive predictive value)
For each confidence bin:
- `PPV = guilty_suspect_IDs / (guilty_suspect_IDs + estimated_innocent_suspect_IDs)`
- Compute PPV using all three correction methods to define a **PPV range**.

## Proposed compute functions (R)

### A) Effective size and correction helpers
- `effective_size_from_counts(counts)`
  - Wraps `esize_T()` on a table of CA mistaken IDs.

- `innocent_id_rate_nominal(error_rate, lineup_size)`
- `innocent_id_rate_effective(error_rate, effective_size)`
- `innocent_id_rate_uncorrected(error_rate)`

### B) PPV by confidence, with correction options
- `ppv_by_confidence(data, conf_bins = NULL, lineup_size, correction = c("nominal","effective","none"), effective_size = NULL)`
  - Input `data` with `target_present`, `identification`, `confidence`.
  - For each confidence bin:
    - `guilty_ids = count(TP & suspect)`
    - `mistaken_ids = count(TA & suspect) + count(TA & filler)`
    - `error_rate = mistaken_ids / n_TA`
    - Apply correction to estimate innocent‑suspect IDs.
    - Compute PPV.

- `ppv_range_by_confidence(...)`
  - Computes all three corrections in one call.
  - Returns a tibble with `ppv_nominal`, `ppv_effective`, `ppv_none`.

### C) Effective size by confidence
- `effective_size_by_confidence(data, conf_bins)`
  - Within each confidence bin, compute effective size from the CA mistaken ID distribution.
  - Useful for showing how lineup plausibility changes with confidence.

## Visualization functions (R)

### 1) PPV range plot
- `plot_ppv_range(ppv_tbl)`
  - X = confidence bin
  - Y = PPV
  - Draw 3 curves: nominal, effective, none
  - Shaded band between nominal and none = **PPV range**
  - Emphasize effective size curve as the “middle” assumption

### 2) Effective size vs confidence
- `plot_effective_size(ppv_tbl)` or `plot_effective_size_conf(eff_tbl)`
  - X = confidence bin
  - Y = effective size (+ optional CI ribbon)

### 3) Mistaken ID distribution (culprit‑absent)
- `plot_ca_choice_distribution(counts)`
  - Bar/point plot of mistaken IDs by lineup member
  - Useful to show bias/implausible fillers at a glance

### 4) Bias sensitivity plot (optional)
- `plot_innocent_id_rate_vs_bias()`
  - X = effective size (or fairness index)
  - Y = estimated innocent‑suspect ID rate
  - Overlay nominal vs effective correction curves

## How this aligns with existing r4lineups code
- Effective size is already implemented (`esize_T()` etc.).
- CAC infrastructure exists (`make_cacdata()`), which already bins confidence and computes suspect‑ID accuracy with **nominal correction** for CA filler IDs.
- The new compute functions should reuse the **same data schema** as CAC/ROC:
  - `target_present`, `identification`, `confidence`.

### Minimal integration path
1. Implement `ppv_range_by_confidence()` using the CAC data pipeline, but replace the CA correction with a configurable method.
2. Use `esize_T()` to compute effective size from CA distributions (overall or per confidence bin).
3. Add `plot_ppv_range()` and `plot_effective_size_conf()` to `R/visualization*.R`.

## Notes / caveats highlighted by the paper
- Nominal correction is appropriate only under **pristine lineup conditions** where suspects are no more plausible than fillers.
- Effective size correction is more realistic when **implausible fillers** exist or suspicion is appearance‑based.
- Neither correction is universally applicable, so **reporting a range** is the recommended practice.

---

## Pseudocode (compute + plotting)

### PPV range by confidence
```
function ppv_range_by_confidence(data, conf_bins, lineup_size):
    bin_confidence(data, conf_bins)

    for each bin:
        TP_suspect = count(target_present==TRUE & identification=="suspect" & bin)
        TA_suspect = count(target_present==FALSE & identification=="suspect" & bin)
        TA_filler  = count(target_present==FALSE & identification=="filler"  & bin)

        n_TA = count(target_present==FALSE & bin)
        error_rate = (TA_suspect + TA_filler) / n_TA

        # effective size from CA distribution
        eff_size = effective_size_from_counts(CA_counts_by_member_in_bin)

        innocent_nominal   = error_rate / lineup_size
        innocent_effective = error_rate / eff_size
        innocent_none      = error_rate

        ppv_nominal   = TP_suspect / (TP_suspect + innocent_nominal * n_TA)
        ppv_effective = TP_suspect / (TP_suspect + innocent_effective * n_TA)
        ppv_none      = TP_suspect / (TP_suspect + innocent_none * n_TA)

    return table(conf_bin, ppv_nominal, ppv_effective, ppv_none, eff_size)
```

### Plot PPV range
```
function plot_ppv_range(ppv_tbl):
    plot conf_bin vs ppv
    add line(ppv_nominal)  # best-case
    add line(ppv_effective)
    add line(ppv_none)     # worst-case
    shade between ppv_nominal and ppv_none
```

### Plot effective size
```
function plot_effective_size(ppv_tbl):
    plot conf_bin vs eff_size
    optionally add CI ribbon
```

---

## File suggestion
Save this summary as `FITZGERALD_TREDOUX_JUNCU_APPROACH.md` in the repo root for easy discovery by other agents.
