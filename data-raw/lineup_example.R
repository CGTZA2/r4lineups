# Generate lineup_example
# ------------------------------------------------------------------
# lineup_example is simulated data for package examples and vignettes. It is
# generated here by the package's own simulator, so it is reproducible from
# package code rather than being a hand-built artefact.
#
# Two properties matter and are enforced by the guards at the foot of this
# script:
#
#   1. Foils are NOT interchangeable. `foil_bias` gives each foil its own mean
#      memory strength, so some are plausible alternatives to the suspect and
#      others are near-dead weight, as in real lineups.
#
#   2. High confidence must not be a PERFECT discriminator. The previous
#      version of this dataset had no target-absent suspect identification
#      above confidence 80, which left the (80, 100] likelihood cell empty. The
#      likelihood ratio there was infinite and every prior-posterior curve was
#      pinned at 1.0. Real data never behaves that way, and a degenerate cell
#      is a bad thing to teach from.
#
# Run with:  Rscript data-raw/lineup_example.R

pkgload::load_all(".", quiet = TRUE)

# --- Parameters -----------------------------------------------------------
# Chosen against the acceptance criteria below rather than by eye; see the
# guards for what "acceptable" means here.
n_per_condition <- 700L
d_prime         <- 1.6
c_criterion     <- 0.90
lineup_size     <- 6L
n_conf_levels   <- 9L

# One mean per foil, positions 2..6. Deliberately graded: foil 2 is a strong
# alternative, foil 6 is chosen only rarely.
foil_bias <- c(0.5, 0.25, 0.0, -0.4, -1.2)

# The simulator returns ordinal confidence 1..n_conf_levels; the vignettes and
# the published literature use a 0-100 scale.
confidence_scale <- round(seq(20, 100, length.out = n_conf_levels))

sim <- simulate_lineup_data(
  n_tp          = n_per_condition,
  n_ta          = n_per_condition,
  d_prime       = d_prime,
  c_criterion   = c_criterion,
  lineup_size   = lineup_size,
  conf_levels   = n_conf_levels,
  foil_bias     = foil_bias,
  seed          = 2026
)

lineup_example <- as.data.frame(sim)
lineup_example$confidence <- confidence_scale[lineup_example$confidence]

lineup_example <- lineup_example[, c("target_present", "lineup_member",
                                     "suspect_pos", "identification",
                                     "confidence")]
rownames(lineup_example) <- NULL


# --- Acceptance criteria --------------------------------------------------
# These are the reason the parameters above are what they are. If a change to
# the simulator or the parameters breaks one of these, the dataset is not fit
# for the vignettes and this script must fail rather than ship it.

ta <- lineup_example[!lineup_example$target_present, ]
tp <- lineup_example[ lineup_example$target_present, ]

# 1. The derived label must be exactly recoverable from the raw choice.
round_trip <- classify_lineup_choices(
  lineup_example[, c("lineup_member", "suspect_pos")],
  k = lineup_size
)
stopifnot(identical(round_trip$identification, lineup_example$identification))

# 2. Every lineup member must be chosen by someone: no accidental null foils.
stopifnot(
  setequal(sort(unique(lineup_example$lineup_member)), 0:lineup_size)
)

# 3. Foils must be genuinely unequal, not merely separately simulated.
foil_counts <- table(factor(
  lineup_example$lineup_member[lineup_example$identification == "filler"],
  levels = 2:lineup_size
))
stopifnot(all(foil_counts > 0), max(foil_counts) / min(foil_counts) > 3)

# 4. A plausible rejection rate: witnesses must sometimes decline to choose.
ta_reject_rate <- mean(ta$identification == "reject")
stopifnot(ta_reject_rate > 0.20, ta_reject_rate < 0.40)

# 5. High confidence must be strongly diagnostic but NOT perfect: some innocent
#    suspects are identified with high confidence, as happens in real cases.
ta_high_conf <- sum(ta$identification == "suspect" & ta$confidence > 80)
stopifnot(ta_high_conf >= 3, ta_high_conf < 0.05 * nrow(ta))

# 6. No empty cell in the Bayesian likelihood table, which is what pinned the
#    prior-posterior curve at 1.0 in the previous dataset.
lik <- make_bayes_curves(lineup_example,
                         response_categories = "confidence",
                         confidence_bins = c(0, 60, 80, 100))$likelihoods
stopifnot(
  all(lik$p_x_given_guilty > 0),
  all(lik$p_x_given_innocent > 0)
)

# 7. Sanity: the target-present hit rate should be in a believable range.
tp_hit_rate <- mean(tp$identification == "suspect")
stopifnot(tp_hit_rate > 0.40, tp_hit_rate < 0.75)

# 8. Confidence must actually track accuracy. A dataset in which it does not is
#    useless for demonstrating calibration, CAC and resolution, which is most
#    of what this dataset is for.
cac <- make_cac(lineup_example, confidence_bins = c(0, 60, 80, 100))$cac_data
stopifnot(
  nrow(cac) == 3L,
  all(diff(cac$accuracy) > 0),          # monotonically increasing
  all(cac$n_total >= 30)                # every bin usably populated
)


save(lineup_example, file = "data/lineup_example.rda",
     compress = "bzip2", version = 2)

message("lineup_example regenerated: ", nrow(lineup_example), " rows")
message("  TA rejection rate:      ", round(100 * ta_reject_rate, 1), "%")
message("  TP hit rate:            ", round(100 * tp_hit_rate, 1), "%")
message("  High-conf innocent IDs: ", ta_high_conf)
message("  Foil choice counts:     ",
        paste(as.integer(foil_counts), collapse = ", "))
