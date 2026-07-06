# Tests for the Winter et al. (2022) 2-HT model

.sim_2ht_counts <- function(dP, dA, b, g, n_tp, n_ta, L = 6, seed = 1) {
  # Model probabilities (Winter et al., 2022)
  p_tp <- c(
    suspect = dP + (1 - dP) * (b + (1 - b) * g * (1 / L)),
    filler = (1 - dP) * (1 - b) * g * ((L - 1) / L),
    reject = (1 - dP) * (1 - b) * (1 - g)
  )
  p_ta <- c(
    suspect = (1 - dA) * (b + (1 - b) * g * (1 / L)),
    filler = (1 - dA) * (1 - b) * g * ((L - 1) / L),
    reject = dA + (1 - dA) * (1 - b) * (1 - g)
  )
  set.seed(seed)
  tp <- as.vector(stats::rmultinom(1, n_tp, p_tp))
  ta <- as.vector(stats::rmultinom(1, n_ta, p_ta))
  c(n_tp_suspect = tp[1], n_tp_filler = tp[2], n_tp_reject = tp[3],
    n_ta_suspect = ta[1], n_ta_filler = ta[2], n_ta_reject = ta[3])
}

test_that("fit_winter_2ht recovers known parameters", {
  true <- c(dP = 0.45, dA = 0.30, b = 0.08, g = 0.55)
  counts <- .sim_2ht_counts(true["dP"], true["dA"], true["b"], true["g"],
                            n_tp = 5000, n_ta = 5000, seed = 42)
  fit <- fit_winter_2ht(counts, lineup_size = 6)

  expect_s3_class(fit, "winter_2ht")
  expect_equal(unname(fit$parameters["dP"]), unname(true["dP"]), tolerance = 0.05)
  expect_equal(unname(fit$parameters["dA"]), unname(true["dA"]), tolerance = 0.05)
  expect_equal(unname(fit$parameters["b"]), unname(true["b"]), tolerance = 0.05)
  expect_equal(unname(fit$parameters["g"]), unname(true["g"]), tolerance = 0.05)
})

test_that("fitted probabilities sum to 1 for both lineup types", {
  counts <- c(
    n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
    n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
  )
  fit <- fit_winter_2ht(counts, lineup_size = 6)
  expect_equal(sum(fit$expected_counts$tp), sum(counts[1:3]), tolerance = 1e-6)
  expect_equal(sum(fit$expected_counts$ta), sum(counts[4:6]), tolerance = 1e-6)
})

test_that("summary reports saturation instead of an invalid chi-square test", {
  counts <- c(
    n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
    n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
  )
  fit <- fit_winter_2ht(counts, lineup_size = 6)
  out <- paste(capture.output(summary(fit)), collapse = "\n")
  expect_match(out, "Saturated")
  expect_match(out, "0 residual df")
  # A saturated model fits the observed counts (near) exactly
  expect_equal(unname(fit$expected_counts$tp), unname(counts[1:3]),
               tolerance = 0.5)
})

test_that("fit_winter_2ht accepts data frame input", {
  data(lineup_example)
  fit <- fit_winter_2ht(
    lineup_example,
    lineup_size = 6,
    target_present = "target_present",
    identification = "identification"
  )
  expect_s3_class(fit, "winter_2ht")
  expect_true(all(fit$parameters >= 0 & fit$parameters <= 1))
})

test_that("plot methods return ggplot objects", {
  counts <- c(
    n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
    n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
  )
  fit <- fit_winter_2ht(counts, lineup_size = 6)
  expect_s3_class(plot_2ht_parameters(fit), "ggplot")
  expect_s3_class(plot_2ht_fit(fit), "ggplot")
})
