# Tests for Expected Information Gain and PPV-range analyses

test_that("EIG is ~0 for uninformative responses", {
  n <- 400
  set.seed(1)
  ids <- sample(c("suspect", "filler", "reject"), n / 2, replace = TRUE)
  confs <- sample(c(30, 60, 90), n / 2, replace = TRUE)
  data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = n / 2),
    identification = rep(ids, 2),
    confidence = rep(confs, 2)
  )
  result <- compute_eig(data, prior_guilt = 0.5)
  expect_lt(result$eig, 0.01)
})

test_that("EIG equals prior entropy (1 bit) for perfect separation at prior 0.5", {
  n <- 200
  data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = n / 2),
    identification = rep(c("suspect", "reject"), each = n / 2),
    confidence = rep(90, n)
  )
  result <- compute_eig(data, prior_guilt = 0.5)
  expect_equal(result$eig, 1, tolerance = 1e-8)
  expect_equal(result$prior_entropy, 1, tolerance = 1e-8)
})

test_that("EIG is bounded by prior entropy and non-negative", {
  data(lineup_example)
  result <- compute_eig(lineup_example, confidence_bins = c(0, 60, 80, 100))
  expect_gte(result$eig, 0)
  expect_lte(result$eig, result$prior_entropy + 1e-10)
})

test_that("ppv corrections are ordered: none <= effective <= nominal for fair lineups", {
  data(lineup_example)
  bins <- c(0, 60, 80, 100)
  ppv_none <- ppv_by_confidence(lineup_example, correction = "none",
                                confidence_bins = bins)
  ppv_eff <- ppv_by_confidence(lineup_example, correction = "effective",
                               confidence_bins = bins)
  ppv_nom <- ppv_by_confidence(lineup_example, correction = "nominal",
                               confidence_bins = bins)

  # "none" treats every mistaken ID as an innocent-suspect ID (worst case),
  # so it gives the lowest PPV; nominal spreads errors over the full lineup
  # (best case); effective sits between them.
  expect_lte(ppv_none$overall_ppv, ppv_eff$overall_ppv)
  expect_lte(ppv_eff$overall_ppv, ppv_nom$overall_ppv + 1e-10)
})

test_that("effective correction computes a plausible effective size (regression)", {
  # Regression for the bug where the fallback tabulated the values of the
  # per-position count vector, collapsing E' to ~1.38 regardless of data.
  data(lineup_example)
  ppv_eff <- ppv_by_confidence(lineup_example, correction = "effective",
                               confidence_bins = c(0, 60, 80, 100))
  esizes <- ppv_eff$ppv_data$effective_size
  esizes <- esizes[!is.na(esizes)]
  expect_true(all(esizes > 2))
  expect_true(all(esizes <= 6 + 1e-10))
})

test_that("innocent_id_rate helpers compute the documented formulas", {
  expect_equal(innocent_id_rate_nominal(0.30, 6), 0.05)
  expect_equal(innocent_id_rate_effective(0.30, 4), 0.075)
  expect_equal(innocent_id_rate_uncorrected(0.30), 0.30)
  expect_error(innocent_id_rate_nominal(0.30, 0))
  expect_error(innocent_id_rate_effective(0.30, 0))
})

test_that("ppv_range_by_confidence returns all three corrections", {
  data(lineup_example)
  result <- ppv_range_by_confidence(lineup_example,
                                    confidence_bins = c(0, 60, 80, 100))
  expect_true(all(c("ppv_nominal", "ppv_effective", "ppv_none",
                    "ppv_range_data") %in% names(result)))
  expect_true(all(c("ppv_nominal", "ppv_effective", "ppv_none") %in%
                    names(result$ppv_range_data)))
})
