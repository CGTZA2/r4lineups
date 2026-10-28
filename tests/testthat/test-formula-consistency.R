testthat::test_that("esize_T matches Tredoux transformed index of diversity", {
  lineup_table <- table(factor(c(1, 1, 2, 3, 3, 3, 4, 4), levels = 1:4))
  n <- sum(lineup_table)
  index_diversity <- 1 - sum(lineup_table^2) / n^2
  expected <- 1 / (1 - index_diversity)

  testthat::expect_equal(esize_T(lineup_table), expected)
  testthat::expect_equal(esize_T(lineup_table), n^2 / sum(lineup_table^2))
})

testthat::test_that("diag_ratio_T matches documented corrected count formula", {
  lineup_pres <- c(1, 2, 3, 3, 3, 4, 5, 6)
  lineup_abs <- c(1, 2, 2, 3, 4, 5)
  pos_pres <- 3
  pos_abs <- 2

  expected <- ((sum(lineup_pres == pos_pres) + 0.5) / (length(lineup_pres) + 0.5)) /
    ((sum(lineup_abs == pos_abs) + 0.5) / (length(lineup_abs) + 0.5))

  testthat::expect_equal(
    diag_ratio_T(lineup_pres, lineup_abs, pos_pres, pos_abs, k1 = 6, k2 = 5),
    expected
  )
})

testthat::test_that("make_dpp reports normalized DPP and raw AUC gap separately", {
  lineup_data <- data.frame(
    target_present = rep(c(TRUE, FALSE), each = 8),
    identification = c(
      "suspect", "suspect", "suspect", "filler", "reject", "reject", "filler", "suspect",
      "suspect", "filler", "reject", "reject", "filler", "reject", "suspect", "reject"
    ),
    confidence = c(100, 90, 80, 70, 60, 50, 40, 30, 90, 80, 70, 60, 50, 40, 30, 20),
    stringsAsFactors = FALSE
  )

  dpp_obj <- make_dpp(lineup_data, lineup_size = 6)

  testthat::expect_true("auc_gap" %in% names(dpp_obj))
  testthat::expect_equal(dpp_obj$auc_gap, dpp_obj$auc_perfect - dpp_obj$auc_observed)
  if (!is.na(dpp_obj$dpp)) {
    testthat::expect_equal(dpp_obj$dpp, 1 - dpp_obj$auc_observed / dpp_obj$auc_perfect)
  }
})

test_that("esize_T default is unchanged by the unbiased argument", {
  cnt <- c(17, 17, 17, 17, 16, 16)
  expect_equal(esize_T(cnt), sum(cnt)^2 / sum(cnt^2))
  expect_equal(esize_T(cnt), esize_T(cnt, unbiased = FALSE))
})

test_that("esize_T(unbiased = TRUE) removes the small-sample downward bias", {
  # The plug-in estimator of E' is biased downward, by approximately
  # k(k-1)/(N + k - 1). With k = 6 and 50 choices a perfectly fair lineup
  # returns about 5.47 rather than 6. The unbiased estimator of sum(p^2)
  # removes essentially all of it.
  set.seed(4242)
  k <- 6L
  n <- 50L

  est <- replicate(1500, {
    cnt <- tabulate(sample.int(k, n, replace = TRUE), nbins = k)
    c(naive = esize_T(cnt), unbiased = esize_T(cnt, unbiased = TRUE))
  })

  naive_mean    <- mean(est["naive", ])
  unbiased_mean <- mean(est["unbiased", ])

  # Naive is detectably below the true value, and close to the predicted bias
  expect_lt(naive_mean, 5.7)
  expect_equal(naive_mean, n * k / (n + k - 1), tolerance = 0.02)

  # Corrected is close to the truth, and closer than the naive estimator
  expect_equal(unbiased_mean, k, tolerance = 0.02)
  expect_lt(abs(unbiased_mean - k), abs(naive_mean - k))
})

test_that("esize_T bias correction degrades gracefully", {
  # Fewer than two choices: correction undefined
  expect_warning(one <- esize_T(c(1, 0, 0), unbiased = TRUE), "undefined")
  expect_equal(one, esize_T(c(1, 0, 0)))

  # Every choice on a different member: sum(o(o-1)) is 0, correction unbounded
  expect_warning(distinct <- esize_T(c(1, 1, 1, 1), unbiased = TRUE),
                 "different lineup member")
  expect_equal(distinct, esize_T(c(1, 1, 1, 1)))

  expect_error(esize_T(c(5, 5), unbiased = "yes"), "TRUE or FALSE")
})
