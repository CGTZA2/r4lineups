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
