testthat::test_that("lineup_bias_boot_dist returns expected length", {
  set.seed(1)
  vec <- sample(1:6, 200, replace = TRUE)
  dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 200)
  testthat::expect_length(dist, 200)
})

testthat::test_that("esize_boot_dist returns expected length", {
  set.seed(1)
  vec <- sample(1:6, 200, replace = TRUE)
  dist_t <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 200)
  dist_m <- esize_boot_dist(vec, k = 6, metric = "malpass", R = 200)
  testthat::expect_length(dist_t, 200)
  testthat::expect_length(dist_m, 200)
})
