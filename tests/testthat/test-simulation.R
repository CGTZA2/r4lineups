# Tests for data simulation functions

test_that("simulate_lineup_data basic functionality", {
  set.seed(123)
  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    lineup_size = 6
  )

  expect_s3_class(sim_data, "simulated_lineup_data")
  expect_equal(nrow(sim_data), 200)
  expect_true(all(c("target_present", "identification", "confidence") %in% names(sim_data)))
})

test_that("simulate_lineup_data generates correct proportions", {
  set.seed(456)
  sim_data <- simulate_lineup_data(
    n_tp = 150,
    n_ta = 150,
    d_prime = 1.5,
    lineup_size = 6
  )

  # Check target_present distribution
  expect_equal(sum(sim_data$target_present), 150)
  expect_equal(sum(!sim_data$target_present), 150)

  # Check identification categories
  ids <- table(sim_data$identification)
  expect_true(all(c("suspect", "filler", "reject") %in% names(ids)))
  expect_true(all(ids > 0))  # All categories should occur
})

test_that("simulate_lineup_data d_prime affects performance", {
  set.seed(789)

  # Weak discriminability
  weak_data <- simulate_lineup_data(
    n_tp = 200,
    n_ta = 200,
    d_prime = 0.8,
    lineup_size = 6
  )

  # Strong discriminability
  strong_data <- simulate_lineup_data(
    n_tp = 200,
    n_ta = 200,
    d_prime = 2.5,
    lineup_size = 6
  )

  # Calculate hit rates
  weak_hit_rate <- mean(weak_data$target_present & weak_data$identification == "suspect")
  strong_hit_rate <- mean(strong_data$target_present & strong_data$identification == "suspect")

  # Higher d' should lead to higher hit rate
  expect_true(strong_hit_rate > weak_hit_rate)
})

test_that("simulate_lineup_data criterion affects conservatism", {
  set.seed(111)

  # Liberal criterion
  liberal_data <- simulate_lineup_data(
    n_tp = 150,
    n_ta = 150,
    d_prime = 1.5,
    c_criterion = 0.0,
    lineup_size = 6
  )

  # Conservative criterion
  conservative_data <- simulate_lineup_data(
    n_tp = 150,
    n_ta = 150,
    d_prime = 1.5,
    c_criterion = 1.0,
    lineup_size = 6
  )

  # Calculate rejection rates
  liberal_reject <- mean(liberal_data$identification == "reject")
  conservative_reject <- mean(conservative_data$identification == "reject")

  # Higher criterion should lead to more rejections
  expect_true(conservative_reject > liberal_reject)
})

test_that("simulate_lineup_data lineup_size parameter", {
  set.seed(222)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    lineup_size = 8
  )

  # Should have identification decisions
  expect_true(all(sim_data$identification %in% c("suspect", "filler", "reject")))

  # Filler IDs should be possible with larger lineups
  expect_true(any(sim_data$identification == "filler"))
})

test_that("simulate_lineup_data confidence levels", {
  set.seed(333)

  # 3-point scale
  sim_3 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    conf_levels = 3
  )

  # 7-point scale
  sim_7 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    conf_levels = 7
  )

  expect_true(max(sim_3$confidence) <= 3)
  expect_true(min(sim_3$confidence) >= 1)
  expect_true(max(sim_7$confidence) <= 7)
  expect_true(min(sim_7$confidence) >= 1)
})

test_that("simulate_lineup_data with response times", {
  set.seed(444)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    include_response_time = TRUE
  )

  expect_true("response_time" %in% names(sim_data))
  expect_true(all(sim_data$response_time > 0))
  expect_true(is.numeric(sim_data$response_time))
})

test_that("simulate_lineup_data without response times", {
  set.seed(555)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    include_response_time = FALSE
  )

  expect_false("response_time" %in% names(sim_data))
})

test_that("simulate_lineup_data seed reproducibility", {
  sim1 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    seed = 999
  )

  sim2 <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.5,
    seed = 999
  )

  expect_equal(sim1$identification, sim2$identification)
  expect_equal(sim1$confidence, sim2$confidence)
})

test_that("print.simulated_lineup_data works", {
  sim_data <- simulate_lineup_data(
    n_tp = 50,
    n_ta = 50,
    d_prime = 1.5,
    seed = 666
  )

  expect_output(print(sim_data), "Simulated Lineup Data")
  expect_output(print(sim_data), "d'")
})

test_that("simulate_lineup_data parameter validation", {
  # Negative sample size
  expect_error(
    simulate_lineup_data(n_tp = -10, n_ta = 100, d_prime = 1.5),
    "must be positive"
  )

  # Invalid lineup size
  expect_error(
    simulate_lineup_data(n_tp = 100, n_ta = 100, d_prime = 1.5, lineup_size = 1),
    "must be at least 2"
  )

  # Invalid conf_levels
  expect_error(
    simulate_lineup_data(n_tp = 100, n_ta = 100, d_prime = 1.5, conf_levels = 1),
    "must be at least 2"
  )
})

test_that("simulated data works with r4lineups functions", {
  set.seed(777)

  sim_data <- simulate_lineup_data(
    n_tp = 100,
    n_ta = 100,
    d_prime = 1.8,
    lineup_size = 6,
    conf_levels = 5
  )

  # Test with make_roc
  roc_result <- make_roc(sim_data, lineup_size = 6, show_plot = FALSE)
  expect_true("pauc" %in% names(roc_result))
  expect_true(roc_result$pauc >= 0)

  # Test with make_cac
  cac_result <- make_cac(sim_data, show_plot = FALSE)
  expect_s3_class(cac_result, "lineup_cac")

  # Test with make_fullroc
  fullroc_result <- make_fullroc(sim_data, show_plot = FALSE)
  expect_true("auc" %in% names(fullroc_result))
})

test_that("simulate_power_analysis basic functionality", {
  skip_if_not(interactive(), "Power analysis is slow, skip in batch tests")

  power_result <- simulate_power_analysis(
    sample_sizes = c(50, 100),
    d_prime = 1.5,
    n_simulations = 10,  # Very small for testing
    seed = 888
  )

  expect_s3_class(power_result, "power_analysis")
  expect_true("power_curve" %in% names(power_result))
  expect_true(is.data.frame(power_result$power_curve))
})
