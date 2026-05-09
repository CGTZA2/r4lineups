## Tests for fit_max_sdt and compare_max_sdt

# Helper: generate synthetic counts from known parameters
sim_max_sdt_counts <- function(d, lambda, n, N_tp, N_ta, seed = 1) {
  set.seed(seed)
  # hit prob
  ph <- integrate(function(x) dnorm(x, d, 1) * pnorm(x)^(n-1), lambda, Inf)$value
  # tp choose prob
  pt <- 1 - pnorm(lambda - d) * pnorm(lambda)^(n-1)
  # fa prob
  pf <- 1 - pnorm(lambda)^n
  n_tp_c  <- rbinom(1, N_tp, pt)
  n_hit_c <- rbinom(1, n_tp_c, ifelse(pt > 0, ph / pt, 0))
  n_fa_c  <- rbinom(1, N_ta, pf)
  list(n_hit = n_hit_c, n_tp_choose = n_tp_c, n_fa = n_fa_c,
       N_tp = N_tp, N_ta = N_ta)
}

# --- class and structure (single condition) ---

test_that("fit_max_sdt returns correct S3 class", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_s3_class(res, "max_sdt_fit")
})

test_that("single-condition result has required fields", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_true(all(c("dprime_1","lambda_1","chisq","df","p_value",
                    "observed","predicted","convergence",
                    "constrain_d","constrain_c","n_conditions","n") %in% names(res)))
})

test_that("single-condition n_conditions is 1", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_equal(res$n_conditions, 1L)
})

test_that("single-condition dprime_2 and lambda_2 are NULL", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_null(res$dprime_2)
  expect_null(res$lambda_2)
})

# --- parameter recovery ---

test_that("recovers known d' within tolerance (single condition)", {
  set.seed(42)
  true_d <- 1.5
  cts <- sim_max_sdt_counts(true_d, 0.5, n=6, N_tp=500, N_ta=500)
  res <- fit_max_sdt(cts$n_hit, cts$n_tp_choose, cts$n_fa, cts$N_tp, cts$N_ta)
  expect_equal(res$dprime_1, true_d, tolerance = 0.4)
})

test_that("higher d' gives higher estimated d'", {
  cts_hi <- sim_max_sdt_counts(2.0, 0, n=6, N_tp=300, N_ta=300, seed=10)
  cts_lo <- sim_max_sdt_counts(0.5, 0, n=6, N_tp=300, N_ta=300, seed=11)
  res_hi <- fit_max_sdt(cts_hi$n_hit, cts_hi$n_tp_choose, cts_hi$n_fa,
                         cts_hi$N_tp, cts_hi$N_ta)
  res_lo <- fit_max_sdt(cts_lo$n_hit, cts_lo$n_tp_choose, cts_lo$n_fa,
                         cts_lo$N_tp, cts_lo$N_ta)
  expect_gt(res_hi$dprime_1, res_lo$dprime_1)
})

test_that("optimizer converges (convergence == 0)", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_equal(res$convergence, 0L)
})

# --- GoF properties ---

test_that("chi-sq is non-negative", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_gte(res$chisq, 0)
})

test_that("p_value is in (0,1)", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_gte(res$p_value, 0)
  expect_lte(res$p_value, 1)
})

test_that("single-condition df == 3 (5 cells - 2 params)", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_equal(res$df, 3L)
})

test_that("two-condition unconstrained df == 6 (10 cells - 4 params)", {
  res <- fit_max_sdt(
    n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
    n_hit_2=40, n_tp_choose_2=65, n_fa_2=20, N_tp_2=100, N_ta_2=100
  )
  expect_equal(res$df, 6L)
})

# --- two-condition models ---

test_that("two-condition result has correct n_conditions", {
  res <- fit_max_sdt(
    n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
    n_hit_2=40, n_tp_choose_2=65, n_fa_2=20, N_tp_2=100, N_ta_2=100
  )
  expect_equal(res$n_conditions, 2L)
})

test_that("two-condition result has dprime_2 and lambda_2", {
  res <- fit_max_sdt(
    n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
    n_hit_2=40, n_tp_choose_2=65, n_fa_2=20, N_tp_2=100, N_ta_2=100
  )
  expect_false(is.null(res$dprime_2))
  expect_false(is.null(res$lambda_2))
})

test_that("constrain_d forces equal d' estimates", {
  res <- fit_max_sdt(
    n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
    n_hit_2=40, n_tp_choose_2=65, n_fa_2=20, N_tp_2=100, N_ta_2=100,
    constrain_d = TRUE
  )
  expect_equal(res$dprime_1, res$dprime_2)
})

test_that("constrain_c forces equal lambda estimates", {
  res <- fit_max_sdt(
    n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
    n_hit_2=40, n_tp_choose_2=65, n_fa_2=20, N_tp_2=100, N_ta_2=100,
    constrain_c = TRUE
  )
  expect_equal(res$lambda_1, res$lambda_2)
})

test_that("constrained model has higher or equal chi-sq than unconstrained", {
  free_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100
  )
  cons_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100,
    constrain_d = TRUE
  )
  expect_gte(cons_fit$chisq, free_fit$chisq - 1e-6)
})

# --- compare_max_sdt ---

test_that("compare_max_sdt returns correct class", {
  free_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100
  )
  cons_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100,
    constrain_d = TRUE
  )
  cmp <- compare_max_sdt(free_fit, cons_fit)
  expect_s3_class(cmp, "max_sdt_compare")
})

test_that("compare_max_sdt delta_chisq >= 0", {
  free_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100
  )
  cons_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100,
    constrain_d = TRUE
  )
  cmp <- compare_max_sdt(free_fit, cons_fit)
  expect_gte(cmp$delta_chisq, -1e-6)
})

test_that("compare_max_sdt test_df == 1 for one constraint", {
  free_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100
  )
  cons_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100,
    constrain_d = TRUE
  )
  cmp <- compare_max_sdt(free_fit, cons_fit)
  expect_equal(cmp$test_df, 1L)
})

test_that("identical conditions give non-significant chi-sq difference", {
  # Same counts in both conditions => equal d' is plausible
  fit_free <- fit_max_sdt(
    n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
    n_hit_2=45, n_tp_choose_2=70, n_fa_2=25, N_tp_2=100, N_ta_2=100
  )
  fit_eqd <- fit_max_sdt(
    n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
    n_hit_2=45, n_tp_choose_2=70, n_fa_2=25, N_tp_2=100, N_ta_2=100,
    constrain_d = TRUE
  )
  cmp <- compare_max_sdt(fit_free, fit_eqd)
  expect_gt(cmp$p_value, 0.05)
})

test_that("compare_max_sdt errors if free has fewer params than constrained", {
  fit1 <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  fit2 <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_error(compare_max_sdt(fit1, fit2))
})

# --- S3 methods ---

test_that("print runs without error", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_output(print(res), "MAX SDT")
})

test_that("plot returns a ggplot", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  p <- plot(res)
  expect_s3_class(p, "gg")
})

test_that("compare print runs without error", {
  free_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100
  )
  cons_fit <- fit_max_sdt(
    n_hit=55, n_tp_choose=80, n_fa=20, N_tp=100, N_ta=100,
    n_hit_2=30, n_tp_choose_2=60, n_fa_2=40, N_tp_2=100, N_ta_2=100,
    constrain_d = TRUE
  )
  expect_output(print(compare_max_sdt(free_fit, cons_fit)), "Model Comparison")
})

# --- input validation ---

test_that("n_hit > n_tp_choose raises error", {
  expect_error(fit_max_sdt(n_hit=80, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100))
})

test_that("partial condition-2 args raise error", {
  expect_error(
    fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
                n_hit_2=40)
  )
})

test_that("n < 2 raises error", {
  expect_error(
    fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100, n=1)
  )
})

# --- bootstrap ---

test_that("boot_ci is NULL when nboot = 0", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100)
  expect_null(res$boot_ci)
})

test_that("boot_ci has correct dimensions when nboot > 0", {
  res <- fit_max_sdt(n_hit=45, n_tp_choose=70, n_fa=25, N_tp=100, N_ta=100,
                      nboot=100, seed=1)
  expect_false(is.null(res$boot_ci))
  expect_equal(nrow(res$boot_ci), 2L)   # lower, upper
  expect_equal(ncol(res$boot_ci), 2L)   # d1, lambda1
})

test_that("bootstrap CI contains true parameter (single condition)", {
  set.seed(7)
  true_d <- 1.2
  cts <- sim_max_sdt_counts(true_d, 0.3, n=6, N_tp=500, N_ta=500, seed=99)
  res <- fit_max_sdt(cts$n_hit, cts$n_tp_choose, cts$n_fa, cts$N_tp, cts$N_ta,
                      nboot = 200, seed = 7)
  expect_gte(true_d, res$boot_ci["lower", "d1"])
  expect_lte(true_d, res$boot_ci["upper", "d1"])
})
