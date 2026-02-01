#' SDT Summary Statistics from Counts
#'
#' Compute d', criterion, and beta from 2x2 SDT counts (hits, false alarms, misses, correct rejections).
#'
#' @param hits Number of hits.
#' @param fas Number of false alarms.
#' @param misses Number of misses.
#' @param cr Number of correct rejections.
#' @param correction Correction for extreme rates ("loglinear", "half", or "none").
#'
#' @return A list with rates and SDT metrics.
#' @export
sdt_summary_from_counts <- function(hits,
                                    fas,
                                    misses,
                                    cr,
                                    correction = c("loglinear", "half", "none")) {
  correction <- match.arg(correction)
  ns <- hits + misses
  nn <- fas + cr
  if (ns <= 0 || nn <= 0) {
    stop("Signal and noise trial counts must be positive.", call. = FALSE)
  }
  pH <- sdt_adjust_rate(hits, ns, correction)
  pF <- sdt_adjust_rate(fas, nn, correction)
  zH <- stats::qnorm(pH)
  zF <- stats::qnorm(pF)
  dprime <- zH - zF
  cval <- -0.5 * (zH + zF)
  ln_beta <- dprime * cval
  beta <- exp(ln_beta)
  list(
    hits = hits,
    fas = fas,
    misses = misses,
    cr = cr,
    ns = ns,
    nn = nn,
    hit_rate = pH,
    fa_rate = pF,
    zH = zH,
    zF = zF,
    dprime = dprime,
    c = cval,
    ln_beta = ln_beta,
    beta = beta,
    correction = correction
  )
}

#' Variance Estimates for SDT Summary Metrics
#'
#' Estimates the variance of d' (and criterion) from summary SDT counts using
#' the Gourevitch & Galanter (1967) delta method, the Miller (1996)
#' exact-binomial method, or a parametric bootstrap.
#'
#' @param hits Number of hits.
#' @param fas Number of false alarms.
#' @param misses Number of misses.
#' @param cr Number of correct rejections.
#' @param method Variance method: "gourevitch", "miller", or "bootstrap".
#' @param correction Correction for extreme rates ("loglinear", "half", or "none").
#' @param nboot Number of bootstrap samples when method = "bootstrap".
#' @param seed Optional random seed for bootstrap variance.
#'
#' @return A list with variance estimates for zH, zF, d', and c.
#' @export
sdt_summary_variance <- function(hits,
                                 fas,
                                 misses,
                                 cr,
                                 method = c("miller", "gourevitch", "bootstrap"),
                                 correction = c("loglinear", "half", "none"),
                                 nboot = 1000,
                                 seed = NULL) {
  method <- match.arg(method)
  correction <- match.arg(correction)
  summ <- sdt_summary_from_counts(hits, fas, misses, cr, correction = correction)
  ns <- summ$ns
  nn <- summ$nn
  pH <- summ$hit_rate
  pF <- summ$fa_rate
  zH <- summ$zH
  zF <- summ$zF

  if (method == "gourevitch") {
    var_zH <- pH * (1 - pH) / (ns * stats::dnorm(zH)^2)
    var_zF <- pF * (1 - pF) / (nn * stats::dnorm(zF)^2)
  } else if (method == "miller") {
    var_zH <- sdt_var_z_from_binom(ns, pH, correction)
    var_zF <- sdt_var_z_from_binom(nn, pF, correction)
  } else {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    boot_stats <- replicate(
      nboot,
      {
        bh <- stats::rbinom(1, ns, pH)
        bf <- stats::rbinom(1, nn, pF)
        bm <- ns - bh
        bc <- nn - bf
        bs <- sdt_summary_from_counts(bh, bf, bm, bc, correction = correction)
        c(zH = bs$zH, zF = bs$zF, dprime = bs$dprime, c = bs$c)
      }
    )
    var_zH <- stats::var(boot_stats["zH", ])
    var_zF <- stats::var(boot_stats["zF", ])
    var_dprime <- stats::var(boot_stats["dprime", ])
  }

  if (method != "bootstrap") {
    var_dprime <- var_zH + var_zF
  }
  if (method == "bootstrap") {
    var_c <- stats::var(boot_stats["c", ])
  } else {
    var_c <- 0.25 * (var_zH + var_zF)
  }
  list(
    var_zH = var_zH,
    var_zF = var_zF,
    var_dprime = var_dprime,
    var_c = var_c,
    nboot = nboot,
    method = method,
    correction = correction
  )
}

#' Compare SDT Summary Metrics Between Two Conditions
#'
#' Performs a large-sample z-test comparing d' (or criterion or ln beta)
#' between two independent conditions using summary counts only.
#'
#' @param hits_a Hits in condition A.
#' @param fas_a False alarms in condition A.
#' @param misses_a Misses in condition A.
#' @param cr_a Correct rejections in condition A.
#' @param hits_b Hits in condition B.
#' @param fas_b False alarms in condition B.
#' @param misses_b Misses in condition B.
#' @param cr_b Correct rejections in condition B.
#' @param metric Which metric to compare: "dprime", "c", or "ln_beta".
#' @param method Variance method: "miller", "gourevitch", or "bootstrap".
#' @param correction Correction for extreme rates ("loglinear", "half", or "none").
#' @param nboot Number of bootstrap samples when method = "bootstrap".
#' @param seed Optional random seed for bootstrap variance.
#'
#' @return A list with estimates, standard error, z, and p-value.
#' @export
compare_sdt_summary <- function(hits_a, fas_a, misses_a, cr_a,
                                hits_b, fas_b, misses_b, cr_b,
                                metric = c("dprime", "c", "ln_beta"),
                                method = c("miller", "gourevitch", "bootstrap"),
                                correction = c("loglinear", "half", "none"),
                                nboot = 1000,
                                seed = NULL) {
  metric <- match.arg(metric)
  method <- match.arg(method)
  correction <- match.arg(correction)

  summ_a <- sdt_summary_from_counts(hits_a, fas_a, misses_a, cr_a, correction = correction)
  summ_b <- sdt_summary_from_counts(hits_b, fas_b, misses_b, cr_b, correction = correction)
  var_a <- sdt_summary_variance(
    hits_a, fas_a, misses_a, cr_a,
    method = method, correction = correction,
    nboot = nboot, seed = seed
  )
  var_b <- sdt_summary_variance(
    hits_b, fas_b, misses_b, cr_b,
    method = method, correction = correction,
    nboot = nboot, seed = seed
  )

  metric_a <- summ_a[[metric]]
  metric_b <- summ_b[[metric]]
  if (metric == "ln_beta") {
    var_metric_a <- sdt_var_ln_beta(summ_a, var_a)
    var_metric_b <- sdt_var_ln_beta(summ_b, var_b)
  } else if (metric == "c") {
    var_metric_a <- var_a$var_c
    var_metric_b <- var_b$var_c
  } else {
    var_metric_a <- var_a$var_dprime
    var_metric_b <- var_b$var_dprime
  }

  se <- sqrt(var_metric_a + var_metric_b)
  z <- (metric_a - metric_b) / se
  p <- 2 * (1 - stats::pnorm(abs(z)))

  list(
    metric = metric,
    estimate_a = metric_a,
    estimate_b = metric_b,
    diff = metric_a - metric_b,
    se = se,
    z = z,
    p_value = p,
    method = method,
    correction = correction
  )
}

sdt_adjust_rate <- function(count, n, correction) {
  if (correction == "none") {
    p <- count / n
  } else if (correction == "half") {
    adj <- count
    if (count == 0) adj <- 0.5
    if (count == n) adj <- n - 0.5
    p <- adj / n
  } else {
    p <- (count + 0.5) / (n + 1)
  }
  pmin(pmax(p, 1e-6), 1 - 1e-6)
}

sdt_var_z_from_binom <- function(n, p, correction) {
  counts <- 0:n
  probs <- stats::dbinom(counts, size = n, prob = p)
  p_i <- vapply(counts, sdt_adjust_rate, numeric(1), n = n, correction = correction)
  z_i <- stats::qnorm(p_i)
  e_z <- sum(z_i * probs)
  e_z2 <- sum((z_i^2) * probs)
  e_z2 - e_z^2
}

sdt_var_ln_beta <- function(summary_obj, var_obj) {
  dprime <- summary_obj$dprime
  cval <- summary_obj$c
  var_d <- var_obj$var_dprime
  var_c <- var_obj$var_c
  # Approximate via delta method, ignoring covariance between d' and c.
  (cval^2) * var_d + (dprime^2) * var_c
}
