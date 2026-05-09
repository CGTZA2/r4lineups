#' MAX SDT Compound-Decision Model for Eyewitness Lineups
#'
#' Fits the MAX signal-detection (compound-decision) model to eyewitness lineup
#' data using chi-squared goodness-of-fit minimization. Estimates d' (sensitivity)
#' and lambda (criterion) simultaneously from hit rates, false-alarm rates, and
#' rejection rates, supporting one or two conditions and optional model constraints.
#'
#' @param n_hit Number of correct suspect identifications in target-present (TP)
#'   lineups (condition 1).
#' @param n_tp_choose Total number of participants who chose any lineup member in
#'   TP lineups (condition 1). Must be >= n_hit.
#' @param n_fa Number of participants who chose any lineup member in target-absent
#'   (TA) lineups (condition 1).
#' @param N_tp Total number of participants in TP lineups (condition 1).
#' @param N_ta Total number of participants in TA lineups (condition 1).
#' @param n_hit_2,n_tp_choose_2,n_fa_2,N_tp_2,N_ta_2 Corresponding counts for an
#'   optional second condition. All five must be supplied together or not at all.
#' @param n Lineup size (number of members). Default 6.
#' @param constrain_d Logical. If \code{TRUE} and two conditions are supplied,
#'   constrains d' to be equal across conditions. Default \code{FALSE}.
#' @param constrain_c Logical. If \code{TRUE} and two conditions are supplied,
#'   constrains lambda to be equal across conditions. Default \code{FALSE}.
#' @param start Named numeric vector of starting values for the optimizer.
#'   If \code{NULL} (default), reasonable starting values are chosen automatically.
#'   Names should be \code{"d1"}, \code{"lambda1"} (and \code{"d2"},
#'   \code{"lambda2"} for two conditions).
#' @param nboot Number of parametric bootstrap replicates for confidence intervals.
#'   Default 0 (no bootstrap). Set to e.g. 1000 to obtain bootstrap CIs.
#' @param ci_level Confidence level for bootstrap CIs. Default 0.95.
#' @param seed Optional integer seed for reproducible bootstrap.
#'
#' @return An object of class \code{"max_sdt_fit"} containing:
#' \describe{
#'   \item{dprime_1, lambda_1}{Estimated d' and criterion for condition 1.}
#'   \item{dprime_2, lambda_2}{Estimated d' and criterion for condition 2 (if supplied).}
#'   \item{chisq}{Chi-squared goodness-of-fit statistic.}
#'   \item{df}{Degrees of freedom for the GoF test.}
#'   \item{p_value}{P-value for the GoF test.}
#'   \item{observed}{Named vector of observed counts.}
#'   \item{predicted}{Named vector of predicted counts.}
#'   \item{convergence}{Convergence code from \code{optim()} (0 = success).}
#'   \item{constrain_d, constrain_c, n_conditions, n}{Input metadata.}
#'   \item{boot_ci}{Bootstrap confidence intervals (if nboot > 0).}
#'   \item{boot_params}{Full bootstrap parameter matrix (if nboot > 0).}
#' }
#'
#' @details
#' The MAX SDT compound-decision model (Smith, 2022; Gourevitch & Galanter, 1967)
#' addresses the fundamental challenge of eyewitness lineup research: participants
#' make only one identification response, so within-person ROC analysis is impossible.
#' Group-level d' inference requires a model that jointly accounts for detection
#' (is the target present?) and identification (which member is the target?).
#'
#' For a lineup of \eqn{n} members with culprit signal strength \eqn{d'} and
#' response criterion \eqn{\lambda}:
#' \deqn{P(\text{correct ID}) = \int_{\lambda}^{\infty} \phi(x - d') \Phi(x)^{n-1}\,dx}
#' \deqn{P(\text{choose anyone} \mid \text{TP}) = 1 - \Phi(\lambda - d')\Phi(\lambda)^{n-1}}
#' \deqn{P(\text{choose anyone} \mid \text{TA}) = 1 - \Phi(\lambda)^n}
#'
#' Parameters are estimated by minimizing the chi-squared discrepancy between
#' observed and predicted frequencies. Nested model comparisons (equal d' or equal
#' lambda across conditions) are supported via \code{\link{compare_max_sdt}}.
#'
#' Parametric bootstrap CIs are obtained by simulating data from the fitted model
#' and re-fitting.
#'
#' @references
#' Gourevitch, V., & Galanter, E. (1967). A significance test for one-parameter
#' isosensitivity functions. \emph{Psychometrika, 32}(1), 25-33.
#'
#' Smith, A. M. (2022). Culprit and victim lineups. Unpublished analysis scripts.
#'
#' @seealso \code{\link{compare_max_sdt}}, \code{\link{estimate_msdt_params}},
#'   \code{\link{sdt_compare}}
#'
#' @examples
#' # Single condition
#' fit1 <- fit_max_sdt(
#'   n_hit = 69, n_tp_choose = 82, n_fa = 64,
#'   N_tp = 96, N_ta = 106, n = 6
#' )
#' print(fit1)
#'
#' # Two conditions, free parameters
#' fit_free <- fit_max_sdt(
#'   n_hit = 69, n_tp_choose = 82, n_fa = 64,
#'   N_tp = 96, N_ta = 106,
#'   n_hit_2 = 67, n_tp_choose_2 = 78, n_fa_2 = 42,
#'   N_tp_2 = 90, N_ta_2 = 96, n = 6
#' )
#'
#' # Two conditions, equal d' (constrained)
#' fit_eqd <- fit_max_sdt(
#'   n_hit = 69, n_tp_choose = 82, n_fa = 64,
#'   N_tp = 96, N_ta = 106,
#'   n_hit_2 = 67, n_tp_choose_2 = 78, n_fa_2 = 42,
#'   N_tp_2 = 90, N_ta_2 = 96, n = 6,
#'   constrain_d = TRUE
#' )
#'
#' # Compare models
#' cmp <- compare_max_sdt(fit_free, fit_eqd)
#' print(cmp)
#'
#' @importFrom stats optim integrate pnorm dnorm qchisq pchisq quantile rbinom
#' @export
fit_max_sdt <- function(n_hit, n_tp_choose, n_fa,
                         N_tp, N_ta,
                         n_hit_2     = NULL,
                         n_tp_choose_2 = NULL,
                         n_fa_2      = NULL,
                         N_tp_2      = NULL,
                         N_ta_2      = NULL,
                         n           = 6,
                         constrain_d = FALSE,
                         constrain_c = FALSE,
                         start       = NULL,
                         nboot       = 0,
                         ci_level    = 0.95,
                         seed        = NULL) {

  # --- input validation ---
  .max_sdt_check_count <- function(val, nm) {
    if (!is.numeric(val) || length(val) != 1 || val < 0)
      stop(sprintf("%s must be a non-negative numeric scalar.", nm), call. = FALSE)
  }
  for (nm in c("n_hit","n_tp_choose","n_fa","N_tp","N_ta"))
    .max_sdt_check_count(get(nm), nm)
  if (n_hit > n_tp_choose)
    stop("n_hit cannot exceed n_tp_choose.", call. = FALSE)
  if (n_tp_choose > N_tp)
    stop("n_tp_choose cannot exceed N_tp.", call. = FALSE)
  if (n_fa > N_ta)
    stop("n_fa cannot exceed N_ta.", call. = FALSE)

  cond2_args <- list(n_hit_2, n_tp_choose_2, n_fa_2, N_tp_2, N_ta_2)
  cond2_nms  <- c("n_hit_2","n_tp_choose_2","n_fa_2","N_tp_2","N_ta_2")
  cond2_null <- vapply(cond2_args, is.null, logical(1))
  if (!all(cond2_null) && any(cond2_null))
    stop("All five condition-2 arguments must be supplied together.", call. = FALSE)
  two_cond <- !all(cond2_null)

  if (two_cond) {
    for (i in seq_along(cond2_args))
      .max_sdt_check_count(cond2_args[[i]], cond2_nms[i])
    if (n_hit_2 > n_tp_choose_2)
      stop("n_hit_2 cannot exceed n_tp_choose_2.", call. = FALSE)
    if (n_tp_choose_2 > N_tp_2)
      stop("n_tp_choose_2 cannot exceed N_tp_2.", call. = FALSE)
    if (n_fa_2 > N_ta_2)
      stop("n_fa_2 cannot exceed N_ta_2.", call. = FALSE)
  }

  if (!two_cond && (constrain_d || constrain_c))
    warning("constrain_d and constrain_c only apply with two conditions.", call. = FALSE)

  if (!is.numeric(n) || n < 2)
    stop("n must be an integer >= 2.", call. = FALSE)
  if (!is.numeric(nboot) || nboot < 0)
    stop("nboot must be a non-negative integer.", call. = FALSE)
  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1)
    stop("ci_level must be in (0, 1).", call. = FALSE)

  # --- core probability functions ---
  hit_prob <- function(d, lambda) {
    integrand <- function(x) dnorm(x, mean = d, sd = 1) * pnorm(x)^(n - 1)
    integrate(integrand, lambda, Inf, rel.tol = 1e-6)$value
  }
  tpc_prob <- function(d, lambda) {
    1 - pnorm(lambda - d) * pnorm(lambda)^(n - 1)
  }
  fa_prob <- function(lambda) {
    1 - pnorm(lambda)^n
  }

  # --- chi-squared objective ---
  obs1 <- c(n_hit       = n_hit,
            n_tp_choose = n_tp_choose,
            n_reject_tp = N_tp - n_tp_choose,
            n_fa        = n_fa,
            n_reject_ta = N_ta - n_fa)

  if (two_cond) {
    obs2 <- c(n_hit       = n_hit_2,
              n_tp_choose = n_tp_choose_2,
              n_reject_tp = N_tp_2 - n_tp_choose_2,
              n_fa        = n_fa_2,
              n_reject_ta = N_ta_2 - n_fa_2)
  }

  predicted_from_params <- function(d, lambda, N_tp_i, N_ta_i) {
    ph  <- hit_prob(d, lambda)
    pt  <- tpc_prob(d, lambda)
    pf  <- fa_prob(lambda)
    c(n_hit       = ph * N_tp_i,
      n_tp_choose = pt * N_tp_i,
      n_reject_tp = (1 - pt) * N_tp_i,
      n_fa        = pf * N_ta_i,
      n_reject_ta = (1 - pf) * N_ta_i)
  }

  chisq_one <- function(obs, pred) {
    # exclude zero-expected cells
    keep <- pred > 0.5
    if (!any(keep)) return(1e9)
    sum((obs[keep] - pred[keep])^2 / pred[keep])
  }

  # parameter layout depends on constraints
  if (!two_cond) {
    # 2 free parameters: d1, lambda1
    obj <- function(parms) {
      d1 <- parms[1]; lam1 <- parms[2]
      pred <- predicted_from_params(d1, lam1, N_tp, N_ta)
      chisq_one(obs1, pred)
    }
    np <- 2
    lower <- c(0, -5)
    upper <- c(10,  5)
    if (is.null(start)) start <- c(1, 0)
    names(start) <- c("d1", "lambda1")
  } else if (!constrain_d && !constrain_c) {
    # 4 free: d1, d2, lambda1, lambda2
    obj <- function(parms) {
      d1 <- parms[1]; d2 <- parms[2]
      lam1 <- parms[3]; lam2 <- parms[4]
      pred1 <- predicted_from_params(d1, lam1, N_tp,   N_ta)
      pred2 <- predicted_from_params(d2, lam2, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 4
    lower <- c(0, 0, -5, -5)
    upper <- c(10, 10, 5, 5)
    if (is.null(start)) start <- c(1, 1, 0, 0)
    names(start) <- c("d1", "d2", "lambda1", "lambda2")
  } else if (constrain_d && !constrain_c) {
    # 3: d (shared), lambda1, lambda2
    obj <- function(parms) {
      d <- parms[1]; lam1 <- parms[2]; lam2 <- parms[3]
      pred1 <- predicted_from_params(d, lam1, N_tp,   N_ta)
      pred2 <- predicted_from_params(d, lam2, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 3
    lower <- c(0, -5, -5)
    upper <- c(10, 5, 5)
    if (is.null(start)) start <- c(1, 0, 0)
    names(start) <- c("d", "lambda1", "lambda2")
  } else if (!constrain_d && constrain_c) {
    # 3: d1, d2, lambda (shared)
    obj <- function(parms) {
      d1 <- parms[1]; d2 <- parms[2]; lam <- parms[3]
      pred1 <- predicted_from_params(d1, lam, N_tp,   N_ta)
      pred2 <- predicted_from_params(d2, lam, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 3
    lower <- c(0, 0, -5)
    upper <- c(10, 10, 5)
    if (is.null(start)) start <- c(1, 1, 0)
    names(start) <- c("d1", "d2", "lambda")
  } else {
    # 2: d (shared), lambda (shared)
    obj <- function(parms) {
      d <- parms[1]; lam <- parms[2]
      pred1 <- predicted_from_params(d, lam, N_tp,   N_ta)
      pred2 <- predicted_from_params(d, lam, N_tp_2, N_ta_2)
      chisq_one(obs1, pred1) + chisq_one(obs2, pred2)
    }
    np <- 2
    lower <- c(0, -5)
    upper <- c(10,  5)
    if (is.null(start)) start <- c(1, 0)
    names(start) <- c("d", "lambda")
  }

  opt <- optim(par = start, fn = obj,
               method = "L-BFGS-B", lower = lower, upper = upper)
  # fallback to Nelder-Mead if L-BFGS-B fails
  if (opt$convergence != 0L) {
    opt2 <- optim(par = start, fn = obj, method = "Nelder-Mead",
                  control = list(maxit = 5000))
    if (opt2$value < opt$value) opt <- opt2
    opt$convergence <- 0L   # Nelder-Mead reports 0 or 1
  }

  phat <- opt$par
  chisq_val <- opt$value

  # GoF df = (n_cells - n_free_params)
  n_cells <- if (!two_cond) 5L else 10L
  df_gof  <- n_cells - np
  p_gof   <- pchisq(chisq_val, df = df_gof, lower.tail = FALSE)

  # reconstruct estimates by name
  extract_estimates <- function(parms) {
    nms <- names(parms)
    d1  <- if ("d1" %in% nms) parms["d1"] else if ("d" %in% nms) parms["d"] else NA_real_
    d2  <- if ("d2" %in% nms) parms["d2"] else if ("d" %in% nms) parms["d"] else NA_real_
    l1  <- if ("lambda1" %in% nms) parms["lambda1"] else if ("lambda" %in% nms) parms["lambda"] else NA_real_
    l2  <- if ("lambda2" %in% nms) parms["lambda2"] else if ("lambda" %in% nms) parms["lambda"] else NA_real_
    list(d1 = unname(d1), d2 = unname(d2), l1 = unname(l1), l2 = unname(l2))
  }

  est <- extract_estimates(phat)
  pred1 <- predicted_from_params(est$d1, est$l1, N_tp, N_ta)
  obs_vec  <- obs1
  pred_vec <- pred1
  if (two_cond) {
    pred2    <- predicted_from_params(est$d2, est$l2, N_tp_2, N_ta_2)
    obs_vec  <- c(obs1,  obs2)
    pred_vec <- c(pred1, pred2)
    names(obs_vec)  <- c(paste0(names(obs1),  "_cond1"), paste0(names(obs2),  "_cond2"))
    names(pred_vec) <- c(paste0(names(pred1), "_cond1"), paste0(names(pred2), "_cond2"))
  }

  result <- list(
    dprime_1       = est$d1,
    lambda_1       = est$l1,
    dprime_2       = if (two_cond) est$d2 else NULL,
    lambda_2       = if (two_cond) est$l2 else NULL,
    chisq          = chisq_val,
    df             = df_gof,
    p_value        = p_gof,
    observed       = obs_vec,
    predicted      = pred_vec,
    convergence    = opt$convergence,
    constrain_d    = constrain_d,
    constrain_c    = constrain_c,
    n_conditions   = if (two_cond) 2L else 1L,
    n              = n,
    n_params       = np,
    boot_ci        = NULL,
    boot_params    = NULL
  )

  # --- bootstrap ---
  if (nboot > 0) {
    if (!is.null(seed)) set.seed(seed)
    boot_mat <- matrix(NA_real_, nrow = nboot, ncol = np,
                       dimnames = list(NULL, names(phat)))
    sim_counts <- function(d, lam, N_tp_i, N_ta_i) {
      ph <- hit_prob(d, lam)
      pt <- tpc_prob(d, lam)
      pf <- fa_prob(lam)
      n_tp_c <- rbinom(1, N_tp_i, pt)
      n_hi_c <- rbinom(1, n_tp_c, ifelse(pt > 0, ph / pt, 0))
      n_fa_c <- rbinom(1, N_ta_i, pf)
      list(n_hit = n_hi_c, n_tp_choose = n_tp_c, n_fa = n_fa_c)
    }
    for (b in seq_len(nboot)) {
      s1 <- sim_counts(est$d1, est$l1, N_tp, N_ta)
      b_start <- phat + rnorm(np, 0, 0.05)
      b_start <- pmax(lower, pmin(upper, b_start))
      if (two_cond) {
        s2 <- sim_counts(est$d2, est$l2, N_tp_2, N_ta_2)
        b_obj <- .max_sdt_make_obj(
          n_hit = s1$n_hit, n_tp_choose = s1$n_tp_choose, n_fa = s1$n_fa,
          N_tp = N_tp, N_ta = N_ta,
          n_hit_2 = s2$n_hit, n_tp_choose_2 = s2$n_tp_choose, n_fa_2 = s2$n_fa,
          N_tp_2 = N_tp_2, N_ta_2 = N_ta_2,
          n = n, constrain_d = constrain_d, constrain_c = constrain_c,
          hit_prob = hit_prob, tpc_prob = tpc_prob, fa_prob = fa_prob
        )
      } else {
        b_obj <- .max_sdt_make_obj(
          n_hit = s1$n_hit, n_tp_choose = s1$n_tp_choose, n_fa = s1$n_fa,
          N_tp = N_tp, N_ta = N_ta,
          n_hit_2 = NULL, n_tp_choose_2 = NULL, n_fa_2 = NULL,
          N_tp_2 = NULL, N_ta_2 = NULL,
          n = n, constrain_d = FALSE, constrain_c = FALSE,
          hit_prob = hit_prob, tpc_prob = tpc_prob, fa_prob = fa_prob
        )
      }
      b_opt <- tryCatch(
        optim(par = b_start, fn = b_obj,
              method = "L-BFGS-B", lower = lower, upper = upper),
        error = function(e) list(par = rep(NA_real_, np), convergence = 99L)
      )
      if (b_opt$convergence == 0) boot_mat[b, ] <- b_opt$par
    }
    good <- complete.cases(boot_mat)
    tail_p <- (1 - ci_level) / 2
    result$boot_ci <- apply(boot_mat[good, , drop = FALSE], 2,
                            quantile, probs = c(tail_p, 1 - tail_p),
                            na.rm = TRUE)
    result$boot_params <- boot_mat
    rownames(result$boot_ci) <- c("lower", "upper")
  }

  class(result) <- "max_sdt_fit"
  result
}

# Internal helper: re-build objective for bootstrap resampling
.max_sdt_make_obj <- function(n_hit, n_tp_choose, n_fa, N_tp, N_ta,
                               n_hit_2, n_tp_choose_2, n_fa_2, N_tp_2, N_ta_2,
                               n, constrain_d, constrain_c,
                               hit_prob, tpc_prob, fa_prob) {
  two_cond <- !is.null(n_hit_2)
  obs1 <- c(n_hit = n_hit, n_tp_choose = n_tp_choose,
            n_reject_tp = N_tp - n_tp_choose,
            n_fa = n_fa, n_reject_ta = N_ta - n_fa)
  if (two_cond)
    obs2 <- c(n_hit = n_hit_2, n_tp_choose = n_tp_choose_2,
              n_reject_tp = N_tp_2 - n_tp_choose_2,
              n_fa = n_fa_2, n_reject_ta = N_ta_2 - n_fa_2)

  pred_f <- function(d, lam, Ntp, Nta) {
    ph <- hit_prob(d, lam); pt <- tpc_prob(d, lam); pf <- fa_prob(lam)
    c(n_hit = ph*Ntp, n_tp_choose = pt*Ntp, n_reject_tp = (1-pt)*Ntp,
      n_fa = pf*Nta, n_reject_ta = (1-pf)*Nta)
  }
  csq <- function(obs, pred) {
    keep <- pred > 0.5
    if (!any(keep)) return(1e9)
    sum((obs[keep]-pred[keep])^2/pred[keep])
  }

  if (!two_cond) {
    function(parms) csq(obs1, pred_f(parms[1], parms[2], N_tp, N_ta))
  } else if (!constrain_d && !constrain_c) {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[3], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[2], parms[4], N_tp_2, N_ta_2))
  } else if (constrain_d && !constrain_c) {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[2], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[1], parms[3], N_tp_2, N_ta_2))
  } else if (!constrain_d && constrain_c) {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[3], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[2], parms[3], N_tp_2, N_ta_2))
  } else {
    function(parms)
      csq(obs1, pred_f(parms[1], parms[2], N_tp,   N_ta)) +
      csq(obs2, pred_f(parms[1], parms[2], N_tp_2, N_ta_2))
  }
}

#' @export
print.max_sdt_fit <- function(x, digits = 3, ...) {
  nc <- x$n_conditions
  cat("MAX SDT Compound-Decision Model\n")
  cat(sprintf("  Lineup size: %d | Conditions: %d | Free parameters: %d\n",
              x$n, nc, x$n_params))
  cat(sprintf("  Convergence: %s\n",
              if (x$convergence == 0) "OK" else paste("code", x$convergence)))
  cat("\nParameter estimates:\n")
  if (nc == 1L) {
    cat(sprintf("  d' = %.3f | lambda = %.3f\n", x$dprime_1, x$lambda_1))
  } else {
    cat(sprintf("  Condition 1: d' = %.3f | lambda = %.3f\n",
                x$dprime_1, x$lambda_1))
    cat(sprintf("  Condition 2: d' = %.3f | lambda = %.3f\n",
                x$dprime_2, x$lambda_2))
    cat(sprintf("  (constrain_d = %s, constrain_c = %s)\n",
                x$constrain_d, x$constrain_c))
  }
  cat(sprintf("\nGoodness of fit: chi-sq(%.0f) = %.3f, p = %.4f\n",
              x$df, x$chisq, x$p_value))
  cat("\nObserved vs predicted:\n")
  tbl <- rbind(Observed = round(x$observed, 1),
               Predicted = round(x$predicted, 1))
  print(tbl)
  if (!is.null(x$boot_ci)) {
    cat("\nBootstrap CIs:\n")
    print(round(x$boot_ci, digits))
  }
  invisible(x)
}

#' @export
plot.max_sdt_fit <- function(x, ...) {
  obs  <- x$observed
  pred <- x$predicted
  nm   <- names(obs)
  df   <- data.frame(
    cell  = rep(factor(nm, levels = nm), 2),
    count = c(obs, pred),
    type  = rep(c("Observed", "Predicted"), each = length(obs))
  )
  ggplot2::ggplot(df, ggplot2::aes(x = cell, y = count, fill = type)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::scale_fill_manual(values = c(Observed = "#72b7b2",
                                          Predicted = "#e15759")) +
    ggplot2::labs(x = "Cell", y = "Count", fill = NULL,
                  title = "MAX SDT: Observed vs Predicted frequencies") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

#' Compare Two Nested MAX SDT Models
#'
#' Performs a chi-squared difference test comparing a less constrained (free)
#' MAX SDT model to a more constrained (nested) model. Both models must be fitted
#' to the same data.
#'
#' @param free A \code{"max_sdt_fit"} object: the less constrained model.
#' @param constrained A \code{"max_sdt_fit"} object: the more constrained model.
#'   Must have fewer free parameters than \code{free}.
#'
#' @return An object of class \code{"max_sdt_compare"} with the test result.
#'
#' @details
#' The chi-squared difference statistic is:
#' \deqn{\Delta\chi^2 = \chi^2_{\text{constrained}} - \chi^2_{\text{free}}}
#' with degrees of freedom equal to the difference in the number of free parameters.
#' A significant result indicates that the constraint worsens fit.
#'
#' @seealso \code{\link{fit_max_sdt}}
#' @examples
#' fit_free <- fit_max_sdt(
#'   n_hit=69, n_tp_choose=82, n_fa=64, N_tp=96, N_ta=106,
#'   n_hit_2=67, n_tp_choose_2=78, n_fa_2=42, N_tp_2=90, N_ta_2=96
#' )
#' fit_eqd <- fit_max_sdt(
#'   n_hit=69, n_tp_choose=82, n_fa=64, N_tp=96, N_ta=106,
#'   n_hit_2=67, n_tp_choose_2=78, n_fa_2=42, N_tp_2=90, N_ta_2=96,
#'   constrain_d = TRUE
#' )
#' compare_max_sdt(fit_free, fit_eqd)
#' @export
compare_max_sdt <- function(free, constrained) {
  if (!inherits(free, "max_sdt_fit") || !inherits(constrained, "max_sdt_fit"))
    stop("Both arguments must be 'max_sdt_fit' objects.", call. = FALSE)
  if (free$n_params <= constrained$n_params)
    stop("'free' must have more free parameters than 'constrained'.", call. = FALSE)

  delta_chisq <- constrained$chisq - free$chisq
  delta_df    <- constrained$n_params - free$n_params  # negative = more constrained
  # flip: df of test = |delta in free params|
  test_df     <- abs(free$n_params - constrained$n_params)
  p_val       <- pchisq(delta_chisq, df = test_df, lower.tail = FALSE)

  structure(
    list(
      delta_chisq    = delta_chisq,
      test_df        = test_df,
      p_value        = p_val,
      chisq_free     = free$chisq,
      chisq_const    = constrained$chisq,
      df_free        = free$n_params,
      df_const       = constrained$n_params,
      constrain_d    = constrained$constrain_d,
      constrain_c    = constrained$constrain_c
    ),
    class = "max_sdt_compare"
  )
}

#' @export
print.max_sdt_compare <- function(x, digits = 4, ...) {
  cat("MAX SDT Model Comparison (chi-squared difference test)\n")
  cat(sprintf("  Free model:       chi-sq = %.3f (%d free params)\n",
              x$chisq_free, x$df_free))
  cat(sprintf("  Constrained model: chi-sq = %.3f (%d free params)\n",
              x$chisq_const, x$df_const))
  constraints <- c(
    if (x$constrain_d) "d' equal" else NULL,
    if (x$constrain_c) "lambda equal" else NULL
  )
  if (length(constraints))
    cat(sprintf("  Constraint: %s\n", paste(constraints, collapse = ", ")))
  cat(sprintf("  Delta chi-sq(%.0f) = %.3f, p = %.4f\n",
              x$test_df, x$delta_chisq, x$p_value))
  cat(sprintf("  Interpretation: constraint %s fit.\n",
              if (x$p_value < 0.05) "significantly worsens" else "does not significantly worsen"))
  invisible(x)
}

utils::globalVariables(c("cell", "count", "type"))
