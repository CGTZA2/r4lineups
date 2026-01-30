#' Winter et al. (2022) Two-High Threshold (2-HT) MPT Model for Eyewitness Identification
#'
#' Fits the 2-HT multinomial processing tree (MPT) model to lineup identification data.
#' This model separates detection-based processes (dP, dA) from non-detection-based
#' processes (biased selection b, guessing-based selection g) using the full 2x3
#' outcome structure of lineup procedures.
#'
#' @param data A data frame with lineup identification data, OR a named vector/list
#'   of counts with the following elements:
#'   - n_tp_suspect: Number of suspect IDs in target-present (TP) lineups
#'   - n_tp_filler: Number of filler IDs in TP lineups
#'   - n_tp_reject: Number of rejections in TP lineups
#'   - n_ta_suspect: Number of suspect IDs in target-absent (TA) lineups
#'   - n_ta_filler: Number of filler IDs in TA lineups
#'   - n_ta_reject: Number of rejections in TA lineups
#' @param lineup_size Size of the lineup (L). Default is 6.
#' @param target_present Column name indicating target presence (TRUE/FALSE) if data is a data frame.
#' @param identification Column name for identification decision if data is a data frame.
#'   Should contain "suspect", "filler", or "reject".
#' @param start_params Optional named vector of starting parameter values (dP, dA, b, g).
#'   If NULL, uses reasonable defaults.
#' @param method Optimization method. Default is "L-BFGS-B" for bounded optimization.
#' @param ... Additional arguments passed to optim().
#'
#' @return An object of class "winter_2ht" containing:
#'   \item{parameters}{Estimated parameters (dP, dA, b, g)}
#'   \item{se}{Standard errors of parameter estimates}
#'   \item{loglik}{Log-likelihood of the fitted model}
#'   \item{aic}{Akaike Information Criterion}
#'   \item{bic}{Bayesian Information Criterion}
#'   \item{fitted_probs}{Model-predicted probabilities for each outcome}
#'   \item{observed_counts}{Observed counts}
#'   \item{expected_counts}{Expected counts under the model}
#'   \item{lineup_size}{Lineup size used}
#'   \item{convergence}{Convergence code from optim}
#'   \item{n_total}{Total sample size}
#'
#' @details
#' The 2-HT model (Winter, Menne, Bell, & Buchner, 2022) uses four parameters:
#' \itemize{
#'   \item \strong{dP}: Probability of detecting culprit presence (0 to 1)
#'   \item \strong{dA}: Probability of detecting culprit absence (0 to 1)
#'   \item \strong{b}: Probability of biased suspect selection (0 to 1)
#'   \item \strong{g}: Probability of guessing-based selection (0 to 1)
#' }
#'
#' Model equations for culprit-present lineups:
#' \itemize{
#'   \item P(suspect ID) = dP + (1-dP) * [b + (1-b) * g * (1/L)]
#'   \item P(filler ID) = (1-dP) * (1-b) * g * ((L-1)/L)
#'   \item P(reject) = (1-dP) * (1-b) * (1-g)
#' }
#'
#' Model equations for culprit-absent lineups:
#' \itemize{
#'   \item P(suspect ID) = (1-dA) * [b + (1-b) * g * (1/L)]
#'   \item P(filler ID) = (1-dA) * (1-b) * g * ((L-1)/L)
#'   \item P(reject) = dA + (1-dA) * (1-b) * (1-g)
#' }
#'
#' @references
#' Winter, K., Menne, N. M., Bell, R., & Buchner, A. (2022). Experimental validation
#' of a multinomial processing tree model for analyzing eyewitness identification
#' decisions. Scientific Reports, 12, 15571. https://doi.org/10.1038/s41598-022-19513-w
#'
#' @examples
#' \dontrun{
#' # Example 1: Using count data directly
#' counts <- c(
#'   n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
#'   n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
#' )
#' fit <- fit_winter_2ht(counts, lineup_size = 6)
#' print(fit)
#' summary(fit)
#'
#' # Example 2: Using a data frame
#' # Assuming you have data with columns 'target_present' and 'identification'
#' fit <- fit_winter_2ht(
#'   lineup_data,
#'   lineup_size = 6,
#'   target_present = "target_present",
#'   identification = "identification"
#' )
#' }
#'
#' @export
fit_winter_2ht <- function(data,
                           lineup_size = 6,
                           target_present = NULL,
                           identification = NULL,
                           start_params = NULL,
                           method = "L-BFGS-B",
                           ...) {

  # Process input data
  if (is.data.frame(data)) {
    if (is.null(target_present) || is.null(identification)) {
      stop("When data is a data frame, must specify 'target_present' and 'identification' column names")
    }
    counts <- .extract_counts_from_df(data, target_present, identification)
  } else {
    counts <- data
  }

  # Validate counts
  required_names <- c("n_tp_suspect", "n_tp_filler", "n_tp_reject",
                      "n_ta_suspect", "n_ta_filler", "n_ta_reject")

  if (!all(required_names %in% names(counts))) {
    stop("Count data must contain: ", paste(required_names, collapse = ", "))
  }

  # Extract counts
  n_tp <- c(counts["n_tp_suspect"], counts["n_tp_filler"], counts["n_tp_reject"])
  n_ta <- c(counts["n_ta_suspect"], counts["n_ta_filler"], counts["n_ta_reject"])

  # Total sample sizes
  N_tp <- sum(n_tp)
  N_ta <- sum(n_ta)
  N_total <- N_tp + N_ta

  # Set starting parameters if not provided
  if (is.null(start_params)) {
    start_params <- c(
      dP = 0.3,  # Detection of culprit presence
      dA = 0.1,  # Detection of culprit absence
      b = 0.05,  # Biased selection
      g = 0.5    # Guessing
    )
  }

  # Negative log-likelihood function
  negloglik <- function(params) {
    dP <- params[1]
    dA <- params[2]
    b <- params[3]
    g <- params[4]

    # Compute predicted probabilities
    probs <- .compute_2ht_probs(dP, dA, b, g, lineup_size)

    # Log-likelihood (multinomial)
    ll_tp <- sum(n_tp * log(probs$tp + 1e-10))  # Add small constant to avoid log(0)
    ll_ta <- sum(n_ta * log(probs$ta + 1e-10))

    ll_total <- ll_tp + ll_ta

    # Return negative (for minimization)
    return(-ll_total)
  }

  # Optimize with bounded parameters [0, 1]
  opt_result <- stats::optim(
    par = start_params,
    fn = negloglik,
    method = method,
    lower = c(0, 0, 0, 0),
    upper = c(1, 1, 1, 1),
    hessian = TRUE,
    ...
  )

  # Extract estimates
  estimates <- opt_result$par
  names(estimates) <- c("dP", "dA", "b", "g")

  # Compute standard errors from Hessian
  se <- tryCatch({
    sqrt(diag(solve(opt_result$hessian)))
  }, error = function(e) {
    warning("Could not compute standard errors from Hessian")
    rep(NA, 4)
  })
  names(se) <- names(estimates)

  # Compute fitted probabilities and expected counts
  fitted_probs <- .compute_2ht_probs(
    estimates["dP"], estimates["dA"],
    estimates["b"], estimates["g"],
    lineup_size
  )

  expected_counts <- list(
    tp = fitted_probs$tp * N_tp,
    ta = fitted_probs$ta * N_ta
  )

  # Log-likelihood
  loglik <- -opt_result$value

  # AIC and BIC
  k <- 4  # Number of parameters
  aic <- 2 * k - 2 * loglik
  bic <- k * log(N_total) - 2 * loglik

  # Create result object
  result <- list(
    parameters = estimates,
    se = se,
    loglik = loglik,
    aic = aic,
    bic = bic,
    fitted_probs = fitted_probs,
    observed_counts = list(
      tp = n_tp,
      ta = n_ta
    ),
    expected_counts = expected_counts,
    lineup_size = lineup_size,
    convergence = opt_result$convergence,
    n_total = N_total,
    n_tp = N_tp,
    n_ta = N_ta
  )

  class(result) <- "winter_2ht"
  return(result)
}


#' Compute 2-HT Model Predicted Probabilities
#'
#' @param dP Detection of culprit presence
#' @param dA Detection of culprit absence
#' @param b Biased selection
#' @param g Guessing
#' @param L Lineup size
#'
#' @return List with tp (target-present) and ta (target-absent) probability vectors
#' @keywords internal
.compute_2ht_probs <- function(dP, dA, b, g, L) {

  # Target-present probabilities
  p_tp_suspect <- dP + (1 - dP) * (b + (1 - b) * g * (1/L))
  p_tp_filler <- (1 - dP) * (1 - b) * g * ((L - 1)/L)
  p_tp_reject <- (1 - dP) * (1 - b) * (1 - g)

  # Target-absent probabilities
  p_ta_suspect <- (1 - dA) * (b + (1 - b) * g * (1/L))
  p_ta_filler <- (1 - dA) * (1 - b) * g * ((L - 1)/L)
  p_ta_reject <- dA + (1 - dA) * (1 - b) * (1 - g)

  return(list(
    tp = c(p_tp_suspect, p_tp_filler, p_tp_reject),
    ta = c(p_ta_suspect, p_ta_filler, p_ta_reject)
  ))
}


#' Extract Counts from Data Frame
#'
#' @param data Data frame with lineup identification data
#' @param target_present Column name for target presence indicator
#' @param identification Column name for identification decision
#'
#' @return Named vector of counts
#' @keywords internal
.extract_counts_from_df <- function(data, target_present, identification) {

  # Ensure columns exist
  if (!target_present %in% names(data)) {
    stop("Column '", target_present, "' not found in data")
  }
  if (!identification %in% names(data)) {
    stop("Column '", identification, "' not found in data")
  }

  # Split by target presence
  tp_data <- data[data[[target_present]] == TRUE, ]
  ta_data <- data[data[[target_present]] == FALSE, ]

  # Count outcomes
  count_outcomes <- function(df, id_col) {
    id_vals <- tolower(as.character(df[[id_col]]))
    c(
      suspect = sum(id_vals == "suspect"),
      filler = sum(id_vals == "filler"),
      reject = sum(id_vals == "reject")
    )
  }

  tp_counts <- count_outcomes(tp_data, identification)
  ta_counts <- count_outcomes(ta_data, identification)

  return(c(
    n_tp_suspect = unname(tp_counts["suspect"]),
    n_tp_filler = unname(tp_counts["filler"]),
    n_tp_reject = unname(tp_counts["reject"]),
    n_ta_suspect = unname(ta_counts["suspect"]),
    n_ta_filler = unname(ta_counts["filler"]),
    n_ta_reject = unname(ta_counts["reject"])
  ))
}


#' Print Method for winter_2ht Objects
#'
#' @param x A winter_2ht object
#' @param ... Additional arguments (not used)
#'
#' @export
print.winter_2ht <- function(x, ...) {
  cat("\nWinter et al. (2022) Two-High Threshold MPT Model\n")
  cat("==================================================\n\n")

  cat("Sample size:\n")
  cat(sprintf("  Target-present: %d\n", x$n_tp))
  cat(sprintf("  Target-absent:  %d\n", x$n_ta))
  cat(sprintf("  Total:          %d\n", x$n_total))
  cat(sprintf("  Lineup size:    %d\n\n", x$lineup_size))

  cat("Parameter Estimates:\n")
  param_table <- data.frame(
    Estimate = round(x$parameters, 4),
    SE = round(x$se, 4)
  )
  rownames(param_table) <- c("dP (culprit presence)", "dA (culprit absence)",
                              "b  (biased selection)", "g  (guessing)")
  print(param_table)

  cat(sprintf("\nLog-likelihood: %.2f\n", x$loglik))
  cat(sprintf("AIC: %.2f\n", x$aic))
  cat(sprintf("BIC: %.2f\n", x$bic))

  if (x$convergence != 0) {
    cat(sprintf("\nWarning: Optimization did not converge (code %d)\n", x$convergence))
  }

  invisible(x)
}


#' Summary Method for winter_2ht Objects
#'
#' @param object A winter_2ht object
#' @param ... Additional arguments (not used)
#'
#' @export
summary.winter_2ht <- function(object, ...) {
  cat("\nWinter et al. (2022) Two-High Threshold MPT Model\n")
  cat("==================================================\n\n")

  cat("Sample size:\n")
  cat(sprintf("  Target-present: %d\n", object$n_tp))
  cat(sprintf("  Target-absent:  %d\n", object$n_ta))
  cat(sprintf("  Total:          %d\n", object$n_total))
  cat(sprintf("  Lineup size:    %d\n\n", object$lineup_size))

  cat("Parameter Estimates:\n")
  param_table <- data.frame(
    Estimate = round(object$parameters, 4),
    SE = round(object$se, 4),
    Lower_95 = round(object$parameters - 1.96 * object$se, 4),
    Upper_95 = round(object$parameters + 1.96 * object$se, 4)
  )
  rownames(param_table) <- c("dP (culprit presence)", "dA (culprit absence)",
                              "b  (biased selection)", "g  (guessing)")
  print(param_table)

  cat("\n\nModel Fit:\n")
  cat(sprintf("  Log-likelihood: %.2f\n", object$loglik))
  cat(sprintf("  AIC: %.2f\n", object$aic))
  cat(sprintf("  BIC: %.2f\n\n", object$bic))

  cat("Observed vs. Expected Counts:\n")
  cat("\nTarget-Present Lineups:\n")
  tp_table <- data.frame(
    Observed = object$observed_counts$tp,
    Expected = round(object$expected_counts$tp, 1),
    Residual = object$observed_counts$tp - object$expected_counts$tp
  )
  rownames(tp_table) <- c("Suspect ID", "Filler ID", "Reject")
  print(tp_table)

  cat("\nTarget-Absent Lineups:\n")
  ta_table <- data.frame(
    Observed = object$observed_counts$ta,
    Expected = round(object$expected_counts$ta, 1),
    Residual = object$observed_counts$ta - object$expected_counts$ta
  )
  rownames(ta_table) <- c("Suspect ID", "Filler ID", "Reject")
  print(ta_table)

  # Chi-square goodness of fit
  chi_sq <- sum((object$observed_counts$tp - object$expected_counts$tp)^2 / object$expected_counts$tp) +
            sum((object$observed_counts$ta - object$expected_counts$ta)^2 / object$expected_counts$ta)
  df <- 6 - 4  # 6 categories - 4 parameters
  p_value <- 1 - stats::pchisq(chi_sq, df)

  cat(sprintf("\nGoodness-of-fit test:\n"))
  cat(sprintf("  Chi-square = %.2f, df = %d, p = %.4f\n", chi_sq, df, p_value))

  if (object$convergence != 0) {
    cat(sprintf("\nWarning: Optimization did not converge (code %d)\n", object$convergence))
  }

  invisible(object)
}
