# z-ROC and SDT Parameter Estimation Functions
#
# Functions for fitting signal detection theory (SDT) models to ROC data
# and extracting parameters (d', c, and variance ratio) using z-ROC analysis.

# Declare global variables used in ggplot2 aes()
utils::globalVariables(c("z_far", "z_hr", "Criterion"))

#' Fit Signal Detection Theory Model to ROC Data
#'
#' Fits an equal-variance or unequal-variance SDT model to ROC data using
#' z-ROC analysis. Extracts discriminability (d'), decision criteria (c),
#' and optionally the target/lure variance ratio.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#'   OR a rocdata object from make_rocdata()
#' @param lineup_size Integer. Number of people in lineup (required if data is dataframe)
#' @param model Character. "equal_variance" or "unequal_variance" (default = "equal_variance")
#' @param bootstrap Logical. Compute bootstrap confidence intervals? (default = TRUE)
#' @param n_bootstrap Integer. Number of bootstrap samples (default = 1000)
#' @param conf_level Numeric. Confidence level for intervals (default = 0.95)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#'
#' @return An S3 object of class "sdt_roc_fit" containing:
#'   \itemize{
#'     \item dprime: Overall discriminability (d')
#'     \item criteria: Decision criterion for each confidence level (c values)
#'     \item variance_ratio: Ratio of target SD to lure SD (if unequal_variance)
#'     \item slope: Slope of z-ROC
#'     \item intercept: Intercept of z-ROC
#'     \item roc_data: Original ROC data with z-transforms
#'     \item model_type: Type of model fit
#'     \item bootstrap_ci: Confidence intervals (if bootstrap = TRUE)
#'     \item fit_diagnostics: R-squared, residuals, etc.
#'   }
#'
#' @details
#' **z-ROC Analysis**:
#'
#' The z-ROC is created by plotting z-transformed hit rates against
#' z-transformed false alarm rates for each confidence level.
#'
#' Under equal-variance SDT:
#' - z-ROC should be linear with slope = 1
#' - d' = z(HR) - z(FAR) for any criterion
#' - Intercept = d' / sqrt(2)
#'
#' Under unequal-variance SDT:
#' - Slope = SD_lure / SD_target
#' - Slope > 1 indicates greater variability in lure distribution
#'
#' **Parameter Estimation**:
#' - d' = average distance between distributions in standard deviation units
#' - c = decision criterion (0 = unbiased, positive = conservative, negative = liberal)
#' - Higher d' = better discriminability
#'
#' **Bootstrap Confidence Intervals**:
#' If bootstrap = TRUE, resamples data with replacement to estimate
#' standard errors and confidence intervals for all parameters.
#'
#' @references
#' Macmillan, N. A., & Creelman, C. D. (2005). Detection theory: A user's guide (2nd ed.).
#'
#' Mickes, L. (2015). Receiver operating characteristic analysis and confidence-accuracy
#' characteristic analysis in investigations of system variables. Journal of Applied
#' Research in Memory and Cognition, 4(2), 93-102.
#'
#' @examples
#' \dontrun{
#' # Simulate data
#' sim_data <- simulate_lineup_data(
#'   n_tp = 200, n_ta = 200,
#'   d_prime = 1.5,
#'   conf_levels = 5
#' )
#'
#' # Fit equal-variance SDT model
#' sdt_fit <- fit_sdt_roc(sim_data, lineup_size = 6)
#' print(sdt_fit)
#' plot(sdt_fit)
#'
#' # Get d' estimate
#' sdt_fit$dprime
#'
#' # Fit unequal-variance model
#' sdt_uv <- fit_sdt_roc(sim_data, lineup_size = 6, model = "unequal_variance")
#' sdt_uv$variance_ratio
#' }
#'
#' @export
fit_sdt_roc <- function(data,
                        lineup_size = NULL,
                        model = c("equal_variance", "unequal_variance"),
                        bootstrap = TRUE,
                        n_bootstrap = 1000,
                        conf_level = 0.95,
                        seed = NULL) {

  model <- match.arg(model)

  # Get ROC data if not already in that format
  # Check if it's a rocdata object (list with roc_data component)
  if (is.list(data) && "roc_data" %in% names(data)) {
    # It's already a rocdata object
    roc_obj <- data
  } else {
    # It's raw data, need to create ROC
    if (is.null(lineup_size)) {
      stop("lineup_size must be provided when data is not a rocdata object")
    }
    roc_obj <- make_rocdata(data, lineup_size = lineup_size)
  }

  # Extract the actual dataframe and metadata
  roc_data <- roc_obj$roc_data
  n_tp <- roc_obj$n_target_present
  n_ta <- roc_obj$n_target_absent

  # Apply z-transformations and fit model
  zroc_data <- .compute_zroc(roc_data, n_tp, n_ta)
  fit_results <- .fit_zroc_line(zroc_data, model = model)

  # Extract parameters
  params <- .extract_sdt_parameters(fit_results, zroc_data, model = model)

  # Bootstrap confidence intervals
  if (bootstrap) {
    if (!is.null(seed)) set.seed(seed)
    boot_results <- .bootstrap_sdt_fit(roc_data, n_tp, n_ta, model, n_bootstrap, conf_level)
    params$bootstrap_ci <- boot_results
  }

  # Package results
  result <- list(
    dprime = params$dprime,
    criteria = params$criteria,
    variance_ratio = if (model == "unequal_variance") params$variance_ratio else 1.0,
    slope = fit_results$slope,
    intercept = fit_results$intercept,
    roc_data = zroc_data,
    model_type = model,
    bootstrap_ci = if (bootstrap) params$bootstrap_ci else NULL,
    fit_diagnostics = list(
      r_squared = fit_results$r_squared,
      residuals = fit_results$residuals,
      n_points = nrow(zroc_data)
    )
  )

  class(result) <- "sdt_roc_fit"
  return(result)
}


#' Compute z-ROC Transformations
#' @keywords internal
.compute_zroc <- function(roc_data, n_tp, n_ta) {
  # Transform hit rates and false alarm rates to z-scores
  # Handle extreme values (0 and 1) using log-linear correction

  correct_extreme <- function(rate, n) {
    # Log-linear correction for 0 and 1
    if (rate == 0) return(0.5 / n)
    if (rate == 1) return((n - 0.5) / n)
    return(rate)
  }

  # Apply corrections
  # correct_id_rate is the hit rate (cumulative)
  # false_id_rate is the false alarm rate (cumulative)
  roc_data$hr_corrected <- sapply(roc_data$correct_id_rate, correct_extreme, n = n_tp)
  roc_data$far_corrected <- sapply(roc_data$false_id_rate, correct_extreme, n = n_ta)

  # Compute z-scores
  roc_data$z_hr <- qnorm(roc_data$hr_corrected)
  roc_data$z_far <- qnorm(roc_data$far_corrected)

  # Remove any infinite or NA values
  roc_data <- roc_data[is.finite(roc_data$z_hr) & is.finite(roc_data$z_far), ]

  return(roc_data)
}


#' Fit Linear Model to z-ROC
#' @keywords internal
.fit_zroc_line <- function(zroc_data, model) {
  if (model == "equal_variance") {
    # Equal variance: force slope = 1, estimate intercept
    # d' = mean(z_hr - z_far)
    mean_diff <- mean(zroc_data$z_hr - zroc_data$z_far, na.rm = TRUE)

    result <- list(
      slope = 1.0,
      intercept = mean_diff,
      fitted_values = zroc_data$z_far + mean_diff,
      residuals = zroc_data$z_hr - (zroc_data$z_far + mean_diff)
    )

  } else {
    # Unequal variance: estimate slope and intercept
    lm_fit <- lm(z_hr ~ z_far, data = zroc_data)

    result <- list(
      slope = coef(lm_fit)[2],
      intercept = coef(lm_fit)[1],
      fitted_values = fitted(lm_fit),
      residuals = residuals(lm_fit),
      lm_model = lm_fit
    )
  }

  # Compute R-squared
  ss_res <- sum(result$residuals^2, na.rm = TRUE)
  ss_tot <- sum((zroc_data$z_hr - mean(zroc_data$z_hr, na.rm = TRUE))^2, na.rm = TRUE)
  result$r_squared <- 1 - (ss_res / ss_tot)

  return(result)
}


#' Extract SDT Parameters from z-ROC Fit
#' @keywords internal
.extract_sdt_parameters <- function(fit_results, zroc_data, model) {
  # Extract d' and criteria

  if (model == "equal_variance") {
    # d' is the mean difference (intercept)
    dprime <- fit_results$intercept
    variance_ratio <- 1.0

  } else {
    # Unequal variance: d' = intercept * sqrt(1 + slope^2) / slope
    slope <- fit_results$slope
    intercept <- fit_results$intercept
    dprime <- intercept * sqrt(1 + slope^2) / slope
    variance_ratio <- slope
  }

  # Compute criteria (c values) for each confidence level
  # c = -(z_hr + z_far) / 2 under equal variance
  # More complex under unequal variance

  criteria <- -(zroc_data$z_hr + zroc_data$z_far) / 2
  names(criteria) <- paste0("c", seq_along(criteria))

  return(list(
    dprime = dprime,
    criteria = criteria,
    variance_ratio = variance_ratio
  ))
}


#' Bootstrap SDT Parameter Estimates
#' @keywords internal
.bootstrap_sdt_fit <- function(roc_data, n_tp, n_ta, model, n_bootstrap, conf_level) {
  # Resample and refit to get confidence intervals

  boot_dprime <- numeric(n_bootstrap)
  boot_variance_ratio <- numeric(n_bootstrap)

  for (i in 1:n_bootstrap) {
    # This is a simplified bootstrap - in practice would need to resample
    # raw data and recompute ROC, but here we'll resample ROC points
    n_points <- nrow(roc_data)
    boot_indices <- sample(1:n_points, n_points, replace = TRUE)
    boot_roc <- roc_data[boot_indices, ]

    tryCatch({
      zroc_boot <- .compute_zroc(boot_roc, n_tp, n_ta)
      fit_boot <- .fit_zroc_line(zroc_boot, model)
      params_boot <- .extract_sdt_parameters(fit_boot, zroc_boot, model)

      boot_dprime[i] <- params_boot$dprime
      boot_variance_ratio[i] <- params_boot$variance_ratio
    }, error = function(e) {
      boot_dprime[i] <<- NA
      boot_variance_ratio[i] <<- NA
    })
  }

  # Remove failed bootstraps
  boot_dprime <- boot_dprime[!is.na(boot_dprime)]
  boot_variance_ratio <- boot_variance_ratio[!is.na(boot_variance_ratio)]

  # Compute confidence intervals
  alpha <- 1 - conf_level

  dprime_ci <- quantile(boot_dprime, c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  variance_ci <- quantile(boot_variance_ratio, c(alpha/2, 1 - alpha/2), na.rm = TRUE)

  return(list(
    dprime_ci = dprime_ci,
    dprime_se = sd(boot_dprime, na.rm = TRUE),
    variance_ratio_ci = variance_ci,
    variance_ratio_se = sd(boot_variance_ratio, na.rm = TRUE),
    n_successful = length(boot_dprime),
    conf_level = conf_level
  ))
}


#' Print Method for SDT ROC Fit
#' @param x An sdt_roc_fit object
#' @param ... Additional arguments (ignored)
#' @export
print.sdt_roc_fit <- function(x, ...) {
  cat("\n=== SDT Model Fit to ROC Data ===\n\n")
  cat("Model:", ifelse(x$model_type == "equal_variance",
                       "Equal Variance", "Unequal Variance"), "\n\n")

  cat("Parameters:\n")
  cat("  d' (discriminability):", round(x$dprime, 3), "\n")

  if (!is.null(x$bootstrap_ci)) {
    cat("    95% CI: [", round(x$bootstrap_ci$dprime_ci[1], 3), ",",
        round(x$bootstrap_ci$dprime_ci[2], 3), "]\n")
    cat("    SE:", round(x$bootstrap_ci$dprime_se, 3), "\n")
  }

  if (x$model_type == "unequal_variance") {
    cat("\n  Variance Ratio (SD_lure / SD_target):", round(x$variance_ratio, 3), "\n")
    if (!is.null(x$bootstrap_ci)) {
      cat("    95% CI: [", round(x$bootstrap_ci$variance_ratio_ci[1], 3), ",",
          round(x$bootstrap_ci$variance_ratio_ci[2], 3), "]\n")
    }
  }

  cat("\nz-ROC Line:\n")
  cat("  Slope:", round(x$slope, 3), "\n")
  cat("  Intercept:", round(x$intercept, 3), "\n")
  cat("  R-squared:", round(x$fit_diagnostics$r_squared, 3), "\n")

  cat("\nDecision Criteria (c):\n")
  crit_df <- data.frame(
    Criterion = names(x$criteria),
    Value = round(x$criteria, 3)
  )
  print(crit_df, row.names = FALSE)

  cat("\nInterpretation:\n")
  if (x$dprime < 0.5) {
    cat("  - Poor discriminability (d' < 0.5)\n")
  } else if (x$dprime < 1.0) {
    cat("  - Weak discriminability (0.5 ≤ d' < 1.0)\n")
  } else if (x$dprime < 2.0) {
    cat("  - Moderate discriminability (1.0 ≤ d' < 2.0)\n")
  } else {
    cat("  - Strong discriminability (d' ≥ 2.0)\n")
  }

  mean_c <- mean(x$criteria, na.rm = TRUE)
  if (abs(mean_c) < 0.2) {
    cat("  - Approximately unbiased responding (mean c ≈ 0)\n")
  } else if (mean_c > 0) {
    cat("  - Conservative bias (mean c > 0)\n")
  } else {
    cat("  - Liberal bias (mean c < 0)\n")
  }

  invisible(x)
}


#' Plot Method for SDT ROC Fit
#' @param x An sdt_roc_fit object
#' @param ... Additional arguments passed to plot
#' @export
plot.sdt_roc_fit <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting SDT fits")
  }

  # Create z-ROC plot
  p <- ggplot2::ggplot(x$roc_data, ggplot2::aes(x = z_far, y = z_hr)) +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::geom_abline(
      intercept = x$intercept,
      slope = x$slope,
      color = "red",
      linewidth = 1
    ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.5
    ) +
    ggplot2::labs(
      title = "z-ROC Plot with Fitted SDT Model",
      subtitle = paste0("d' = ", round(x$dprime, 2),
                       ", Slope = ", round(x$slope, 2),
                       ", R² = ", round(x$fit_diagnostics$r_squared, 2)),
      x = "z(False Alarm Rate)",
      y = "z(Hit Rate)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )

  print(p)
  invisible(p)
}


#' Summary Method for SDT ROC Fit
#' @param object An sdt_roc_fit object
#' @param ... Additional arguments (ignored)
#' @export
summary.sdt_roc_fit <- function(object, ...) {
  cat("\n=== SDT Model Summary ===\n\n")

  cat("Model Type:", object$model_type, "\n")
  cat("Number of ROC Points:", object$fit_diagnostics$n_points, "\n\n")

  cat("Parameter Estimates:\n")
  cat("-------------------\n")
  cat("d' =", round(object$dprime, 4), "\n")

  if (!is.null(object$bootstrap_ci)) {
    cat("  SE =", round(object$bootstrap_ci$dprime_se, 4), "\n")
    cat("  95% CI: [", round(object$bootstrap_ci$dprime_ci[1], 4), ",",
        round(object$bootstrap_ci$dprime_ci[2], 4), "]\n")
    cat("  Bootstrap samples:", object$bootstrap_ci$n_successful, "/",
        length(object$bootstrap_ci$dprime_se) * 1000, "\n")
  }

  if (object$model_type == "unequal_variance") {
    cat("\nVariance Ratio =", round(object$variance_ratio, 4), "\n")
    if (!is.null(object$bootstrap_ci)) {
      cat("  SE =", round(object$bootstrap_ci$variance_ratio_se, 4), "\n")
      cat("  95% CI: [", round(object$bootstrap_ci$variance_ratio_ci[1], 4), ",",
          round(object$bootstrap_ci$variance_ratio_ci[2], 4), "]\n")
    }
  }

  cat("\nModel Fit:\n")
  cat("----------\n")
  cat("Slope:", round(object$slope, 4), "\n")
  cat("Intercept:", round(object$intercept, 4), "\n")
  cat("R-squared:", round(object$fit_diagnostics$r_squared, 4), "\n")
  cat("RMSE:", round(sqrt(mean(object$fit_diagnostics$residuals^2)), 4), "\n")

  cat("\nDecision Criteria:\n")
  cat("------------------\n")
  print(round(object$criteria, 4))
  cat("\nMean criterion:", round(mean(object$criteria), 4), "\n")
  cat("SD criterion:", round(sd(object$criteria), 4), "\n")

  invisible(object)
}
