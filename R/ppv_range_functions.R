#' Compute Innocent Suspect ID Rate with Nominal Size Correction
#'
#' Estimates innocent-suspect identification rate using nominal lineup size,
#' assuming all lineup members are equally plausible (perfectly fair lineup).
#'
#' @param error_rate Numeric. Overall mistaken identification rate from
#'   culprit-absent lineups.
#' @param lineup_size Integer. Nominal number of people in the lineup.
#'
#' @return Numeric. Estimated innocent-suspect identification rate.
#'
#' @details
#' This represents the "best case" estimate assuming perfect lineup fairness.
#' The formula is: innocent_id_rate = error_rate / lineup_size
#'
#' @references
#' Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
#' eyewitness error rates in fair and biased lineups.
#' \emph{Law and Human Behavior}.
#'
#' @export
innocent_id_rate_nominal <- function(error_rate, lineup_size) {
  if (lineup_size <= 0) {
    stop("lineup_size must be positive")
  }
  error_rate / lineup_size
}


#' Compute Innocent Suspect ID Rate with Effective Size Correction
#'
#' Estimates innocent-suspect identification rate using effective lineup size,
#' accounting for implausible fillers and lineup bias.
#'
#' @param error_rate Numeric. Overall mistaken identification rate from
#'   culprit-absent lineups.
#' @param effective_size Numeric. Effective size computed from the distribution
#'   of mistaken IDs (typically via \code{\link{esize_T}}).
#'
#' @return Numeric. Estimated innocent-suspect identification rate.
#'
#' @details
#' This represents a more realistic estimate when lineup fairness is uncertain.
#' Uses the distribution of mistaken IDs to estimate the number of plausible
#' lineup members. The formula is: innocent_id_rate = error_rate / effective_size
#'
#' When effective size < nominal size, this indicates lineup bias and increases
#' the estimated innocent-suspect risk.
#'
#' @references
#' Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
#' eyewitness error rates in fair and biased lineups.
#' \emph{Law and Human Behavior}.
#'
#' Tredoux, C. G. (1998). Statistical inference on measures of lineup fairness.
#' \emph{Law and Human Behavior, 22}(2), 217-237.
#'
#' @export
innocent_id_rate_effective <- function(error_rate, effective_size) {
  if (effective_size <= 0) {
    stop("effective_size must be positive")
  }
  error_rate / effective_size
}


#' Compute Innocent Suspect ID Rate with No Correction
#'
#' Estimates innocent-suspect identification rate assuming all mistaken IDs
#' are innocent-suspect IDs (no correction for lineup size).
#'
#' @param error_rate Numeric. Overall mistaken identification rate from
#'   culprit-absent lineups.
#'
#' @return Numeric. Estimated innocent-suspect identification rate
#'   (equal to error_rate).
#'
#' @details
#' This represents the "worst case" estimate, treating every mistaken ID as
#' an innocent-suspect identification. Provides an upper bound on innocent-suspect
#' risk. The formula is simply: innocent_id_rate = error_rate
#'
#' @references
#' Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
#' eyewitness error rates in fair and biased lineups.
#' \emph{Law and Human Behavior}.
#'
#' @export
innocent_id_rate_uncorrected <- function(error_rate) {
  error_rate
}


#' Compute PPV by Confidence with Lineup Size Correction
#'
#' Calculates Positive Predictive Value (PPV) for each confidence level,
#' using a specified method to correct for lineup size when estimating
#' innocent-suspect identification rates.
#'
#' @param data A dataframe with columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param lineup_size Integer. Nominal number of people in lineup (default = 6)
#' @param confidence_bins Numeric vector of bin edges for grouping confidence
#'   (e.g., c(0, 60, 80, 100)). If NULL, uses individual confidence levels.
#' @param correction Character. Method for estimating innocent-suspect IDs:
#'   "nominal" (best-case, assumes fair lineup), "effective" (accounts for bias),
#'   "none" (worst-case, no correction). Default = "nominal".
#' @param effective_size_data Optional dataframe with pre-computed effective sizes
#'   per confidence bin (must have columns: conf_level, effective_size).
#'   If NULL and correction="effective", computes from data.
#'
#' @return A list containing:
#'   \itemize{
#'     \item ppv_data: Dataframe with PPV by confidence level
#'     \item overall_ppv: Overall PPV across all confidence levels
#'     \item correction_method: The correction method used
#'     \item lineup_size: Nominal lineup size
#'   }
#'
#' @details
#' PPV (Positive Predictive Value) is the probability that a suspect is guilty
#' given an identification. The formula is:
#'
#' PPV = guilty_suspect_IDs / (guilty_suspect_IDs + innocent_suspect_IDs)
#'
#' Different correction methods estimate innocent_suspect_IDs differently:
#' \itemize{
#'   \item "nominal": assumes perfect lineup fairness (error_rate / lineup_size)
#'   \item "effective": uses effective size to account for bias (error_rate / effective_size)
#'   \item "none": assumes all errors are innocent-suspect IDs (error_rate)
#' }
#'
#' @references
#' Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
#' eyewitness error rates in fair and biased lineups.
#' \emph{Law and Human Behavior}.
#'
#' @examples
#' \dontrun{
#' # Nominal correction (assumes fair lineup)
#' ppv_result <- ppv_by_confidence(lineup_data,
#'                                  correction = "nominal",
#'                                  confidence_bins = c(0, 60, 80, 100))
#'
#' # Effective size correction (accounts for bias)
#' ppv_result <- ppv_by_confidence(lineup_data,
#'                                  correction = "effective",
#'                                  confidence_bins = c(0, 60, 80, 100))
#' }
#'
#' @export
#' @import tibble
ppv_by_confidence <- function(data,
                                lineup_size = 6,
                                confidence_bins = NULL,
                                correction = c("nominal", "effective", "none"),
                                effective_size_data = NULL) {

  # Validate inputs
  correction <- match.arg(correction)

  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Apply confidence binning if specified
  if (!is.null(confidence_bins)) {
    data$conf_level <- cut(data$confidence,
                           breaks = confidence_bins,
                           include.lowest = TRUE,
                           right = TRUE)
  } else {
    data$conf_level <- as.factor(data$confidence)
  }

  # Separate target-present and target-absent
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  # Get unique confidence levels
  conf_levels <- levels(data$conf_level)

  # Initialize results
  ppv_results <- tibble::tibble(
    confidence = character(),
    n_guilty_ids = numeric(),
    n_innocent_ids_est = numeric(),
    n_total_ids = numeric(),
    ppv = numeric(),
    error_rate = numeric(),
    effective_size = numeric()
  )

  for (conf in conf_levels) {
    # Target-present: guilty suspect IDs
    n_guilty <- sum(tp_data$identification == "suspect" &
                   tp_data$conf_level == conf, na.rm = TRUE)

    # Target-absent: count all mistaken IDs
    ta_conf <- ta_data[ta_data$conf_level == conf, ]
    n_ta_total <- nrow(ta_conf)

    if (n_ta_total == 0) {
      # Skip empty bins
      next
    }

    n_ta_suspect <- sum(ta_conf$identification == "suspect", na.rm = TRUE)
    n_ta_filler <- sum(ta_conf$identification == "filler", na.rm = TRUE)

    # Overall error rate in this confidence bin
    error_rate <- (n_ta_suspect + n_ta_filler) / n_ta_total

    # Estimate innocent-suspect IDs based on correction method
    if (correction == "nominal") {
      innocent_rate <- innocent_id_rate_nominal(error_rate, lineup_size)
      eff_size <- lineup_size

    } else if (correction == "effective") {
      # Compute effective size from TA distribution
      if (!is.null(effective_size_data)) {
        # Use pre-computed effective size
        eff_size_row <- effective_size_data[effective_size_data$conf_level == conf, ]
        if (nrow(eff_size_row) == 0) {
          eff_size <- lineup_size  # fallback
        } else {
          eff_size <- eff_size_row$effective_size[1]
        }
      } else {
        # Compute from TA member choices
        # Need to get individual lineup member choices
        # For now, use a simple approximation based on suspect vs filler ratio
        # This is a simplified version; proper implementation would need member-level data

        # If we have member-level data, compute properly
        if ("lineup_member" %in% names(ta_conf)) {
          ta_choices <- table(ta_conf$lineup_member)
          eff_size <- esize_T(ta_choices)
        } else {
          # Fallback: estimate from suspect/filler distribution
          # Create pseudo-distribution
          choices <- c(rep("suspect", n_ta_suspect),
                      rep("filler", n_ta_filler))
          if (length(choices) > 0) {
            choice_table <- table(choices)
            # Distribute fillers evenly across remaining positions
            if (n_ta_filler > 0) {
              filler_per_pos <- n_ta_filler / (lineup_size - 1)
              pseudo_table <- c(n_ta_suspect, rep(filler_per_pos, lineup_size - 1))
              names(pseudo_table) <- 1:lineup_size
              eff_size <- esize_T(table(pseudo_table))
            } else {
              eff_size <- 1  # All chose suspect
            }
          } else {
            eff_size <- lineup_size
          }
        }
      }

      innocent_rate <- innocent_id_rate_effective(error_rate, eff_size)

    } else {  # correction == "none"
      innocent_rate <- innocent_id_rate_uncorrected(error_rate)
      eff_size <- 1
    }

    # Estimated number of innocent-suspect IDs
    n_innocent_est <- innocent_rate * n_ta_total

    # PPV calculation
    n_total_ids <- n_guilty + n_innocent_est
    if (n_total_ids > 0) {
      ppv <- n_guilty / n_total_ids
    } else {
      ppv <- NA
    }

    ppv_results <- rbind(ppv_results, tibble::tibble(
      confidence = as.character(conf),
      n_guilty_ids = n_guilty,
      n_innocent_ids_est = n_innocent_est,
      n_total_ids = n_total_ids,
      ppv = ppv,
      error_rate = error_rate,
      effective_size = eff_size
    ))
  }

  # Calculate overall PPV
  total_guilty <- sum(ppv_results$n_guilty_ids)
  total_innocent_est <- sum(ppv_results$n_innocent_ids_est)
  overall_ppv <- total_guilty / (total_guilty + total_innocent_est)

  result <- list(
    ppv_data = ppv_results,
    overall_ppv = overall_ppv,
    correction_method = correction,
    lineup_size = lineup_size,
    n_target_present = nrow(tp_data),
    n_target_absent = nrow(ta_data)
  )

  class(result) <- c("lineup_ppv", "list")
  result
}


#' Compute PPV Range by Confidence (All Three Corrections)
#'
#' Calculates PPV for each confidence level using all three correction methods
#' (nominal, effective, none) to show the range of plausible PPV estimates.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param lineup_size Integer. Nominal number of people in lineup (default = 6)
#' @param confidence_bins Numeric vector of bin edges (optional)
#' @param effective_size_data Optional pre-computed effective sizes
#'
#' @return A list of class "lineup_ppv_range" containing:
#'   \itemize{
#'     \item ppv_range_data: Dataframe with all three PPV estimates per confidence level
#'     \item ppv_nominal: Nominal correction results
#'     \item ppv_effective: Effective size correction results
#'     \item ppv_none: No correction results
#'     \item lineup_size: Nominal lineup size
#'   }
#'
#' @details
#' This function provides the full PPV range recommended by Fitzgerald et al. (2023):
#' \itemize{
#'   \item \strong{Nominal} (upper bound): Assumes perfectly fair lineup
#'   \item \strong{Effective}: Accounts for lineup bias via effective size
#'   \item \strong{None} (lower bound): Assumes worst-case (all errors are innocent-suspect)
#' }
#'
#' The range between nominal and none represents the uncertainty in PPV due to
#' unknown lineup fairness conditions.
#'
#' @references
#' Fitzgerald, R. J., Tredoux, C. G., & Juncu, S. (2023). Estimation of
#' eyewitness error rates in fair and biased lineups.
#' \emph{Law and Human Behavior}.
#'
#' @examples
#' \dontrun{
#' # Compute full PPV range
#' ppv_range <- ppv_range_by_confidence(lineup_data,
#'                                       confidence_bins = c(0, 60, 80, 100))
#' print(ppv_range)
#'
#' # Access individual corrections
#' ppv_range$ppv_nominal$overall_ppv
#' ppv_range$ppv_effective$overall_ppv
#' ppv_range$ppv_none$overall_ppv
#' }
#'
#' @export
ppv_range_by_confidence <- function(data,
                                     lineup_size = 6,
                                     confidence_bins = NULL,
                                     effective_size_data = NULL) {

  # Compute PPV with each correction method
  ppv_nominal <- ppv_by_confidence(data, lineup_size, confidence_bins,
                                    "nominal", effective_size_data)
  ppv_effective <- ppv_by_confidence(data, lineup_size, confidence_bins,
                                      "effective", effective_size_data)
  ppv_none <- ppv_by_confidence(data, lineup_size, confidence_bins,
                                 "none", effective_size_data)

  # Combine into a single dataframe for easy plotting
  ppv_range_data <- data.frame(
    confidence = ppv_nominal$ppv_data$confidence,
    ppv_nominal = ppv_nominal$ppv_data$ppv,
    ppv_effective = ppv_effective$ppv_data$ppv,
    ppv_none = ppv_none$ppv_data$ppv,
    effective_size = ppv_effective$ppv_data$effective_size,
    error_rate = ppv_effective$ppv_data$error_rate,
    stringsAsFactors = FALSE
  )

  result <- list(
    ppv_range_data = ppv_range_data,
    ppv_nominal = ppv_nominal,
    ppv_effective = ppv_effective,
    ppv_none = ppv_none,
    lineup_size = lineup_size,
    confidence_bins = confidence_bins
  )

  class(result) <- c("lineup_ppv_range", "list")
  result
}


#' Print Method for lineup_ppv Objects
#'
#' @param x A lineup_ppv object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_ppv <- function(x, ...) {
  cat("\n=== Lineup PPV Analysis ===\n\n")
  cat("Correction method:", x$correction_method, "\n")
  cat("Overall PPV:", round(x$overall_ppv, 3), "\n")
  cat("Lineup size:", x$lineup_size, "\n")
  cat("Sample sizes:\n")
  cat("  Target-present:", x$n_target_present, "\n")
  cat("  Target-absent:", x$n_target_absent, "\n\n")

  cat("PPV by confidence level:\n")
  print(x$ppv_data, n = Inf)

  invisible(x)
}


#' Print Method for lineup_ppv_range Objects
#'
#' @param x A lineup_ppv_range object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_ppv_range <- function(x, ...) {
  cat("\n=== Lineup PPV Range Analysis ===\n\n")
  cat("Lineup size:", x$lineup_size, "\n\n")

  cat("Overall PPV by correction method:\n")
  cat("  Nominal (best-case):", round(x$ppv_nominal$overall_ppv, 3), "\n")
  cat("  Effective (realistic):", round(x$ppv_effective$overall_ppv, 3), "\n")
  cat("  None (worst-case):", round(x$ppv_none$overall_ppv, 3), "\n\n")

  cat("PPV range by confidence level:\n")
  # Format for display
  display_data <- x$ppv_range_data
  display_data$ppv_nominal <- sprintf("%.3f", display_data$ppv_nominal)
  display_data$ppv_effective <- sprintf("%.3f", display_data$ppv_effective)
  display_data$ppv_none <- sprintf("%.3f", display_data$ppv_none)
  display_data$effective_size <- sprintf("%.2f", display_data$effective_size)
  display_data$error_rate <- sprintf("%.3f", display_data$error_rate)

  print(display_data, row.names = FALSE)

  invisible(x)
}
