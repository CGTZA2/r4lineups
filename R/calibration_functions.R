#' Compute Calibration Statistics for Eyewitness Identification
#'
#' Computes calibration analysis metrics following Juslin, Olsson, & Winman (1996).
#' Calibration analysis assesses the match between confidence and accuracy, providing
#' a more appropriate measure than simple confidence-accuracy correlation.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param confidence_bins Numeric vector of bin edges for grouping confidence
#'   (e.g., c(0, 60, 80, 100) creates bins 0-60, 60-80, 80-100).
#'   If NULL, uses individual confidence levels.
#' @param choosers_only Logical. If TRUE (default), only analyze suspect identifications.
#'   If FALSE, analyze all responses (including fillers and rejections).
#' @param lineup_size Integer. Number of people in lineup (default = 6).
#'   Used for estimating incorrect suspect IDs from filler choices in target-absent lineups.
#'
#' @return A list containing:
#'   \itemize{
#'     \item calibration_data: Dataframe with per-bin confidence, accuracy, and counts
#'     \item C: Calibration statistic (weighted mean squared difference between confidence and accuracy)
#'     \item OU: Over/underconfidence (mean confidence minus mean accuracy)
#'     \item NRI: Normalized Resolution Index (standardized within-person variance)
#'     \item overall_accuracy: Mean accuracy across all responses
#'     \item overall_confidence: Mean confidence across all responses
#'     \item n_total: Total number of responses analyzed
#'   }
#'
#' @details
#' Calibration analysis distinguishes between:
#' \itemize{
#'   \item \strong{Calibration (C)}: How well confidence matches accuracy (perfect = 0)
#'   \item \strong{Over/underconfidence (O/U)}: Overall bias in confidence judgments (0 = perfectly calibrated)
#'   \item \strong{Resolution (NRI)}: Ability to discriminate correct from incorrect responses
#' }
#'
#' The calibration statistic C measures the weighted mean squared deviation between
#' mean confidence and accuracy in each bin:
#' \deqn{C = \sum_{j=1}^{J} \frac{n_j}{N} (c_j - a_j)^2}
#'
#' The Normalized Resolution Index (NRI) captures how well confidence discriminates
#' correct from incorrect responses:
#' \deqn{NRI = \frac{\frac{1}{N} \sum_{j=1}^{J} n_j (a_j - \bar{a})^2}{\bar{a}(1-\bar{a})}}
#'
#' When \code{choosers_only = TRUE}, only suspect identifications are included (standard
#' for eyewitness calibration analysis). When \code{choosers_only = FALSE}, all responses
#' are included with fillers and rejections counted as incorrect.
#'
#' @references
#' Juslin, P., Olsson, N., & Winman, A. (1996). Calibration and diagnosticity
#' of confidence in eyewitness identification: Comments on what can be inferred
#' from the low confidence-accuracy correlation. \emph{Journal of Experimental
#' Psychology: Learning, Memory, and Cognition, 22}(5), 1304-1316.
#'
#' Brewer, N., & Wells, G. L. (2006). The confidence-accuracy relationship in
#' eyewitness identification: Effects of lineup instructions, foil similarity,
#' and target-absent base rates. \emph{Journal of Experimental Psychology:
#' Applied, 12}(1), 11-30.
#'
#' @export
#' @import tibble
make_calibration_data <- function(data, confidence_bins = NULL, choosers_only = TRUE,
                                   lineup_size = 6) {

  # Validate required columns
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter to choosers only if requested
  if (choosers_only) {
    data_filtered <- data[data$identification == "suspect", ]
    if (nrow(data_filtered) == 0) {
      stop("No suspect identifications found in data")
    }
  } else {
    data_filtered <- data
  }

  # Add correctness indicator
  data_filtered$correct <- ifelse(
    data_filtered$target_present == TRUE & data_filtered$identification == "suspect",
    1, 0
  )

  # Apply confidence binning if specified
  if (!is.null(confidence_bins)) {
    data_filtered$conf_bin <- cut(data_filtered$confidence,
                                   breaks = confidence_bins,
                                   include.lowest = TRUE,
                                   right = TRUE)
    conf_var <- "conf_bin"
    conf_levels <- levels(data_filtered$conf_bin)
  } else {
    conf_var <- "confidence"
    conf_levels <- sort(unique(data_filtered$confidence))
  }

  # Initialize results
  calibration_results <- tibble::tibble(
    bin = character(),
    n = numeric(),
    mean_confidence = numeric(),
    accuracy = numeric(),
    n_correct = numeric(),
    n_incorrect = numeric()
  )

  # Compute per-bin statistics
  for (level in conf_levels) {
    if (!is.null(confidence_bins)) {
      bin_data <- data_filtered[data_filtered$conf_bin == level, ]
      bin_label <- as.character(level)
    } else {
      bin_data <- data_filtered[data_filtered$confidence == level, ]
      bin_label <- as.character(level)
    }

    if (nrow(bin_data) == 0) next

    n_bin <- nrow(bin_data)
    mean_conf <- mean(bin_data$confidence, na.rm = TRUE)
    n_correct <- sum(bin_data$correct, na.rm = TRUE)
    n_incorrect <- n_bin - n_correct
    accuracy <- n_correct / n_bin

    calibration_results <- rbind(calibration_results, tibble::tibble(
      bin = bin_label,
      n = n_bin,
      mean_confidence = mean_conf,
      accuracy = accuracy,
      n_correct = n_correct,
      n_incorrect = n_incorrect
    ))
  }

  # Convert mean_confidence to proportion scale if needed (0-1)
  # Detect if confidence is on 0-100 scale
  if (max(data_filtered$confidence, na.rm = TRUE) > 1) {
    calibration_results$mean_confidence_prop <- calibration_results$mean_confidence / 100
    data_filtered$confidence_prop <- data_filtered$confidence / 100
    conf_scale <- 100
  } else {
    calibration_results$mean_confidence_prop <- calibration_results$mean_confidence
    data_filtered$confidence_prop <- data_filtered$confidence
    conf_scale <- 1
  }

  # Compute overall statistics
  N <- nrow(data_filtered)
  overall_accuracy <- mean(data_filtered$correct, na.rm = TRUE)
  overall_confidence <- mean(data_filtered$confidence_prop, na.rm = TRUE)

  # Calibration statistic (C) - weighted mean squared deviation
  C <- sum((calibration_results$n / N) *
           (calibration_results$mean_confidence_prop - calibration_results$accuracy)^2)

  # Over/underconfidence (O/U)
  OU <- overall_confidence - overall_accuracy

  # Normalized Resolution Index (NRI)
  if (overall_accuracy > 0 && overall_accuracy < 1) {
    variance_component <- sum((calibration_results$n / N) *
                              (calibration_results$accuracy - overall_accuracy)^2)
    NRI <- variance_component / (overall_accuracy * (1 - overall_accuracy))
  } else {
    NRI <- NA  # Cannot compute when accuracy is 0 or 1
  }

  list(
    calibration_data = calibration_results,
    C = C,
    OU = OU,
    NRI = NRI,
    overall_accuracy = overall_accuracy,
    overall_confidence = overall_confidence,
    n_total = N,
    choosers_only = choosers_only,
    confidence_scale = conf_scale
  )
}


#' Compute Calibration Statistics by Condition
#'
#' Computes calibration analysis separately for different experimental conditions.
#' Useful for examining how calibration varies across lineup instructions, foil
#' similarity, or other system/estimator variables.
#'
#' @param data A dataframe with columns: target_present, identification, confidence,
#'   plus one or more condition variables
#' @param condition_vars Character vector of column names defining conditions
#'   (e.g., c("instruction_type", "foil_similarity"))
#' @param confidence_bins Numeric vector of bin edges (optional)
#' @param choosers_only Logical. Whether to analyze only suspect IDs (default = TRUE)
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#'
#' @return A list containing:
#'   \itemize{
#'     \item by_condition: List of calibration results for each condition combination
#'     \item condition_summary: Dataframe summarizing C, O/U, NRI across conditions
#'   }
#'
#' @details
#' This function splits the data by the specified condition variables and computes
#' calibration statistics separately for each combination. Useful for examining
#' questions like "Does calibration improve with biased vs unbiased instructions?"
#' or "How does foil similarity affect over/underconfidence?"
#'
#' @references
#' Brewer, N., & Wells, G. L. (2006). The confidence-accuracy relationship in
#' eyewitness identification: Effects of lineup instructions, foil similarity,
#' and target-absent base rates. \emph{Journal of Experimental Psychology:
#' Applied, 12}(1), 11-30.
#'
#' @export
#' @import tibble
make_calibration_by_condition <- function(data, condition_vars,
                                          confidence_bins = NULL,
                                          choosers_only = TRUE,
                                          lineup_size = 6) {

  # Validate condition variables exist
  missing_vars <- setdiff(condition_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing condition variables: ", paste(missing_vars, collapse = ", "))
  }

  # Create condition combinations
  if (length(condition_vars) == 1) {
    data$condition_combo <- as.character(data[[condition_vars[1]]])
  } else {
    data$condition_combo <- apply(data[, condition_vars, drop = FALSE], 1,
                                   function(x) paste(x, collapse = "_"))
  }

  condition_levels <- unique(data$condition_combo)

  # Compute calibration for each condition
  results_by_condition <- list()
  summary_rows <- list()

  for (cond in condition_levels) {
    cond_data <- data[data$condition_combo == cond, ]

    if (nrow(cond_data) > 0) {
      cal_result <- make_calibration_data(
        cond_data,
        confidence_bins = confidence_bins,
        choosers_only = choosers_only,
        lineup_size = lineup_size
      )

      results_by_condition[[cond]] <- cal_result

      summary_rows[[cond]] <- tibble::tibble(
        condition = cond,
        n = cal_result$n_total,
        C = cal_result$C,
        OU = cal_result$OU,
        NRI = cal_result$NRI,
        overall_accuracy = cal_result$overall_accuracy,
        overall_confidence = cal_result$overall_confidence
      )
    }
  }

  # Combine summary
  condition_summary <- do.call(rbind, summary_rows)

  list(
    by_condition = results_by_condition,
    condition_summary = condition_summary,
    condition_vars = condition_vars
  )
}


#' Plot Calibration Curve
#'
#' Creates a calibration curve plot showing the relationship between confidence
#' and accuracy. A perfectly calibrated witness would fall on the diagonal line.
#'
#' @param cal_obj List output from make_calibration_data()
#' @param show_stats Logical. Whether to display calibration statistics on plot (default = TRUE)
#' @param show_n Logical. Whether to show sample sizes per bin (default = TRUE)
#' @param show_diagonal Logical. Whether to show perfect calibration line (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' The calibration curve plots mean confidence (x-axis) against accuracy (y-axis)
#' for each confidence bin. Points on the diagonal indicate perfect calibration.
#' Points above the diagonal indicate underconfidence, while points below indicate
#' overconfidence.
#'
#' @export
#' @import ggplot2
make_calibration_gg <- function(cal_obj, show_stats = TRUE, show_n = TRUE,
                                show_diagonal = TRUE) {

  cal_data <- cal_obj$calibration_data

  # Convert confidence to same scale as accuracy (0-1)
  if (cal_obj$confidence_scale == 100) {
    cal_data$mean_confidence_plot <- cal_data$mean_confidence / 100
    x_label <- "Mean Confidence (proportion)"
  } else {
    cal_data$mean_confidence_plot <- cal_data$mean_confidence
    x_label <- "Mean Confidence"
  }

  # Create base plot
  p <- ggplot(cal_data, aes(x = mean_confidence_plot, y = accuracy)) +
    geom_point(aes(size = n), color = "darkblue", alpha = 0.7) +
    geom_line(color = "darkblue", size = 0.8) +
    theme_bw(base_size = 14) +
    labs(
      x = x_label,
      y = "Proportion Correct",
      title = "Calibration Curve",
      subtitle = "Perfect calibration falls on diagonal line",
      size = "N"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    coord_fixed(ratio = 1)

  # Add perfect calibration diagonal
  if (show_diagonal) {
    p <- p + geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                         color = "gray50", size = 0.8)
  }

  # Add sample size labels
  if (show_n) {
    p <- p + geom_text(
      aes(label = paste0("n=", n)),
      nudge_y = -0.05,
      size = 3,
      color = "gray30"
    )
  }

  # Add statistics annotation
  if (show_stats) {
    stats_text <- sprintf(
      "C = %.3f\nO/U = %+.3f\nNRI = %.3f",
      cal_obj$C, cal_obj$OU, cal_obj$NRI
    )
    p <- p + annotate(
      "text",
      x = 0.2, y = 0.85,
      label = stats_text,
      hjust = 0,
      size = 4,
      color = "black"
    )
  }

  p
}


#' Plot Calibration Curves by Condition
#'
#' Creates a faceted or overlaid plot comparing calibration across conditions.
#'
#' @param cal_by_cond_obj List output from make_calibration_by_condition()
#' @param facet Logical. If TRUE (default), creates faceted plot. If FALSE, overlays curves.
#' @param show_stats Logical. Whether to display statistics (default = TRUE when faceted)
#'
#' @return A ggplot2 object
#'
#' @export
#' @import ggplot2
make_calibration_by_condition_gg <- function(cal_by_cond_obj, facet = TRUE,
                                             show_stats = TRUE) {

  # Combine calibration data from all conditions
  all_cal_data <- list()
  for (cond_name in names(cal_by_cond_obj$by_condition)) {
    cond_result <- cal_by_cond_obj$by_condition[[cond_name]]
    cond_data <- cond_result$calibration_data
    cond_data$condition <- cond_name

    # Normalize confidence scale
    if (cond_result$confidence_scale == 100) {
      cond_data$mean_confidence_plot <- cond_data$mean_confidence / 100
    } else {
      cond_data$mean_confidence_plot <- cond_data$mean_confidence
    }

    all_cal_data[[cond_name]] <- cond_data
  }

  combined_data <- do.call(rbind, all_cal_data)

  # Base plot
  p <- ggplot(combined_data, aes(x = mean_confidence_plot, y = accuracy,
                                  color = condition, group = condition)) +
    geom_point(aes(size = n), alpha = 0.7) +
    geom_line(size = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "gray50", size = 0.6) +
    theme_bw(base_size = 14) +
    labs(
      x = "Mean Confidence (proportion)",
      y = "Proportion Correct",
      title = "Calibration Curves by Condition",
      subtitle = "Perfect calibration falls on diagonal line",
      color = "Condition",
      size = "N"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

  # Facet or overlay
  if (facet) {
    p <- p + facet_wrap(~ condition, ncol = 2) +
      coord_fixed(ratio = 1)

    # Add stats if requested
    if (show_stats) {
      stats_df <- cal_by_cond_obj$condition_summary
      stats_df$stats_label <- sprintf(
        "C=%.3f\nO/U=%+.3f\nNRI=%.3f",
        stats_df$C, stats_df$OU, stats_df$NRI
      )
      p <- p + geom_text(
        data = stats_df,
        aes(x = 0.2, y = 0.85, label = stats_label),
        hjust = 0,
        size = 3,
        color = "black",
        inherit.aes = FALSE
      )
    }
  } else {
    p <- p + coord_fixed(ratio = 1)
  }

  p
}


#' Compute and Plot Calibration for Lineup Identification
#'
#' Main function to compute and plot calibration analysis for eyewitness lineup data.
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param confidence_bins Numeric vector of bin edges (optional)
#' @param choosers_only Logical. Whether to analyze only suspect IDs (default = TRUE)
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @param ... Additional arguments passed to make_calibration_gg()
#'
#' @return A list containing calibration data, statistics, and plot
#'
#' @examples
#' \dontrun{
#' # Example with binned confidence
#' cal_result <- make_calibration(lineup_data,
#'                                confidence_bins = c(0, 60, 80, 100))
#' print(cal_result)
#' }
#'
#' @export
make_calibration <- function(data, confidence_bins = NULL, choosers_only = TRUE,
                            lineup_size = 6, show_plot = TRUE, ...) {

  # Compute calibration data
  cal_obj <- make_calibration_data(
    data,
    confidence_bins = confidence_bins,
    choosers_only = choosers_only,
    lineup_size = lineup_size
  )

  # Create plot
  if (show_plot) {
    cal_plot <- make_calibration_gg(cal_obj, ...)
  } else {
    cal_plot <- NULL
  }

  result <- list(
    plot = cal_plot,
    calibration_data = cal_obj$calibration_data,
    C = cal_obj$C,
    OU = cal_obj$OU,
    NRI = cal_obj$NRI,
    overall_accuracy = cal_obj$overall_accuracy,
    overall_confidence = cal_obj$overall_confidence,
    n_total = cal_obj$n_total,
    choosers_only = cal_obj$choosers_only
  )

  class(result) <- c("lineup_calibration", "list")
  result
}


#' Print Method for lineup_calibration Objects
#' @param x A lineup_calibration object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_calibration <- function(x, ...) {
  cat("\n=== Lineup Calibration Analysis ===\n\n")
  cat("Analysis type:", ifelse(x$choosers_only, "Choosers only (suspect IDs)\n", "All responses\n"))
  cat("Total N:", x$n_total, "\n\n")

  cat("Calibration Statistics:\n")
  cat("  C (Calibration):        ", sprintf("%.4f", x$C), "\n")
  cat("  O/U (Over/Under):       ", sprintf("%+.4f", x$OU), "\n")
  cat("  NRI (Resolution):       ", sprintf("%.4f", x$NRI), "\n\n")

  cat("Overall Performance:\n")
  cat("  Mean Accuracy:     ", sprintf("%.3f", x$overall_accuracy), "\n")
  cat("  Mean Confidence:   ", sprintf("%.3f", x$overall_confidence), "\n\n")

  cat("Calibration Data by Bin:\n")
  print(x$calibration_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot available in $plot\n")
  }

  invisible(x)
}
