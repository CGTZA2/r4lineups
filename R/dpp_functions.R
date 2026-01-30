#' Compute Deviation from Perfect Performance (DPP)
#'
#' Computes DPP metric following Smith et al. (2018). DPP measures how much
#' an observed ROC curve deviates from perfect performance, providing a
#' single-number summary that is less affected by ROC truncation than pAUC.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating
#'   }
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param use_roc_obj Logical. If FALSE (default), computes ROC from data.
#'   If TRUE, data should be the output from make_rocdata().
#'
#' @return A list containing:
#'   \itemize{
#'     \item dpp: Deviation from Perfect Performance (0 = perfect, 1 = worst)
#'     \item auc_observed: Area under observed ROC curve
#'     \item auc_perfect: Area under perfect ROC curve (for same FA range)
#'     \item roc_data: ROC curve data
#'     \item max_fa: Maximum false alarm rate in observed data
#'   }
#'
#' @details
#' DPP (Deviation from Perfect Performance) compares the observed ROC curve
#' to a perfect ROC curve within the same false alarm rate range:
#'
#' \deqn{DPP = 1 - \frac{AUC_{observed}}{AUC_{perfect}}}
#'
#' Where:
#' \itemize{
#'   \item \strong{Perfect ROC}: Goes from (0,0) → (0,1) → (max_FA,1)
#'     (immediate jump to 100% hit rate, then horizontal)
#'   \item \strong{Observed ROC}: Actual performance from data
#'   \item \strong{AUC}: Area under curve computed via trapezoidal rule
#' }
#'
#' **Interpretation**:
#' \itemize{
#'   \item DPP = 0: Perfect performance (all correct IDs at 0% false alarms)
#'   \item DPP → 1: Very poor performance (near chance)
#'   \item Lower DPP = better performance
#' }
#'
#' **Advantages over pAUC** (Smith et al. 2018):
#' \itemize{
#'   \item Less affected by ROC truncation (different confidence distributions)
#'   \item Normalized relative to best achievable performance
#'   \item More consistent rankings across datasets
#'   \item Accounts for achievable performance given observed FA range
#' }
#'
#' The perfect ROC is constrained to the same FA range as the observed data,
#' making DPP a fair measure even when procedures produce different confidence
#' distributions (and thus different ROC truncation points).
#'
#' @references
#' Smith, A. M., Wilford, M. M., Quigley-McBride, A., & Wells, G. L. (2019).
#' Mistaken eyewitness identification rates increase when either witnessing or
#' testing conditions get worse. \emph{Law and Human Behavior, 43}(4), 358-368.
#'
#' Smith, A. M., et al. (2018). Deviation from perfect performance measures
#' the diagnostic utility of eyewitness lineups but partial area under the
#' ROC does not. \emph{Journal of Applied Research in Memory and Cognition}.
#'
#' @export
#' @import tibble
make_dpp <- function(data, lineup_size = 6, use_roc_obj = FALSE) {

  # If data is already ROC object, use it; otherwise compute ROC
  if (use_roc_obj) {
    roc_obj <- data
    roc_data <- roc_obj$roc_data
  } else {
    # Validate required columns
    required_cols <- c("target_present", "identification", "confidence")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Compute ROC data
    roc_obj <- make_rocdata(data, lineup_size = lineup_size)
    roc_data <- roc_obj$roc_data
  }

  # Sort ROC points by false alarm rate
  roc_sorted <- roc_data[order(roc_data$false_id_rate), ]

  # Get maximum false alarm rate (defines truncation point)
  max_fa <- max(roc_sorted$false_id_rate)

  # Compute area under observed ROC curve using trapezoidal rule
  auc_observed <- 0
  for (i in 2:nrow(roc_sorted)) {
    width <- roc_sorted$false_id_rate[i] - roc_sorted$false_id_rate[i-1]
    height <- (roc_sorted$correct_id_rate[i] + roc_sorted$correct_id_rate[i-1]) / 2
    auc_observed <- auc_observed + (width * height)
  }

  # Compute area under perfect ROC curve
  # Perfect curve: (0,0) → (0,1) → (max_fa, 1)
  # This is a rectangle with width = max_fa and height = 1
  auc_perfect <- max_fa * 1.0

  # Compute DPP
  if (auc_perfect == 0) {
    # Special case: If max_fa = 0, check if we have perfect performance
    # If hit rate is 1.0 at FA=0, then DPP = 0 (perfect)
    # Otherwise, can't compute DPP meaningfully
    max_hit <- max(roc_sorted$correct_id_rate)
    if (max_hit >= 0.999) {  # Allow small numerical error
      dpp <- 0  # Perfect performance
    } else {
      dpp <- NA  # Cannot compute DPP without any false alarms
    }
  } else {
    dpp <- 1 - (auc_observed / auc_perfect)
  }

  # Create perfect ROC data for plotting
  perfect_roc <- tibble::tibble(
    false_id_rate = c(0, 0, max_fa),
    correct_id_rate = c(0, 1, 1)
  )

  list(
    dpp = dpp,
    auc_observed = auc_observed,
    auc_perfect = auc_perfect,
    roc_data = roc_sorted,
    perfect_roc = perfect_roc,
    max_fa = max_fa,
    n_target_present = if (use_roc_obj) roc_obj$n_target_present else nrow(data[data$target_present == TRUE, ]),
    n_target_absent = if (use_roc_obj) roc_obj$n_target_absent else nrow(data[data$target_present == FALSE, ])
  )
}


#' Compare DPP Between Two Procedures
#'
#' Computes and compares DPP for two lineup procedures.
#'
#' @param data_proc_a Dataframe for procedure A (standard lineup format)
#' @param data_proc_b Dataframe for procedure B (standard lineup format)
#' @param lineup_size Integer. Lineup size (default = 6)
#'
#' @return A list containing:
#'   \itemize{
#'     \item dpp_a: DPP for procedure A
#'     \item dpp_b: DPP for procedure B
#'     \item dpp_difference: DPP_A - DPP_B (negative = A is better)
#'     \item dpp_obj_a: Full DPP object for procedure A
#'     \item dpp_obj_b: Full DPP object for procedure B
#'   }
#'
#' @details
#' Compares two procedures using DPP. Since lower DPP indicates better
#' performance, a negative difference (DPP_A - DPP_B < 0) means procedure A
#' is better.
#'
#' DPP comparison is advantageous when:
#' \itemize{
#'   \item ROC curves have different truncation points
#'   \item Confidence distributions differ between procedures
#'   \item pAUC comparisons might be misleading
#' }
#'
#' @export
compare_dpp <- function(data_proc_a, data_proc_b, lineup_size = 6) {

  dpp_a <- make_dpp(data_proc_a, lineup_size = lineup_size)
  dpp_b <- make_dpp(data_proc_b, lineup_size = lineup_size)

  list(
    dpp_a = dpp_a$dpp,
    dpp_b = dpp_b$dpp,
    dpp_difference = dpp_a$dpp - dpp_b$dpp,
    dpp_obj_a = dpp_a,
    dpp_obj_b = dpp_b
  )
}


#' Plot DPP with Observed and Perfect ROC Curves
#'
#' Creates a plot showing the observed ROC curve and perfect ROC curve,
#' with shaded area representing deviation from perfect performance.
#'
#' @param dpp_obj List output from make_dpp()
#' @param show_perfect Logical. Whether to show perfect ROC curve (default = TRUE)
#' @param show_dpp Logical. Whether to display DPP value on plot (default = TRUE)
#' @param show_shading Logical. Whether to shade area between curves (default = TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' The plot shows:
#' \itemize{
#'   \item \strong{Observed ROC} (blue): Actual performance
#'   \item \strong{Perfect ROC} (red dashed): Best achievable performance
#'   \item \strong{Shaded area} (red): Deviation from perfect (area = DPP × AUC_perfect)
#'   \item \strong{DPP value}: Proportion of perfect area that is lost
#' }
#'
#' A smaller shaded area (lower DPP) indicates better performance.
#'
#' @export
#' @import ggplot2
plot_dpp <- function(dpp_obj, show_perfect = TRUE, show_dpp = TRUE,
                    show_shading = TRUE) {

  roc_data <- dpp_obj$roc_data
  perfect_data <- dpp_obj$perfect_roc

  # Create base plot
  p <- ggplot(roc_data, aes(x = false_id_rate, y = correct_id_rate)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(size = 2, color = "steelblue") +
    theme_bw(base_size = 14) +
    labs(
      x = "False ID Rate (Innocent Suspect)",
      y = "Correct ID Rate (Guilty Suspect)",
      title = "ROC Curve with Deviation from Perfect Performance (DPP)",
      caption = "Lower DPP = better performance"
    ) +
    coord_cartesian(xlim = c(0, max(roc_data$false_id_rate) * 1.1),
                    ylim = c(0, 1))

  # Add perfect ROC curve
  if (show_perfect) {
    p <- p + geom_line(data = perfect_data,
                      aes(x = false_id_rate, y = correct_id_rate),
                      linewidth = 1, color = "red", linetype = "dashed")
  }

  # Shade area between observed and perfect
  if (show_shading) {
    # Create polygon for shaded area
    # Perfect curve: (0,0) → (0,1) → (max_fa,1)
    # Observed curve: follows roc_data
    # Shade the area between them

    # Start with perfect curve points (going right)
    shade_data <- rbind(
      data.frame(false_id_rate = 0, correct_id_rate = 0),
      data.frame(false_id_rate = 0, correct_id_rate = 1),
      data.frame(false_id_rate = dpp_obj$max_fa, correct_id_rate = 1)
    )

    # Add observed curve points (going left, in reverse)
    obs_reversed <- roc_data[nrow(roc_data):1, ]
    shade_data <- rbind(shade_data, obs_reversed[, c("false_id_rate", "correct_id_rate")])

    p <- p + geom_polygon(data = shade_data,
                         aes(x = false_id_rate, y = correct_id_rate),
                         fill = "red", alpha = 0.2)
  }

  # Add DPP annotation
  if (show_dpp) {
    p <- p + annotate(
      "text",
      x = dpp_obj$max_fa * 0.6,
      y = 0.2,
      label = sprintf("DPP = %.3f\nAUC_obs = %.3f\nAUC_perf = %.3f",
                     dpp_obj$dpp, dpp_obj$auc_observed, dpp_obj$auc_perfect),
      size = 4.5,
      hjust = 0
    )
  }

  p
}


#' Plot DPP Comparison Between Two Procedures
#'
#' Creates side-by-side or overlaid plots comparing DPP for two procedures.
#'
#' @param compare_obj List output from compare_dpp()
#' @param layout Character. "side-by-side" or "overlay" (default = "side-by-side")
#' @param proc_a_label Character. Label for procedure A (default = "Procedure A")
#' @param proc_b_label Character. Label for procedure B (default = "Procedure B")
#'
#' @return A ggplot2 object
#'
#' @export
#' @import ggplot2
plot_dpp_comparison <- function(compare_obj,
                                layout = c("side-by-side", "overlay"),
                                proc_a_label = "Procedure A",
                                proc_b_label = "Procedure B") {

  layout <- match.arg(layout)

  dpp_a <- compare_obj$dpp_obj_a
  dpp_b <- compare_obj$dpp_obj_b

  if (layout == "side-by-side") {
    # Create two separate plots
    p1 <- plot_dpp(dpp_a, show_dpp = TRUE) +
      labs(title = proc_a_label,
           subtitle = sprintf("DPP = %.3f", dpp_a$dpp))

    p2 <- plot_dpp(dpp_b, show_dpp = TRUE) +
      labs(title = proc_b_label,
           subtitle = sprintf("DPP = %.3f", dpp_b$dpp))

    # Combine using patchwork if available, otherwise just return first plot
    if (requireNamespace("patchwork", quietly = TRUE)) {
      p <- p1 + p2 + patchwork::plot_annotation(
        title = "DPP Comparison",
        subtitle = sprintf("Difference (A - B) = %.3f  (negative = A better)",
                          compare_obj$dpp_difference)
      )
    } else {
      warning("Install 'patchwork' package for side-by-side plots. Returning first plot only.")
      p <- p1
    }

  } else {  # overlay
    # Combine ROC data
    roc_a <- dpp_a$roc_data
    roc_b <- dpp_b$roc_data
    roc_a$procedure <- proc_a_label
    roc_b$procedure <- proc_b_label
    roc_combined <- rbind(roc_a, roc_b)

    # Combine perfect curves
    perfect_a <- dpp_a$perfect_roc
    perfect_b <- dpp_b$perfect_roc
    perfect_a$procedure <- proc_a_label
    perfect_b$procedure <- proc_b_label
    perfect_combined <- rbind(perfect_a, perfect_b)

    # Create overlay plot
    p <- ggplot(roc_combined, aes(x = false_id_rate, y = correct_id_rate,
                                   color = procedure, group = procedure)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_line(data = perfect_combined,
               aes(x = false_id_rate, y = correct_id_rate, color = procedure),
               linetype = "dashed", linewidth = 0.8) +
      theme_bw(base_size = 14) +
      labs(
        x = "False ID Rate",
        y = "Correct ID Rate",
        title = "DPP Comparison",
        subtitle = sprintf("%s: DPP=%.3f  |  %s: DPP=%.3f  |  Diff=%.3f",
                          proc_a_label, dpp_a$dpp,
                          proc_b_label, dpp_b$dpp,
                          compare_obj$dpp_difference),
        color = "Procedure",
        caption = "Solid = observed ROC; Dashed = perfect ROC; Lower DPP = better"
      ) +
      coord_cartesian(xlim = c(0, max(roc_combined$false_id_rate) * 1.1),
                     ylim = c(0, 1))
  }

  p
}


#' Compute and Plot DPP
#'
#' Main wrapper function to compute and plot DPP analysis.
#'
#' @param data Dataframe with standard lineup format
#' @param lineup_size Integer. Lineup size (default = 6)
#' @param show_plot Logical. Whether to display plot (default = TRUE)
#' @param ... Additional arguments passed to plot_dpp()
#'
#' @return A list containing DPP results and plot
#'
#' @examples
#' \dontrun{
#' dpp_result <- compute_dpp(lineup_data)
#' print(dpp_result)
#' }
#'
#' @export
compute_dpp <- function(data, lineup_size = 6, show_plot = TRUE, ...) {

  # Compute DPP
  dpp_obj <- make_dpp(data, lineup_size = lineup_size)

  # Create plot
  if (show_plot) {
    dpp_plot <- plot_dpp(dpp_obj, ...)
  } else {
    dpp_plot <- NULL
  }

  result <- list(
    plot = dpp_plot,
    dpp = dpp_obj$dpp,
    auc_observed = dpp_obj$auc_observed,
    auc_perfect = dpp_obj$auc_perfect,
    roc_data = dpp_obj$roc_data,
    perfect_roc = dpp_obj$perfect_roc,
    max_fa = dpp_obj$max_fa
  )

  class(result) <- c("lineup_dpp", "list")
  result
}


#' Print Method for lineup_dpp Objects
#' @param x A lineup_dpp object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_dpp <- function(x, ...) {
  cat("\n=== Deviation from Perfect Performance (DPP) ===\n\n")

  cat("DPP:                ", sprintf("%.4f", x$dpp), "\n")
  cat("  (0 = perfect performance, 1 = worst performance)\n\n")

  cat("Area under observed ROC:  ", sprintf("%.4f", x$auc_observed), "\n")
  cat("Area under perfect ROC:   ", sprintf("%.4f", x$auc_perfect), "\n")
  cat("Maximum FA rate observed: ", sprintf("%.4f", x$max_fa), "\n\n")

  cat("ROC Data:\n")
  print(x$roc_data, n = Inf)

  if (!is.null(x$plot)) {
    cat("\nPlot available in $plot\n")
  }

  invisible(x)
}
