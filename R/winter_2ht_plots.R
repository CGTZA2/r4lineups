#' Plot Parameter Estimates from 2-HT Model
#'
#' @description
#' Creates a visual display of the parameter estimates from a fitted 2-HT model,
#' with error bars showing 95\% confidence intervals.
#'
#' @param x A winter_2ht object from fit_winter_2ht()
#' @param ... Additional arguments passed to plotting functions
#' @param which Character vector specifying which parameters to plot.
#'   Default is c("dP", "dA", "b", "g") (all parameters).
#' @param show_ci Logical indicating whether to show 95\% confidence intervals. Default is TRUE.
#'
#' @return A ggplot object
#'
#' @export
plot_2ht_parameters <- function(x, ..., which = c("dP", "dA", "b", "g"), show_ci = TRUE) {

  if (!inherits(x, "winter_2ht")) {
    stop("x must be a winter_2ht object")
  }

  # Filter requested parameters
  params <- x$parameters[which]
  se <- x$se[which]

  # Create data frame for plotting
  plot_data <- data.frame(
    parameter = factor(names(params), levels = names(params)),
    estimate = as.numeric(params),
    se = as.numeric(se),
    lower = as.numeric(params) - 1.96 * as.numeric(se),
    upper = as.numeric(params) + 1.96 * as.numeric(se)
  )

  # Create labels
  param_labels <- c(
    dP = "dP\n(Culprit Presence\nDetection)",
    dA = "dA\n(Culprit Absence\nDetection)",
    b = "b\n(Biased\nSelection)",
    g = "g\n(Guessing)"
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = parameter, y = estimate)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7, width = 0.6) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(
      title = "2-HT Model Parameter Estimates",
      x = NULL,
      y = "Parameter Value"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 9)
    ) +
    ggplot2::scale_x_discrete(labels = param_labels[which])

  # Add error bars if requested
  if (show_ci && !any(is.na(se))) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(lower, 0), ymax = pmin(upper, 1)),
      width = 0.2,
      size = 0.8
    )
  }

  # Add value labels
  p <- p + ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", estimate)),
    vjust = -0.5,
    size = 3.5
  )

  return(p)
}


#' Plot Observed vs. Expected Counts
#'
#' Creates a comparison plot showing observed counts vs. model-predicted expected counts
#' for both target-present and target-absent lineups.
#'
#' @param x A winter_2ht object from fit_winter_2ht()
#' @param ... Additional arguments (not used)
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' counts <- c(
#'   n_tp_suspect = 147, n_tp_filler = 94, n_tp_reject = 141,
#'   n_ta_suspect = 38, n_ta_filler = 138, n_ta_reject = 206
#' )
#' fit <- fit_winter_2ht(counts, lineup_size = 6)
#' plot_2ht_fit(fit)
#' }
#'
#' @export
plot_2ht_fit <- function(x, ...) {

  if (!inherits(x, "winter_2ht")) {
    stop("x must be a winter_2ht object")
  }

  # Create data frame for plotting
  plot_data <- data.frame(
    lineup_type = rep(c("Target-Present", "Target-Absent"), each = 3),
    outcome = rep(c("Suspect ID", "Filler ID", "Reject"), 2),
    observed = c(x$observed_counts$tp, x$observed_counts$ta),
    expected = c(x$expected_counts$tp, x$expected_counts$ta)
  )

  # Reshape for plotting using base R
  plot_data_long <- data.frame(
    lineup_type = rep(plot_data$lineup_type, 2),
    outcome = rep(plot_data$outcome, 2),
    type = rep(c("Observed", "Expected"), each = nrow(plot_data)),
    count = c(plot_data$observed, plot_data$expected)
  )

  plot_data_long$type <- factor(
    plot_data_long$type,
    levels = c("Observed", "Expected")
  )

  plot_data_long$outcome <- factor(
    plot_data_long$outcome,
    levels = c("Suspect ID", "Filler ID", "Reject")
  )

  # Create plot
  p <- ggplot2::ggplot(
    plot_data_long,
    ggplot2::aes(x = outcome, y = count, fill = type)
  ) +
    ggplot2::geom_col(position = "dodge", alpha = 0.8) +
    ggplot2::facet_wrap(~ lineup_type, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = c("Observed" = "steelblue", "Expected" = "coral"),
      name = NULL
    ) +
    ggplot2::labs(
      title = "2-HT Model Fit: Observed vs. Expected Counts",
      x = NULL,
      y = "Count"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}


#' Plot Method for winter_2ht Objects
#'
#' Creates a default plot for winter_2ht objects showing parameter estimates.
#'
#' @param x A winter_2ht object
#' @param ... Additional arguments passed to plot_2ht_parameters()
#'
#' @export
plot.winter_2ht <- function(x, ...) {
  plot_2ht_parameters(x, ...)
}
