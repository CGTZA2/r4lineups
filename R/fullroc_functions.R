#' Compute Full ROC Data for Lineup Identification (Smith & Yang, 2020)
#'
#' Computes full Receiver Operating Characteristic (ROC) data using ALL response
#' categories (suspect ID, filler ID, rejection) following Smith & Yang (2020).
#' This method distinguishes between investigator discriminability and eyewitness
#' discriminability.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup, FALSE if innocent
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating (higher = more confident)
#'   }
#' @param conf_bins Numeric vector specifying confidence bin boundaries (default = NULL,
#'   which uses unique confidence values). Can specify bins like c(0, 60, 80, 100) to
#'   create low (0-60), medium (60-80), and high (80-100) confidence categories.
#' @param order Character. Method for ordering response categories:
#'   \itemize{
#'     \item "diagnosticity": Order by diagnosticity ratio (DR = HR/FAR) (default)
#'     \item "apriori": Use a-priori ordering (suspect high conf → filler/reject low conf)
#'   }
#' @param lineup_size Integer. Number of people in the lineup (default = 6)
#' @param epsilon Numeric. Small value to add when FAR = 0 to avoid infinite DR (default = 0.001)
#'
#' @return A list containing:
#'   \itemize{
#'     \item roc_data: Dataframe with cumulative hit rates and false alarm rates
#'     \item auc: Full area under the curve
#'     \item order_used: The ordering method used
#'     \item diagnosticity_table: Table showing non-cumulative rates and DR for each category
#'     \item n_target_present: Number of target-present lineups
#'     \item n_target_absent: Number of target-absent lineups
#'   }
#'
#' @details
#' This function implements the "full ROC" method described in Smith & Yang (2020).
#' Unlike traditional partial ROC curves that only use suspect identifications,
#' this method uses ALL eyewitness responses:
#' \itemize{
#'   \item Suspect identifications (evidence of guilt)
#'   \item Filler identifications (evidence of innocence)
#'   \item Lineup rejections (evidence of innocence)
#' }
#'
#' Each response type is crossed with confidence levels to create decision criteria.
#' These are ordered by their diagnosticity ratio (HR/FAR) or by a-priori ordering,
#' then cumulative hit and false alarm rates are computed to form the full ROC curve.
#'
#' The full ROC provides a threshold-free measure of investigator discriminability—
#' the ability to distinguish guilty from innocent suspects using ALL available
#' eyewitness evidence.
#'
#' @examples
#' \dontrun{
#' # Example data structure:
#' lineup_data <- data.frame(
#'   target_present = c(rep(TRUE, 50), rep(FALSE, 50)),
#'   identification = sample(c("suspect", "filler", "reject"), 100, replace = TRUE),
#'   confidence = sample(c(20, 40, 60, 80, 100), 100, replace = TRUE)
#' )
#'
#' # Compute full ROC with diagnosticity ordering
#' fullroc_result <- make_fullroc_data(lineup_data)
#' print(fullroc_result$auc)
#'
#' # Compute full ROC with specified confidence bins
#' fullroc_binned <- make_fullroc_data(lineup_data, conf_bins = c(0, 60, 80, 100))
#' }
#'
#' @references
#' Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator
#' discriminability and eyewitness discriminability: A method for creating full receiver
#' operating characteristic curves of lineup identification performance.
#' \emph{Perspectives on Psychological Science, 15}(3), 589-607.
#'
#' @export
#' @import tibble
make_fullroc_data <- function(data,
                               conf_bins = NULL,
                               order = c("diagnosticity", "apriori"),
                               lineup_size = 6,
                               epsilon = 0.001) {

  order <- match.arg(order)

  # Validate required columns
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Bin confidence if requested
  if (!is.null(conf_bins)) {
    data$conf_bin <- cut(data$confidence,
                         breaks = conf_bins,
                         include.lowest = TRUE,
                         right = FALSE,
                         labels = paste0("conf_", seq_along(conf_bins[-1])))
  } else {
    # Use unique confidence values as bins
    data$conf_bin <- factor(data$confidence)
  }

  # Separate target-present and target-absent lineups
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  n_tp <- nrow(tp_data)
  n_ta <- nrow(ta_data)

  if (n_tp == 0 | n_ta == 0) {
    stop("Data must include both target-present and target-absent lineups")
  }

  # Create all combinations of decision × confidence
  decision_types <- c("suspect", "filler", "reject")
  conf_levels <- levels(data$conf_bin)

  # Compute non-cumulative hit rates and false alarm rates for each category
  diagnosticity_table <- expand.grid(
    decision = decision_types,
    conf_bin = conf_levels,
    stringsAsFactors = FALSE
  )

  diagnosticity_table$hit_rate <- 0
  diagnosticity_table$false_alarm_rate <- 0
  diagnosticity_table$n_hits <- 0
  diagnosticity_table$n_false_alarms <- 0

  for (i in 1:nrow(diagnosticity_table)) {
    dec <- diagnosticity_table$decision[i]
    conf <- diagnosticity_table$conf_bin[i]

    # Count hits (target-present)
    n_hits <- sum(tp_data$identification == dec & tp_data$conf_bin == conf)
    hit_rate <- n_hits / n_tp

    # Count false alarms (target-absent)
    n_fa <- sum(ta_data$identification == dec & ta_data$conf_bin == conf)
    false_alarm_rate <- n_fa / n_ta

    diagnosticity_table$hit_rate[i] <- hit_rate
    diagnosticity_table$false_alarm_rate[i] <- false_alarm_rate
    diagnosticity_table$n_hits[i] <- n_hits
    diagnosticity_table$n_false_alarms[i] <- n_fa
  }

  # Compute diagnosticity ratio (with epsilon for FAR = 0)
  diagnosticity_table$diagnosticity_ratio <- ifelse(
    diagnosticity_table$false_alarm_rate == 0,
    diagnosticity_table$hit_rate / epsilon,  # Use epsilon to avoid Inf
    diagnosticity_table$hit_rate / diagnosticity_table$false_alarm_rate
  )

  # Handle cases where both HR and FAR are 0
  diagnosticity_table$diagnosticity_ratio[
    diagnosticity_table$hit_rate == 0 & diagnosticity_table$false_alarm_rate == 0
  ] <- 1.0  # Neutral evidence

  # Create evidence label
  diagnosticity_table$evidence_label <- paste(
    diagnosticity_table$decision,
    diagnosticity_table$conf_bin,
    sep = "_"
  )

  # Order the categories
  if (order == "diagnosticity") {
    # Order by diagnosticity ratio (descending = evidence of guilt)
    diagnosticity_table <- diagnosticity_table[
      order(diagnosticity_table$diagnosticity_ratio, decreasing = TRUE),
    ]
  } else {
    # A-priori ordering: suspect (high→low conf) → filler (low→high conf) → reject (low→high conf)
    diagnosticity_table$decision_order <- factor(
      diagnosticity_table$decision,
      levels = c("suspect", "filler", "reject")
    )

    # For suspect: high confidence first
    # For filler and reject: low confidence first (more diagnostic of innocence when rare)
    diagnosticity_table <- diagnosticity_table[
      order(
        diagnosticity_table$decision_order,
        ifelse(diagnosticity_table$decision == "suspect",
               -as.numeric(diagnosticity_table$conf_bin),  # High conf first for suspect
               as.numeric(diagnosticity_table$conf_bin))   # Low conf first for filler/reject
      ),
    ]
  }

  # Remove rows with zero counts (don't contribute to ROC)
  diagnosticity_table <- diagnosticity_table[
    diagnosticity_table$hit_rate > 0 | diagnosticity_table$false_alarm_rate > 0,
  ]

  # Compute cumulative rates
  diagnosticity_table$cumulative_hit_rate <- cumsum(diagnosticity_table$hit_rate)
  diagnosticity_table$cumulative_false_alarm_rate <- cumsum(diagnosticity_table$false_alarm_rate)

  # Add origin point (0, 0)
  roc_data <- rbind(
    data.frame(
      cumulative_hit_rate = 0,
      cumulative_false_alarm_rate = 0,
      evidence_label = "origin"
    ),
    data.frame(
      cumulative_hit_rate = diagnosticity_table$cumulative_hit_rate,
      cumulative_false_alarm_rate = diagnosticity_table$cumulative_false_alarm_rate,
      evidence_label = diagnosticity_table$evidence_label
    )
  )

  # Calculate full AUC using trapezoidal rule
  auc <- 0
  for (i in 2:nrow(roc_data)) {
    width <- roc_data$cumulative_false_alarm_rate[i] - roc_data$cumulative_false_alarm_rate[i-1]
    height <- (roc_data$cumulative_hit_rate[i] + roc_data$cumulative_hit_rate[i-1]) / 2
    auc <- auc + (width * height)
  }

  list(
    roc_data = roc_data,
    auc = auc,
    order_used = order,
    diagnosticity_table = diagnosticity_table,
    n_target_present = n_tp,
    n_target_absent = n_ta,
    lineup_size = lineup_size
  )
}


#' Compute and Plot Full ROC Curve (Smith & Yang, 2020)
#'
#' Main function to compute and plot a full ROC curve using all eyewitness responses
#' (suspect ID, filler ID, rejection) following Smith & Yang (2020).
#'
#' @param data A dataframe with columns: target_present, identification, confidence
#' @param conf_bins Numeric vector for confidence bin boundaries (default = NULL)
#' @param order Character. "diagnosticity" or "apriori" (default = "diagnosticity")
#' @param lineup_size Integer. Number of people in lineup (default = 6)
#' @param show_plot Logical. Whether to display the plot (default = TRUE)
#' @param epsilon Numeric. Small value for FAR = 0 cases (default = 0.001)
#' @param ... Additional arguments passed to plot_fullroc()
#'
#' @return A list containing:
#'   \itemize{
#'     \item plot: ggplot2 object (if show_plot = TRUE)
#'     \item roc_data: Dataframe with ROC curve points
#'     \item auc: Full area under the curve
#'     \item diagnosticity_table: Table with diagnosticity ratios
#'     \item summary: Summary statistics
#'   }
#'
#' @details
#' This is the main user-facing function for computing full ROC curves.
#' It calls make_fullroc_data() to compute the ROC, then plot_fullroc() to visualize it.
#'
#' @examples
#' \dontrun{
#' # Example with simulated data
#' set.seed(123)
#' lineup_data <- data.frame(
#'   target_present = c(rep(TRUE, 100), rep(FALSE, 100)),
#'   identification = c(
#'     sample(c("suspect", "filler", "reject"), 100, replace = TRUE, prob = c(0.6, 0.2, 0.2)),
#'     sample(c("suspect", "filler", "reject"), 100, replace = TRUE, prob = c(0.2, 0.3, 0.5))
#'   ),
#'   confidence = sample(seq(0, 100, by = 10), 200, replace = TRUE)
#' )
#'
#' # Compute and plot full ROC
#' result <- make_fullroc(lineup_data)
#' print(result$auc)
#'
#' # With custom confidence bins
#' result2 <- make_fullroc(lineup_data, conf_bins = c(0, 60, 80, 100))
#' }
#'
#' @references
#' Smith, A. M., Yang, Y., & Wells, G. L. (2020). Distinguishing between investigator
#' discriminability and eyewitness discriminability: A method for creating full receiver
#' operating characteristic curves of lineup identification performance.
#' \emph{Perspectives on Psychological Science, 15}(3), 589-607.
#'
#' @export
make_fullroc <- function(data,
                         conf_bins = NULL,
                         order = c("diagnosticity", "apriori"),
                         lineup_size = 6,
                         show_plot = TRUE,
                         epsilon = 0.001,
                         ...) {

  order <- match.arg(order)

  # Compute full ROC data
  fullroc_obj <- make_fullroc_data(
    data = data,
    conf_bins = conf_bins,
    order = order,
    lineup_size = lineup_size,
    epsilon = epsilon
  )

  # Create plot
  if (show_plot) {
    fullroc_plot <- plot_fullroc(fullroc_obj, ...)
  } else {
    fullroc_plot <- NULL
  }

  # Create summary
  summary_stats <- list(
    auc = fullroc_obj$auc,
    n_target_present = fullroc_obj$n_target_present,
    n_target_absent = fullroc_obj$n_target_absent,
    lineup_size = fullroc_obj$lineup_size,
    order_method = fullroc_obj$order_used,
    n_operating_points = nrow(fullroc_obj$roc_data) - 1,  # Exclude (0,0) point
    max_hit_rate = max(fullroc_obj$roc_data$cumulative_hit_rate),
    max_false_alarm_rate = max(fullroc_obj$roc_data$cumulative_false_alarm_rate)
  )

  result <- list(
    plot = fullroc_plot,
    roc_data = fullroc_obj$roc_data,
    auc = fullroc_obj$auc,
    diagnosticity_table = fullroc_obj$diagnosticity_table,
    summary = summary_stats
  )

  class(result) <- c("lineup_fullroc", "list")
  result
}


#' Plot Full ROC Curve
#'
#' Creates a ggplot2 visualization of full ROC data from lineup identification
#' experiments using the Smith & Yang (2020) method.
#'
#' @param fullroc_obj List output from make_fullroc_data()
#' @param show_auc Logical. Whether to display AUC value on plot (default = TRUE)
#' @param point_labels Logical. Whether to label points (default = FALSE, can be crowded)
#' @param title Character. Plot title (default = "Full ROC Curve (Smith & Yang, 2020)")
#'
#' @return A ggplot2 object
#'
#' @export
#' @import ggplot2 ggrepel
plot_fullroc <- function(fullroc_obj,
                         show_auc = TRUE,
                         point_labels = FALSE,
                         title = "Full ROC Curve (Smith & Yang, 2020)") {

  roc_data <- fullroc_obj$roc_data
  auc <- fullroc_obj$auc

  # Create base plot
  p <- ggplot(roc_data, aes(x = cumulative_false_alarm_rate, y = cumulative_hit_rate)) +
    geom_line(size = 1, color = "darkblue") +
    geom_point(shape = 21, color = "black", fill = "steelblue", size = 3) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    theme_bw(base_size = 14) +
    labs(
      x = "Cumulative False Alarm Rate (Innocent Suspect)",
      y = "Cumulative Hit Rate (Guilty Suspect)",
      title = title,
      subtitle = paste0("Ordering method: ", fullroc_obj$order_used),
      caption = "Includes ALL responses: suspect IDs, filler IDs, and rejections\nDashed line represents chance performance"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2))

  # Add point labels if requested
  if (point_labels && nrow(roc_data) <= 20) {  # Only label if not too many points
    p <- p + geom_text_repel(
      aes(label = evidence_label),
      size = 2.5,
      max.overlaps = 15
    )
  }

  # Add shaded area under curve
  p <- p + geom_ribbon(
    aes(ymin = cumulative_false_alarm_rate, ymax = cumulative_hit_rate),
    alpha = 0.2,
    fill = "steelblue"
  )

  # Add AUC annotation
  if (show_auc) {
    p <- p + annotate(
      "text",
      x = 0.7,
      y = 0.15,
      label = paste0("Full AUC = ", round(auc, 3)),
      size = 5,
      fontface = "bold"
    )
  }

  p
}


#' Print Method for lineup_fullroc Objects
#'
#' @param x A lineup_fullroc object
#' @param show_diagnosticity Logical. Whether to print diagnosticity table (default = TRUE)
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_fullroc <- function(x, show_diagnosticity = TRUE, ...) {
  cat("\n=== Full Lineup ROC Analysis (Smith & Yang, 2020) ===\n\n")
  cat("Full AUC:", round(x$auc, 3), "\n")
  cat("Target-present lineups:", x$summary$n_target_present, "\n")
  cat("Target-absent lineups:", x$summary$n_target_absent, "\n")
  cat("Lineup size:", x$summary$lineup_size, "\n")
  cat("Ordering method:", x$summary$order_method, "\n")
  cat("Operating points:", x$summary$n_operating_points, "\n\n")

  cat("ROC Data (first 10 points):\n")
  print(head(x$roc_data, 10))

  if (show_diagnosticity && nrow(x$diagnosticity_table) <= 30) {
    cat("\n\nDiagnosticity Table (ordered by evidence strength):\n")
    diag_display <- x$diagnosticity_table[, c("evidence_label", "hit_rate",
                                                "false_alarm_rate", "diagnosticity_ratio")]
    print(diag_display, row.names = FALSE)
  } else if (show_diagnosticity) {
    cat("\n\nDiagnosticity Table (first 15 rows, ordered by evidence strength):\n")
    diag_display <- x$diagnosticity_table[1:15, c("evidence_label", "hit_rate",
                                                    "false_alarm_rate", "diagnosticity_ratio")]
    print(diag_display, row.names = FALSE)
    cat(sprintf("\n... and %d more rows\n", nrow(x$diagnosticity_table) - 15))
  }

  if (!is.null(x$plot)) {
    cat("\nPlot available in $plot\n")
  }

  invisible(x)
}
