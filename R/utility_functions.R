#' Compute Expected Utility Curves for Lineup Identification
#'
#' Computes expected utility at different confidence criteria following
#' Lampinen, Smith, & Wells (2019). Utility analysis accounts for the
#' costs and benefits of different identification outcomes.
#'
#' @param data A dataframe with the following columns:
#'   \itemize{
#'     \item target_present: Logical. TRUE if guilty suspect in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric. Confidence rating (higher = more confident)
#'   }
#' @param base_rate Numeric. Prior probability that suspect is guilty (0 to 1).
#'   Default = 0.5.
#' @param utility_matrix Named numeric vector with utilities/costs for:
#'   \itemize{
#'     \item tp: True positive (correct suspect ID) - typically positive
#'     \item fn: False negative (miss/rejection of guilty) - typically negative
#'     \item fp: False positive (innocent suspect ID) - typically very negative
#'     \item tn: True negative (correct rejection of innocent) - typically positive
#'   }
#'   Default: c(tp=1, fn=-0.5, fp=-2, tn=0.5)
#' @param lineup_size Integer. Number of people in lineup (default = 6).
#'   Used to estimate false IDs from filler choices.
#' @param criteria Character. How to define decision criteria:
#'   \itemize{
#'     \item "confidence": Use confidence thresholds (cumulative from high to low)
#'     \item "all": Include all identifications regardless of confidence
#'   }
#'   Default = "confidence".
#'
#' @return A list containing:
#'   \itemize{
#'     \item utility_data: Dataframe with criterion, hit rate, false alarm rate,
#'       and expected utility
#'     \item max_utility: Maximum expected utility and corresponding criterion
#'     \item avg_utility: Average expected utility across all criteria
#'     \item utility_all_ids: Expected utility if all IDs are accepted
#'     \item base_rate: Base rate used
#'     \item utility_matrix: Utility matrix used
#'   }
#'
#' @details
#' Expected utility combines hit rates and false alarm rates with the costs/benefits
#' of different outcomes:
#'
#' \deqn{EU = base\_rate \cdot [hit \cdot U_{TP} + (1-hit) \cdot U_{FN}] +}
#' \deqn{     (1-base\_rate) \cdot [fa \cdot U_{FP} + (1-fa) \cdot U_{TN}]}
#'
#' Where:
#' \itemize{
#'   \item hit = p(suspect ID | guilty) at criterion
#'   \item fa = p(suspect ID | innocent) at criterion
#'   \item U_TP, U_FN, U_FP, U_TN = utilities for each outcome
#' }
#'
#' The utility matrix should reflect the relative value/cost of outcomes. For example:
#' \itemize{
#'   \item tp = 1: Correctly identifying the perpetrator
#'   \item fn = -0.5: Missing the perpetrator (they remain at large)
#'   \item fp = -2: Wrongly convicting an innocent person (severe injustice)
#'   \item tn = 0.5: Correctly rejecting when innocent
#' }
#'
#' Lampinen et al. (2019) show that comparing procedures by ROC curves alone
#' can be misleading. Utility analysis incorporates base rates and the relative
#' costs of errors, providing a more complete evaluation.
#'
#' @references
#' Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities in
#' eyewitness identification practice: Dissociations between receiver operating
#' characteristic analysis and expected utility analysis. \emph{Law and Human
#' Behavior, 43}(1), 26-44.
#'
#' @export
#' @import tibble
make_utility_curves <- function(data,
                                base_rate = 0.5,
                                utility_matrix = c(tp = 1, fn = -0.5, fp = -2, tn = 0.5),
                                lineup_size = 6,
                                criteria = c("confidence", "all")) {

  # Validate inputs
  required_cols <- c("target_present", "identification", "confidence")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (base_rate < 0 || base_rate > 1) {
    stop("base_rate must be between 0 and 1")
  }

  required_utils <- c("tp", "fn", "fp", "tn")
  missing_utils <- setdiff(required_utils, names(utility_matrix))
  if (length(missing_utils) > 0) {
    stop("utility_matrix must include: ", paste(required_utils, collapse = ", "))
  }

  criteria <- match.arg(criteria)

  # Separate by target presence
  tp_data <- data[data$target_present == TRUE, ]
  ta_data <- data[data$target_present == FALSE, ]

  n_tp <- nrow(tp_data)
  n_ta <- nrow(ta_data)

  if (n_tp == 0 | n_ta == 0) {
    stop("Data must include both target-present and target-absent lineups")
  }

  # Get confidence levels (sorted from high to low for cumulative criteria)
  conf_levels <- sort(unique(data$confidence), decreasing = TRUE)

  # Initialize results
  utility_results <- tibble::tibble(
    criterion = numeric(),
    hit_rate = numeric(),
    false_alarm_rate = numeric(),
    expected_utility = numeric(),
    n_ids_tp = numeric(),
    n_ids_ta = numeric()
  )

  # Compute for each confidence criterion
  for (conf in conf_levels) {
    # Hit rate: proportion of TP trials with suspect ID at or above this confidence
    n_hits <- sum(tp_data$identification == "suspect" & tp_data$confidence >= conf)
    hit_rate <- n_hits / n_tp

    # False alarm rate: proportion of TA trials with suspect ID at or above this confidence
    n_false_suspects <- sum(ta_data$identification == "suspect" & ta_data$confidence >= conf)
    # Estimate from fillers
    n_false_fillers <- sum(ta_data$identification == "filler" & ta_data$confidence >= conf)
    n_estimated_false <- n_false_fillers / lineup_size
    n_total_false <- n_false_suspects + n_estimated_false
    fa_rate <- n_total_false / n_ta

    # Compute expected utility
    # EU = base_rate * [hit*U_tp + (1-hit)*U_fn] + (1-base_rate) * [fa*U_fp + (1-fa)*U_tn]
    eu_guilty <- hit_rate * utility_matrix["tp"] + (1 - hit_rate) * utility_matrix["fn"]
    eu_innocent <- fa_rate * utility_matrix["fp"] + (1 - fa_rate) * utility_matrix["tn"]
    expected_utility <- base_rate * eu_guilty + (1 - base_rate) * eu_innocent

    utility_results <- rbind(utility_results, tibble::tibble(
      criterion = conf,
      hit_rate = hit_rate,
      false_alarm_rate = fa_rate,
      expected_utility = expected_utility,
      n_ids_tp = n_hits,
      n_ids_ta = n_total_false
    ))
  }

  # Add "reject all" criterion (0 hit rate, 0 false alarm rate)
  eu_reject_guilty <- 0 * utility_matrix["tp"] + 1 * utility_matrix["fn"]
  eu_reject_innocent <- 0 * utility_matrix["fp"] + 1 * utility_matrix["tn"]
  eu_reject_all <- base_rate * eu_reject_guilty + (1 - base_rate) * eu_reject_innocent

  utility_results <- rbind(utility_results, tibble::tibble(
    criterion = min(conf_levels) - 1,
    hit_rate = 0,
    false_alarm_rate = 0,
    expected_utility = eu_reject_all,
    n_ids_tp = 0,
    n_ids_ta = 0
  ))

  # Find maximum utility
  max_idx <- which.max(utility_results$expected_utility)
  max_utility <- list(
    expected_utility = utility_results$expected_utility[max_idx],
    criterion = utility_results$criterion[max_idx],
    hit_rate = utility_results$hit_rate[max_idx],
    false_alarm_rate = utility_results$false_alarm_rate[max_idx]
  )

  # Compute average utility across criteria (excluding reject-all)
  avg_utility <- mean(utility_results$expected_utility[utility_results$criterion != min(conf_levels) - 1])

  # Utility if all IDs accepted (lowest threshold)
  utility_all_ids <- utility_results$expected_utility[1]

  list(
    utility_data = utility_results,
    max_utility = max_utility,
    avg_utility = avg_utility,
    utility_all_ids = utility_all_ids,
    base_rate = base_rate,
    utility_matrix = utility_matrix,
    n_target_present = n_tp,
    n_target_absent = n_ta
  )
}


#' Compute Utility Difference Curves Comparing Two Procedures
#'
#' Computes the difference in expected utility between two lineup procedures
#' across a range of base rates and/or utility matrices, following Lampinen,
#' Smith, & Wells (2019).
#'
#' @param data_proc_a A dataframe for procedure A (standard lineup format)
#' @param data_proc_b A dataframe for procedure B (standard lineup format)
#' @param base_rate_grid Numeric vector of base rates to evaluate.
#'   Default: seq(0.01, 0.99, 0.01)
#' @param utility_matrix Named numeric vector with utilities (tp, fn, fp, tn).
#'   Default: c(tp=1, fn=-0.5, fp=-2, tn=0.5)
#' @param utility_type Character. Which utility to compare:
#'   \itemize{
#'     \item "max": Maximum utility (optimal criterion)
#'     \item "avg": Average utility across all criteria
#'     \item "all": Utility of accepting all identifications
#'   }
#'   Default = "max".
#' @param lineup_size Integer. Lineup size (default = 6)
#'
#' @return A list containing:
#'   \itemize{
#'     \item difference_curve: Dataframe with base_rate, eu_a, eu_b, and difference
#'     \item utility_type: Type of utility compared
#'     \item utility_matrix: Utility matrix used
#'     \item crossover_points: Base rates where procedures have equal utility (if any)
#'   }
#'
#' @details
#' Utility difference curves show how the relative value of two procedures
#' changes with the base rate (prevalence) of guilty suspects. This addresses
#' the question: "Which procedure is better, and does it depend on the base rate?"
#'
#' \deqn{Utility Difference = EU_A - EU_B}
#'
#' Interpretation:
#' \itemize{
#'   \item Positive difference: Procedure A has higher utility (preferred)
#'   \item Negative difference: Procedure B has higher utility (preferred)
#'   \item Zero difference: Procedures equivalent at that base rate
#' }
#'
#' Lampinen et al. (2019) show that ROC dominance does not guarantee utility
#' dominance. A procedure with a lower pAUC might have higher utility at
#' certain base rates and cost structures.
#'
#' @references
#' Lampinen, J. M., Smith, A. M., & Wells, G. L. (2019). Four utilities in
#' eyewitness identification practice: Dissociations between receiver operating
#' characteristic analysis and expected utility analysis. \emph{Law and Human
#' Behavior, 43}(1), 26-44.
#'
#' @export
#' @import tibble
make_utility_difference <- function(data_proc_a,
                                    data_proc_b,
                                    base_rate_grid = seq(0.01, 0.99, 0.01),
                                    utility_matrix = c(tp = 1, fn = -0.5, fp = -2, tn = 0.5),
                                    utility_type = c("max", "avg", "all"),
                                    lineup_size = 6) {

  utility_type <- match.arg(utility_type)

  # Initialize results
  difference_results <- tibble::tibble(
    base_rate = numeric(),
    eu_a = numeric(),
    eu_b = numeric(),
    difference = numeric()
  )

  # Compute utilities at each base rate
  for (br in base_rate_grid) {
    # Compute utility curves for both procedures
    util_a <- make_utility_curves(data_proc_a,
                                  base_rate = br,
                                  utility_matrix = utility_matrix,
                                  lineup_size = lineup_size)

    util_b <- make_utility_curves(data_proc_b,
                                  base_rate = br,
                                  utility_matrix = utility_matrix,
                                  lineup_size = lineup_size)

    # Extract the relevant utility value based on type
    if (utility_type == "max") {
      eu_a <- util_a$max_utility$expected_utility
      eu_b <- util_b$max_utility$expected_utility
    } else if (utility_type == "avg") {
      eu_a <- util_a$avg_utility
      eu_b <- util_b$avg_utility
    } else if (utility_type == "all") {
      eu_a <- util_a$utility_all_ids
      eu_b <- util_b$utility_all_ids
    }

    difference <- eu_a - eu_b

    difference_results <- rbind(difference_results, tibble::tibble(
      base_rate = br,
      eu_a = eu_a,
      eu_b = eu_b,
      difference = difference
    ))
  }

  # Find crossover points (where difference changes sign)
  crossover_points <- numeric()
  for (i in 2:nrow(difference_results)) {
    if (sign(difference_results$difference[i]) != sign(difference_results$difference[i-1])) {
      # Linear interpolation to find approximate crossover
      br1 <- difference_results$base_rate[i-1]
      br2 <- difference_results$base_rate[i]
      diff1 <- difference_results$difference[i-1]
      diff2 <- difference_results$difference[i]

      crossover <- br1 + (br2 - br1) * (-diff1) / (diff2 - diff1)
      crossover_points <- c(crossover_points, crossover)
    }
  }

  list(
    difference_curve = difference_results,
    utility_type = utility_type,
    utility_matrix = utility_matrix,
    crossover_points = crossover_points
  )
}


#' Plot Expected Utility Curves
#'
#' Creates a plot showing expected utility across confidence criteria.
#'
#' @param utility_obj List output from make_utility_curves()
#' @param show_max Logical. Whether to highlight maximum utility point.
#'   Default = TRUE.
#' @param show_reject Logical. Whether to show reject-all utility point.
#'   Default = TRUE.
#'
#' @return A ggplot2 object
#'
#' @details
#' The utility curve shows the expected utility at each confidence criterion.
#' Higher confidence criteria have lower hit rates but also lower false alarm
#' rates. The optimal criterion maximizes expected utility given the base rate
#' and cost structure.
#'
#' @export
#' @import ggplot2
plot_utility_curves <- function(utility_obj, show_max = TRUE, show_reject = TRUE) {

  util_data <- utility_obj$utility_data

  # Filter out reject-all if not showing
  if (!show_reject) {
    util_data <- util_data[util_data$hit_rate > 0, ]
  }

  # Create plot
  p <- ggplot(util_data, aes(x = criterion, y = expected_utility)) +
    geom_line(linewidth = 1, color = "darkgreen") +
    geom_point(size = 2, color = "darkgreen") +
    theme_bw(base_size = 14) +
    labs(
      x = "Confidence Criterion",
      y = "Expected Utility",
      title = "Expected Utility Curve",
      subtitle = sprintf("Base rate = %.2f", utility_obj$base_rate),
      caption = sprintf("Utilities: TP=%.1f, FN=%.1f, FP=%.1f, TN=%.1f",
                       utility_obj$utility_matrix["tp"],
                       utility_obj$utility_matrix["fn"],
                       utility_obj$utility_matrix["fp"],
                       utility_obj$utility_matrix["tn"])
    )

  # Highlight maximum utility point
  if (show_max) {
    max_data <- data.frame(
      criterion = utility_obj$max_utility$criterion,
      expected_utility = utility_obj$max_utility$expected_utility
    )
    p <- p + geom_point(data = max_data,
                       aes(x = criterion, y = expected_utility),
                       size = 5, color = "red", shape = 1, stroke = 2)
    p <- p + annotate("text",
                     x = utility_obj$max_utility$criterion,
                     y = utility_obj$max_utility$expected_utility,
                     label = sprintf("Max: %.3f", utility_obj$max_utility$expected_utility),
                     vjust = -1.5, color = "red", fontface = "bold")
  }

  p
}


#' Plot Utility Difference Curve
#'
#' Creates a plot showing the difference in expected utility between two
#' procedures across base rates.
#'
#' @param util_diff_obj List output from make_utility_difference()
#' @param show_crossover Logical. Whether to mark crossover points.
#'   Default = TRUE.
#'
#' @return A ggplot2 object
#'
#' @details
#' Utility difference curves reveal which procedure is superior at different
#' base rates. Regions above zero favor Procedure A, while regions below zero
#' favor Procedure B. Crossover points indicate base rates where procedures
#' are equivalent.
#'
#' @export
#' @import ggplot2
plot_utility_difference <- function(util_diff_obj, show_crossover = TRUE) {

  diff_data <- util_diff_obj$difference_curve

  # Create plot
  p <- ggplot(diff_data, aes(x = base_rate, y = difference)) +
    geom_line(linewidth = 1, color = "darkblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
    theme_bw(base_size = 14) +
    labs(
      x = "Base Rate (Prior Probability of Guilt)",
      y = "Utility Difference (Proc A - Proc B)",
      title = "Utility Difference Curve",
      subtitle = sprintf("Utility type: %s", util_diff_obj$utility_type),
      caption = sprintf("Utilities: TP=%.1f, FN=%.1f, FP=%.1f, TN=%.1f\nPositive: Proc A better; Negative: Proc B better",
                       util_diff_obj$utility_matrix["tp"],
                       util_diff_obj$utility_matrix["fn"],
                       util_diff_obj$utility_matrix["fp"],
                       util_diff_obj$utility_matrix["tn"])
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))

  # Add shaded regions
  p <- p + geom_ribbon(aes(ymin = 0, ymax = pmax(difference, 0)),
                      alpha = 0.2, fill = "blue") +
    geom_ribbon(aes(ymin = pmin(difference, 0), ymax = 0),
               alpha = 0.2, fill = "red")

  # Mark crossover points
  if (show_crossover && length(util_diff_obj$crossover_points) > 0) {
    crossover_data <- data.frame(
      base_rate = util_diff_obj$crossover_points,
      difference = 0
    )
    p <- p + geom_point(data = crossover_data,
                       aes(x = base_rate, y = difference),
                       size = 4, color = "darkgreen", shape = 4, stroke = 2)
    p <- p + geom_text(data = crossover_data,
                      aes(x = base_rate, y = difference,
                          label = sprintf("%.3f", base_rate)),
                      vjust = -1, color = "darkgreen", fontface = "bold")
  }

  p
}


#' Compare Utilities for Two Procedures
#'
#' Main wrapper function to compute and plot utility comparison between
#' two lineup procedures.
#'
#' @param data_proc_a Dataframe for procedure A
#' @param data_proc_b Dataframe for procedure B
#' @param base_rate_grid Numeric vector of base rates (default: seq(0.01, 0.99, 0.01))
#' @param utility_matrix Named vector of utilities (default: c(tp=1, fn=-0.5, fp=-2, tn=0.5))
#' @param utility_type Character. "max", "avg", or "all" (default = "max")
#' @param show_plot Logical. Whether to display plot (default = TRUE)
#' @param ... Additional arguments passed to plot_utility_difference()
#'
#' @return A list containing difference curve data and plot
#'
#' @examples
#' \dontrun{
#' comparison <- compare_utility(sequential_data, simultaneous_data,
#'                              utility_type = "max")
#' print(comparison)
#' }
#'
#' @export
compare_utility <- function(data_proc_a,
                           data_proc_b,
                           base_rate_grid = seq(0.01, 0.99, 0.01),
                           utility_matrix = c(tp = 1, fn = -0.5, fp = -2, tn = 0.5),
                           utility_type = c("max", "avg", "all"),
                           show_plot = TRUE,
                           ...) {

  utility_type <- match.arg(utility_type)

  # Compute utility difference
  util_diff <- make_utility_difference(
    data_proc_a,
    data_proc_b,
    base_rate_grid = base_rate_grid,
    utility_matrix = utility_matrix,
    utility_type = utility_type
  )

  # Create plot
  if (show_plot) {
    util_plot <- plot_utility_difference(util_diff, ...)
  } else {
    util_plot <- NULL
  }

  result <- list(
    plot = util_plot,
    difference_curve = util_diff$difference_curve,
    utility_type = util_diff$utility_type,
    utility_matrix = util_diff$utility_matrix,
    crossover_points = util_diff$crossover_points
  )

  class(result) <- c("lineup_utility_comparison", "list")
  result
}


#' Print Method for lineup_utility_comparison Objects
#' @param x A lineup_utility_comparison object
#' @param ... Additional arguments (ignored)
#' @export
print.lineup_utility_comparison <- function(x, ...) {
  cat("\n=== Lineup Utility Comparison ===\n\n")
  cat("Utility type:", x$utility_type, "\n")
  cat("Utility matrix:\n")
  print(x$utility_matrix)
  cat("\n")

  if (length(x$crossover_points) > 0) {
    cat("Crossover points (base rates where procedures are equivalent):\n")
    cat("  ", paste(sprintf("%.3f", x$crossover_points), collapse = ", "), "\n\n")
  } else {
    cat("No crossover points - one procedure dominates across all base rates\n\n")
  }

  # Summary at key base rates
  key_rates <- c(0.1, 0.3, 0.5, 0.7, 0.9)
  cat("Utility difference at key base rates:\n")
  cat("  (Positive = Proc A better; Negative = Proc B better)\n\n")
  for (br in key_rates) {
    idx <- which.min(abs(x$difference_curve$base_rate - br))
    if (length(idx) > 0) {
      diff <- x$difference_curve$difference[idx]
      cat(sprintf("  Base rate %.1f: %+.4f\n", br, diff))
    }
  }

  if (!is.null(x$plot)) {
    cat("\nPlot available in $plot\n")
  }

  invisible(x)
}
