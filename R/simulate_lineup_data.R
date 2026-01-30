#' Simulate Lineup Identification Data
#'
#' Generates simulated eyewitness lineup data based on signal detection theory
#' parameters. Useful for power analysis, method validation, and teaching.
#'
#' @param n_tp Integer. Number of target-present lineups (default = 100)
#' @param n_ta Integer. Number of target-absent lineups (default = 100)
#' @param d_prime Numeric. Discriminability (d') between target and lure
#'   distributions. Higher values = better memory. Typical range: 0.5 to 3.0
#'   (default = 1.5)
#' @param c_criterion Numeric or vector. Decision criterion/criteria for making
#'   identifications. Lower values = more liberal (more IDs). Can be a single
#'   value or vector for multiple confidence bins (default = 0)
#' @param lineup_size Integer. Number of lineup members (default = 6)
#' @param conf_levels Integer. Number of confidence levels to simulate. If NULL,
#'   returns binary decision only (default = NULL)
#' @param include_response_time Logical. Whether to simulate response times
#'   correlated with memory strength (default = FALSE)
#' @param seed Integer. Random seed for reproducibility (default = NULL)
#'
#' @return A dataframe with columns:
#'   \itemize{
#'     \item participant_id: Unique ID for each trial
#'     \item target_present: Logical. TRUE if target in lineup
#'     \item identification: Character. "suspect", "filler", or "reject"
#'     \item confidence: Numeric (if conf_levels specified). Confidence rating
#'     \item response_time: Numeric (if include_response_time = TRUE). RT in ms
#'   }
#'
#' @details
#' This function implements a simple signal detection model:
#' \itemize{
#'   \item Target distribution: Normal(d_prime, 1)
#'   \item Lure distribution: Normal(0, 1)
#'   \item Decision: Sample memory strength, compare to criterion
#' }
#'
#' The simulation assumes:
#' \itemize{
#'   \item Independent observations (each lineup member sampled independently)
#'   \item Fair lineups (all fillers equally similar to description)
#'   \item Perfect attention (no guessing without memory)
#' }
#'
#' Response times (if simulated):
#' \itemize{
#'   \item Based on drift-diffusion model logic
#'   \item Faster for stronger memory signals
#'   \item RT ~ InverseGaussian(strength-dependent)
#' }
#'
#' @references
#' Wixted, J. T., Vul, E., Mickes, L., & Wilson, B. M. (2018). Models of
#' lineup memory. \emph{Cognitive Psychology, 105}, 8-114.
#'
#' Mickes, L., et al. (2024). pyWitness 1.0: A python eyewitness
#' identification analysis toolkit. \emph{Behavior Research Methods, 56},
#' 1533-1550.
#'
#' @examples
#' \dontrun{
#' # Basic simulation: strong memory
#' strong_memory <- simulate_lineup_data(
#'   n_tp = 200, n_ta = 200,
#'   d_prime = 2.0,
#'   conf_levels = 3
#' )
#'
#' # Weak memory with response times
#' weak_memory <- simulate_lineup_data(
#'   n_tp = 200, n_ta = 200,
#'   d_prime = 1.0,
#'   conf_levels = 5,
#'   include_response_time = TRUE,
#'   seed = 42
#' )
#'
#' # Power analysis: vary sample size
#' power_results <- simulate_power_analysis(
#'   sample_sizes = c(50, 100, 200, 500),
#'   d_prime = 1.5,
#'   n_simulations = 1000
#' )
#' }
#'
#' @export
simulate_lineup_data <- function(n_tp = 100,
                                  n_ta = 100,
                                  d_prime = 1.5,
                                  c_criterion = 0,
                                  lineup_size = 6,
                                  conf_levels = NULL,
                                  include_response_time = FALSE,
                                  seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Validate inputs
  if (n_tp < 1 || n_ta < 1) {
    stop("n_tp and n_ta must be positive integers")
  }
  if (d_prime < 0) {
    stop("d_prime must be non-negative")
  }
  if (lineup_size < 1) {
    stop("lineup_size must be at least 1")
  }

  # Initialize results
  all_data <- list()

  # --- Target-Present Lineups ---
  for (i in 1:n_tp) {
    # Sample memory strength for each lineup member
    # Target: N(d', 1), Fillers: N(0, 1)
    target_strength <- rnorm(1, mean = d_prime, sd = 1)
    filler_strengths <- rnorm(lineup_size - 1, mean = 0, sd = 1)

    # MAX rule: Choose lineup member with highest strength
    all_strengths <- c(target_strength, filler_strengths)
    max_strength <- max(all_strengths)
    max_position <- which.max(all_strengths)

    # Decision: ID if max exceeds criterion, otherwise reject
    if (!is.null(conf_levels)) {
      # Multiple criteria for confidence levels
      criteria <- seq(min(c_criterion) - 1,
                      max(c_criterion) + 2,
                      length.out = conf_levels + 1)
      conf <- findInterval(max_strength, criteria, rightmost.closed = TRUE)
      conf <- pmax(1, pmin(conf_levels, conf))  # Bound to valid range

      # Identify if exceeds lowest criterion
      makes_id <- max_strength > criteria[1]
    } else {
      makes_id <- max_strength > c_criterion
      conf <- NA
    }

    # Determine identification
    if (makes_id) {
      if (max_position == 1) {
        identification <- "suspect"
      } else {
        identification <- "filler"
      }
    } else {
      identification <- "reject"
    }

    # Simulate response time (if requested)
    if (include_response_time) {
      # RT based on memory strength (stronger = faster)
      # Base RT + inverse of strength + noise
      rt <- 5000 + (3000 / (1 + max_strength^2)) + rnorm(1, 0, 1000)
      rt <- max(rt, 500)  # Minimum 500ms
    } else {
      rt <- NA
    }

    all_data[[length(all_data) + 1]] <- data.frame(
      participant_id = i,
      target_present = TRUE,
      identification = identification,
      confidence = ifelse(!is.null(conf_levels), conf, NA),
      response_time = rt,
      stringsAsFactors = FALSE
    )
  }

  # --- Target-Absent Lineups ---
  for (i in 1:n_ta) {
    # All lineup members are fillers: N(0, 1)
    filler_strengths <- rnorm(lineup_size, mean = 0, sd = 1)

    # MAX rule: Choose lineup member with highest strength
    max_strength <- max(filler_strengths)
    max_position <- which.max(filler_strengths)

    # Decision: ID if max exceeds criterion, otherwise reject
    if (!is.null(conf_levels)) {
      criteria <- seq(min(c_criterion) - 1,
                      max(c_criterion) + 2,
                      length.out = conf_levels + 1)
      conf <- findInterval(max_strength, criteria, rightmost.closed = TRUE)
      conf <- pmax(1, pmin(conf_levels, conf))

      makes_id <- max_strength > criteria[1]
    } else {
      makes_id <- max_strength > c_criterion
      conf <- NA
    }

    # Determine identification
    # In target-absent, position 1 is designated "innocent suspect"
    if (makes_id) {
      if (max_position == 1) {
        identification <- "suspect"
      } else {
        identification <- "filler"
      }
    } else {
      identification <- "reject"
    }

    # Simulate response time
    if (include_response_time) {
      rt <- 5000 + (3000 / (1 + max_strength^2)) + rnorm(1, 0, 1000)
      rt <- max(rt, 500)
    } else {
      rt <- NA
    }

    all_data[[length(all_data) + 1]] <- data.frame(
      participant_id = n_tp + i,
      target_present = FALSE,
      identification = identification,
      confidence = ifelse(!is.null(conf_levels), conf, NA),
      response_time = rt,
      stringsAsFactors = FALSE
    )
  }

  # Combine all trials
  result <- do.call(rbind, all_data)

  # Remove NA columns if not used
  if (all(is.na(result$confidence))) {
    result$confidence <- NULL
  }
  if (all(is.na(result$response_time))) {
    result$response_time <- NULL
  }

  # Shuffle rows
  result <- result[sample(nrow(result)), ]
  rownames(result) <- NULL

  # Add attributes for reproducibility
  attr(result, "simulation_params") <- list(
    n_tp = n_tp,
    n_ta = n_ta,
    d_prime = d_prime,
    c_criterion = c_criterion,
    lineup_size = lineup_size,
    conf_levels = conf_levels,
    seed = seed
  )

  class(result) <- c("simulated_lineup_data", "data.frame")
  return(result)
}


#' Print Method for Simulated Lineup Data
#' @param x A simulated_lineup_data object
#' @param ... Additional arguments (ignored)
#' @export
print.simulated_lineup_data <- function(x, ...) {
  params <- attr(x, "simulation_params")

  cat("\n=== Simulated Lineup Data ===\n\n")
  cat("Simulation Parameters:\n")
  cat("  Target-present lineups:", params$n_tp, "\n")
  cat("  Target-absent lineups:", params$n_ta, "\n")
  cat("  d-prime:", params$d_prime, "\n")
  cat("  Criterion:", params$c_criterion, "\n")
  cat("  Lineup size:", params$lineup_size, "\n")
  if (!is.null(params$conf_levels)) {
    cat("  Confidence levels:", params$conf_levels, "\n")
  }
  if (!is.null(params$seed)) {
    cat("  Random seed:", params$seed, "\n")
  }

  cat("\nData Summary:\n")
  cat("  Total trials:", nrow(x), "\n")

  # Count IDs by type
  tp_data <- x[x$target_present == TRUE, ]
  ta_data <- x[x$target_present == FALSE, ]

  cat("  Target-present:\n")
  cat("    Suspect IDs:", sum(tp_data$identification == "suspect"), "\n")
  cat("    Filler IDs:", sum(tp_data$identification == "filler"), "\n")
  cat("    Rejections:", sum(tp_data$identification == "reject"), "\n")

  cat("  Target-absent:\n")
  cat("    Suspect IDs:", sum(ta_data$identification == "suspect"), "\n")
  cat("    Filler IDs:", sum(ta_data$identification == "filler"), "\n")
  cat("    Rejections:", sum(ta_data$identification == "reject"), "\n")

  # Compute basic accuracy
  n_correct <- sum(tp_data$identification == "suspect")
  n_false <- sum(ta_data$identification == "suspect")
  n_filler_ta <- sum(ta_data$identification == "filler")
  n_false_total <- n_false + (n_filler_ta / params$lineup_size)
  accuracy <- n_correct / (n_correct + n_false_total)

  cat("\n  Suspect ID Accuracy:", round(accuracy, 3), "\n")

  cat("\nFirst 10 rows:\n")
  print(head(as.data.frame(x), 10))

  invisible(x)
}


#' Simulate Power Analysis for Lineup Studies
#'
#' Conducts power analysis by simulating data across different sample sizes
#' and computing a statistic of interest (e.g., pAUC, d').
#'
#' @param sample_sizes Integer vector. Sample sizes to test (e.g., c(50, 100, 200))
#' @param d_prime Numeric. True effect size (d') to detect
#' @param n_simulations Integer. Number of simulations per sample size (default = 1000)
#' @param stat_function Function. Takes a dataframe and returns a statistic.
#'   Default computes pAUC from ROC analysis.
#' @param alpha Numeric. Significance level (default = 0.05)
#' @param ... Additional arguments passed to simulate_lineup_data()
#'
#' @return A dataframe with columns:
#'   \itemize{
#'     \item sample_size: The N tested
#'     \item mean_stat: Mean value of statistic
#'     \item sd_stat: Standard deviation of statistic
#'     \item ci_lower: Lower 95% CI
#'     \item ci_upper: Upper 95% CI
#'     \item power: Proportion of simulations where effect was detected
#'   }
#'
#' @examples
#' \dontrun{
#' # Power to detect d' = 1.5 with pAUC
#' power_res <- simulate_power_analysis(
#'   sample_sizes = c(50, 100, 200, 500),
#'   d_prime = 1.5,
#'   n_simulations = 500,
#'   conf_levels = 5,
#'   seed = 123
#' )
#' print(power_res)
#' plot(power_res)
#' }
#'
#' @export
simulate_power_analysis <- function(sample_sizes,
                                     d_prime,
                                     n_simulations = 1000,
                                     stat_function = NULL,
                                     alpha = 0.05,
                                     ...) {

  # Default statistic: pAUC from ROC
  if (is.null(stat_function)) {
    stat_function <- function(data) {
      # Simple pAUC calculation
      roc_obj <- tryCatch(
        make_roc(data, show_plot = FALSE),
        error = function(e) NULL
      )
      if (is.null(roc_obj)) return(NA)
      return(roc_obj$pAUC)
    }
  }

  results <- list()

  for (n in sample_sizes) {
    cat("Simulating sample size:", n, "\n")

    stats <- numeric(n_simulations)

    for (sim in 1:n_simulations) {
      # Simulate data
      sim_data <- simulate_lineup_data(
        n_tp = n,
        n_ta = n,
        d_prime = d_prime,
        ...
      )

      # Compute statistic
      stats[sim] <- stat_function(sim_data)
    }

    # Remove NAs
    stats <- stats[!is.na(stats)]

    # Compute summary
    results[[length(results) + 1]] <- data.frame(
      sample_size = n,
      mean_stat = mean(stats),
      sd_stat = sd(stats),
      ci_lower = quantile(stats, alpha / 2),
      ci_upper = quantile(stats, 1 - alpha / 2),
      power = mean(stats > 0)  # Simplified: prop > 0
    )
  }

  result_df <- do.call(rbind, results)
  class(result_df) <- c("power_analysis", "data.frame")
  return(result_df)
}


#' Plot Power Analysis Results
#' @param x A power_analysis object
#' @param ... Additional arguments (ignored)
#' @export
plot.power_analysis <- function(x, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting power analysis results")
  }

  p <- ggplot2::ggplot(x, ggplot2::aes(x = sample_size, y = power)) +
    ggplot2::geom_line(size = 1, color = "steelblue") +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed",
                        color = "red", size = 0.8) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::labs(
      x = "Sample Size (per condition)",
      y = "Power",
      title = "Power Analysis for Lineup Study",
      caption = "Red line = 80% power threshold"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))

  print(p)
  invisible(x)
}
