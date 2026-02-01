#' Bootstrap Distribution of Lineup Bias (Target Proportion)
#'
#' Computes a bootstrap distribution for the target-position proportion.
#'
#' @param lineup_input A numeric lineup vector or a lineup table.
#' @param target_pos Target position in the lineup (scalar).
#' @param k Nominal lineup size (suspect + fillers).
#' @param input_type Either "vector" or "table".
#' @param R Number of bootstrap resamples.
#'
#' @return A numeric vector of bootstrap estimates.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 500)
lineup_bias_boot_dist <- function(lineup_input,
                                  target_pos,
                                  k,
                                  input_type = c("vector", "table"),
                                  R = 1000) {
  input_type <- match.arg(input_type)
  if (length(target_pos) != 1 || !is.finite(target_pos)) {
    stop("target_pos must be a finite scalar.", call. = FALSE)
  }
  if (length(k) != 1 || !is.finite(k) || k < 2) {
    stop("k must be a finite scalar >= 2.", call. = FALSE)
  }
  k <- as.integer(k)
  target_pos <- as.integer(target_pos)
  if (target_pos < 1 || target_pos > k) {
    stop("target_pos must be between 1 and k.", call. = FALSE)
  }

  vec <- msdt_make_lineup_vector(lineup_input, k, input_type)
  if (length(vec) < 2) {
    stop("Need at least two selections for bootstrap.", call. = FALSE)
  }
  boot::boot(vec, lineup_prop_boot, target_pos = target_pos, R = R)$t
}

#' Plot Bootstrap Distribution of Lineup Bias
#'
#' @param boot_values Numeric vector of bootstrap estimates.
#' @param target_pos Optional target position label for title.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- lineup_bias_boot_dist(vec, target_pos = 3, k = 6, R = 500)
#' plot_lineup_bias_distribution(dist, target_pos = 3)
plot_lineup_bias_distribution <- function(boot_values, target_pos = NULL) {
  df <- data.frame(value = boot_values)
  title <- "Bootstrap distribution of target proportion"
  if (!is.null(target_pos)) {
    title <- paste0(title, " (target position ", target_pos, ")")
  }
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)), bins = 30,
                             fill = "#72b7b2", alpha = 0.6) +
    ggplot2::geom_density(color = "#1f2d3d", linewidth = 1) +
    ggplot2::labs(x = "Target proportion", y = "Density", title = title) +
    ggplot2::theme_minimal(base_size = 12)
}

#' Bootstrap Distribution of Effective Size
#'
#' Computes a bootstrap distribution for effective size using either
#' Tredoux's E' or Malpass's adjusted E.
#'
#' @param lineup_input A numeric lineup vector or a lineup table.
#' @param k Nominal lineup size (suspect + fillers).
#' @param metric Either "tredoux" or "malpass".
#' @param input_type Either "vector" or "table".
#' @param R Number of bootstrap resamples.
#'
#' @return A numeric vector of bootstrap estimates.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 500)
esize_boot_dist <- function(lineup_input,
                            k,
                            metric = c("tredoux", "malpass"),
                            input_type = c("vector", "table"),
                            R = 1000) {
  metric <- match.arg(metric)
  input_type <- match.arg(input_type)
  if (length(k) != 1 || !is.finite(k) || k < 2) {
    stop("k must be a finite scalar >= 2.", call. = FALSE)
  }
  k <- as.integer(k)

  vec <- msdt_make_lineup_vector(lineup_input, k, input_type)
  if (length(vec) < 2) {
    stop("Need at least two selections for bootstrap.", call. = FALSE)
  }

  if (metric == "tredoux") {
    boot_fun <- function(lineup_vec, d) {
      boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
      esize_T(boot_tab)
    }
  } else {
    boot_fun <- function(lineup_vec, d) {
      boot_tab <- table(factor(lineup_vec[d], levels = seq_len(k)))
      esize_m(boot_tab, k)
    }
  }

  boot::boot(vec, boot_fun, R = R)$t
}

#' Plot Bootstrap Distribution of Effective Size
#'
#' @param boot_values Numeric vector of bootstrap estimates.
#' @param metric Label for the effective size statistic.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' vec <- round(runif(200, 1, 6))
#' dist <- esize_boot_dist(vec, k = 6, metric = "tredoux", R = 500)
#' plot_esize_distribution(dist, metric = "E (Tredoux, 1998)")
plot_esize_distribution <- function(boot_values, metric = "Effective size") {
  df <- data.frame(value = boot_values)
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(aes(y = after_stat(density)), bins = 30,
                             fill = "#72b7b2", alpha = 0.6) +
    ggplot2::geom_density(color = "#1f2d3d", linewidth = 1) +
    ggplot2::labs(x = metric, y = "Density",
                  title = "Bootstrap distribution of effective size") +
    ggplot2::theme_minimal(base_size = 12)
}

msdt_make_lineup_vector <- function(lineup_input, k, input_type) {
  if (input_type == "vector") {
    if (is.data.frame(lineup_input) || is.matrix(lineup_input)) {
      vec <- lineup_input[[1]]
    } else {
      vec <- lineup_input
    }
    if (!is.numeric(vec)) {
      vec <- as.numeric(vec)
    }
    vec <- vec[!is.na(vec)]
    if (any(vec %% 1 != 0)) {
      stop("Lineup vector must use integer positions.", call. = FALSE)
    }
    if (any(vec < 1 | vec > k)) {
      stop("Lineup vector values must be between 1 and k.", call. = FALSE)
    }
    return(as.integer(vec))
  }
  tab <- as.numeric(lineup_input)
  if (length(tab) != k) {
    stop("Lineup table length must equal k.", call. = FALSE)
  }
  if (any(tab < 0)) {
    stop("Lineup table cannot include negative counts.", call. = FALSE)
  }
  rep(seq_len(k), tab)
}
