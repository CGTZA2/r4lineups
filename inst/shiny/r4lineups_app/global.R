library(shiny)
library(shinyWidgets)
library(ggplot2)
library(DT)
library(r4lineups)

parse_numeric_vector <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(numeric(0))
  }
  vals <- unlist(regmatches(x, gregexpr("-?\\d+\\.?\\d*", x)))
  as.numeric(vals)
}

parse_bins <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(NULL)
  }
  vals <- parse_numeric_vector(x)
  if (length(vals) < 2) {
    return(NULL)
  }
  sort(unique(vals))
}

save_plot <- function(p, filename) {
  ggplot2::ggsave(filename, plot = p, width = 7, height = 5, dpi = 300)
}
