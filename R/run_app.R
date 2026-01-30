#' Run the r4lineups Shiny app
#'
#' Launches the bundled Shiny app for interactive use of key r4lineups
#' functionality (lineup bias, effective size, face similarity, ROC/CAC).
#'
#' @return None. Launches a Shiny app.
#' @examples
#' \dontrun{
#' run_r4lineups_app()
#' }
#' @export
run_r4lineups_app <- function() {
  app_dir <- system.file("shiny/r4lineups_app", package = "r4lineups")
  if (app_dir == "") {
    stop("Shiny app directory not found. Was the package installed with inst/shiny?", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
