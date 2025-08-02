#' Launch Domain Expertise Collector `Shiny` app to collect prior information
#'
#' Run this function to start the Collector `Shiny` app.
#'
#' @returns The `collect` function is used for the side effect of starting
#' the Domain Expertise Collector `Shiny` app
#' @export
#'
#' @examples
#' if (interactive()) {
#'   collect()
#' }
collect <- function() {
  app_dir <- system.file("shiny", "collect", package = "epicprior")
  if (app_dir == "") {
    stop("Could not find directory. Try re-installing `epicprior`.")
  }

  shiny::shinyAppDir(app_dir, options = list(launch.browser = TRUE))
}
