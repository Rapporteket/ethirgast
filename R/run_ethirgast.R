#' Run the ETHIRGAST Shiny Application
#'
#' @return An object representing the ETHIRGAST app
#' @export
run_ethirgast <- function() {
  shiny::shinyApp(ui = appUi, server = appServer)
}
