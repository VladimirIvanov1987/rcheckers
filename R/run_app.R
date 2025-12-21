#' Run the Russian Checkers Game
#'
#' Launches the Shiny application for Russian Checkers.
#'
#' @return No return value, called for side effects.
#' @export
#' @import shiny
run_app <- function() {
  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "united"),
    title = "Russian Checkers",

    # Подключаем наш модуль
    rcheckers_ui("game1")
  )

  server <- function(input, output, session) {
    rcheckers_server("game1")
  }

  shiny::shinyApp(ui, server)
}
