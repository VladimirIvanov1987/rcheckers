#' Add Checkers Resources
#' @keywords internal
#' @noRd
add_rcheckers_resources <- function() {
  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = system.file("rcheckers/css/checkers.css", package = "your_package_name")
      )
    )
  )
}
