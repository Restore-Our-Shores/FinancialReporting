#' Run the financial reporting application
#' @export
run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL, ...) {
  base::options(shiny.maxRequestSize = 100 * 1024^2)
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui, server = app_server, onStart = onStart,
      options = options, enableBookmarking = enableBookmarking
    ),
    golem_opts = list(...)
  )
}

app_sys <- function(...) system.file(..., package = "financialreports", mustWork = TRUE)
