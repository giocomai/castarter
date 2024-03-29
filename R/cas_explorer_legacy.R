#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
cas_explorer_legacy <- function(corpus = castarter::cas_demo_corpus,
                                default_string = NULL,
                                custom_head_html = '<meta name="referrer" content="no-referrer" />',
                                onStart = NULL,
                                options = list(),
                                enableBookmarking = NULL,
                                uiPattern = "/",
                                ...) {
  with_golem_options(
    app = shinyApp(
      ui = cass_explorer_legacy_app_ui,
      server = cass_explorer_legacy_app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      corpus = corpus,
      default_string = default_string,
      custom_head_html = custom_head_html
    )
  )
}
