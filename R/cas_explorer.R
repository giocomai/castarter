#' Run the Shiny Application
#'
#' @param collect Defaults to FALSE. If TRUE, retrieves the corpus in memory,
#'   even if is originally read from a parquet file or a database. With
#'   `arrow` version before 14 that do not have full support of using stringr
#'   in context, setting this to TRUE is probably advisable (currently,
#'   depending on the arrow version, there may be issues where upper/lower case 
#'   is not ignored).
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
cas_explorer <- function(corpus = castarter::cas_demo_corpus,
                         default_pattern = NULL,
                         title = "castarter",
                         collect = FALSE,
                         advanced = FALSE,
                         custom_head_html = '<meta name="referrer" content="no-referrer" />',
                         onStart = NULL,
                         options = list(),
                         enableBookmarking = NULL,
                         uiPattern = "/",
                         ...) {
  if ("date" %in% colnames(corpus)) {
    corpus <- corpus %>%
      dplyr::filter(is.na(date) == FALSE)
  }

  with_golem_options(
    app = shinyApp(
      ui = cass_explorer_app_ui,
      server = cass_explorer_app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      corpus = corpus,
      default_pattern = default_pattern,
      title = title,
      collect = collect,
      advanced = advanced,
      custom_head_html = custom_head_html
    )
  )
}
