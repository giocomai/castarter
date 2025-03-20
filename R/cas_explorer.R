#' Run a web interface allowing basic word frequency analysis
#'
#' @param corpus Defaults to NULL. If not given, by default the main table with
#'   extracted contents will tentatively be retrieved with
#'   `cas_read_db_contents_data()`. Consider that for improved speed and reduced
#'   memory requirements it may be more efficient to store locally the corpus
#'   (e.g. with `cas_write_corpus(partition = "year")`), and read it as input to
#'   this argument with `cas_read_corpus(partition = "year")`.
#' @param collect Defaults to FALSE. If TRUE, retrieves the corpus in memory,
#'   even if is originally read from a parquet file or a database. With
#'   `arrow` version before 14 that do not have full support of using stringr
#'   in context, setting this to TRUE is probably advisable (currently,
#'   depending on the arrow version, there may be issues where upper/lower case
#'   is not ignored).
#' @param custom_head_html Chunk of code to be included in the app's `<head>`.
#'   This can be used, e.g., for custom analytics snippets. The default value,
#'   `<meta name="referrer" content="no-referrer" />` asks the browser not to
#'   include the source website when following links to external websites.
#' @param ... arguments to pass to golem_opts. See `?golem::get_golem_options`
#'   for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
cas_explorer <- function(
  corpus = NULL,
  default_pattern = NULL,
  title = "castarter",
  collect = FALSE,
  advanced = FALSE,
  custom_head_html = '<meta name="referrer" content="no-referrer" />',
  footer_html = shiny::tagList(),
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  if (is.null(corpus)) {
    corpus <- cas_check_read_db_contents_data() |>
      dplyr::collect()

    if (nrow(corpus) == 0) {
      cli::cli_abort(
        message = c(
          x = "No relevant contents could be retrieved from the local database.",
          i = "Pass a corpus to this function or make sure that the project settings are effectively set and that relevant files are at their expected location."
        )
      )
    }
  }

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
      custom_head_html = custom_head_html,
      footer_html = footer_html
    )
  )
}
