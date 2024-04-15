#' Export the textual dataset for the current website
#'
#' @param corpus Defaults to NULL. If NULL, retrieves corpus from the current
#'   website with `cas_read_db_contents_data()`. If given, it is expected to be
#'   a corresponding data frame.
#' @param tif_compliant Defaults to FALSE. If TRUE, it ensures that the first
#'   column is a character vector named "doc_id" and that the second column is a
#'   character vector named "text". See \url{https://docs.ropensci.org/tif/} for
#'   details
#' @param text Unquoted text column, defaults to `text`. If `tif_compliant` is
#'   set to TRUE, it will be renamed to "text" even if originally it had a
#'   different name.
#' @param date Unquoted date column, defaults to `date`.
#' @param path Defaults to NULL. If NULL, path is set to the
#'   project/website/export/dataset/file_format folder.
#' @param file_format Defaults to "parquet". Currently, other options are not
#'   implemented.
#' @param partition Defaults to NULL. If NULL, the parquet file is not
#'   partitioned. "year" is a common alternative: if set to "year", the parquet
#'   file is partitioned by year. If a `year` column does not exist, it is
#'   created based on the assumption that a `date` column exists and it is (or
#'   can be coerced to) a vector of class `Date`.
#' @param token Defaults to "full_text", which does not tokenise the text
#'   column. If different from `full_text`, it is passed to
#'   `tidytext::unnest_tokens` (see its help for details). Accepted values
#'   include "words", "sentences", and "paragraphs". See
#'   `?tidytext::unnest_tokens()` for details.
#' @param to_lower Defaults to FALSE. Whether to convert tokens to lowercase.
#'   Passed to `tidytext` if token is not `full_text`.
#' @param drop_na Defaults to TRUE. If TRUE, items that have NA in their `text`
#'   or `date` columns are dropped. This is often useful, as in many cases these
#'   may have other issues and/or cause inconsistencies in further analyses.
#' @param drop_empty Defaults to TRUE. If TRUE, items that have empty elements
#'   ("") in their `text` or `date` columns are dropped. This is often useful,
#'   as in many cases these may have other issues and/or cause inconsistencies
#'   in further analyses.
#' @inheritParams cas_read_from_db
#'
#' @return
#' @export
#'
#' @examples
cas_write_corpus <- function(corpus = NULL,
                             to_lower = FALSE,
                             drop_na = TRUE,
                             drop_empty = TRUE,
                             date = date,
                             text = text,
                             tif_compliant = FALSE,
                             file_format = "parquet",
                             partition = NULL,
                             token = "full_text",
                             corpus_folder = "corpus",
                             path = NULL,
                             db_connection = NULL,
                             db_folder = NULL,
                             ...) {
  rlang::check_installed("arrow")

  corpus_df <- cas_check_read_db_contents_data(
    corpus = corpus,
    collect = TRUE,
    db_connection = NULL,
    db_folder = NULL,
    ...
  )

  if (drop_na == TRUE) {
    corpus_df <- corpus_df %>%
      dplyr::filter(is.na({{ text }}) == FALSE, is.na({{ date }}) == FALSE)
  }

  if (drop_empty == TRUE) {
    corpus_df <- corpus_df %>%
      dplyr::filter({{ text }} != "", {{ date }} != "")
  }

  cas_options_l <- cas_get_options(...)

  if (is.null(path) == TRUE) {
    path <- cas_get_corpus_path(
      corpus_folder = corpus_folder,
      file_format = file_format,
      partition = partition,
      token = token,
      ...
    )
  }

  if (fs::file_exists(path)) {
    cli::cli_abort("The folder {.path {path}} already exists. Please remove or rename it before writing corpus.")
  }

  fs::dir_create(path = path)

  if (tif_compliant == TRUE) {
    corpus_df <- corpus_df %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(doc_id = stringr::str_c(cas_options_l$website,
        id,
        sep = "-"
      )) %>%
      dplyr::ungroup() %>%
      dplyr::rename(text = {{ text }}) %>%
      dplyr::select("doc_id", "text", dplyr::everything())
  }

  if (token == "full_text") {
    # do nothing
  } else {
    corpus_df <- corpus_df %>%
      tidytext::unnest_tokens(
        output = "text",
        input = {{ text }},
        token = token,
        to_lower = to_lower
      )
  }

  if (is.null(partition)) {
    corpus_df %>%
      arrow::write_dataset(path = path)
  } else if (partition == "year") {
    corpus_df %>%
      dplyr::mutate({{ date }} := {{ date }} %>% lubridate::as_date()) %>%
      dplyr::mutate(year = lubridate::year({{ date }})) %>%
      dplyr::group_by(year) %>%
      arrow::write_dataset(path = path)
  }
  cli::cli_inform(message = c(i = "Corpus stored in {.path {path}}."))
  invisible(path)
}
