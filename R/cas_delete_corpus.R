#' Delete previously stored corpora written with `cas_write_corpus()`.
#'
#' Typically used for file maintainance, especially when datasets are routinely updated.
#'
#' @param keep Numeric, defaults to 1. Number of corpus files to keep. Only the most recent files are kept.
#' @inheritParams cas_write_corpus
#'
#' @return
#' @export
#'
#' @examples
cas_delete_corpus <- function(keep = 1,
                              ask = TRUE,
                              file_format = "parquet",
                              partition = "year",
                              token = "full_text",
                              corpus_folder = "corpus",
                              path = NULL,
                              ...) {
  cas_options_l <- cas_get_options(...)

  if (is.null(path) == TRUE) {
    path <- cas_get_corpus_path(
      corpus_folder = corpus_folder,
      file_format = file_format,
      partition = partition,
      token = token,
      ...
    ) %>%
      fs::path_dir()
  }

  if (fs::file_exists(path) == FALSE) {
    cli::cli_warn("The folder {.path {path}} does not exists. No corpus to remove.")
    return(invisible(NULL))
  }

  corpus_path_df <- tibble::tibble(path = fs::dir_ls(path))

  corpus_path_to_remove_df <- corpus_path_df %>%
    dplyr::slice_head(n = (nrow(corpus_path_df) - keep))

  corpus_path_to_keep_df <- corpus_path_df %>%
    dplyr::slice_tail(n = keep)

  if (nrow(corpus_path_to_remove_df) > 0) {
    paths_to_remove_l <- corpus_path_to_remove_df$path
    names(paths_to_remove_l) <- rep("x", length(paths_to_remove_l))

    cli::cli_inform(c(`!` = cli::cli_text("{length(paths_to_remove_l)} corpus file{?s} about to be removed:")))
    cli::cli_bullets(paths_to_remove_l)
  } else {
    cli::cli_inform(c(`i` = "No corpus files to be removed."))
    return(invisible(NULL))
  }

  if (nrow(corpus_path_to_keep_df) > 0) {
    paths_to_keep_l <- corpus_path_to_keep_df$path
    names(paths_to_keep_l) <- rep("*", length(paths_to_keep_l))

    cli::cli_inform(c(`i` = cli::cli_text("{length(paths_to_keep_l)} corpus file{?s} will be kept:")))
    cli::cli_bullets(paths_to_keep_l)
  }

  if (ask == FALSE) {
    confirmed <- TRUE
  } else {
    cli::cli_inform(cli::cli_text("A total of {length(paths_to_remove_l)} corpus file{?s} will be removed and {length(paths_to_keep_l)} will be kept."))
    confirmed <- usethis::ui_yeah(x = "Proceed?")
  }

  if (confirmed == TRUE) {
    fs::dir_delete(path = paths_to_remove_l)
    cli::cli_alert_success(text = "Done.")
  }
}
