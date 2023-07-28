#' Checks if given corpus exists, and, optionally updates it
#'
#'
#' @param update Logical, defaults to FALSE. If set to TRUE, it checks if the
#'   local database has contents with a higher content id than is currently
#'   available in previously exported corpus, if any. If so, it writes a new,
#'   updated corpus.
#' @param keep_only_latest Logical, defaults to FALSE. If set to TRUE, it
#'   deletes previous, older, corpora of the same type.
#' @return Path to corpus. NULL, if no corpus is found and update is set to
#'   FALSE.
#' @inheritParams cas_write_corpus
#' @inheritParams rlang::args_dots_used
#'
#' @export
#'
#' @examples
cas_check_corpus <- function(...,
                             update = FALSE,
                             keep_only_latest = FALSE,
                             path = NULL,
                             file_format = "parquet",
                             partition = NULL,
                             token = "full_text",
                             corpus_folder = "corpus") {
  rlang::check_dots_used()

  if (is.null(path) == TRUE) {
    base_path <- cas_get_corpus_path(
      ...,
      corpus_folder = corpus_folder,
      file_format = file_format,
      partition = partition,
      token = token
    ) %>%
      fs::path_dir()

    if (fs::dir_exists(base_path) == FALSE) {
      cli::cli_inform(message = c(
        x = "No relevant corpus has yet been created; folder {.path {base_path}} does not exist."
      ))
      if (update == TRUE) {
        cli::cli_inform(message = c(
          i = "A corpus will now be created from database."
        ))
      } else if (update == FALSE) {
        cli::cli_inform(message = c(
          i = "You can store a corpus with {.fun cas_write_corpus}"
        ))
      }
      path <- NULL
    } else {
      path <- base_path %>%
        fs::dir_ls(type = "directory") %>%
        tail(1)
    }
  }

  if (update == FALSE) {
    return(path)
  }

  if (is.null(path) == TRUE) {
    # when a previous corpus does not exist
    path <- cas_write_corpus(
      corpus_folder = corpus_folder,
      file_format = file_format,
      partition = partition,
      token = token,
      ...
    )
  } else {
    # when a previous corpus exists
    corpus_a <- arrow::open_dataset(sources = path)

    max_db_id <- cas_read_db_contents_data(...) %>%
      dplyr::distinct(id) %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      dplyr::slice_max(order_by = id, n = 1, with_ties = FALSE) %>%
      dplyr::pull(id)

    max_corpus_id <- corpus_a %>%
      dplyr::distinct(id) %>%
      dplyr::mutate(id = as.numeric(id)) %>%
      dplyr::slice_max(order_by = id, n = 1, with_ties = FALSE) %>%
      dplyr::pull(id)

    if (max_corpus_id < max_db_id) {
      path <- cas_write_corpus(
        corpus_folder = corpus_folder,
        file_format = file_format,
        partition = partition,
        token = token,
        ...
      )
    }

    if (keep_only_latest) {
      available_folders <- fs::dir_ls(path %>%
        fs::path_dir())

      if (length(available_folders) > 1) {
        fs::dir_delete(path = available_folders[-length(available_folders)])
      }
    }
  }

  path
}
