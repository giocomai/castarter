#' Get path to folder where the corpus is stored.
#'
#' @inheritParams cas_write_corpus
#'
#' @return
#' @export
#'
#' @examples
cas_get_corpus_path <- function(corpus_folder = "corpus",
                                file_format = "parquet",
                                partition = NULL,
                                token = "full_text",
                                ...) {
  if (is.null(partition)) {
    partition <- "single_file"
  }

  fs::path(
    cas_get_base_path(
      create_if_missing = FALSE,
      ...
    ) %>%
      fs::path_dir(),
    corpus_folder,
    file_format %>%
      stringr::str_replace(
        pattern = stringr::fixed("."),
        replacement = "_"
      ),
    token,
    partition,
    fs::path_sanitize(Sys.time(),
      replacement = "_"
    ) %>%
      stringr::str_replace(
        pattern = " ",
        replacement = "-"
      )
  )
}
