#' Read datasets created with `cas_write_dataset`
#'
#' @inheritParams cas_write_corpus
#'
#' @return A dataset as `ArrowObject`
#' @export
#'
#' @examples
#' \dontrun{
#' cas_read_corpus()
#' }
cas_read_corpus <- function(path = NULL,
                            file_format = "parquet",
                            partition = NULL,
                            token = "full_text",
                            corpus_folder = "corpus",
                            ...) {
  if (is.null(path) == TRUE) {
    path <- cas_get_corpus_path(
      corpus_folder = corpus_folder,
      file_format = file_format,
      partition = partition,
      token = token,
      ...
    ) %>%
      fs::path_dir() %>%
      fs::dir_ls(type = "directory") %>%
      tail(1)
  }

  arrow::open_dataset(sources = path)
}
