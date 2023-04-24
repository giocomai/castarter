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
    base_path <- cas_get_corpus_path(
      corpus_folder = corpus_folder,
      file_format = file_format,
      partition = partition,
      token = token,
      ...
    ) %>%
      fs::path_dir() 
    
    if (fs::dir_exists(base_path)==FALSE) {
      cli::cli_abort(message = c(x = "No relevant corpus has yet been created; folder {.path {base_path}} does not exist.",
                                 i = "You can store a corpus with {.code cas_write_corpus()}"))
    }
    
    path <- base_path %>%
      fs::dir_ls(type = "directory") %>%
      tail(1)
  }

  arrow::open_dataset(sources = path)
}
