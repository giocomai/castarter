#' Read datasets created with `cas_write_dataset`
#'
#' @param update Logical, defaults to FALSE. If FALSE, just checks if relevant corpus has been previously stored. If TRUE, it checks if more recent contents are available in the local database.
#' @inheritParams cas_write_corpus
#'
#' @return A dataset as `ArrowObject`
#' @export
#'
#' @examples
#' \dontrun{
#' cas_read_corpus()
#' }
cas_read_corpus <- function(update = FALSE,
                            path = NULL,
                            file_format = "parquet",
                            partition = NULL,
                            token = "full_text",
                            corpus_folder = "corpus",
                            ...) {
  path <- cas_check_corpus(
    update = update,
    path = path,
    file_format = file_format,
    partition = partition,
    token = token,
    corpus_folder = corpus_folder,
    ...
  )

  arrow::open_dataset(sources = path)
}
