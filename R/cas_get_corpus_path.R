#' Get path to folder where the corpus is stored.
#'
#' @inheritParams cas_write_corpus
#' @inheritParams rlang::args_dots_used
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' cas_get_corpus_path()
#' }
cas_get_corpus_path <- function(...,
                                corpus_folder = "corpus",
                                file_format = "parquet",
                                partition = NULL,
                                token = "full_text") {
  rlang::check_dots_used()

  if (is.null(partition)) {
    partition <- "single_file"
  }

  fs::path(
    cas_get_base_folder(
      ...,
      create_if_missing = FALSE
    ),
    corpus_folder,
    file_format %>%
      stringr::str_replace(
        pattern = stringr::fixed("."),
        replacement = "_"
      ),
    token,
    partition,
    fs::path_sanitize(Sys.time() %>% 
                        as.character(digits = 0L),
      replacement = "_"
    ) %>%
      stringr::str_replace(
        pattern = " ",
        replacement = "-"
      )
  )
}
