#' Generate basic metadata about the corpus, including start and end date and
#' total number of items available.
#'
#' @inheritParams cas_read_corpus
#'
#' @return A list.
#' @export
#'
#' @examples
cas_generate_metadata <- function(corpus = NULL,
                                  db_connection = NULL,
                                  db_folder = NULL,
                                  ...) {
  corpus_df <- cas_check_read_db_contents_data(
    corpus = corpus,
    collect = FALSE,
    db_connection = db_connection,
    db_folder = db_folder,
    ...
  )

  metadata_l <- list()

  corpus_distinct_date_df <- corpus_df %>%
    dplyr::distinct(date) %>%
    dplyr::collect()

  metadata_l$start_date <- corpus_distinct_date_df %>%
    dplyr::slice_min(date,
      n = 1,
      with_ties = FALSE
    ) %>%
    dplyr::pull(date)

  metadata_l$end_date <- corpus_distinct_date_df %>%
    dplyr::slice_max(date,
      n = 1,
      with_ties = FALSE
    ) %>%
    dplyr::pull(date)

  metadata_l$total_items <- corpus_df %>%
    dplyr::distinct(id) %>%
    dplyr::tally(name = "n") %>%
    dplyr::collect() %>% 
    dplyr::pull(n)

  metadata_l$columns <- colnames(corpus_df)

  metadata_l
}
