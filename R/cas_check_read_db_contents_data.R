#' Returns a corpus from the `contents_data` table in the database; if corpus is give, it just returns that instead.
#' 
#' Mostly used internally
#'
#' @param collect Logical, defaults to FALSE. If TRUE, it always returns a data
#'   frame and not a database connection, no matter the input.
#' @inheritParams cas_read_corpus
#'
#' @return
#' @export
#'
#' @examples
cas_check_read_db_contents_data <- function(corpus = NULL,
                                            collect = FALSE,
                                            db_connection = NULL,
                                            db_folder = NULL,
                                            ...) {
  
  if (is.null(corpus)) {
    if (cas_check_use_db(...) == FALSE) {
      cli::cli_abort(c(x = "Database not set.", 
                     i = "Set the database connection with {.fun cas_set_options} or pass database connection with the argument {.arg db_connection}."))
    }
    
    db <- cas_connect_to_db(
      db_connection = db_connection,
      read_only = TRUE,
      ...
    )
    corpus_df <- cas_read_db_contents_data(
      db_connection = db,
      db_folder = db_folder,
      ...
    )
  } else {
    corpus_df <- corpus
  }
  
  if (collect == TRUE) {
    corpus_df %>% 
      dplyr::collect()
  } else {
    corpus_df
  }
}