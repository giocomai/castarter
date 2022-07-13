#' Write index URLs to local database
#' 
#' If some URLs are already included in the database, it appends only the new ones: URLs are expected to be unique.
#'
#' @param url A data frame with three columns, such as \code{casdb_empty_index_id}, or a character vector.
#'
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_write_index <- function(url,
                            use_db = NULL,
                            overwrite = FALSE,
                            db_connection = NULL,
                            disconnect_db = TRUE) {
  if (is.data.frame(url)) {
    if (identical(colnames(df), colnames(casdb_empty_index_id)) & identical(sapply(df, class), sapply(casdb_empty_index_id, class))) {
      url_df <- url
    } else {
      usethis::ui_stop("Url data frame must match exactly the column names and types of {usethis::ui_code('casdb_empty_index_id')}")
    }
  } else {
    url_df <- cas_build_urls(url_beginning = url,
                             url_ending = "",
                             start_page = NULL,
                             end_page = NULL)
  }
  
  cas_write_to_db(url_df,
                  table = "index_id",
                  use_db = use_db,
                  overwrite = overwrite,
                  db_connection = db_connection,
                  disconnect_db = disconnect_db)
  
  url_df
}

#' Read index from local database
#'
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_read_index <- function(use_db = NULL,
                           db_connection = NULL,
                           disconnect_db = TRUE) {
  
  cas_read_from_db(table = "index_id",
                   use_db = use_db,
                   db_connection = db_connection,
                   disconnect_db = disconnect_db)
}
