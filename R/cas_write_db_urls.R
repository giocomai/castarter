#' Write index or contents urls directly to the local database
#'
#' @inheritParams cas_write_db_contents_id
#' @inheritParams cas_download
#'
#' @return
#' @export
#'
#' @examples
cas_write_db_urls <- function(urls,
                              index = FALSE,
                              overwrite = FALSE,
                              db_connection = NULL,
                              disconnect_db = FALSE,
                              quiet = FALSE,
                              check_previous = TRUE,
                              ...) {
  if (index == TRUE) {
    cas_write_db_index(
      urls = urls,
      overwrite = overwrite,
      db_connection = db_connection,
      disconnect_db = disconnect_db,
      ...
    )
  } else if (index == FALSE) {
    cas_write_db_contents_id(
      urls = urls,
      overwrite = overwrite,
      db_connection = db_connection,
      disconnect_db = disconnect_db,
      quiet = quiet,
      check_previous = check_previous,
      ...
    )
  } else {
    cli::cli_abort("{.var index} must be either TRUE or FALSE.")
  }
}

#' Read urls stored in the local database
#'
#' @inheritParams cas_read_db_contents_id
#' @inheritParams cas_read_db_index
#'
#' @return
#' @export
#'
#' @examples
cas_read_db_urls <- function(index = FALSE,
                             db_connection = NULL,
                             db_folder = NULL,
                             index_group = NULL,
                             ...) {
  if (index == TRUE) {
    cas_read_db_index(
      db_connection = db_connection,
      db_folder = db_folder,
      index_group = index_group,
      ...
    )
  } else if (index == FALSE) {
    cas_read_db_contents_id(
      db_connection = db_connection,
      db_folder = db_folder,
      ...
    )
  } else {
    cli::cli_abort("{.var index} must be either TRUE or FALSE.")
  }
}
