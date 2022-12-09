#' Read contents data from local database
#'
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_read_db_contents_data <- function(db_connection = NULL,
                                      db_folder = NULL,
                                      ...) {
  db_result <- tryCatch(cas_read_from_db(
    table = "contents_data",
    db_folder = db_folder,
    db_connection = db_connection,
    ...
  ),
  error = function(e) {
    logical(1L)
  }
  )

  if (isFALSE(db_result)) {
    # TODO
  } else if (is.data.frame(db_result)) {
    if (nrow(db_result) == 0) {
      # TODO
    } else {
      db_result
    }
  }
}
