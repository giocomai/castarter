#' Ignore a set of ids from the download or processing step
#'
#' There are two main use cases for this function:
#' - a number of the files downloaded turned out to be irrelevant. Rather than delete any trace about them, it may be preferrable to just ignore them, so they are not processed when extracting data.
#' - urls originally included for download, but not yet downloaded, should be ignored and not downloaded. This may or may not be a temporary arrangement, but it is considered useful to keep the urls in the database.
#'
#' @param id Defaults to NULL. A vector of id. Rows with the given id will be
#'   added to the ignore table.
#'
#' @return
#' @export
#'
#' @examples
cas_ignore_id <- function(id,
                          db_folder = NULL,
                          db_connection = NULL,
                          disconnect_db = FALSE,
                          ...) {
  if (NROW(id) == 0) {
    return(invisible(NULL))
  }

  if (is.data.frame(id)) {
    ignore_df <- tibble::tibble(id = as.numeric(id[[1]]))
  } else {
    ignore_df <- tibble::tibble(id = as.numeric(id))
  }

  if (sum(is.na(ignore_df[["id"]])) > 0) {
    cli::cli_abort("Invalid values passed to {.arg id}. {.arg id} must be a numeric vector or a data frame with a single column with numeric identifiers.")
  }

  if (cas_check_use_db(...) == FALSE) {
    cli::cli_abort("Database not set. Set the database connection with `cas_set_options()` or pass database connection with the parameter `db_connection`.")
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = FALSE,
    ...
  )

  table <- "contents_ignore"

  if (DBI::dbExistsTable(conn = db, name = table) == FALSE) {
    previous_df <- tibble::tibble(id = double())
  } else {
    previous_df <- cas_read_from_db(
      table = table,
      db_connection = db_connection,
      disconnect_db = FALSE
    ) |>
      dplyr::collect() |>
      tibble::as_tibble()
  }

  new_df <- ignore_df |>
    dplyr::anti_join(
      y = previous_df,
      by = "id"
    )

  if (nrow(new_df) > 0) {
    DBI::dbWriteTable(
      conn = db,
      name = table,
      value = new_df,
      append = TRUE
    )
    cli::cli_inform(c(v = "{nrow(new_df)} identifiers added to ignore table."))
  } else {
    cli::cli_inform(c(v = "All given identifiers already incldued in ignore table."))
  }
}
