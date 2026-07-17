#' Generic function for removing lines from a database table
#'
#' @param df A data frame. Must correspond with the type of data expected for
#'   each table.
#' @param table Name of the table. See readme for details.
#' @param overwrite Logical, defaults to `FALSE`. If `TRUE`, checks if matching data
#'   are previously held in the table and overwrites them. This should be used
#'   with caution, as it may overwrite completely the selected table.
#'
#' @family database functions
#'
#' @inheritParams cas_connect_to_db
#' @inheritParams cas_disconnect_from_db
#'
#' @return If successful, returns invisibly the same data frame provided as
#'   input and written to the database. Returns silently `NULL`, if nothing is
#'   added, e.g. because `use_db` is set to `FALSE`.
#' @export
#'
#' @examples
#'
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "castarter_data"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#'
#' urls_df <- cas_build_urls(
#'   url = "https://www.example.com/news/",
#'   start_page = 1,
#'   end_page = 10
#' )
#'
#' cas_write_to_db(
#'   df = urls_df,
#'   table = "index_id"
#' )
#'
#' cas_delete_from_db(id = 3:6, table = "index_id", ask = FALSE)
cas_delete_from_db <- function(
  id,
  table,
  ask = TRUE,
  db_connection = NULL,
  disconnect_db = FALSE,
  ...
) {
  if (cas_check_use_db(...) == FALSE) {
    return(invisible(NULL))
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  table_id <- DBI::dbQuoteIdentifier(conn = db, x = table)

  if (!DBI::dbExistsTable(conn = db, name = table_id)) {
    cli::cli_alert_warning(
      "Table {.var {table}} does not exist in the database."
    )
  }

  id_to_remove <- id

  to_remove_df <- DBI::dbReadTable(conn = db, name = table_id) |>
    dplyr::filter(id %in% id_to_remove)

  cli::cli_alert_danger(
    "The following {nrow(to_remove_df)} rows are about to be removed from the {.var {table}} table."
  )

  if (!ask) {
    proceed <- TRUE
  } else {
    proceed <- utils::askYesNo("Do you want to proceed?")
  }

  if (!isTRUE(proceed)) {
    cli::cli_abort(
      "The processed has been interrupted, no rows have been removed."
    )
  }

  delete_query <- stringr::str_c("DELETE FROM ", table_id, " WHERE id = ?")

  rows_removed <- DBI::dbExecute(db, delete_query, params = list(id_to_remove))

  cli::cli_alert_success(
    text = "{rows_removed} rows have been removed from the table {.var {table}}."
  )

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )
  invisible(df)
}
