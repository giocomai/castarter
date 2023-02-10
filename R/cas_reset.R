#' Delete a specific table from database
#'
#' @param table Name of the table. Yuu can use
#'   `DBI::dbListTables(cas_connect_to_db())` to see currently available tables.
#'   See `vignette("castarter-database")` for more information about the
#'   contents and structure of each table.
#' @param ask Logical, defaults to TRUE. If set to FALSE, the relevant table
#'   will be deleted without asking for confirmation from the user.
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_db <- function(table,
                         db_connection = NULL,
                         db_folder = NULL,
                         ask = TRUE,
                         ...) {
  if (cas_check_use_db(...) == FALSE) {
    usethis::ui_stop("Database not set. Set the database connection with `cas_set_options()` or pass database connection with the parameter `db_connection`.")
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = FALSE,
    ...
  )

  if (DBI::dbExistsTable(conn = db, name = table) == FALSE) {
    # do nothing: if table does not exist, previous data cannot be there
  } else {
    if (isFALSE(ask)) {
      DBI::dbRemoveTable(conn = db, name = table)
      cli::cli_alert_success("The table {.field {sQuote(table)}} has been removed from the local database")
    } else if (usethis::ui_yeah(x = paste0("Are you sure you want to remove from the local database the following table: ", sQuote(table), "?"))) {
      DBI::dbRemoveTable(conn = db, name = table)
      cli::cli_alert_success("The table {.field {sQuote(table)}} has been removed from the local database")
    }
  }

  cas_disconnect_from_db(
    db_connection = db,
    ...
  )
}

#' Removes from the local database the folder where extracted data are stored
#'
#' @inheritParams cas_reset_db
#'
#' @return
#' @export
#'
#' @examples
cas_reset_db_contents_data <- function(db_connection = NULL,
                                       db_folder = NULL,
                                       ask = TRUE,
                                       ...) {
  cas_reset_db(
    table = "contents_data",
    db_connection = db_connection,
    db_folder = db_folder,
    ask = ask,
    ...
  )
}
