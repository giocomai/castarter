#' Convert database type, e.g. from DuckDB to SQLite
#'
#' @param source_db_type A database type, such as "DuckDB" or "SQLite". Must be declared explicitly.
#' @param destination_db_type A database type, such as "DuckDB" or "SQLite". Must be declared explicitly.
#'
#' @return
#' @export
#'
#' @examples
cas_convert_db_type <- function(source_db_type,
                                destination_db_type,
                                ...) {
  if (fs::file_exists(cas_get_db_file(db_type = source_db_type)) == FALSE) {
    cli::cli_abort(c("Source database does not exist or cannot be found.",
      "x" = "Expected location of the database is {cas_get_db_file(db_type = source_db_type)}",
      "i" = "Make sure that website and project have been set correctly with {.fun cas_set_options}.",
      "i" = "Also check that the {.code source_db_type} has been set correctly."
    ))
  }

  source_db <- cas_connect_to_db(
    db_type = source_db_type,
    read_only = TRUE
  )

  destination_db <- cas_connect_to_db(
    db_type = destination_db_type,
    read_only = FALSE
  )

  purrr::walk(
    .x = DBI::dbListTables(conn = source_db),
    .f = function(current_table_name) {
      DBI::dbReadTable(
        conn = source_db,
        name = current_table_name
      ) %>%
        DBI::dbWriteTable(
          conn = destination_db,
          name = current_table_name
        )
    }
  )


  cas_disconnect_from_db(
    db_connection = source_db,
    ...
  )

  cas_disconnect_from_db(
    db_connection = destination_db,
    ...
  )
}
