#' Delete rows from selected database table
#'
#' @param table Name of the table from where rows should be deleted.
#' @param id Defaults to NULL. A vector of id. Rows with the given id will be
#'   removed from the database.
#' @param batch  Defaults to NULL. A vector of batch identifiers. Rows with the
#'   given batch id will be removed from the database.
#' @param index_group  Defaults to NULL. A vector of "index_group" names. Rows
#'   with the given "index_group" will be removed from the database.
#' @param ask Defaults to TRUE. If TRUE, it runs a query checking how many rows
#'   would be deleted, and actually deletes them only after confirming.
#' @inheritParams cas_read_from_db
#'
#' @return Nothing, used for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive) {
#'   cas_delete_from_db(table = "contents_data", id = id_to_delete)
#' }
#' }
cas_delete_from_db <- function(
  table,
  id = NULL,
  batch = NULL,
  index_group = NULL,
  ask = TRUE,
  db_folder = NULL,
  db_connection = NULL,
  disconnect_db = FALSE,
  ...
) {
  if (cas_check_use_db(...) == FALSE) {
    cli::cli_abort(
      "Database not set. Set the database connection with `cas_set_options()` or pass database connection with the parameter `db_connection`."
    )
  }

  if (!is.null(index_group) & (!is.null(id) | !is.null(batch))) {
    cli::cli_abort(
      "Only removal of whole index groups supported at this point."
    )
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = FALSE,
    ...
  )

  if (DBI::dbExistsTable(conn = db, name = table) == FALSE) {
    cli::cli_abort(message = "Table {.var {table}} does not exist.")
  }

  if (!is.null(id) & !is.null(batch)) {
    if (length(batch) > 1 & length(id) > 1) {
      cli::cli_abort(
        message = "When providing both {.var id} and {.var batch} only one of them can be a vector of length higher than 1."
      )
    }
    if (ask) {
      rows_to_be_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "SELECT id, batch FROM {`table`} WHERE (id = {id}) AND (batch = {batch})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbGetQuery(
            conn = db,
            statement = current_statement
          ) %>%
            dplyr::collect()

          nrow(result)
        }
      )

      confirm <- usethis::ui_yeah(
        x = glue::glue(
          "A total of {scales::number(sum(rows_to_be_deleted_v))} rows is going to be removed from the table {sQuote(table)}. Do you confirm?"
        )
      )
    } else {
      confirm <- TRUE
    }

    if (isTRUE(confirm)) {
      rows_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "DELETE id, batch FROM {`table`} WHERE (id = {id}) AND (batch = {batch})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbExecute(
            conn = db,
            statement = current_statement
          )
          result
        }
      )
    } else {
      return(invisible(NULL))
    }
  } else if (is.null(id) == FALSE) {
    if (ask == TRUE) {
      rows_to_be_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "SELECT id FROM {`table`} WHERE (id = {id})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbGetQuery(
            conn = db,
            statement = current_statement
          ) %>%
            dplyr::collect()

          nrow(result)
        }
      )
      confirm <- usethis::ui_yeah(
        x = glue::glue(
          "A total of {scales::number(sum(rows_to_be_deleted_v))} rows is going to be removed from the table {sQuote(table)}. Do you confirm?"
        )
      )
    } else {
      confirm <- TRUE
    }

    if (isTRUE(confirm)) {
      rows_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "DELETE FROM {`table`} WHERE (id = {id})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbExecute(
            conn = db,
            statement = current_statement
          )

          result
        }
      )
    } else {
      return(invisible(NULL))
    }
  } else if (is.null(batch) == FALSE) {
    if (ask == TRUE) {
      rows_to_be_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "SELECT batch FROM {`table`} WHERE (batch = {batch})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbGetQuery(
            conn = db,
            statement = current_statement
          ) %>%
            dplyr::collect()

          nrow(result)
        }
      )
      confirm <- usethis::ui_yeah(
        x = glue::glue(
          "A total of {scales::number(sum(rows_to_be_deleted_v))} rows is going to be removed from the table {sQuote(table)}. Do you confirm?"
        )
      )
    } else {
      confirm <- TRUE
    }

    if (isTRUE(confirm)) {
      rows_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "DELETE FROM {`table`} WHERE (batch = {batch})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbExecute(
            conn = db,
            statement = current_statement
          )

          result
        }
      )
    } else {
      return(invisible(NULL))
    }
  } else if (is.null(index_group) == FALSE) {
    if (ask == TRUE) {
      rows_to_be_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "SELECT index_group FROM {`table`} WHERE (index_group = {index_group})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbGetQuery(
            conn = db,
            statement = current_statement
          ) %>%
            dplyr::collect()

          nrow(result)
        }
      )
      confirm <- usethis::ui_yeah(
        x = glue::glue(
          "A total of {scales::number(sum(rows_to_be_deleted_v))} rows is going to be removed from the table {sQuote(table)}. Do you confirm?"
        )
      )
    } else {
      confirm <- TRUE
    }

    if (isTRUE(confirm)) {
      rows_deleted_v <- purrr::map_dbl(
        .progress = TRUE,
        .x = glue::glue_sql(
          "DELETE FROM {`table`} WHERE (index_group = {index_group})",
          .con = db
        ),
        .f = function(current_statement) {
          result <- DBI::dbExecute(
            conn = db,
            statement = current_statement
          )

          result
        }
      )
    } else {
      return(invisible(NULL))
    }
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )

  cli::cli_inform(
    message = c(
      i = "{sum(rows_deleted_v)} rows have been deleted from the table {.var {table}}"
    )
  )
}
