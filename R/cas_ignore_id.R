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
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "cas_write_db_ignore_id"),
#'   db_folder = fs::path(tempdir(), "R", "cas_write_db_ignore_id"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#'
#' cas_write_db_ignore_id(id = sample(x = 1:100, size = 10))
#'
#' cas_read_db_ignore_id()
cas_write_db_ignore_id <- function(id,
                                   db_folder = NULL,
                                   db_connection = NULL,
                                   disconnect_db = FALSE,
                                   ...) {
  if (NROW(id) == 0) {
    return(tibble::tibble(id = double()))
  }

  if (is.data.frame(id)) {
    ignore_df <- tibble::tibble(id = unique(as.numeric(id[[1]])))
  } else {
    ignore_df <- tibble::tibble(id = unique(as.numeric(id)))
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


#' @rdname cas_write_db_ignore_id
#' @export
cas_ignore_id <- cas_write_db_ignore_id



#' Read identifiers to be ignored from the local database
#'
#' @inheritParams cas_write_db_ignore_id
#'
#' @return A data frame with a single column, `id`
#' @export
#'
#' @examples
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "cas_read_db_ignore_id"),
#'   db_folder = fs::path(tempdir(), "R", "cas_read_db_ignore_id"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#'
#' cas_write_db_ignore_id(id = sample(x = 1:100, size = 10))
#'
#' cas_read_db_ignore_id()
cas_read_db_ignore_id <- function(db_connection = NULL,
                                  db_folder = NULL,
                                  index_group = NULL,
                                  disconnect_db = TRUE,
                                  ...) {
  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = TRUE,
    ...
  )
  
  db_result <- tryCatch(
    cas_read_from_db(
      table = "contents_ignore",
      db_connection = db,
      disconnect_db = FALSE,
      ...
    ),
    error = function(e) {
      logical(1L)
    }
  )
  
  if (is.null(db_result)) {
    output_df <- tibble::tibble(id = double())
  } else if (isFALSE(db_result)) {
    output_df <- tibble::tibble(id = double())
  } else {
    output_df <- db_result |> 
      dplyr::collect() 
  }
  
  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )
  
  output_df
}
