#' Write extracted contents to local database
#'
#' If some IDs are already present in the database, only the new ones are
#' appended: IDs are expected to be unique.
#'
#' Check for consistency in database columns: if new columns do not match
#' previous columns, it throws an error.
#'
#' @param quiet Defaults to FALSE. If set to TRUE, messages on number of lines
#'   added are not shown.
#' @param check_previous Defaults to TRUE. If set to FALSE, the given input is
#'   stored in the database without checking if the same id had already been
#'   stored.
#'
#' @inheritParams cas_write_to_db
#'
#' @return Invisibly returns only new rows added.
#' @export
#'
#' @examples
cas_write_db_contents_data <- function(contents_df,
                                       overwrite = FALSE,
                                       db_connection = NULL,
                                       disconnect_db = FALSE,
                                       quiet = FALSE,
                                       check_previous = TRUE,
                                       ...) {
  if (cas_check_use_db(...) == FALSE) {
    return(invisible(NULL))
  }

  if (nrow(contents_df) == 0) {
    return(invisible(NULL))
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  if (check_previous == TRUE) {
    previous_contents_original_df <- cas_read_db_contents_data(
      ...,
      disconnect_db = FALSE
    )

    if (is.null(previous_contents_original_df) == TRUE) {
      contents_to_add_df <- contents_df %>%
        dplyr::collect()
    } else {
      previous_contents_df <- previous_contents_original_df |>
        dplyr::select(id) |>
        dplyr::collect()

      previous_contents_reference_df <- previous_contents_original_df |>
        head(0) |>
        dplyr::collect()

      if (nrow(previous_contents_df) > 0) {
        if (identical(
          colnames(contents_df),
          colnames(previous_contents_reference_df)
        ) &
          identical(
            sapply(contents_df, class),
            sapply(previous_contents_reference_df, class)
          )) {
          # do nothing, columns are cosistent
        } else {
          cli::cli_abort(c(
            x = "One or more column names or types in {.var contents_df} do not match those of previously stored contents.",
            i = "You can retrieve previously stored contents with {.fun cas_read_db_contents_data}"
          ))
        }
        contents_to_add_df <- contents_df %>%
          dplyr::anti_join(
            y = previous_contents_df,
            by = c("id")
          )
      } else {
        contents_to_add_df <- contents_df %>%
          dplyr::collect()
      }
    }
  } else {
    contents_to_add_df <- contents_df %>%
      dplyr::collect()
  }

  contents_to_add_n <- nrow(contents_to_add_df)

  if (contents_to_add_n > 0) {
    added_contents_df <- cas_write_to_db(
      df = contents_to_add_df,
      table = "contents_data",
      overwrite = overwrite,
      db_connection = db,
      disconnect_db = FALSE,
      ...
    )
    if (quiet == FALSE) {
      cli::cli_inform(c(v = "Contents added to {.field contents_data} table: {.val {contents_to_add_n}}"))
    }
  } else {
    if (quiet == FALSE) {
      cli::cli_inform(c(v = "No new contents added to {.field contents_data} table."))
    }
    added_contents_df <- NULL
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )

  invisible(added_contents_df)
}
