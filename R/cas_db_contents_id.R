#' Write contents URLs to local database
#'
#' If some URLs are already included in the database, it appends only the new
#' ones: URLs are expected to be unique.
#'
#' @param contents_id_df A data frame with five columns, such as
#'   \code{casdb_empty_contents_id}, or a character vector.
#' @param quiet Defaults to FALSE. If set to TRUE, messages on number of lines
#'   added are not shown.
#' @param check_previous Defaulst to TRUE. If set to FALSE, the given input is
#'   stored in the database without checking if the same url had already been
#'   stored.
#'
#' @inheritParams cas_write_to_db
#'
#' @return Invisibly returns only new rows added.
#' @export
#'
#' @examples
#'
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "castarter_data"),
#'   db_folder = fs::path(tempdir(), "R", "castarter_data"),
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
#' cas_write_db_contents_id(urls = urls_df)
#'
#' cas_read_db_contents_id()
cas_write_db_contents_id <- function(contents_id_df,
                                     overwrite = FALSE,
                                     db_connection = NULL,
                                     disconnect_db = TRUE,
                                     quiet = FALSE,
                                     check_previous = TRUE,
                                     ...) {
  if (cas_check_use_db(...) == FALSE) {
    return(invisible(NULL))
  }

  if (nrow(contents_id_df) == 0) {
    return(invisible(NULL))
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  if (check_previous == TRUE) {

    # TODO check what happens when check_previous is actually set to TRUE
    # the following is likely legacy code

    previous_contents_df <- cas_read_db_contents_id(
      ...,
      disconnect_db = FALSE
    )

    if (nrow(previous_contents_df) > 0) {
      links_to_add_df <- contents_id_df %>%
        dplyr::anti_join(
          y = previous_contents_df,
          by = c("url", "source_index_id", "source_index_batch")
        )

      if (sum(is.element(links_to_add_df$id, previous_contents_df$id)) > 0) {
        if (quiet == FALSE) {
          usethis::ui_info("Introducing new {usethis::ui_code('id')} to ensure unique values")
        }
        links_to_add_df$id <- seq(
          sum(max(previous_contents_df$id), 1),
          sum(
            max(previous_contents_df$id),
            nrow(links_to_add_df)
          )
        ) %>%
          as.numeric()
      }
    } else {
      links_to_add_df <- contents_id_df
    }
  } else {
    links_to_add_df <- contents_id_df
  }

  links_to_add_n <- nrow(links_to_add_df)

  if (links_to_add_n > 0) {
    cas_write_to_db(
      df = links_to_add_df,
      table = "contents_id",
      overwrite = overwrite,
      db_connection = db,
      disconnect_db = FALSE,
      ...
    )
    if (quiet == FALSE) {
      usethis::ui_done("Urls added to {usethis::ui_field('contents_id')} table: {usethis::ui_value(links_to_add_n)}")
    }
  } else {
    if (quiet == FALSE) {
      usethis::ui_info("No new url added to {usethis::ui_field('contents_id')} table.")
    }
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db,
    ...
  )

  invisible(links_to_add_df)
}

#' Read contents from local database
#'
#' @inheritParams cas_write_to_db
#'
#' @return A data frame with three columns and data stored in the `contents_id`
#'   table of the local database. The data frame has zero rows if the database
#'   does not exist or no data was previously stored there.
#' @export
#'
#' @examples
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "castarter_data"),
#'   db_folder = fs::path(tempdir(), "R", "castarter_data"),
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
#' cas_write_db_contents(urls = urls_df)
#'
#' cas_read_db_contents_id()
cas_read_db_contents_id <- function(db_connection = NULL,
                                    db_folder = NULL,
                                    ...) {
  db_result <- tryCatch(cas_read_from_db(
    table = "contents_id",
    db_folder = db_folder,
    db_connection = db_connection,
    ...
  ),
  error = function(e) {
    logical(1L)
  }
  )

  if (isFALSE(db_result)) {
    tibble::as_tibble(casdb_empty_contents_id)
  } else if (is.data.frame(db_result)) {
    if (nrow(db_result) == 0) {
      casdb_empty_contents_id
    } else {
      tibble::as_tibble(db_result)
    }
  }
}