#' Write contents URLs to local database
#'
#' If some URLs are already included in the database, it appends only the new ones: URLs are expected to be unique.
#'
#' @param urls A data frame with three columns, such as \code{casdb_empty_contents_id}, or a character vector.
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
#'   url_beginning = "https://www.example.com/news/",
#'   start_page = 1,
#'   end_page = 10
#' )
#'
#' cas_write_db_contents(urls = urls_df)
#'
#' cas_read_db_contents()
cas_write_db_contents <- function(urls,
                                  use_db = NULL,
                                  overwrite = FALSE,
                                  db_connection = NULL,
                                  disconnect_db = TRUE) {
  if (is.data.frame(urls)) {
    if (identical(colnames(urls), colnames(casdb_empty_contents_id)) & identical(sapply(urls, class), sapply(casdb_empty_contents_id, class))) {
      urls_df <- urls
    } else {
      usethis::ui_stop("urls data frame must match exactly the column names and types of {usethis::ui_code('casdb_empty_contents_id')}")
    }
  } else {
    urls_df <- cas_build_urls(
      url_beginning = urls,
      url_ending = "",
      start_page = NULL,
      end_page = NULL
    )
  }

  if (cas_check_use_db(use_db = use_db) == FALSE) {
    return(invisible(NULL))
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    use_db = use_db
  )

  previous_contents_df <- cas_read_db_contents(
    use_db = use_db,
    db_connection = db,
    disconnect_db = FALSE
  )

  if (nrow(previous_contents_df) > 0) {
    urls_to_add_df <- urls_df %>%
      dplyr::anti_join(
        y = previous_contents_df,
        by = c("url", "type")
      )

    if (sum(is.element(urls_to_add_df$id, previous_contents_df$id)) > 0) {
      usethis::ui_info("Introducing new {usethis::ui_code('id')} to ensure unique values")
      urls_to_add_df$id <- seq(
        sum(max(previous_contents_df$id), 1),
        sum(
          max(previous_contents_df$id),
          nrow(urls_to_add_df)
        )
      ) %>%
        as.numeric()
    }
  } else {
    urls_to_add_df <- urls_df
  }

  urls_to_add_n <- nrow(urls_to_add_df)
  if (urls_to_add_n > 0) {
    cas_write_to_db(
      df = urls_to_add_df,
      table = "contents_id",
      use_db = use_db,
      overwrite = overwrite,
      db_connection = db,
      disconnect_db = FALSE
    )

    usethis::ui_done("Urls added to {usethis::ui_field('contents_id')} table: {usethis::ui_value(urls_to_add_n)}")
  } else {
    usethis::ui_info("No new url added to {usethis::ui_field('contents_id')} table.")
  }

  cas_disconnect_from_db(
    use_db = use_db,
    db_connection = db,
    disconnect_db = disconnect_db
  )

  invisible(urls_to_add_df)
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
#'   url_beginning = "https://www.example.com/news/",
#'   start_page = 1,
#'   end_page = 10
#' )
#'
#' cas_write_db_contents(urls = urls_df)
#'
#' cas_read_db_contents()
cas_read_db_contents <- function(use_db = NULL,
                                 db_connection = NULL,
                                 disconnect_db = TRUE,
                                 db_folder = NULL,
                                 project = cas_get_options()$project,
                                 website = cas_get_options()$website) {
  db_result <- tryCatch(cas_read_from_db(
    table = "contents_id",
    use_db = use_db,
    db_connection = db_connection,
    disconnect_db = disconnect_db,
    db_folder = db_folder,
    project = project,
    website = website
  ),
  error = function(e) {
    logical(1L)
  }
  )

  if (isFALSE(db_result)) {
    tibble::as_tibble(casdb_empty_contents_id)
  } else {
    tibble::as_tibble(db_result)
  }
}
