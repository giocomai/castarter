#' Write index URLs to local database
#'
#' If some URLs are already included in the database, it appends only the new ones: URLs are expected to be unique.
#'
#' @param urls A data frame with three columns, such as \code{casdb_empty_index_id}, or a character vector.
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
#' cas_write_db_index(urls = urls_df)
#'
#' cas_read_db_index()
cas_write_db_index <- function(urls,
                               overwrite = FALSE,
                               db_connection = NULL,
                               disconnect_db = FALSE,
                               ...) {
  if (is.data.frame(urls)) {
    if (identical(colnames(urls), colnames(casdb_empty_index_id)) & identical(sapply(urls, class), sapply(casdb_empty_index_id, class))) {
      urls_df <- urls
    } else {
      usethis::ui_stop("urls data frame must match exactly the column names and types of {usethis::ui_code('casdb_empty_index_id')}")
    }
  } else {
    urls_df <- cas_build_urls(
      url = urls,
      url_ending = "",
      start_page = NULL,
      end_page = NULL
    )
  }

  if (cas_check_use_db(...) == FALSE) {
    return(invisible(NULL))
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  previous_index_df <- cas_read_db_index(
    db_connection = db,
    disconnect_db = FALSE,
    ...
  ) %>%
    dplyr::collect()

  if (nrow(previous_index_df) > 0) {
    urls_to_add_df <- urls_df %>%
      dplyr::anti_join(
        y = previous_index_df,
        by = c("url", "index_group")
      )

    if (sum(is.element(urls_to_add_df$id, previous_index_df$id)) > 0) {
      usethis::ui_info("Introducing new {usethis::ui_code('id')} to ensure unique values")
      urls_to_add_df$id <- seq(
        sum(max(previous_index_df$id), 1),
        sum(
          max(previous_index_df$id),
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
      table = "index_id",
      overwrite = overwrite,
      disconnect_db = FALSE,
      db_connection = db,
      ...
    )

    usethis::ui_done("Urls added to {usethis::ui_field('index_id')} table: {usethis::ui_value(urls_to_add_n)}")
  } else {
    usethis::ui_info("No new url added to {usethis::ui_field('index_id')} table.")
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )

  invisible(urls_to_add_df)
}

#' Read index from local database
#'
#' @inheritParams cas_write_to_db
#'
#' @return A data frame with three columns and data stored in the `index_id`
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
#' cas_write_db_index(urls = urls_df)
#'
#' cas_read_db_index()
cas_read_db_index <- function(db_folder = NULL,
                              db_connection = NULL,
                              index_group = NULL,
                              ...) {
  db_result <- tryCatch(
    cas_read_from_db(
      table = "index_id",
      db_folder = db_folder,
      db_connection = db_connection,
      ...
    ),
    error = function(e) {
      logical(1L)
    }
  )

  if (is.null(db_result)) {
    tibble::as_tibble(casdb_empty_index_id)
  } else if (isFALSE(db_result)) {
    tibble::as_tibble(casdb_empty_index_id)
  } else {
    if (is.null(index_group) == FALSE) {
      db_result %>%
        dplyr::filter(index_group == !!index_group)
    } else {
      db_result
    }
  }
}



#' Read index from local database
#'
#' @inheritParams cas_write_to_db
#' @param batch Default to "latest": returns only the path to the file with the
#'   highest batch identifier available. Valid values are: "latest", "all", or a
#'   numeric identifier corresponding to desired batch.
#' @param status Defaults to 200. Keeps only files downloaded with the given
#'   status (can be more than one, given as a vector). If NULL, no filter based
#'   on status is applied.
#'
#' @return A data frame with three columns and data stored in the `index_id`
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
#' cas_write_db_index(urls = urls_df)
#'
#' cas_read_db_index()
cas_read_db_download <- function(index = FALSE,
                                 id = NULL,
                                 batch = "latest",
                                 status = 200L,
                                 db_connection = NULL,
                                 db_folder = NULL,
                                 ...) {
  type <- dplyr::if_else(
    condition = index,
    true = "index",
    false = "contents"
  )

  db_result <- tryCatch(
    cas_read_from_db(
      table = stringr::str_c(type, "_", "download"),
      db_connection = db_connection,
      db_folder = db_folder,
      ...
    ),
    error = function(e) {
      logical(1L)
    }
  )

  if (is.null(db_result)) {
    tibble::as_tibble(casdb_empty_download)
  } else if (isFALSE(db_result)) {
    tibble::as_tibble(casdb_empty_download)
  } else {
    if (ncol(db_result) == 0) {
      tibble::as_tibble(casdb_empty_download)
    } else {
      if (is.null(id) == FALSE) {
        db_result <- db_result %>%
          dplyr::filter(id %in% {{ id }})
      }

      if (is.null(batch) == TRUE) {
        # do nothing
      } else if (is.numeric(batch)) {
        db_result <- db_result %>%
          dplyr::filter(batch %in% {{ batch }})
      } else if (batch == "latest") {
        db_result <- db_result %>%
          dplyr::slice_max(batch,
            n = 1,
            by = "id"
          )
      }

      if (is.null(status) == TRUE) {
        # do nothing
      } else if (is.numeric(status)) {
        db_result <- db_result %>%
          dplyr::filter(status %in% {{ status }})
      }

      db_result %>%
        dplyr::collect() %>%
        dplyr::mutate(
          datetime = lubridate::as_datetime(datetime),
          size = fs::as_fs_bytes(size)
        )
    }
  }
}
