#' Checks for availability of a sitemap in xml format.
#'
#' Searches in common locations (namely, `example.com/sitemap.xml`, and
#' `example.com/sitemap_index.xml`) and then in robots.txt and returns a url to
#' the sitemap, along with the contents of the sitemap itself, if found.
#'
#' @param domain Defaults to `NULL`, but required unless `sitemap_url` given.
#'   Expected to be a full domain name. If input does not start with `http`,
#'   then `https://` is prepended automatically.
#' @param sitemap_url Defaults to `NULL`. If given, `domain` is ignored.
#' @inheritParams cas_connect_to_db
#'
#' @returns A data frame, including a `sitemap_url` column, the response as an
#'   httr2 object, and the body of the xml.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   cas_get_sitemap(domain = "https://www.europeandatajournalism.eu/")
#' }
cas_get_sitemap <- function(
  domain = NULL,
  sitemap_url = NULL,
  check_robots = TRUE,
  check_common = TRUE,
  read_from_db = TRUE,
  write_to_db = FALSE,
  db_connection = NULL,
  disconnect_db = FALSE,
  ...
) {
  sitemap_response <- NULL

  if (read_from_db) {
    db <- cas_connect_to_db(
      db_connection = db_connection,
      ...
    )

    sitemap_df <- cas_read_db_sitemap(
      db_connection = db,
      disconnect_db = FALSE
    ) |>
      dplyr::collect()

    if (nrow(sitemap_df) > 0) {
      cas_disconnect_from_db(
        db_connection = db,
        disconnect_db = disconnect_db
      )
      return(sitemap_df)
    }
  }

  if (stringr::str_starts(string = domain, pattern = "http", negate = TRUE)) {
    domain <- stringr::str_flatten(string = c("https://", domain))
  }

  if (check_robots) {
    if (is.null(sitemap_url)) {
      url <- httr2::url_modify(url = domain, path = "robots.txt")

      req <- httr2::request(url) |>
        httr2::req_error(is_error = \(resp) FALSE)

      resp <- tryCatch(req |> httr2::req_perform(), error = \(e) FALSE)

      if (isFALSE(resp)) {
        return(invisible(NULL))
      } else if (!httr2::resp_has_body(resp)) {
        return(invisible(NULL))
      } else {
        body_string <- httr2::resp_body_string(resp)

        sitemap_df <- tibble::tibble(
          robots = stringr::str_split(
            string = body_string,
            pattern = "\n",
            simplify = TRUE
          ) |>
            as.character()
        ) |>
          dplyr::filter(stringr::str_starts(
            robots,
            stringr::fixed("Sitemap", ignore_case = TRUE)
          )) |>
          dplyr::transmute(
            sitemap = stringr::str_remove(robots, stringr::fixed("Sitemap:")) |>
              stringr::str_squish()
          )

        if (nrow(sitemap_df) == 0) {
          sitemap_url <- NULL
        } else {
          sitemap_url <- sitemap_df |> dplyr::pull(sitemap)
        }
      }
    }
  }

  if (check_common) {
    if (is.null(sitemap_url)) {
      url <- httr2::url_modify(url = domain, path = "sitemap.xml")

      req <- httr2::request(url) |>
        httr2::req_error(is_error = \(resp) FALSE)

      resp <- tryCatch(req |> httr2::req_perform(), error = \(e) FALSE)

      if (isFALSE(resp)) {
        sitemap_url <- NULL
      } else if (!httr2::resp_has_body(resp)) {
        sitemap_url <- NULL
      } else {
        sitemap_response <- resp
        sitemap_url <- url
      }
    }

    if (is.null(sitemap_url)) {
      url <- httr2::url_modify(url = domain, path = "sitemap_index.xml")

      req <- httr2::request(url) |>
        httr2::req_error(is_error = \(resp) FALSE)

      resp <- tryCatch(req |> httr2::req_perform(), error = \(e) FALSE)

      if (isFALSE(resp)) {
        sitemap_url <- NULL
      } else if (!httr2::resp_has_body(resp)) {
        sitemap_url <- NULL
      } else {
        sitemap_response <- resp
        sitemap_url <- url
      }
    }
  }

  if (write_to_db) {
    db <- cas_connect_to_db(
      db_connection = db_connection,
      ...
    )

    cas_write_db_sitemap(
      sitemap = sitemap_url,
      table = "sitemap",
      db_connection = db,
      disconnect_db = FALSE
    )
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )

  sitemap_df <- tibble::tibble(sitemap_url = sitemap_url)
  sitemap_df
}


#' Read sitemap from local database
#'
#' @inheritParams cas_write_to_db
#'
#' @return A data frame with three columns and data stored in the `sitemap`
#'   table of the local database. The data frame has zero rows if the database
#'   does not exist or no data was previously stored there.
#' @export
#'
#' @examples
#' cas_set_options(
#'   base_folder = fs::path(fs::path_temp(), "R", "castarter_data"),
#'   db_folder = fs::path(fs::path_temp(), "R", "castarter_data"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#' cas_write_db_sitemap(sitemap = "https://example.com/sitemap.xml")
#'
#' cas_read_db_sitemap()
cas_read_db_sitemap <- function(
  db_connection = NULL,
  db_folder = NULL,
  ...
) {
  db_result <- tryCatch(
    cas_read_from_db(
      table = "sitemap",
      db_folder = db_folder,
      db_connection = db_connection,
      ...
    ),
    error = function(e) {
      logical(1L)
    }
  )

  if (is.null(db_result)) {
    tibble::as_tibble(casdb_empty_sitemap)
  } else if (isFALSE(db_result)) {
    tibble::as_tibble(casdb_empty_sitemap)
  } else {
    db_result
  }
}


#' Write index URLs to local database
#'
#' If some URLs are already included in the database, it appends only the new
#' ones: URLs are expected to be unique.
#'
#' @param sitemap A sitemap url, or a dataframe with at least one column named
#'   `sitemap_url`.
#'
#' @inheritParams cas_write_to_db
#'
#' @return Invisibly returns a data frame with a single column, `sitemap_url`.
#' @export
#'
#' @examples
#' cas_set_options(
#'   base_folder = fs::path(fs::path_temp(), "R", "castarter_data"),
#'   db_folder = fs::path(fs::path_temp(), "R", "castarter_data"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#' cas_enable_db()
#'
#' cas_write_db_sitemap(sitemap = "https://example.com/sitemap.xml")
#'
#' cas_read_db_sitemap()
cas_write_db_sitemap <- function(
  sitemap,
  overwrite = FALSE,
  db_connection = NULL,
  disconnect_db = FALSE,
  ...
) {
  if (is.data.frame(sitemap)) {
    if ("sitemap_url" %in% colnames(sitemap)) {
      sitemap_url_input <- sitemap |>
        dplyr::pull(sitemap_url)
    } else {
      cli::cli_abort(
        "When the {.arg sitemap} input is a data frame, it must have a {.field sitemap_url} column."
      )
    }
  } else {
    sitemap_url_input <- sitemap
  }

  sitemap_df <- tibble::tibble(sitemap_url = sitemap_url_input)

  if (cas_check_use_db(...) == FALSE) {
    return(invisible(casdb_empty_sitemap))
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  previous_sitemap_df <- cas_read_db_sitemap(
    db_connection = db,
    disconnect_db = FALSE,
    ...
  ) |>
    dplyr::collect()

  if (nrow(previous_sitemap_df) > 0) {
    sitemap_to_add_df <- sitemap_df |>
      dplyr::anti_join(
        y = previous_sitemap_df,
        by = c("sitemap_url")
      )

    if (nrow(sitemap_to_add_df) == 0) {
      cas_disconnect_from_db(
        db_connection = db,
        disconnect_db = disconnect_db
      )
      cli::cli_inform(
        c(i = "No new url added to {.field sitemap} table.")
      )
      return(invisible(previous_sitemap_df))
    }
  } else {
    sitemap_to_add_df <- sitemap_df
  }

  sitemap_to_add_n <- nrow(sitemap_to_add_df)
  if (sitemap_to_add_n > 0) {
    cas_write_to_db(
      df = sitemap_to_add_df,
      table = "sitemap",
      overwrite = overwrite,
      disconnect_db = FALSE,
      db_connection = db
    )

    cli::cli_inform(
      c(v = "Urls added to {.field sitemap} table: {.val {sitemap_to_add_n}}")
    )
  } else {
    cli::cli_inform(
      c(i = "No new url added to {.field sitemap} table.")
    )
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )

  invisible(sitemap_to_add_df)
}
