#' Check httr response code and cache locally results
#'
#' @param url A character vector of urls, or a data frame with a `url` column,
#'   typically retrieved with `cas_get_urls_df()`.
#' @inheritParams cas_ia_check
#'
#' @returns A data frame with response status for each given url.
#' @export
#'
#' @examples
#' \dontrun{
#' cas_check_response("https://example.com")
#' }
cas_check_response <- function(
  url = NULL,
  wait = 1,
  output_only_newly_checked = FALSE,
  index = FALSE,
  index_group = NULL,
  db_connection = NULL,
  disconnect_db = FALSE,
  check_db = TRUE,
  write_db = TRUE,
  ...
) {
  if (is.null(url)) {
    url <- cas_get_urls_df(
      urls = NULL,
      index = index,
      index_group = index_group,
      ...
    )
  }

  if (is.character(url)) {
    urls_df <- tibble::tibble(url = url)
  } else if (is.data.frame(url) & "url" %in% colnames(url)) {
    urls_df <- dplyr::distinct(url)
  } else {
    cli::cli_abort(
      message = "{.arg url} must either be a charcater vector or a data frame with a {.var url} column."
    )
  }

  if (!check_db & !write_db) {
    # do nothing, as connection won't be needed
  } else {
    db <- cas_connect_to_db(
      db_connection = db_connection,
      ...
    )
  }

  if (check_db) {
    db_result <- tryCatch(
      cas_read_db_response(
        db_connection = db,
        disconnect_db = FALSE,
        ...
      ),
      error = function(e) {
        logical(1L)
      }
    )

    if (is.null(db_result)) {
      previous_df <- casdb_empty_response_check
    } else if (isFALSE(db_result)) {
      previous_df <- casdb_empty_response_check
    } else {
      if (length(db_result |> dplyr::pull(url)) == 0) {
        previous_df <- casdb_empty_response_check
      } else {
        previous_df <- db_result
      }
    }

    url_v <- urls_df |>
      dplyr::pull(url)

    urls_to_process_df <- tibble::tibble(url = unique(url_v)) |>
      dplyr::anti_join(
        y = previous_df,
        by = "url",
        copy = TRUE
      )
  } else {
    urls_to_process_df <- urls_df |> dplyr::distinct()
  }

  if (nrow(urls_to_process_df) < 2) {
    wait <- 0
  }

  output_df <- purrr::map(
    .progress = TRUE,
    .x = purrr::transpose(urls_to_process_df),
    .f = \(x) {
      if (is.na(x[["url"]])) {
        resp_df <- tibble::tibble(
          url = NA_character_,
          response_url = NA_character_,
          status = NA_integer_,
          status_description = NA_character_,
          type = NA_character_,
          encoding = NA_character_,
          retry_after = NA_character_,
          available = FALSE,
          checked_at = lubridate::as_datetime(Sys.time())
        )
      } else {
        req <- httr2::request(x[["url"]]) |>
          httr2::req_error(is_error = \(resp) FALSE)

        resp <- tryCatch(req |> httr2::req_perform(), error = \(e) FALSE)

        if (isFALSE(resp)) {
          resp_df <- tibble::tibble(
            url = as.character(x[["url"]]),
            response_url = NA_character_,
            status = NA_integer_,
            status_description = NA_character_,
            type = NA_character_,
            encoding = NA_character_,
            retry_after = NA_character_,
            available = FALSE,
            checked_at = lubridate::as_datetime(Sys.time())
          )
        } else {
          resp_df <- tibble::tibble(
            url = as.character(x[["url"]]),
            response_url = as.character(httr2::resp_url(resp = resp)),
            status = as.numeric(httr2::resp_status(resp)),
            status_description = as.character(httr2::resp_status_desc(resp)),
            type = as.character(httr2::resp_content_type(resp)),
            encoding = as.character(httr2::resp_encoding(resp)),
            retry_after = as.character(httr2::resp_retry_after(resp)),
            available = TRUE,
            checked_at = lubridate::as_datetime(Sys.time())
          )
        }
      }

      if (write_db) {
        cas_write_to_db(
          df = resp_df,
          table = "response_check",
          db_connection = db,
          disconnect_db = FALSE,
          ...
        )
      }
      Sys.sleep(wait)
      resp_df
    }
  ) |>
    purrr::list_rbind()

  if (!check_db & !write_db) {
    return(output_df)
  } else {
    if (output_only_newly_checked) {
      cas_disconnect_from_db(
        db_connection = db,
        disconnect_db = disconnect_db
      )
      return(output_df)
    }

    cas_read_db_response() |>
      dplyr::filter(url %in% {{ url_v }}) |>
      dplyr::group_by(url) |>
      dplyr::slice_max(checked_at) |>
      dplyr::ungroup()
  }
}


#' Check response type of URLs as stored in the local database.
#'
#' Data stored locally typically by relying on `cas_check_response()`.
#'
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_read_db_response <- function(db_connection = NULL, db_folder = NULL, ...) {
  db_result <- tryCatch(
    cas_read_from_db(
      table = "response_check",
      db_folder = db_folder,
      db_connection = db_connection,
      ...
    ),
    error = function(e) {
      logical(1L)
    }
  )

  if (is.null(db_result)) {
    tibble::as_tibble(casdb_empty_response_check)
  } else if (isFALSE(db_result)) {
    tibble::as_tibble(casdb_empty_response_check)
  } else {
    db_result
  }
}
