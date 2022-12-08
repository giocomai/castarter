#' Gets an Archive.org Wayback Machine URL
#'
#' For details on API access to the Wayback Machine see:
#' https://archive.org/help/wayback_api.php
#'
#' For an R package facilitating more extensive interaction with the API, see:
#' https://github.com/hrbrmstr/wayback
#'
#' @param url A charachter vector of length one, a url.
#'
#' @inheritParams cas_download
#'
#' @return A url linking to the version on the Internet Archive
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
cas_ia_check <- function(url = NULL,
                         wait = 1,
                         db_connection = NULL,
                         ...) {
  if (length(url) < 2) {
    wait <- 0
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  db_result <- tryCatch(cas_read_from_db(
    table = "ia_check",
    db_folder = db_folder,
    db_connection = db_connection,
    disconnect_db = FALSE,
    ...
  ),
  error = function(e) {
    logical(1L)
  }
  )

  if (isFALSE(db_result)) {
    previous_df <- casdb_empty_ia_check
  } else if (is.data.frame(db_result)) {
    if (nrow(db_result) == 0) {
      previous_df <- casdb_empty_ia_check
    } else {
      previous_df <- db_result
    }
  }

  if (is.null(url)) {
    url_df <- cas_read_db_contents_id(
      db_connection = db,
      disconnect_db = FALSE
    )

    url_v <- url_df %>%
      dplyr::pull(url)
  } else if (is.data.frame(url)) {
    url_v <- url_df %>%
      dplyr::pull(url)
  } else {
    url_v <- as.character(url)
  }

  url_to_process_df <- tibble::tibble(url = unique(url_v)) %>%
    dplyr::anti_join(
      y = previous_df,
      by = "url"
    )

  pb <- progress::progress_bar$new(total = nrow(url_to_process_df))

  output_df <- purrr::map_dfr(
    .x = url_to_process_df$url,
    .f = function(x) {
      pb$tick()

      Sys.sleep(time = wait)

      ia_available <- httr::GET("https://archive.org/wayback/available",
        query = list(url = x)
      )

      httr::stop_for_status(ia_available)

      ia_available_text <- httr::content(ia_available,
        as = "text",
        encoding = "UTF-8"
      )

      ia_available_list <- jsonlite::fromJSON(txt = ia_available_text)

      if (is.null(ia_available_list$archived_snapshots$closest$available)) {
        current_df <- tibble::tibble(
          url = as.character(x),
          status = NA_character_,
          available = FALSE,
          ia_url = NA_character_,
          timestamp = lubridate::as_datetime(NA),
          checked_at = lubridate::as_datetime(Sys.time())
        )
      } else {
        current_df <- tibble::as_tibble(ia_available_list$archived_snapshots$closest) %>%
          dplyr::rename(ia_url = url) %>%
          dplyr::mutate(
            timestamp = lubridate::ymd_hms(timestamp),
            checked_at = lubridate::as_datetime(Sys.time()),
            url = as.character(x)
          ) %>%
          dplyr::select("url", dplyr::everything())
      }

      cas_write_to_db(
        df = current_df,
        table = "ia_check",
        db_connection = db,
        disconnect_db = FALSE,
        ...
      )

      current_df
    }
  )

  cas_disconnect_from_db(
    db_connection = db,
    ...
  )

  output_df
}

#' Save a URL the Internet Archive's Wayback Machine
#'
#' @inheritParams cas_ia_check
#'
#' @return
#' @export
#'
#' @examples
cas_ia_save <- function(url,
                        wait = 1,
                        ...) {
  if (length(url) < 2) {
    wait <- 0
  }

  pb <- progress::progress_bar$new(total = length(url))

  purrr::map_chr(
    .x = url,
    .f = function(x) {
      pb$tick()

      ia_saved <- httr::GET(
        url = stringr::str_c("https://web.archive.org/save/", x)
      )

      httr::stop_for_status(ia_saved)

      saved_url <- ia_saved[["url"]]

      if (is.null(saved_url)) {
        NA_character_
      } else {
        as.character(saved_url)
      }
    }
  )
}
