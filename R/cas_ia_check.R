#' Gets an Archive.org Wayback Machine URL
#'
#' For details on API access to the Wayback Machine see:
#' https://archive.org/help/wayback_api.php
#'
#' For an R package facilitating more extensive interaction with the API, see:
#' https://github.com/hrbrmstr/wayback
#'
#' Integration with Wayback CDX Server API to be considered.
#'
#' @param url A charachter vector of length one, a url.
#' @param check_db Defaults to TRUE. If TRUE, checks if given URL has already
#'   been checked in local database, and queries APIs only for URLs that have
#'   not been previously checked.
#' @param write_db Defaults to TRUE. If TRUE, writes result to a local database.
#' @inheritParams cas_download
#'
#' @return A url linking to the version on the Internet Archive
#' @export
#'
#' @examples
cas_ia_check <- function(url = NULL,
                         wait = 1,
                         retry_times = 16,
                         pause_base = 2,
                         pause_cap = 512,
                         pause_min = 4,
                         db_connection = NULL,
                         disconnect_db = FALSE,
                         check_db = TRUE,
                         write_db = TRUE,
                         output_only_newly_checked = FALSE,
                         ...) {
  if (check_db == FALSE & write_db == FALSE) {
    # do nothing, as connection won't be needed
  } else {
    db <- cas_connect_to_db(
      db_connection = db_connection,
      ...
    )
  }


  if (check_db == TRUE) {
    db_result <- tryCatch(
      cas_read_db_ia(
        db_connection = db,
        disconnect_db = FALSE,
        ...
      ),
      error = function(e) {
        logical(1L)
      }
    )

    if (is.null(db_result)) {
      previous_df <- casdb_empty_ia_check
    } else if (isFALSE(db_result)) {
      previous_df <- casdb_empty_ia_check
    } else {
      if (length(db_result %>% dplyr::pull(url)) == 0) {
        previous_df <- casdb_empty_ia_check
      } else {
        previous_df <- db_result
      }
    }

    if (length(url) == 0) {
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
        by = "url",
        copy = TRUE
      )
  } else {
    url_v <- unique(url)
    url_to_process_df <- tibble::tibble(url = url_v)
  }

  if (nrow(url_to_process_df) < 2) {
    wait <- 0
  }

  pb <- progress::progress_bar$new(total = nrow(url_to_process_df))

  output_df <- purrr::map_dfr(
    .x = url_to_process_df %>% dplyr::pull("url"),
    .f = function(x) {
      pb$tick()

      ia_available <- httr::RETRY(
        verb = "GET",
        url = "https://archive.org/wayback/available",
        query = list(url = x),
        pause_base = pause_base,
        pause_cap = pause_cap,
        pause_min = pause_min,
        times = retry_times
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
          dplyr::select(
            "url",
            dplyr::everything()
          )
      }

      if (write_db == TRUE) {
        cas_write_to_db(
          df = current_df,
          table = "ia_check",
          db_connection = db,
          disconnect_db = FALSE,
          ...
        )
      }

      Sys.sleep(time = wait)

      current_df
    }
  )

  if (check_db == FALSE & write_db == FALSE) {
    # do nothing, as db object won't exist
  } else {
    if (output_only_newly_checked == TRUE) {
      cas_disconnect_from_db(
        db_connection = db,
        disconnect_db = disconnect_db
      )
      return(output_df)
    }

    cas_read_db_ia() %>%
      dplyr::filter(url %in% {{ url_v }}) %>%
      dplyr::group_by(url) %>%
      dplyr::slice_max(checked_at) %>%
      dplyr::ungroup()
  }
}

#' Save a URL the Internet Archive's Wayback Machine
#'
#' Consider using long waiting times, and using a high number of retry. Retry is
#' done graciously, using `httr::RETRY`, and respecting the waiting time given
#' when error 529 "too many requests" is returned by the server. This is still
#' likely to take a long amount of time.
#'
#' @param wait Defaults to 32. I have found no information online about what
#'   wait time is considered suitable by Archive.org itself, but I've noticed
#'   that with wait time shorter than 10 seconds the whole process stops getting
#'   positive replies from the server very soon.
#' @param ia_check Defaults to TRUE. If TRUE, checks again the URL after saving
#'   it and keeps record in the local database.
#' @param ia_check_wait Defaults to 2, passed to `cas_ia_check()`. Can generally
#'   be kept low, as this is a light API.
#' @param only_if_unavailable Defaults to TRUE. If TRUE, checks for availability
#'   of urls before attempting to save them.
#' @inheritParams cas_ia_check
#' @inheritParams httr::RETRY
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Once the usual parameters are set with `cas_set_options()` it is generally
#'   # ok to just let it get urls from the database and let it run without any
#'   # additional parameter.
#'   cas_ia_save()
#' }
#' }
cas_ia_save <- function(url = NULL,
                        wait = 32,
                        retry_times = 64,
                        pause_base = 16,
                        pause_cap = 1024,
                        pause_min = 64,
                        only_if_unavailable = TRUE,
                        ia_check = TRUE,
                        ia_check_wait = 2,
                        db_connection = NULL,
                        check_db = TRUE,
                        write_db = TRUE,
                        ...) {
  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  if (only_if_unavailable) {
    url <- cas_ia_check(
      url = url,
      db_connection = db,
      check_db = check_db,
      write_db = write_db,
      wait = ia_check_wait,
      pause_base = pause_base,
      pause_cap = pause_cap,
      pause_min = pause_min,
      output_only_newly_checked = FALSE
    ) %>%
      dplyr::filter(available == FALSE) %>%
      dplyr::pull(url)
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


  if (length(url_v) < 2) {
    wait <- 0
  }

  pb <- progress::progress_bar$new(total = length(url_v))

  purrr::map_dfr(
    .x = url_v,
    .f = function(x) {
      pb$tick()

      ia_saved <- httr::RETRY(
        verb = "GET",
        url = stringr::str_c("https://web.archive.org/save/", x),
        times = retry_times,
        pause_base = pause_base,
        pause_cap = pause_cap,
        pause_min = pause_min
      )

      httr::stop_for_status(ia_saved)

      Sys.sleep(time = wait)

      if (ia_check) {
        cas_ia_check(
          url = x,
          wait = 0,
          db_connection = db,
          check_db = FALSE,
          write_db = write_db,
          retry_times = retry_times,
          ...
        )
      } else {
        saved_url <- ia_saved[["url"]]

        if (is.null(saved_url)) {
          tibble::tibble(ia_url = NA_character_)
        } else {
          tibble::tibble(ia_url = as.character(saved_url))
        }
      }
    }
  )
}


#' Read status on the Internet Archive of given URLs
#'
#' @inheritParams cas_write_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_read_db_ia <- function(db_connection = NULL,
                           db_folder = NULL,
                           ...) {
  db_result <- tryCatch(
    cas_read_from_db(
      table = "ia_check",
      db_folder = db_folder,
      db_connection = db_connection,
      ...
    ),
    error = function(e) {
      logical(1L)
    }
  )

  if (is.null(db_result)) {
    tibble::as_tibble(casdb_empty_ia_check)
  } else if (isFALSE(db_result)) {
    tibble::as_tibble(casdb_empty_ia_check)
  } else {
    db_result
  }
}
