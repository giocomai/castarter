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
cas_ia_check <- function(url,
                         wait = 1) {
  if (length(url) < 2) {
    wait <- 0
  }

  pb <- progress::progress_bar$new(total = length(url))

  purrr::map_dfr(
    .x = url,
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
        tibble::tibble(
          status = NA_character_,
          available = FALSE,
          url = NA_character_,
          timestamp = lubridate::as_datetime(NA),
          checked_at = lubridate::as_datetime(Sys.time())
        )
      } else {
        tibble::as_tibble(ia_available_list$archived_snapshots$closest) %>%
          dplyr::mutate(
            timestamp = lubridate::ymd_hms(timestamp),
            checked_at = lubridate::as_datetime(Sys.time())
          )
      }
    }
  )
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
                        wait = 1) {
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
