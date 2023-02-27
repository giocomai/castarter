#' Downloads one file at a time with chromium
#'
#' N.B. Not working, not yet implemented
#'
#' Mostly used internally by `cas_download`.
#'
#' @param download_df A data frame with four columns: `id`, `url`, `path`, `type`.
#' @param overwrite_file Logical, defaults to FALSE.
#'
#' @return Invisibly returns the full `httr` response.
#' @inheritParams cas_download
#' @inheritParams cas_write_to_db
#' @export
#'
#' @examples
cas_download_chromium <- function(download_df = NULL,
                                  index = FALSE,
                                  index_group = NULL,
                                  overwrite_file = FALSE,
                                  wait = 1,
                                  db_connection = NULL,
                                  sample = FALSE,
                                  file_format = "html",
                                  download_again_if_status_is_not = NULL,
                                  ...) {
  if (requireNamespace("chromote", quietly = TRUE) == FALSE) {
    # check chromote https://github.com/rstudio/chromote
    cli::cli_abort("You need to install the `chromote` package to download pages with headless chrome/chromium.")
  }

  type <- dplyr::if_else(condition = index,
    true = "index",
    false = "contents"
  )
  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  if (is.null(download_df)) {
    download_df <- cas_get_files_to_download(
      index = index,
      index_group = index_group,
      db_connection = db,
      disconnect_db = FALSE,
      file_format = file_format,
      download_again_if_status_is_not = download_again_if_status_is_not,
      ...
    ) %>%
      dplyr::collect()
  }

  if (nrow(download_df) == 0) {
    usethis::ui_info("No new files or pages to download.")
    return(invisible(NULL))
  } else {
    current_batch_folder <- fs::path_dir(path = download_df[["path"]][1])
    if (fs::file_exists(current_batch_folder) == FALSE) {
      fs::dir_create(path = current_batch_folder)
      usethis::ui_info(
        stringr::str_c(
          "The folder",
          usethis::ui_path(current_batch_folder),
          "for the current download batch has been created.",
          sep = " "
        )
      )
    }
  }

  if (is.numeric(sample) == TRUE) {
    download_df <- download_df %>%
      dplyr::slice_sample(n = sample)
  } else if (isTRUE(sample)) {
    download_df <- download_df %>%
      dplyr::slice_sample(p = 1)
  }

  pb <- progress::progress_bar$new(total = nrow(download_df))

  purrr::walk(
    .x = purrr::transpose(download_df),
    .f = function(x) {
      pb$tick()
      if (fs::file_exists(x$path) == FALSE | overwrite_file == TRUE) {
        raw <- httr::RETRY(
          verb = "GET",
          url = x$url,
          times = retry_times,
          pause_base = pause_base,
          pause_cap = pause_cap,
          pause_min = pause_min,
          terminate_on = terminate_on,
          httr::write_disk(
            path = x$path,
            overwrite = overwrite_file
          )
        )

        info_df <- tibble::tibble(
          id = x$id,
          batch = x$batch,
          datetime = Sys.time(),
          status = raw$status_code,
          size = fs::file_size(x$path)
        )

        cas_write_to_db(
          df = info_df,
          table = stringr::str_c(type, "_", "download"),
          db_connection = db,
          disconnect_db = FALSE,
          ...
        )
        Sys.sleep(time = wait)
      }
    }
  )

  cas_disconnect_from_db(
    db_connection = db,
    ...
  )
}
