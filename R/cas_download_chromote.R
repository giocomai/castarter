#' Downloads one file at a time with `chromote`
#'
#' @inheritParams cas_download_httr
#' @inheritParams cas_get_files_to_download
#' @param delay Defaults to 0. Passed to `chromote`'s internal method `go_to`.
#'   Number of seconds to wait after the page load event fires.
#' @param timeout Defaults to 20. Passed to `chromote`'s internal method
#'   `go_to`. Maximum time in seconds to wait for the page load event.
#'
#' @return
#' @export
#' 
#' @examples
cas_download_chromote <- function(
  download_df = NULL,
  index = FALSE,
  index_group = NULL,
  overwrite_file = FALSE,
  ignore_id = TRUE,
  wait = 1,
  delay = 0,
  timeout = 20,
  db_connection = NULL,
  sample = FALSE,
  file_format = "html",
  download_again = FALSE,
  download_again_if_status_is_not = NULL,
  disconnect_db = FALSE,
  ...
) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    cli::cli_abort(
      "You need to install the {.pkg chromote} package to download pages with headless chrome/chromium."
    )
  }

  type <- dplyr::if_else(condition = index, true = "index", false = "contents")

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
      ignore_id = ignore_id,
      download_again = download_again,
      download_again_if_status_is_not = download_again_if_status_is_not,
      ...
    ) |>
      dplyr::collect()
  }

  if (nrow(download_df) == 0) {
    cli::cli_inform(c(i = "No new files or pages to download."))
    return(invisible(NULL))
  } else {
    current_batch_folder <- fs::path_dir(path = download_df[["path"]][1])
    if (fs::file_exists(current_batch_folder) == FALSE) {
      fs::dir_create(path = current_batch_folder)
      cli::cli_inform(c(
        i = "The folder {.path {current_batch_folder}} for the current download batch has been created."
      ))
    }
  }

  if (is.numeric(sample)) {
    download_df <- download_df |>
      dplyr::slice_sample(n = sample)
  } else if (isTRUE(sample)) {
    download_df <- download_df |>
      dplyr::slice_sample(p = 1)
  }

  pb <- progress::progress_bar$new(total = nrow(download_df))

  purrr::walk(
    .x = purrr::transpose(download_df),
    .f = function(x) {
      pb$tick()
      if (fs::file_exists(x$path) == FALSE | overwrite_file == TRUE) {
        raw <- tryCatch(
          expr = {
            b <- chromote::ChromoteSession$new()
            
            b$go_to(url = x$url,
                    delay = delay, 
                    timeout_ = timeout)
            
            result <- b$Runtime$evaluate(
              expression = "document.documentElement.outerHTML"
            )
            b$close()
          },
          error = function(e) {
            e
          }
        )

        if (inherits(raw, "error") == FALSE) {
          writeLines(
            text = result$result$value,
            con = x$path,
            sep = "\n"
          )

          info_df <- tibble::tibble(
            id = x$id,
            batch = x$batch,
            datetime = Sys.time(),
            status = 200L,
            size = fs::file_size(x$path)
          )

          cas_write_to_db(
            df = info_df,
            table = stringr::str_c(type, "_", "download"),
            db_connection = db,
            disconnect_db = FALSE,
            ...
          )
        } else {
          cli::cli_alert_danger(c(x = raw))
          cli::cli_warn(
            "Error while downloading page with id {.val {x$id}} and url {.url {x$url}}. The process will proceed with other pages."
          )
        }
        Sys.sleep(time = wait)
      }
    }
  )

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )
}
