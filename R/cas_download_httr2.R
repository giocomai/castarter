#' Downloads one file at a time with `httr2`
#'
#' Mostly used internally by [cas_download()].
#'
#' @param download_df A data frame with four columns: `id`, `url`, `path`, `type`.
#' @param overwrite_file Logical, defaults to `FALSE`.
#' @param ignore_ssl_certificates Logical, defaults to `FALSE`. If `TRUE`, it
#'   does not check if the SSL certificate is valid. Useful, for example, for
#'   https pages with expired or mis-configured SSL certificate.
#'
#' @return Invisibly returns the full `httr2` response.
#' @inheritParams cas_download
#' @inheritParams cas_write_to_db
#' @inheritParams cas_check_response
#' @export
#'
#' @examples
cas_download_httr2 <- function(
  download_df = NULL,
  index = FALSE,
  index_group = NULL,
  overwrite_file = FALSE,
  ignore_id = TRUE,
  wait = 1,
  url_encode = TRUE,
  create_folder_if_missing = NULL,
  pause_base = 4,
  pause_cap = 256,
  pause_min = 8,
  terminate_on = NULL,
  retry_times = 3,
  ignore_ssl_certificates = FALSE,
  db_connection = NULL,
  disconnect_db = FALSE,
  sample = FALSE,
  file_format = "html",
  user_agent = NULL,
  download_again = FALSE,
  download_again_if_status_is_not = NULL,
  ...
) {
  type <- dplyr::if_else(condition = index, true = "index", false = "contents")
  ssl_verifypeer_digit <- dplyr::if_else(
    condition = ignore_ssl_certificates,
    true = 0,
    false = 1
  )

  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )

  if (is.null(download_df)) {
    download_df <- cas_get_files_to_download(
      index = index,
      index_group = index_group,
      create_folder_if_missing = create_folder_if_missing,
      ignore_id = ignore_id,
      db_connection = db,
      disconnect_db = FALSE,
      file_format = file_format,
      download_again = download_again,
      download_again_if_status_is_not = download_again_if_status_is_not,
      ...
    )
  }

  if (is.null(download_df)) {
    cli::cli_inform("No new files or pages to download.")
    return(invisible(NULL))
  } else if (nrow(download_df) == 0) {
    cli::cli_inform("No new files or pages to download.")
    return(invisible(NULL))
  } else {
    current_batch_folder <- fs::path_dir(path = download_df[["path"]][1])
    current_base_download_path <- fs::path_dir(current_batch_folder)
    if (!fs::file_exists(current_batch_folder)) {
      if (
        isTRUE(create_folder_if_missing) |
          fs::file_exists(current_base_download_path)
      ) {
        fs::dir_create(path = current_batch_folder)
        cli::cli_inform(c(
          v = "The folder {.path {current_batch_folder}} for the current download batch has been created."
        ))
      } else {
        cli::cli_abort(c(
          v = "The folder {.path {current_batch_folder}} for the current download batch does not exist and has not been created.",
          i = "Set {.var create_folder_if_missing} to {.val TRUE} or create the folder manually."
        ))
      }
    }
  }

  if (is.numeric(sample)) {
    download_df <- download_df |>
      dplyr::slice_sample(n = sample)
  } else if (sample) {
    download_df <- download_df |>
      dplyr::slice_sample(p = 1)
  }

  purrr::walk(
    .progress = TRUE,
    .x = purrr::transpose(download_df),
    .f = function(x) {
      if (!fs::file_exists(x$path) | overwrite_file) {
        if (url_encode) {
          url_to_process_v <- utils::URLencode(
            URL = x[["url"]],
            repeated = FALSE
          )
        } else {
          url_to_process_v <- x[["url"]]
        }

        req <- httr2::request(
          base_url = url_to_process_v
        ) |>
          httr2::req_options(ssl_verifypeer = ssl_verifypeer_digit) |>
          httr2::req_user_agent(string = user_agent) |>
          httr2::req_retry(
            max_tries = retry_times,
            backoff = \(resp_n) {
              max(pause_min, min(pause_base * 2^(resp_n - 1), pause_cap))
            },
            is_transient = \(resp) {
              if (is.null(terminate_on)) {
                return(TRUE)
              }

              if (httr2::resp_status(resp) %in% terminate_on) {
                return(FALSE)
              }
            }
          )
        httr2::req_error(is_error = \(resp) FALSE)

        resp <- tryCatch(
          req |> httr2::req_perform(path = x$path),
          error = \(e) FALSE
        )

        info_df <- tibble::tibble(
          id = x$id,
          batch = x$batch,
          datetime = Sys.time(),
          status = httr2::resp_status(resp),
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
    disconnect_db = disconnect_db
  )
}
