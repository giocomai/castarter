#' Downloads one file at a time with the default R function for downloading
#' files
#'
#' Mostly used internally by [cas_download()], relies on [download.file()]
#' function.
#'
#' @param download_df A data frame with four columns: `id`, `url`, `path`,
#'   `type`.
#' @param overwrite_file Logical, defaults to FALSE.
#' @param ignore_ssl_certificates Logical, defaults to FALSE. If TRUE it uses
#'   `wget` to download the page, and does not check if the SSL certificate is
#'   valid. Useful, for example, for https pages with expired or mis-configured
#'   SSL certificate.
#'
#' @return Invisibly returns the full `httr` response.
#' @inheritParams cas_download
#' @inheritParams cas_write_to_db
#' @export
#'
#' @examples
cas_download_internal <- function(download_df = NULL,
                                  index = FALSE,
                                  index_group = NULL,
                                  overwrite_file = FALSE,
                                  ignore_id = TRUE,
                                  wait = 1,
                                  ignore_ssl_certificates = FALSE,
                                  create_folder_if_missing = NULL,
                                  db_connection = NULL,
                                  disconnect_db = FALSE,
                                  sample = FALSE,
                                  file_format = "html",
                                  ...) {
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
      create_folder_if_missing = create_folder_if_missing,
      ignore_id = ignore_id,
      db_connection = db,
      disconnect_db = FALSE,
      file_format = file_format,
      ...
    ) %>%
      dplyr::collect()
  }
  
  if (nrow(download_df) == 0) {
    cli::cli_inform(c(i = "No new files or pages to download."))
    return(invisible(NULL))
  } else {
    current_batch_folder <- fs::path_dir(path = download_df[["path"]][1])
    if (fs::file_exists(current_batch_folder) == FALSE) {
      fs::dir_create(path = current_batch_folder)
      cli::cli_inform(c(i = "The folder {.path {current_batch_folder}} for the current download batch has been created."))
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
        if (ignore_ssl_certificates == TRUE) {
          raw <- tryCatch(
            expr = download.file(
              url = x$url,
              destfile = x$path,
              method = "wget",
              extra = "--no-check-certificate"
            ),
            error = function(e) {
              e
            }
          )
        } else {
          raw <- tryCatch(
            expr = download.file(
              url = x$url,
              destfile = x$path
            ),
            error = function(e) {
              e
            }
          )
        }
        
        if (inherits(raw, "error") == FALSE) {
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
          cli::cli_warn("Error while downloading page with id {.val {x$id}} and url {.url {x$url}}. The process will proceed with other pages.")
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
