#' Downloads files systematically, and stores details about the download in a
#' local database
#'
#' @param urls_df A data frame with at least two columns named `id` and `url`.
#'   Typically generated with `cas_build_urls()` for index files. If a character
#'   vector is given instead, identifiers will be given automatically.
#' @param index Logical, defaults to FALSE. If TRUE, downloaded files will be
#'   considered `index` files. If not, they will be considered `contents` files.
#'   See Readme for a more extensive explanation.
#' @param overwrite_file Logical, defaults to FALSE. If TRUE, files are
#'   downloaded again even if already present, overwriting previously downloaded
#'   items.
#' @param wait Defaults to 1. Number of seconds to wait between downloading one
#'   page and the next. Can be increased to reduce server load, or can be set to
#'   0 when this is not an issue.
#' @param sample Defaults to FALSE. If TRUE, the download order is randomised.
#'   If a numeric is given, the download order is randomised and at most the
#'   given number of items is downloaded.
#' @param retry_times Defaults to 10. Number of times to retry download in case
#'   of errors.
#' @param user_agent Defaults to NULL. If given, passed to download method.
#'
#' @inheritParams cas_connect_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_download <- function(download_df = NULL,
                         index = FALSE,
                         index_group = NULL,
                         file_format = "html",
                         overwrite_file = FALSE,
                         create_folder_if_missing = NULL,
                         wait = 1,
                         pause_base = 2,
                         pause_cap = 256,
                         pause_min = 4,
                         sample = FALSE,
                         retry_times = 8,
                         terminate_on = 404,
                         user_agent = NULL,
                         download_again_if_status_is_not = NULL,
                         ...) {
  cas_download_httr(
    download_df = download_df,
    index = index,
    index_group = index_group,
    overwrite_file = overwrite_file,
    create_folder_if_missing = create_folder_if_missing,
    wait = wait,
    sample = sample,
    file_format = file_format,
    retry_times = retry_times,
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    terminate_on = terminate_on,
    user_agent = user_agent,
    download_again_if_status_is_not = download_again_if_status_is_not,
    ...
  )
}

#' Downloads index files systematically, and stores details about the download
#' in a local database
#'
#' @param index
#'
#' @return
#' @export
#'
#' @examples
cas_download_index <- function(download_df = NULL,
                               index_group = NULL,
                               file_format = "html",
                               overwrite_file = FALSE,
                               create_folder_if_missing = NULL,
                               wait = 1,
                               pause_base = 2,
                               pause_cap = 256,
                               pause_min = 4,
                               sample = FALSE,
                               retry_times = 8,
                               terminate_on = 404,
                               user_agent = NULL,
                               download_again_if_status_is_not = NULL,
                               ...) {
  cas_download_httr(
    download_df = download_df,
    index = TRUE,
    index_group = index_group,
    create_folder_if_missing = create_folder_if_missing,
    overwrite_file = overwrite_file,
    wait = wait,
    sample = sample,
    file_format = file_format,
    retry_times = retry_times,
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    terminate_on = terminate_on,
    user_agent = user_agent,
    download_again_if_status_is_not = download_again_if_status_is_not,
    ...
  )
}

#' Checks that a given input corresponds to the format expected of a download
#' data frame, consistently returns expected format
#'
#' @param url A character vector or a data frame with at least two columns, `id`
#'   and `url`
#'
#' @return Consistently returns a data frame with at least two columns: a
#'   numeric `id` column, and a character `url` column.
#' @export
#'
#' @examples
#'
#' cas_get_urls_df(c(
#'   "https://example.com/a/",
#'   "https://example.com/b/"
#' ))
cas_get_urls_df <- function(urls = NULL,
                            index = FALSE,
                            index_group = NULL,
                            ...) {
  if (is.null(urls)) {
    if (index == FALSE) {
      urls_df <- cas_read_db_contents_id(...)
    } else if (index == TRUE) {
      urls_df <- cas_read_db_index(index_group = index_group, ...)
    } else {
      usethis::ui_stop("Parameter {usethis::ui_field('index`)} must be either {usethis::ui_value('TRUE`)} or {usethis::ui_value('FALSE`)}")
    }
  } else {
    if (is.data.frame(urls) == FALSE) {
      urls_df <- tibble::tibble(
        id = seq_along(urls),
        url = url
      )
    } else {
      if (sum(c("id", "url") %in% names(urls)) == 2) {
        urls_df <- urls
      } else {
        usethis::ui_stop("{usethis::ui_code('urls')} must either be a character vector or a data frame with at least two columns named {usethis::ui_field('id')} and {usethis::ui_field('url')}.")
      }
      if (is.numeric(urls_df$id) == FALSE) {
        usethis::ui_stop("If given, the {usethis::ui_field('id')} column must be numeric.")
      }
    }
  }
  urls_df
}


#' Create a data frame with not yet downloaded files
#'
#' @param urls Defaults to NULL. If given, it should correspond with a data
#'   frame with at least two columns named `id` and `url`. If not given, an
#'   attempt will be made to load it from the local database.
#' @param desc_id Logical, defaults to FALSE. If TRUE, results are returned with
#'   highest id first.
#' @param batch An integer, defaults to NULL. If not given, a check is performed
#'   in the database to find if previous downloads have taken place. If so, by
#'   default, the current batch will be one unit higher than the highest batch
#'   number found in the database.
#' @param download_again_if_status_is_not Defaults to NULL. If given, it must a
#'   status code as integer, typically `200L`, or `c(200L, 404L)`.
#'
#' @inheritParams cas_download
#' @inheritDotParams cas_get_urls_df -urls -index
#' @inheritDotParams cas_get_base_folder -level
#'
#' @return A data frame with four columns: `id`, `url`, `path` and `type`
#' @export
#'
#' @examples
cas_get_files_to_download <- function(urls = NULL,
                                      index = FALSE,
                                      index_group = NULL,
                                      desc_id = FALSE,
                                      batch = NULL,
                                      create_folder_if_missing = NULL,
                                      custom_folder = NULL,
                                      custom_path = NULL,
                                      file_format = "html",
                                      db_connection = NULL,
                                      download_again = FALSE,
                                      download_again_if_status_is_not = NULL,
                                      ...) {
  type <- dplyr::if_else(condition = index,
    true = "index",
    false = "contents"
  )

  urls_df <- cas_get_urls_df(
    urls = urls,
    index = index,
    index_group = index_group,
    db_connection = db_connection,
    ...
  ) %>%
    dplyr::collect()

  if (nrow(urls_df) == 0) {
    usethis::ui_warn("No {usethis::ui_code(type)} urls for download stored in database.")
    return(NULL)
  }

  path <- cas_get_base_path(
    create_folder_if_missing = create_folder_if_missing,
    custom_path = custom_path,
    custom_folder = custom_folder,
    index = index,
    file_format = file_format
  )

  ## check if previous downloads are stored
  ## if yes, add 1 to highest batch
  ## if not, set batch to 1
  previous_download_df <- cas_read_db_download(
    index = index,
    db_connection = db_connection,
    db_folder = db_folder,
    ...
  ) %>%
    dplyr::collect()

  if (nrow(previous_download_df) == 0) {
    current_batch <- 1
  } else {
    current_batch <- sum(max(previous_download_df$batch), 1)
  }

  expected_filenames_df <- tibble::tibble(
    id = urls_df$id,
    path = fs::path(
      path,
      current_batch,
      stringr::str_c(
        urls_df$id,
        "_",
        current_batch,
        ".",
        file_format
      )
    )
  )

  if (isTRUE(download_again)) {
    files_to_download_df <- expected_filenames_df
  } else {
    if (is.null(download_again_if_status_is_not) == FALSE) {
      previous_download_df <- previous_download_df %>%
        dplyr::filter(status %in% download_again_if_status_is_not)
    }
    files_to_download_df <- dplyr::anti_join(
      x = expected_filenames_df,
      y = previous_download_df,
      by = "id"
    )
  }

  urls_to_download_df <- dplyr::left_join(
    x = files_to_download_df,
    y = urls_df,
    by = "id"
  ) %>%
    dplyr::mutate(batch = as.numeric(current_batch)) %>%
    dplyr::select("id", "batch", "url", "path")

  if (desc_id) {
    urls_to_download_df %>%
      dplyr::arrange(dplyr::desc(x = id))
  } else {
    urls_to_download_df
  }
}
