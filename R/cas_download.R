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
#'
#' @inheritParams cas_connect_to_db
#'
#' @return
#' @export
#'
#' @examples
cas_download <- function(download_df = NULL,
                         index = FALSE,
                         overwrite_file = FALSE,
                         wait = 1,
                         ...) {
  cass_download_httr(
    download_df = download_df,
    index = index,
    overwrite_file = overwrite_file,
    wait = wait,
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
#' cass_get_urls_df(c(
#'   "https://example.com/a/",
#'   "https://example.com/b/"
#' ))
cass_get_urls_df <- function(urls,
                             index = FALSE,
                             ...) {
  if (is.null(urls)) {
    if (index == FALSE) {
      urls_df <- cas_read_db_contents(...)
    } else if (index == TRUE) {
      urls_df <- cas_read_db_index(...)
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


#' Downloads one file at a time with httr
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
cass_download_httr <- function(download_df = NULL,
                               index = FALSE,
                               overwrite_file = FALSE,
                               wait = 1,
                               db_connection = NULL,
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
    download_df <- cass_get_files_to_download(
      index = index,
      db_connection = db,
      disconnect_db = FALSE,
      ...
    )
  }

  if (nrow(download_df) == 0) {
    usethis::ui_info("No new files or pages to download.")
    return(NULL)
  }

  pb <- progress::progress_bar$new(total = nrow(download_df))

  purrr::walk(
    .x = purrr::transpose(download_df),
    .f = function(x) {
      pb$tick()
      if (fs::file_exists(x$path) == FALSE | overwrite_file == TRUE) {
        raw <- httr::GET(
          url = x$url,
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



#' Create a data frame with not yet downloaded files
#'
#' @param urls Defaults to NULL. If given, it should correspond with a data
#'   frame with at least two columns named `id` and `url`. If not given, an
#'   attempt will be made to load it from the local database.
#' @param batch An integer, defaults to NULL. If not given, a check is performed
#'   in the database to find if previous downloads have taken place. If so, by
#'   default, the current batch will be one unit higher than the highest batch
#'   number found in the database.
#'
#' @inheritParams cas_download
#' @inheritDotParams cass_get_urls_df -urls -index
#' @inheritDotParams cas_get_base_folder -level
#'
#' @return A data frame with four columns: `id`, `url`, `path` and `type`
#' @export
#'
#' @examples
cass_get_files_to_download <- function(urls = NULL,
                                       index = FALSE,
                                       batch = NULL,
                                       custom_folder = NULL,
                                       custom_path = NULL,
                                       file_format = "html",
                                       db_connection = NULL,
                                       ...) {
  type <- dplyr::if_else(condition = index,
    true = "index",
    false = "contents"
  )

  urls_df <- cass_get_urls_df(
    urls = urls,
    index = index,
    db_connection = db_connection,
    ...
  )

  if (nrow(urls_df) == 0) {
    usethis::ui_warn("No {usethis::ui_code(type)} urls for download stored in database.")
    return(NULL)
  }

  if (is.null(custom_path)) {
    website_folder <- cas_get_base_folder(
      level = "website",
      ...
    )
    if (is.null(custom_folder) == FALSE) {
      path <- fs::path(
        website_folder,
        stringr::str_c(file_format, "_", custom_folder)
      )
    } else {
      path <- fs::path(
        website_folder,
        stringr::str_c(file_format, "_", type)
      )
    }
  }

  if (fs::file_exists(path) == FALSE) {
    fs::dir_create(path = path)
    usethis::ui_info(stringr::str_c("The folder",
      usethis::ui_path(path),
      "has been created.",
      sep = " "
    ))
  }

  # previous_files_df <- fs::dir_info(path = path) %>%
  #   dplyr::transmute(path,
  #     size,
  #     filename = fs::path_file(path)
  #   ) %>%
  #   dplyr::mutate(id = stringr::str_extract(
  #     string = filename,
  #     pattern = "[[:digit:]]+"
  #   ) %>%
  #     as.numeric()) %>%
  #   dplyr::arrange(id)

  ## check if previous downloads are stored
  ## if yes, add 1 to highest batch
  ## if not, set batch to 1
  previous_download_df <- cas_read_db_download(
    index = index,
    db_connection = db_connection,
    db_folder = db_folder,
    ...
  )

  if (nrow(previous_download_df) == 0) {
    current_batch <- 1
  } else {
    current_batch <- sum(max(previous_download_df$batch), 1)
  }

  expected_filenames_df <- tibble::tibble(
    id = urls_df$id,
    path = fs::path(
      path,
      stringr::str_c(urls_df$id, "_", current_batch, ".", file_format)
    )
  )

  files_to_download_df <- dplyr::anti_join(
    x = expected_filenames_df,
    y = previous_download_df,
    by = "id"
  )

  urls_to_download_df <- dplyr::left_join(
    x = files_to_download_df,
    y = urls_df,
    by = "id"
  ) %>%
    dplyr::mutate(batch = as.numeric(current_batch)) %>%
    dplyr::select("id", "batch", "url", "path")

  urls_to_download_df
}
