#' Open in a browser a URL
#'
#' @param remote Defaults to TRUE. If TRUE, open relevant url online. If FALSE,
#'   it opens the locally stored file.
#' @param random Defaults to 1. By defaults, it opens one random url.
#'
#' @inheritParams cas_download
#'
#' @return
#' @export
#'
#' @examples
cas_browse <- function(index = FALSE,
                       remote = TRUE,
                       id = NULL,
                       batch = NULL,
                       index_group = NULL,
                       file_format = "html",
                       random = 1,
                       ...) {
  type <- dplyr::if_else(condition = index,
    true = "index",
    false = "contents"
  )

  db <- cas_connect_to_db(read_only = TRUE, 
                          ...)

  if (remote == FALSE) {
    local_files <- cas_get_path_to_files(
      index = index,
      file_format = file_format,
      db_connection = db,
      random = random,
      ...
    )

    url_to_open_v <- local_files %>%
      dplyr::pull("path")
  } else {
    if (index == TRUE) {
      if (is.null(index_group)) {
        url_to_open_v <- cas_read_db_index(
          db_connection = db,
          ...
        ) %>%
          dplyr::slice_sample(n = random) %>%
          dplyr::pull("url")
      } else {
        index_group_to_filter <- index_group

        url_to_open_v <- cas_read_db_index(
          db_connection = db,
          ...
        ) %>%
          dplyr::filter(index_group == index_group_to_filter) %>%
          dplyr::slice_sample(n = random) %>%
          dplyr::pull("url")
      }
    } else {
      url_to_open_v <- cas_read_db_contents_id(
        db_connection = db,
        ...
      ) %>%
        dplyr::slice_sample(n = random) %>%
        dplyr::pull("url")
    }
  }
  
  cas_disconnect_from_db(db_connection = DB,
                         ...)

  browseURL(url_to_open_v)
}
