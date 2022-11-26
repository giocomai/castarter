#' Get path to locally downloaded files
#'
#' This function relies on data stored in the database.
#'
#' @return
#' @export
#'
#' @examples
cas_get_path_to_files <- function(urls = NULL,
                                  index = FALSE,
                                  custom_folder = NULL,
                                  custom_path = NULL,
                                  file_format = "html",
                                  db_connection = NULL,
                                  ...) {
  type <- dplyr::if_else(condition = index,
    true = "index",
    false = "contents"
  )

  available_files_df <- cas_read_db_download(
    index = index,
    db_connection = db_connection,
    db_folder = db_folder,
    ...
  ) %>%
    dplyr::arrange(id, dplyr::desc(batch)) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::filter(status == 200)

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
  available_files_df %>%
    dplyr::select("id", "batch") %>%
    dplyr::mutate(path = fs::path(
      path,
      batch,
      stringr::str_c(id, "_", batch, ".", file_format)
    )) %>%
    dplyr::mutate(available = fs::file_exists(path = path))
}
