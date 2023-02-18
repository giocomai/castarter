#' Get path to locally downloaded files
#'
#' This function relies on data stored in the database.
#'
#' @inheritParams cas_download
#'
#' @return
#' @export
#'
#' @examples
cas_get_path_to_files <- function(urls = NULL,
                                  id = NULL,
                                  index = FALSE,
                                  index_group = NULL,
                                  custom_folder = NULL,
                                  custom_path = NULL,
                                  file_format = "html",
                                  sample = FALSE,
                                  db_connection = NULL,
                                  db_folder = NULL,
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

  if (isTRUE(index)) {
    index_group_id_v <- cas_read_db_index(
      db_folder = db_folder,
      db_connection = db_connection,
      index_group = index_group,
      ...
    ) %>%
      dplyr::filter(index_group %in% {{ index_group }}) %>%
      dplyr::pull(id)

    available_files_df <- available_files_df %>%
      dplyr::filter(id %in% {{ index_group_id_v }})
  }

  if (is.null(id) == FALSE) {
    available_files_df <- available_files_df %>%
      dplyr::filter(id %in% {{ id }})
  }

  if (nrow(available_files_df) == 0) {
    return(invisible(NULL))
  }

  if (is.numeric(sample) == TRUE) {
    available_files_df <- available_files_df %>%
      dplyr::slice_sample(n = sample)
  } else if (isTRUE(sample)) {
    available_files_df <- available_files_df %>%
      dplyr::slice_sample(p = 1)
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
  available_files_df %>%
    dplyr::select("id", "batch") %>%
    dplyr::mutate(path = fs::path(
      path,
      batch,
      stringr::str_c(id, "_", batch, ".", file_format)
    )) %>%
    dplyr::mutate(available = fs::file_exists(path = path))
}
