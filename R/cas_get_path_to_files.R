#' Get path to locally downloaded files
#'
#' This function relies on data stored in the database.
#'
#' @inheritParams cas_download
#' @inheritParams cas_read_db_download
#'
#' @return A data frame of one row if "batch" is set to "latest". Possibly more
#'   than one row in other cases.
#' @export
#'
#' @examples
cas_get_path_to_files <- function(
  urls = NULL,
  id = NULL,
  batch = "latest",
  status = 200,
  index = FALSE,
  index_group = NULL,
  custom_folder = NULL,
  custom_path = NULL,
  file_format = "html",
  sample = FALSE,
  db_connection = NULL,
  db_folder = NULL,
  disconnect_db = TRUE,
  ...
) {
  type <- dplyr::if_else(condition = index, true = "index", false = "contents")

  db <- cas_connect_to_db(
    read_only = TRUE,
    ...
  )

  available_files_df <- cas_read_db_download(
    id = id,
    batch = batch,
    status = status,
    index = index,
    db_connection = db,
    disconnect_db = FALSE,
    ...
  )

  if (is.null(available_files_df)) {
    return(invisible(NULL))
  }

  if (isTRUE(index)) {
    if (is.null(index_group) == FALSE) {
      index_group_id_v <- cas_read_db_index(
        db_connection = db,
        index_group = index_group,
        ...
      ) |>
        dplyr::filter(index_group %in% {{ index_group }}) |>
        dplyr::pull(id)

      available_files_df <- available_files_df |>
        dplyr::filter(id %in% {{ index_group_id_v }})
    }
  }

  cas_disconnect_from_db(
    db_connection = db,
    disconnect_db = disconnect_db
  )

  if (nrow(available_files_df) == 0) {
    return(invisible(NULL))
  }

  if (is.numeric(sample)) {
    available_files_df <- available_files_df |>
      dplyr::slice_sample(n = sample)
  } else if (isTRUE(sample)) {
    available_files_df <- available_files_df |>
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
  available_files_df |>
    dplyr::select("id", "batch") |>
    dplyr::mutate(
      path = fs::path(
        path,
        batch,
        stringr::str_c(id, "_", batch, ".", file_format)
      )
    ) |>
    dplyr::mutate(available = fs::file_exists(path = path))
}
