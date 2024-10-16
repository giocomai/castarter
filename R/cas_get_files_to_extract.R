#' Get path to (locally available) files to be extracted
#'
#' Mostly used internally by [cas_extract] or for troubleshooting.
#'
#' @inheritParams cas_extract
#'
#' @return
#' @export
#'
#' @examples
#' #' \dontrun{
#' if (interactive) {
#'   cas_get_files_to_extract()
#' }
#' }
cas_get_files_to_extract <- function(id = NULL,
                                     ignore_id = TRUE,
                                     custom_path = NULL,
                                     index = FALSE,
                                     store_as_character = TRUE,
                                     check_previous = TRUE,
                                     db_connection = NULL,
                                     file_format = "html",
                                     sample = FALSE,
                                     keep_if_status = 200,
                                     ...) {
  ellipsis::check_dots_unnamed()
  
  db <- cas_connect_to_db(
    db_connection = db_connection,
    ...
  )
  
  type <- dplyr::if_else(condition = index,
                         true = "index",
                         false = "contents"
  )
  
  if (is.null(custom_path)) {
    path <- cas_get_base_path(...)
  } else {
    path_ending <- stringr::str_c(file_format, type, sep = "_")
    
    custom_path_ending <- fs::path_file(custom_path)
    
    if (path_ending == custom_path_ending) {
      path <- fs::path(custom_path)
    } else {
      path <- fs::path(
        custom_path,
        path_ending
      )
    }
  }
  
  previous_download_df <- cas_read_db_download(
    index = index,
    db_connection = db,
    disconnect_db = FALSE,
    ...
  ) |>
    dplyr::arrange(dplyr::desc(datetime)) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::arrange(id, batch, datetime) |>
    dplyr::filter(status %in% keep_if_status)
  
  stored_files_df <- previous_download_df |>
    dplyr::select("id", "batch") |>
    dplyr::mutate(path = fs::path(
      path,
      batch,
      stringr::str_c(id, "_", batch, ".", file_format)
    )) |>
    dplyr::select("id", "path")
  
  if (store_as_character == TRUE) {
    stored_files_df <- stored_files_df |>
      dplyr::mutate(id = as.character(id))
  }
  
  
  if (check_previous == FALSE) {
    files_to_extract_pre_df <- stored_files_df
    write_to_db <- FALSE
  } else {
    # Do not process previously extracted
    previously_extracted_df <- cas_read_db_contents_data(
      db_connection = db,
      disconnect_db = FALSE,
      ...
    )
    
    if (is.null(previously_extracted_df) == FALSE) {
      previously_extracted_df <- previously_extracted_df |>
        dplyr::select(id) |>
        dplyr::collect()
      
      if (store_as_character == TRUE) {
        previously_extracted_df <- previously_extracted_df |>
          dplyr::mutate(id = as.character(id))
      }
    }
    
    if (is.null(previously_extracted_df) == FALSE) {
      files_to_extract_pre_df <- dplyr::anti_join(
        x = stored_files_df,
        y = previously_extracted_df,
        by = "id"
      )
    } else {
      files_to_extract_pre_df <- stored_files_df
    }
  }
  
  if (nrow(files_to_extract_pre_df) == 0) {
    # TODO return consistently data frame or S3 object
    return(invisible(NULL))
  }
  
  contents_id_df <- cas_read_db_contents_id(
    db_connection = db,
    disconnect_db = FALSE,
    ...
  ) |>
    dplyr::collect()
  
  if (store_as_character == TRUE) {
    contents_id_df <- contents_id_df |>
      dplyr::mutate(id = as.character(id))
  }
  
  files_to_extract_df <- files_to_extract_pre_df |>
    dplyr::inner_join(
      y = contents_id_df,
      by = "id"
    )
  
  if (is.null(id) == FALSE) {
    id_to_keep <- id
    files_to_extract_df <- files_to_extract_df |>
      dplyr::filter(id %in% id_to_keep)
  }
  
  if (isTRUE(ignore_id)) {
    ignore_id <- cas_read_db_ignore_id(
      db_connection = db,
      disconnect_db = FALSE
    ) |>
      dplyr::pull(id)
    
    files_to_extract_df <- files_to_extract_df |>
      dplyr::filter(!(id %in% ignore_id))
  } else if (is.numeric(ignore_id)) {
    files_to_extract_df <- files_to_extract_df |>
      dplyr::filter(!(id %in% ignore_id))
  }
  
  if (is.numeric(sample) == TRUE) {
    if (sample > nrow(files_to_extract_df)) {
      sample <- nrow(files_to_extract_df)
    }
    
    files_to_extract_df <- files_to_extract_df |>
      dplyr::slice_sample(n = sample)
  } else if (isTRUE(sample)) {
    files_to_extract_df <- files_to_extract_df |>
      dplyr::slice_sample(p = 1)
  }
  
  
  available_files_to_extract_df <- files_to_extract_df |>
    dplyr::mutate(available = fs::file_exists(path)) |>
    dplyr::filter(available)
  
  if (nrow(available_files_to_extract_df) != nrow(files_to_extract_df)) {
    cli::cli_warn(c(
      `x` = glue::glue("Not all downloaded files are currently available in their expected location."),
      `*` = glue::glue("Total files expected:  {scales::number(nrow(files_to_extract_df))}"),
      `*` = glue::glue("Total files available: {scales::number(nrow(available_files_to_extract_df))}"),
      `i` = glue::glue("Only available files will be processed. Consider running `cas_restore()` or otherwise deal with missing files as needed.")
    ))
  }
  available_files_to_extract_df
}