#' Restore files from compressed files
#'
#' @param restore_to Path to archive directory, defaults to NULL. If NULL, path
#'   is set to the project/website/archive folder.
#' @param restore_from Path to archive directory, defaults to NULL. If NULL,
#'   path is set to the project/website/archive folder.
#' @param file_format Defaults to "tar.gz", to ensure cross-platform
#'   compatibility. No other formats are supported at this stage.
#' @inheritParams cas_read_from_db
#' @return A path to the base folder where files are stored. Corresponds to
#'   `restore_to` if given, or to a temporary folder if `restore_to` is set to
#'   NULL.
#' @export
#' 
#' @examples
cas_restore <- function(restore_to = NULL, 
                        restore_from = NULL,
                        file_format = "tar.gz",
                        index = FALSE,
                        contents = FALSE,
                        batch = NULL,
                        db_connection = NULL,
                        db_folder = NULL,
                        ...) {
  cas_options_l <- cas_get_options(...)
  
  website_folder <- cas_get_base_folder(level = "website")
  
  if (is.null(restore_from)) {
    restore_from <- fs::path(
      website_folder,
      "archive")
  }
  
  available_backup_files_v <- fs::dir_ls(path = restore_from,
                                         recurse = TRUE,
                                         type = "file", 
                                         glob = stringr::str_c("*.",
                                                               file_format))
  
  file_types <- available_backup_files_v %>% 
    stringr::str_remove(pattern = stringr::str_c("_",
                                                 "[[:digit:]]+",
                                                 ".",
                                                 file_format, 
                                                 "$")) %>% 
    stringr::str_extract(pattern = "[[:alpha:]]+$")
  
  file_batches <- available_backup_files_v %>% 
    stringr::str_remove(pattern = stringr::str_c(".",
                                                 file_format, 
                                                 "$")) %>% 
    stringr::str_extract(pattern =  "[[:digit:]]+$")
  
  if (is.null(batch)==FALSE) {
    batch <- as.character(batch)
    index_files <- available_backup_files_v[file_types=="index"&file_batches %in% batch]
    contents_files <- available_backup_files_v[file_types=="contents"&file_batches %in% batch]
  } else {
    index_files <- available_backup_files_v[file_types=="index"]
    contents_files <- available_backup_files_v[file_types=="contents"]
  }
  
  if (is.null(restore_to)) {
    restore_to <- cas_get_base_folder(level = "website",
                                      custom_path = restore_to)
  } 
  
  # restore_to <- fs::path(
  #   fs::path_temp(), 
  #   "castarter",
  #   cas_options_l$website
  # )
  
  if (index == TRUE) {
    purrr::walk(
      .progress = "Restoring selected index files",
      .x = index_files,
      .f = function(current_index_file) {
        untar(tarfile = current_index_file,
              exdir = restore_to)
    })
  }
  
  if (contents == TRUE) {
    purrr::walk(
      .progress = "Restoring selected contents files",
      .x = contents_files,
      .f = function(current_contents_file) {
        untar(tarfile = current_contents_file,
              exdir = restore_to)
      })
  }
  
  restore_to
}