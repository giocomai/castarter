#' Archive originals of downloaded files in compressed folders
#'
#' @param path Path to archive directory, defaults to NULL. If NULL, path is set
#'   to the project/website/archive folder.
#' @param file_format Defaults to "tar.gz", to ensure cross-platform
#'   compatibility. No other formats are supported at this stage.
#' @param remove_original Defaults to TRUE. If TRUE, after local files have been
#'   confirmed to be stored in the relevant compressed file, they are removed
#'   from their original folders, and the empty folders deleted.
#' @inheritParams cas_read_from_db
#' @return
#' @export
#'
#' @examples
cas_archive <- function(path = NULL,
                        file_format = "tar.gz",
                        index = TRUE,
                        contents = TRUE,
                        remove_original = TRUE,
                        db_connection = NULL,
                        db_folder = NULL,
                        ...) {
  # if (cas_check_use_db(...) == FALSE) {
  #   cli::cli_abort(message = c(x = "Database not set.",
  #                              i = " Set the database connection with `cas_set_options()` or pass a database connection with the parameter `db_connection`."))
  # }
  # 
  # db <- cas_connect_to_db(
  #   db_connection = db_connection,
  #   read_only = FALSE,
  #   ...
  # )

  cas_options_l <- cas_get_options(...)

  website_folder <- cas_get_base_path(create_if_missing = FALSE,
                                      ...) %>%
    fs::path_dir()

  if (is.null(path) == TRUE) {
    path <- fs::path(
      website_folder,
      "archive",
      fs::path_sanitize(Sys.time(),
        replacement = "_"
      ) %>%
        stringr::str_replace(
          pattern = " ",
          replacement = "-"
        )
    )
  }

  n_folders_to_archive <- 0
  
  if (index==TRUE) {
    base_index_folders_v <- fs::dir_ls(
      path = ".",
      recurse = FALSE,
      type = "directory",
      glob = "*_index"
    )
    
    index_folders_v <- base_index_folders_v %>%
      fs::dir_ls(
        recurse = FALSE,
        type = "directory"
      )
    
    n_folders_to_archive <- sum(n_folders_to_archive, length(index_folders_v))
  }
  
  if (contents == TRUE) {
    base_contents_folders_v <- fs::dir_ls(
      path = ".",
      recurse = FALSE,
      type = "directory",
      glob = "*_contents"
    )
    contents_folders_v <- base_contents_folders_v %>%
      fs::dir_ls(
        recurse = FALSE,
        type = "directory"
      )
    
    n_folders_to_archive <- sum(n_folders_to_archive, length(contents_folders_v))
  }
  
  if (n_folders_to_archive==0) {
    cli::cli_inform(message = c(v = "No new files to archive for the current website."))
    return(invisible(NULL))
  }
  
  fs::dir_create(path = path)

  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(website_folder)

  if (index == TRUE) {

    pb <- progress::progress_bar$new(total = length(index_folders_v))

    purrr::walk(
      .x = index_folders_v,
      .f = function(current_folder) {
        pb$tick()
        current_filename <- stringr::str_c(
          fs::path_dir(current_folder) %>%
            fs::path_file(),
          "_",
          fs::path_file(current_folder),
          ".",
          file_format
        )

        tarfile_v <- fs::path(
          fs::path_rel(path),
          current_filename
        )

        tar(
          tarfile = tarfile_v,
          files = current_folder,
          compression = "gzip"
        )

        if (remove_original == TRUE) {
          ## check if files actually stored

          tarred_files_v <- untar(tarfile = tarfile_v, list = TRUE)
          local_files_v <- fs::dir_ls(path = current_folder)

          if (sum(local_files_v %in% tarred_files_v) == length(local_files_v)) {
            fs::dir_delete(path = current_folder)
          } else {
            cli::cli_abort(c("Something's not right: not all locally stored files in folder {.folder {fs::path_real(current_folder)}} are included in the newly generated {.path {fs::path_real(tarfile_v)}}",
              i = "Archiving process has been stopped."
            ))
          }
        }
      }
    )
    # remove folders if empty
    purrr::walk(
      .x = base_index_folders_v,
      .f = function(current_index_folder) {
        files_in_folder_v <- fs::dir_ls(
          path = current_index_folder,
          all = TRUE,
          recurse = TRUE
        )
        if (length(files_in_folder_v) == 0) {
          fs::dir_delete(current_index_folder)
        } else {
          absolute_path <- fs::path_abs(current_index_folder)
          cli::cli_inform(message = "Index folders have been archived, but some files or folder are still present in {.path {absolute_path}}")
        }
      }
    )
  }


  if (contents == TRUE) {
    base_contents_folders_v <- fs::dir_ls(
      path = ".",
      recurse = FALSE,
      type = "directory",
      glob = "*_contents"
    )
    contents_folders_v <- base_contents_folders_v %>%
      fs::dir_ls(
        recurse = FALSE,
        type = "directory"
      )

    pb <- progress::progress_bar$new(total = length(contents_folders_v))

    purrr::walk(
      .x = contents_folders_v,
      .f = function(current_folder) {
        pb$tick()
        current_filename <- stringr::str_c(
          fs::path_dir(current_folder) %>%
            fs::path_file(),
          "_",
          fs::path_file(current_folder),
          ".",
          file_format
        )

        tarfile_v <- fs::path(
          fs::path_rel(path),
          current_filename
        )

        tar(
          tarfile = tarfile_v,
          files = current_folder,
          compression = "gzip"
        )

        if (remove_original == TRUE) {
          ## check if files actually stored
          tarred_files_v <- untar(tarfile = tarfile_v, list = TRUE)
          local_files_v <- fs::dir_ls(path = current_folder)

          if (sum(local_files_v %in% tarred_files_v) == length(local_files_v)) {
            fs::dir_delete(path = current_folder)
          } else {
            cli::cli_abort(c("Something's not right: not all locally stored files in folder {.folder {fs::path_real(current_folder)}} are included in the newly generated {.path {fs::path_real(tarfile_v)}}",
              i = "Archiving process has been stopped."
            ))
          }
        }
      }
    )
    # remove folders if empty
    purrr::walk(
      .x = base_contents_folders_v,
      .f = function(current_contents_folder) {
        files_in_folder_v <- fs::dir_ls(
          path = current_contents_folder,
          all = TRUE,
          recurse = TRUE
        )
        if (length(files_in_folder_v) == 0) {
          fs::dir_delete(current_contents_folder)
        } else {
          absolute_path <- fs::path_abs(current_contents_folder)
          cli::cli_inform(message = "Index folders have been archived, but some files or folder are still present in {.path {absolute_path}}")
        }
      }
    )
  }
}
