#' Archive originals of downloaded files in compressed folders
#'
#' @param path Path to archive directory, defaults to NULL. If NULL, path is set to the project/website/archive folder.
#' @param file_format Defaults to "tar.gz", to ensure cross-platform compatibility. No other formats are supported at this stage.
#' @inheritParams cas_read_from_db
#' @return
#' @export
#'
#' @examples
cas_archive <- function(path = NULL,
                        file_format = "tar.gz",
                        index = TRUE,
                        contents = TRUE,
                        remove_original = FALSE,
                        db_connection = NULL,
                        db_folder = NULL,
                        ...) {
  if (cas_check_use_db(...) == FALSE) {
    usethis::ui_stop("Database not set. Set the database connection with `cas_set_options()` or pass database connection with the parameter `db_connection`.")
  }

  db <- cas_connect_to_db(
    db_connection = db_connection,
    read_only = FALSE,
    ...
  )

  cas_options_l <- cas_get_options(...)

  website_folder <- cas_get_base_path(...) %>%
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

  fs::dir_create(path = path)

  original_wd <- getwd()
  on.exit(setwd(original_wd))
  setwd(website_folder)

  if (index == TRUE) {
    index_folders_v <- fs::dir_ls(
      path = ".",
      recurse = FALSE,
      type = "directory",
      glob = "*_index"
    ) %>%
      fs::dir_ls(
        recurse = FALSE,
        type = "directory"
      )

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

        tar(
          tarfile = fs::path(
            fs::path_rel(path),
            current_filename
          ),
          files = current_folder,
          compression = "gzip"
        )
      }
    )
  }


  if (contents == TRUE) {
    contents_folders_v <- fs::dir_ls(
      path = ".",
      recurse = FALSE,
      type = "directory",
      glob = "*_contents"
    ) %>%
      fs::dir_ls(recurse = FALSE, type = "directory")

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

        tar(
          tarfile = fs::path(
            fs::path_rel(path),
            current_filename
          ),
          files = current_folder,
          compression = "gzip"
        )
      }
    )
  }
}
