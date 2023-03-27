#' Build full path to base working folder
#'
#' @param create_if_missing Logical, defaults to NULL. If NULL, it will ask
#'   before creating a new folder. If TRUE, it will create it without asking.
#' @param custom_folder
#' @param file_format
#'
#' @inheritParams cas_get_base_folder
#'
#' @return Path to base folder. A character vector of length one of class `fs_path`.
#' @export
#'
#' @examples
cas_get_base_path <- function(create_if_missing = NULL,
                              custom_path = NULL,
                              custom_folder = NULL,
                              index = FALSE,
                              file_format = "html",
                              ...) {
  type <- dplyr::if_else(condition = index,
    true = "index",
    false = "contents"
  )

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
    cli::cli_inform(message = c(
      i = "Folder for {.field {type}} files with file format {.field {file_format}} does not exist:",
      i = "{.path {path}}"
    ))
    if (is.null(create_if_missing)) {
      create_if_missing <- usethis::ui_yeah(x = "Do you want to create it?")
    }

    if (create_if_missing) {
      fs::dir_create(path = path)
      usethis::ui_info(stringr::str_c("The folder",
        usethis::ui_path(path),
        "has been created.",
        sep = " "
      ))
    } else {
      cli::cli_warn(message = c(`!` = "Base path returned, but the folder does not exist and has not been created"))
    }
  }
  fs::path(path)
}
