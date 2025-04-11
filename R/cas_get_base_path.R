#' Build full path to base folder where batches of files will be stored.
#'
#' For more information on how the full path is determined, see details.
#'
#' * `base_folder` - path to the folder where all project files are expected to be stored. Can be retrieved with `cas_get_base_folder(level = "base")`, can be set for the whole session with `cas_set_options(base_folder = fs::path(fs::path_temp(), "castarter")`.
#' * `project` - project name, typically set with `cas_set_options(project = "example_project)`
#' * `website` - website name, typically set with `cas_set_options(website = "example_project)`
#' * a combination of `file_format` and either `index`, `contents`, or something different if set with the `custom_folder` argument.
#'
#'
#' @param create_folder_if_missing Logical, defaults to NULL. If NULL, it will
#'   ask before creating a new folder. If TRUE, it will create it without
#'   asking.
#' @param custom_folder Defaults to NULL. Folder name within the website folder
#'   is typically determined by `file_format` and either `index` or `contents`;
#'   if you prefer another option, different from both `index` and `contents`,
#'   you can set it using the `custom_folder` argument.
#' @param file_format Defaults to `html`. Used to determine folder name, and
#'   internally to assume contents type when using functions such as
#'   [cas_extract()].
#' @param custom_path Defaults to NULL. If given, overrides all other inputs,
#'   and is returned as given.
#'
#' @inheritParams cas_get_base_folder
#'
#' @return Path to base folder. A character vector of length one of class
#'   `fs_path`.
#' @export
#'
#' @examples
#' set.seed(1)
#' cas_set_options(project = "example_project",
#'                 website = "example_website", 
#'                 base_folder = fs::path(fs::path_temp(), "castarter"))
#' 
#' cas_get_base_path(create_folder_if_missing = FALSE)
#' 
#' cas_get_base_path(file_format = "html", 
#'                   create_folder_if_missing = FALSE)
#' 
#' cas_get_base_path(file_format = "xml", 
#'                   create_folder_if_missing = FALSE)
#' 
#' cas_get_base_path(index = TRUE,
#'                   create_folder_if_missing = FALSE)
cas_get_base_path <- function(create_folder_if_missing = NULL,
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
    if (!is.null(custom_folder)) {
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
  } else {
    path <- custom_path
  }

  if (fs::dir_exists(path) == FALSE) {
    cli::cli_inform(message = c(
      i = "Folder for {.field {type}} files with file format {.field {file_format}} does not exist:",
      i = "{.path {path}}"
    ))
    if (is.null(create_folder_if_missing)) {
      create_folder_if_missing <- usethis::ui_yeah(x = "Do you want to create it?")
    }

    if (create_folder_if_missing) {
      fs::dir_create(path = path)
      usethis::ui_info(stringr::str_c("The folder",
        usethis::ui_path(path),
        "has been created.",
        sep = " "
      ))
    } else {
      cli::cli_warn(message = c(`!` = "Base path returned, but the folder does not exist and has not been created."))
    }
  }
  fs::path(path)
}
