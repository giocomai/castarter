#' Build full path to base working folder
#'
#' @param custom_folder
#' @param file_format
#'
#' @inheritParams cas_get_base_folder
#'
#' @return Path to base folder. A character vector of length one of class `fs_path`.
#' @export
#'
#' @examples
cas_get_base_path <- function(custom_path = NULL,
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
    fs::dir_create(path = path)
    usethis::ui_info(stringr::str_c("The folder",
      usethis::ui_path(path),
      "has been created.",
      sep = " "
    ))
  }
  fs::path(path)
}
