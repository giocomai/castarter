#' Get folder were files and data related to the current website are stored
#'
#' @inheritParams cas_get_options
#' @return A path to a folder.
#' @export
#'
#' @examples
#'
#' cas_get_website_folder()
cas_get_website_folder <- function(base_folder = NULL,
                                   project = NULL,
                                   website = NULL) {
  cas_options_l <- cas_get_options(
    base_folder = base_folder,
    project = project,
    website = website
  )
  fs::path(
    cas_options_l$base_folder,
    cas_options_l$project,
    cas_options_l$website
  )
}

#' Checks if current website folder exists
#'
#' Parameters can be left to NULL; it will then rely on parameters set with `cas_set_options()`
#'
#' @inheritParams cas_get_options
#'
#' @return Logical, TRUE if website folder exists, FALSE if it does not.
#' @export
#'
#' @examples
cas_check_website_folder <- function(base_folder = NULL,
                                     project = NULL,
                                     website = NULL) {
  cas_options_l <- cas_get_options(
    base_folder = base_folder,
    project = project,
    website = website
  )

  fs::dir_exists(
    fs::path(
      cas_options_l$base_folder,
      cas_options_l$project,
      cas_options_l$website
    )
  )
}


#' Get connection to database with details about current website
#'
#' @inheritParams cas_get_options
#'
#' @return
#' @export
#'
#' @examples
#'
#' cas_get_db(
#'   base_folder = fs::path_temp(),
#'   project = "example_project",
#'   website = "example_website"
#' )
cas_get_db <- function(db_folder = NULL,
                       base_folder = NULL,
                       project = NULL,
                       website = NULL) {
  db_file <- cas_get_db_file(
    db_folder = db_folder,
    base_folder = base_folder,
    project = project,
    website = website
  )

  DBI::dbConnect(drv = RSQLite::SQLite(), database = db_file)
}
