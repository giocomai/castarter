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
  cas_options <- cas_get_options(
    base_folder = base_folder,
    project = project,
    website = website
  )
  fs::path(
    cas_options$base_folder,
    cas_options$project,
    cas_options$website
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
cas_get_db <- function(base_folder = NULL,
                       project = NULL,
                       website = NULL) {
  cas_options <- cas_get_options(
    base_folder = base_folder,
    project = project,
    website = website
  )

  db_file <- fs::path(
    cas_get_website_folder(
      base_folder = base_folder,
      project = project,
      website = website
    ),
    stringr::str_c(cas_options$website, ".sqlite")
  )

  DBI::dbConnect(drv = RSQLite::SQLite(), database = db_file)
}
