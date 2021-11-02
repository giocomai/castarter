#' Set folder for storing project files
#'
#' Your project folder can be anywhwere on your file system. Considering that this is where possibly a very large number of html files will be downloaded, it is usually preferrable to choose a location that is not included in live backups.
#'
#' These settings determine the names given to these hierarchical folders: `website` folder will be under `project` folder which will be under the `base_folder`.
#'
#' @param base_folder A path to a location used for storing html and other project files. If the folder does not exist, it will be created. Defaults to `castarter_data`
#' @param project Project name. This will be used as first level folder and may be used elsewhere to describe the dataset.
#' @param website Website name. This will be used as a second level folder and may be used elsewhere to describe the dataset.
#'
#' @return A list object with the newly set (or previously set if left to NULL) options,
#' @export

#' @examples
#' \dontrun{
#' cas_set_options(base_folder = "~/R/castarter_projects/")
#' }
cas_set_options <- function(base_folder = NULL,
                            project = NULL,
                            website = NULL) {
  if (is.null(base_folder)) {
    base_folder <- Sys.getenv("castarter_base_folder")
  } else {
    Sys.setenv(castarter_base_folder = base_folder)
  }
  if (base_folder == "") {
    base_folder <- fs::path("castarter_data")
  }


  if (is.null(project)) {
    project <- Sys.getenv("castarter_project")
  } else {
    Sys.setenv(castarter_project = project)
  }

  if (is.null(website)) {
    website <- Sys.getenv("castarter_website")
  } else {
    Sys.setenv(castarter_website = website)
  }

  invisible(list(base_folder = base_folder,
                 project = project,
                 website = website))
}

#' Retrieve options
#'
#' @param base_folder A path to a location used for storing html and other project files. If the folder does not exist, it will be created. Defaults to `castarter_data`
#' @param project Project name. This will be used as first level folder and may be used elsewhere to describe the dataset.
#' @param website Website name. This will be used as a second level folder and may be used elsewhere to describe the dataset.
#'
#' @return A list object with the previously set options,
#' @export

#' @examples
#'
#' cas_get_options()
#'
cas_get_options <- function(base_folder = NULL,
                            project = NULL,
                            website = NULL) {
  if (is.null(base_folder)) {
    base_folder <- Sys.getenv("castarter_base_folder")
  }
  if (base_folder == "") {
    base_folder <- fs::path("castarter_data")
  }

  if (is.null(project)) {
    project <- Sys.getenv("castarter_project")
  }

  if (is.null(website)) {
    website <- Sys.getenv("castarter_website")
  }

  if (is.null(project)|is.null(website)) {
    usethis::ui_stop(x = "Both project and website must be set, either with {usethis::ui_code('cas_set_options()')} or directly as a parameter.")
  }

  list(base_folder = base_folder,
       project = project,
       website = website)
}

