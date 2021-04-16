#' Set folder for caching data
#'
#' Consider using a folder out of your current project directory, e.g. `sn_set_cache_folder("~/R/sn_data/")`: you will be able to use the same cache in different projects, and prevent cached files from being sync-ed if you use services such as Nextcloud or Dropbox.
#'
#' @param path A path to a location used for caching data. If the folder does not exist, it will be created.
#'
#' @return The path to the caching folder, if previously set; the same path as given to the function; or the default, `sn_data` is none is given.
#' @export

#' @examples
#' \dontrun{
#' sn_set_cache_folder("~/R/sn_data/")
#' }
cas_set_options <- function(base_folder = NULL,
                            project = NULL,
                            website = NULL) {
  if (is.null(base_folder)) {
    path <- Sys.getenv("cas_base_folder")
  } else {
    Sys.setenv(cas_base_folder = path)
  }
  if (path == "") {
    path <- fs::path("cas_data")
  }
  path
}
