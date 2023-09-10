#' Backup files to Google Drive
#'
#' @param glob A character vector with all glob selectors for the type of files
#'   to be stored. Defaults to `c("*.tar.gz", "*.sqlite")`.
#' @param email If given, email of the Google account to use for storing files.
#' @param scopes Defaults to `drive.file`, i.e., only give access to files
#'   created with this client. This means that no access to other files and
#'   folders on your Google Drive is ever given to this session.
#' @param client Google app client, defaults to `castarter`'s own. Passed to
#'   `googledrive::drive_auth_configure`. Set to NULL to use `googledrive`'s
#'   defaults.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
cas_backup_gd <- function(glob = c(
                            "*.tar.gz",
                            "*.sqlite"
                          ),
                          email = gargle::gargle_oauth_email(),
                          scopes = "https://www.googleapis.com/auth/drive.file",
                          client = cas_google_client,
                          ...) {
  if (requireNamespace("rbackupr", quietly = TRUE) == FALSE) {
    cli::cli_abort(x = "To use this function you need to install the package `rbackupr`. See https://github.com/giocomai/rbackupr")
  }
  googledrive::drive_auth_configure(client = cas_google_client)

  googledrive::drive_auth(
    email = email,
    scopes = scopes
  )

  rbackupr::rb_enable_cache()

  rbackupr::rb_set_cache_folder(path = cas_get_options(...)$base_folder)

  rbackupr::rb_set_project(project = cas_get_options(...)$project)

  rbackupr::rb_get_project(
    project = cas_get_options(...)$project,
    create = TRUE,
    base_folder = cas_get_options(...)$base_folder
  )

  purrr::walk(
    .x = glob,
    .f = function(current_glob) {
      cli::cli_progress_message("Backup of {.field {current_glob}} in progress")
      rbackupr::rb_backup(
        path = fs::path(
          cas_get_options(...)$base_folder,
          cas_get_options(...)$project
        ),
        base_folder = cas_get_options(...)$base_folder,
        project = cas_get_options(...)$project,
        glob = current_glob
      )
    }
  )
}
