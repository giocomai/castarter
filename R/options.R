# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
CastarterOptions <- settings::options_manager(project = NULL, website = NULL, baseFolder = NULL)

#' Sets 'castarter' options
#'
#' It allows to preliminary store options frequently used by 'castarter', thus removing the requirement to specify them each time a function is called.
#' @param project Name of 'castarter' project. Must correspond to the name of a folder in the current working directory.
#' @param website Name of a website included in a 'castarter' project. Must correspond to the name of a sub-folder of the project folder.
#' @param baseFolder Name of the base folder under which all projects will be stored. This defaults to `castarter` and can be set only trhough `SetCastarter()`. It is usually advisable to leave the defaults and set only project and website names.
#' @return Nothing. Used for its side effects (stores settings).
#' @export
#' @examples
#' SetCastarter(project = "project", website = "website")
SetCastarter <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    CastarterOptions(...)
}
