#' Open in a browser a URL
#'
#' @param random Defaults to 1. 
#' 
#' @inheritParams cas_download
#'
#' @return
#' @export
#'
#' @examples
cas_browse <- function(index = FALSE, 
                       remote = FALSE,
                       id = NULL, 
                       batch = NULL,
                       index_group = FALSE,
                       file_format = "html",
                       random = 1,
                       ...) {
  
  type <- dplyr::if_else(condition = index,
                         true = "index",
                         false = "contents"
  )
  
  db <- cas_connect_to_db(...)
  
  if (remote == FALSE) {
    local_files <- cas_get_path_to_files(index = index,
                                         file_format = file_format,
                                         db_connection = db,
                                         random = random,
                                         ...)
    
    local_files %>% 
      dplyr::slice(1) %>% 
      browseURL()
  } else {
    #TODO
  }

  
}