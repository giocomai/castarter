#' Read datasets created with `cas_write_dataset`
#'
#' @inheritParams cas_write_dataset
#'
#' @return A dataset as `ArrowObject`
#' @export
#'
#' @examples
#' \dontrun{
#'   cas_read_dataset()
#' }
cas_read_dataset <- function(file_format = "parquet", 
                             ...) {
  
  latest_dataset_folder <- fs::dir_ls(
    path = fs::path(cas_get_base_path(...) %>%
                      fs::path_dir(),
                    "export",
                    "dataset",
                    file_format %>%
                      stringr::str_replace(
                        pattern = stringr::fixed("."),
                        replacement = "_"
                      )),
    type = "directory"
  ) %>% 
    tail(1)
  
  arrow::open_dataset(sources = latest_dataset_folder)
}