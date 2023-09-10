#' Takes a character vector and returns it with matches of pattern wrapped in html tags used for highlighting
#'
#' @param string A character vector.
#' @param param Pattern to match.
#' @param ignore_case Defaults to TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' cass_highlight(
#'   string = c(
#'     "The R Foundation for Statistical Computing",
#'     "R is free software and comes with ABSOLUTELY NO WARRANTY"
#'   ),
#'   pattern = "foundation|software|warranty"
#' )
cass_highlight <- function(string,
                           pattern,
                           ignore_case = TRUE) {
  split_l <- stringr::str_split(
    string = string,
    pattern = stringr::regex(
      pattern = pattern,
      ignore_case = ignore_case
    )
  )

  extracted_l <- stringr::str_extract_all(
    string = string,
    pattern = stringr::regex(
      pattern = pattern,
      ignore_case = ignore_case
    )
  )

  purrr::map2_chr(.progress = TRUE, .x = split_l, .y = extracted_l, .f = function(x, y) {
    extracted_marked_v <- stringr::str_c("<mark>", y, "</mark>")
    stringr::str_c(x, c(extracted_marked_v, ""), collapse = "")
  })
}
