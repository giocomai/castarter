#' Takes a character vector and returns it with matches of pattern wrapped in
#' html tags used for highlighting
#'
#' @param string A character vector.
#' @param param Pattern to match.
#' @param ignore_case Defaults to `TRUE`.
#'
#' @return A character vector, fo the same length as the input string. Matched
#'   words are bound by the html `<mark>` tag.
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
cass_highlight <- function(
  string,
  pattern,
  highlight = TRUE,
  bold = FALSE,
  ignore_case = TRUE
) {
  if (length(pattern) > 1) {
    pattern <- stringr::str_flatten(string = pattern, collapse = "|")
  }

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

  if (bold & highlight) {
    before_string <- "<mark>**"
    after_string <- "**</mark>"
  } else if (!bold & highlight) {
    before_string <- "<mark>"
    after_string <- "</mark>"
  } else if (bold & !highlight) {
    before_string <- "**"
    after_string <- "**"
  } else if (!bold & !highlight) {
    before_string <- ""
    after_string <- ""
  }

  purrr::map2_chr(
    .progress = "Highlighting",
    .x = split_l,
    .y = extracted_l,
    .f = function(x, y) {
      extracted_marked_v <- stringr::str_c(before_string, y, after_string)
      stringr::str_c(x, c(extracted_marked_v, ""), collapse = "")
    }
  )
}
