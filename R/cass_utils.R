#' Split string into multiple inputs
#'
#' @param string A text string, typically a user input in a shiny app.
#'
#' @return A character vector
#'
#' @examples
#' if (interactive()) {
#'   cass_split("dogs, cats, horses")
#' }
cass_split <- function(string,
                       squish = TRUE,
                       to_lower = TRUE) {
  v <- string %>%
    stringr::str_split(
      pattern = ",",
      simplify = TRUE
    ) %>%
    as.character()

  if (squish == TRUE) {
    v <- v %>%
      stringr::str_squish()
  }
  if (to_lower == TRUE) {
    v <- v %>%
      stringr::str_to_lower()
  }
  v
}


#' Combines a vector of words into a string to be used for regex matching.
#'
#' @param words A character vector of words to be combined for string matching.
#' @param full_words_only Logical, defaults to TRUE. If TRUE, the correspondent words are matched only when they are a separate word.
#'
#' @return A character vector of length one, ready to be used for regex matching.
#'
#' @examples
#'
#' words <- c("dogs", "cats", "horses")
#'
#' cass_combine_into_pattern(words)
cass_combine_into_pattern <- function(words,
                                      full_words_only = TRUE) {
  if (full_words_only == TRUE) {
    pattern <- purrr::map_chr(
      .x = words,
      .f = function(x) {
        stringr::str_c(
          "\\b",
          x,
          "\\b"
        )
      }
    ) %>%
      stringr::str_c(collapse = "|")
  } else {
    pattern <- words %>%
      stringr::str_c(collapse = "|")
  }
  pattern
}
