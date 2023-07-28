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


#' Get base folder under which files will be stored.
#'
#' @param level Defaults to "website". Valid values are "website", "project", and "base".
#' @param custom_path Defaults to NULL. If given, all other parameters and settings are ignored, and folder is set to this value.
#' @param ... Passed to `cas_get_options()`.
#'
#' @return
#' @export
#'
#' @examples
cas_get_base_folder <- function(...,
                                level = "website",
                                custom_path = NULL) {
  rlang::check_dots_used()

  if (is.null(custom_path) == FALSE) {
    return(custom_path)
  }

  cas_options_l <- cas_get_options(...)

  if (level == "website") {
    path <- fs::path(
      cas_options_l$base_folder,
      cas_options_l$project,
      cas_options_l$website
    )
  } else if (level == "project") {
    path <- fs::path(
      cas_options_l$base_folder,
      cas_options_l$project
    )
  } else if (level == "base") {
    path <- fs::path(
      cas_options_l$base_folder
    )
  }

  path
}
