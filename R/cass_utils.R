#' Split string into multiple inputs
#'
#' @param string A text string, typically a user input in a shiny app.
#' @param to_regex Defaults to FALSE. If TRUE collapses the split string,
#'   separating each element with `|`.
#'
#' @return A character vector
#' @export
#'
#' @examples
#' cass_split_string("dogs, cats, horses")
#' cass_split_string(string = "dogs, cats, horses", to_regex = TRUE)
cass_split_string <- function(string,
                              squish = TRUE,
                              to_lower = TRUE,
                              to_regex = FALSE) {
  v <- string |>
    stringr::str_split(
      pattern = ",",
      simplify = TRUE
    ) |>
    as.character()

  if (squish == TRUE) {
    v <- v |>
      stringr::str_squish()
  }

  if (to_lower == TRUE) {
    v <- v %>%
      stringr::str_to_lower()
  }

  if (to_regex == TRUE) {
    v <- v |>
      stringr::str_flatten(collapse = "|")
  }
  v
}


#' Combines a vector of words into a string to be used for regex matching.
#'
#' @param words A character vector of words to be combined for string matching.
#' @param full_words_only Logical, defaults to TRUE. If TRUE, the correspondent
#'   words are matched only when they are a separate word.
#'
#' @return A character vector of length one, ready to be used for regex
#'   matching.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
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
    ) |>
      stringr::str_flatten(collapse = "|")
  } else {
    pattern <- words |>
      stringr::str_flatten(collapse = "|")
  }
  pattern
}


#' Get base folder under which files will be stored.
#'
#' The path is computed based on inputs and settings; it may or may not exist.
#'
#' @param level Defaults to "base". Valid values are "website", "project",
#'   and "base".
#' @param custom_path Defaults to NULL. If given, all other parameters and
#'   settings are ignored, and this value is returned.
#' @param ... Passed to [cas_get_options()].
#'
#' @return Path to the folder well all project files are stored.
#' @export
#'
#' @examples
#'
#'
#' cas_set_options(
#'   base_folder = fs::path(tempdir(), "R", "castarter_data"),
#'   db_folder = fs::path(tempdir(), "R", "castarter_data"),
#'   project = "example_project",
#'   website = "example_website"
#' )
#'
#' cas_get_base_folder()
#'
#' cas_get_base_folder(level = "website")
#' cas_get_base_folder(level = "project")
#' cas_get_base_folder(level = "base")
#'
#' cas_get_base_folder(custom_path = fs::path(tempdir(), "custom_path"))
cas_get_base_folder <- function(...,
                                level = c("base", "project", "website"),
                                custom_path = NULL) {
  rlang::check_dots_used()

  if (is.null(custom_path) == FALSE) {
    return(custom_path)
  }

  cas_options_l <- cas_get_options(...)

  if (level[[1]] == "website") {
    path <- fs::path(
      cas_options_l$base_folder,
      cas_options_l$project,
      cas_options_l$website
    )
  } else if (level[[1]] == "project") {
    path <- fs::path(
      cas_options_l$base_folder,
      cas_options_l$project
    )
  } else if (level[[1]] == "base") {
    path <- fs::path(
      cas_options_l$base_folder
    )
  }

  path
}
