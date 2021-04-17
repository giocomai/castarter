#' Count words in a corpus
#'
#' @param corpus A textual corpus as a data frame.
#' @param words A character vector of one or more words to be counted.
#' @param text Defaults to text. The unquoted name of the column of the corpus data frame to be used for matching.
#' @param group_by Defaults to NULL. If given, the unquoted name of the column to be used for grouping (e.g. date, or doc_id, or source, etc.)
#' @param ignore_case Defaults to TRUE.
#' @param full_words_only Defaults to TRUE. If FALSE, string is counted even when the it is found in the middle of a word (e.g. if FALSE, "ratio" would be counted as match in the word "irrational").
#' @param locale Locale to be used when ignore_case is set to TRUE. Passed to `stringr::str_to_lower`, defaults to "en".
#' @param n_column_name Defaults to 'n'. The unquoted name of the column to be used for the count in the output.
#' @param word_column_name Defaults to 'word'. The unquoted name of the column to be used for the word in the output (if `word_column` is set to TRUE, as per default).
#' @param word_column Logical, defaults to TRUE. If true, includes a column with the given word as defined by the param `word_column_name`.

#'
#' @return A data frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' cas_count(
#'   corpus = corpus,
#'   words = c("dogs", "cats", "horses"),
#'   text = text,
#'   group_by = date,
#'   n_column_name = n
#' )
#' }
#'
cas_count <- function(corpus,
                      words,
                      text = text,
                      group_by = date,
                      ignore_case = TRUE,
                      fixed = FALSE,
                      full_words_only = FALSE,
                      date_column_name = date,
                      word_column_name = word,
                      n_column_name = n,
                      locale = "en",
                      include_word = TRUE) {
  if (isTRUE(full_words_only)) {
    words <- purrr::map_chr(.x = words,
                            .f = function(x) {
                              stringr::str_c(
                                "\\b",
                                x,
                                "\\b"
                              )
                            })
  }

  purrr::map_dfr(.x = words,
                 .f = function(x) {
                   cas_count_single(corpus = corpus,
                             words = x,
                             text = {{ text }},
                             group_by = {{ group_by }},
                             ignore_case = ignore_case,
                             full_words_only = full_words_only,
                             date_column_name = {{ date_column_name }},
                             word_column_name = {{ word_column_name }},
                             n_column_name = {{ n_column_name }},
                             locale = locale,
                             include_word = TRUE)

                 }
  )
}

# Actually does the counting, but accepts only vectors of length 1 as words
cas_count_single <- function(corpus,
                             words,
                             text = text,
                             group_by = date,
                             ignore_case = TRUE,
                             fixed = FALSE,
                             full_words_only = FALSE,
                             date_column_name = date,
                             word_column_name = word,
                             n_column_name = n,
                             locale = "en",
                             include_word = TRUE) {
  pattern <- words

  if (ignore_case == TRUE) {
    corpus <- corpus %>%
      dplyr::mutate({{ text }} := {{ text }} %>%
                      stringr::str_to_lower(locale = locale))

    pattern <- pattern %>%
      stringr::str_to_lower(locale = locale)
  }

  output_df <- corpus %>%
    dplyr::group_by({{ group_by }}) %>%
    dplyr::summarise({{ n_column_name }} := stringr::str_count(
      string = {{ text }},
      pattern = pattern
    ) %>%
      sum(na.rm = TRUE),
    .groups = "drop"
    )

  if (include_word == TRUE) {
    output_df %>%
      dplyr::transmute({{ date_column_name }},
                       {{ word_column_name }} := stringr::str_c(words,
                                                             collapse = ", "),
                       {{ n_column_name }})
  } else {
    output_df
  }
}
