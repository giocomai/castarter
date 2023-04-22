#' Count strings in a corpus
#'
#' @param corpus A textual corpus as a data frame.
#' @param pattern A character vector of one or more words or strings to be counted.
#' @param text Defaults to text. The unquoted name of the column of the corpus data frame to be used for matching.
#' @param group_by Defaults to NULL. If given, the unquoted name of the column to be used for grouping (e.g. date, or doc_id, or source, etc.)
#' @param ignore_case Defaults to TRUE.
#' @param drop_na Defaults to TRUE. If TRUE, all rows where either date or text is NA are removed before further processing.
#' @param full_words_only Defaults to FALSE. If FALSE, string is counted even when the it is found in the middle of a word (e.g. if FALSE, "ratio" would be counted as match in the word "irrational").
#' @param locale Locale to be used when ignore_case is set to TRUE. Passed to `stringr::str_to_lower`, defaults to "en".
#' @param n_column_name Defaults to 'n'. The unquoted name of the column to be used for the count in the output.
#' @param string_column_name Defaults to 'word'. The unquoted name of the column to be used for the word in the output (if `include_string` is set to TRUE, as per default).
#'
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' cas_count(
#'   corpus = corpus,
#'   pattern = c("dogs", "cats", "horses"),
#'   text = text,
#'   group_by = date,
#'   n_column_name = n
#' )
#' }
#'
cas_count <- function(corpus,
                      pattern,
                      text = text,
                      group_by = date,
                      ignore_case = TRUE,
                      drop_na = TRUE,
                      fixed = FALSE,
                      full_words_only = FALSE,
                      pattern_column_name = pattern,
                      n_column_name = n,
                      locale = "en") {
  if (drop_na == TRUE) {
    corpus <- corpus %>%
      dplyr::filter(is.na({{ text }}) == FALSE, is.na({{ group_by }}) == FALSE)
  }

  if (isTRUE(full_words_only)) {
    pattern <- purrr::map_chr(
      .x = pattern,
      .f = function(x) {
        stringr::str_c(
          "\\b",
          x,
          "\\b"
        )
      }
    )
  }

  purrr::map(
    .x = pattern,
    .f = function(x) {
      cas_count_single(
        corpus = corpus,
        pattern = x,
        text = {{ text }},
        group_by = {{ group_by }},
        ignore_case = ignore_case,
        full_words_only = full_words_only,
        pattern_column_name = {{ pattern_column_name }},
        n_column_name = {{ n_column_name }},
        locale = locale
      )
    }
  ) %>% 
    purrr::list_rbind()
}

# Actually does the counting, but accepts only vectors of length 1 as pattern
cas_count_single <- function(corpus,
                             pattern,
                             text = text,
                             group_by = date,
                             ignore_case = TRUE,
                             fixed = FALSE,
                             full_words_only = FALSE,
                             pattern_column_name = word,
                             n_column_name = n,
                             locale = "en") {
  if (ignore_case == TRUE) {
    corpus <- corpus %>%
      dplyr::mutate({{ text }} := {{ text }} %>%
        stringr::str_to_lower(locale = locale))

    pattern <- pattern %>%
      stringr::str_to_lower(locale = locale)
  }

  output_df <- corpus %>%
    dplyr::mutate({{ n_column_name }} := stringr::str_count(
      string = {{ text }},
      pattern = !!pattern
    )) %>% 
    dplyr::group_by({{ group_by }}) %>% 
    dplyr::summarise(
      {{ n_column_name }} := sum({{ n_column_name }}, na.rm = TRUE),
      .groups = "drop"
    )

  output_df %>%
    dplyr::transmute(
      {{ group_by }},
      {{ pattern_column_name }} := pattern,
      {{ n_column_name }}
    ) %>% 
    dplyr::collect()
}


#' Count total words in a dataset
#'
#' @param corpus A textual corpus as a data frame.
#' @param pattern
#' @param text
#' @param group_by
#' @param ignore_case
#' @param n_column_name
#' @param locale
#'
#' @return
#' @export
#'
#' @examples
cas_count_total_words <- function(corpus,
                                  pattern = "[\\w\']+",
                                  text = text,
                                  group_by = date,
                                  ignore_case = TRUE,
                                  n_column_name = n,
                                  locale = "en") {
  cas_count_single(
    corpus = corpus,
    pattern = pattern,
    text = {{ text }},
    group_by = {{ group_by }},
    ignore_case = FALSE,
    fixed = FALSE,
    full_words_only = FALSE,
    pattern_column_name = cas_pattern,
    n_column_name = {{ n_column_name }},
    locale = locale
  ) %>%
    dplyr::select(-cas_pattern)
}
