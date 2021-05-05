#' Count strings in a corpus relative to the number of words
#'
#' @param corpus A textual corpus as a data frame.
#' @param string A character vector of one or more words or strings to be counted.
#' @param text Defaults to text. The unquoted name of the column of the corpus data frame to be used for matching.
#' @param group_by Defaults to NULL. If given, the unquoted name of the column to be used for grouping (e.g. date, or doc_id, or source, etc.)
#' @param ignore_case Defaults to TRUE.
#' @param full_words_only Defaults to FALSE. If FALSE, string is counted even when the it is found in the middle of a word (e.g. if FALSE, "ratio" would be counted as match in the word "irrational").
#' @param locale Locale to be used when ignore_case is set to TRUE. Passed to `stringr::str_to_lower`, defaults to "en".
#' @param n_column_name Defaults to 'n'. The unquoted name of the column to be used for the count in the output.
#' @param string_column_name Defaults to 'word'. The unquoted name of the column to be used for the word in the output (if `include_string` is set to TRUE, as per default).
#'
#' @return A data frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' cas_count_relative(
#'   corpus = corpus,
#'   string = c("dogs", "cats", "horses"),
#'   text = text,
#'   group_by = date,
#'   n_column_name = n
#' )
#' }
#'

cas_count_relative <- function(corpus,
                               string,
                               text = text,
                               group_by = date,
                               ignore_case = TRUE,
                               fixed = FALSE,
                               full_words_only = FALSE,
                               string_column_name = string,
                               n_column_name = n,
                               locale = "en"
) {
  total_words_df <- cas_count_total_words(corpus = corpus,
                                          text = {{ text }},
                                          n_column_name = cas_total,
                                          group_by = {{ group_by }})

  count_df <- cas_count(corpus = corpus,
                        string = string,
                        text = {{ text }},
                        ignore_case = ignore_case,
                        fixed = fixed,
                        full_words_only = full_words_only,
                        group_by = {{ group_by }},
                        n_column_name = cas_n)

  count_df %>%
    dplyr::left_join(y = total_words_df,
                     by = rlang::as_string(rlang::quo_squash(rlang::enquo(group_by)))) %>%
    dplyr::transmute({{ group_by }},
                     {{ string_column_name }},
                     {{n_column_name}} := cas_n/cas_total)


}
