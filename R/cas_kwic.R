#' Adds a column with n words before and after the selected string to see keywords in context
#'
#' @param corpus A textual corpus as a data frame.
#' @param string A string, typically of one or more words, to be used to break text. Should be of length 1 or length equal to the number of rows.
#' @param text Defaults to text. The unquoted name of the column of the corpus data frame to be used for matching.
#' @param words_before Integer, defaults to 5. Number of columns to include in the `before` column.
#' @param words_after Integer, defaults to 5. Number of columns to include in the `after` column.
#' @param same_sentence Logical, defaults to TRUE. If TRUE, before and after include only words found in the sentence including the matched string.
#' @param ignore_case Defaults to TRUE.
#' @param full_words_only Defaults to FALSE. If FALSE, string is counted even when it is found in the middle of a word (e.g. if FALSE, "ratio" would be counted as match in the word "irrational").
#' @param string_column_name Defaults to 'string'. The unquoted name of the column to be used for the word in the output.
#'
#' @return A data frame (a tibble), with the same columns as input, plus three columns: before, string, and after. Only rows where the string is found are included.
#' @export
#'
#' @examples
#'
#' cas_kwic(corpus = tifkremlinen::kremlin_en,
#'          string = c("china", "india))
#'
#'
cas_kwic <- function(corpus,
                     string,
                     text = text,
                     words_before = 5,
                     words_after = 5,
                     same_sentence = TRUE,
                     ignore_case = TRUE,
                     full_words_only = FALSE,
                     string_column_name = string) {

  purrr::map_dfr(.x = string,
                 .f = function(x) {
                   cas_kwic_single_string(corpus = corpus,
                                          string = x,
                                          text = {{ text }},
                                          words_before = words_before,
                                          words_after = words_after,
                                          ignore_case = ignore_case,
                                          full_words_only = full_words_only)
                 })


}



#' Adds a column with n words before and after the selected string to see keywords in context
#'
#' @param corpus A textual corpus as a data frame.
#' @param string A string, typically of one or more words, to be used to break text. Should be of length 1 or length equal to the number of rows.
#' @param text Defaults to text. The unquoted name of the column of the corpus data frame to be used for matching.
#' @param words_before Integer, defaults to 5. Number of columns to include in the `before` column.
#' @param words_after Integer, defaults to 5. Number of columns to include in the `after` column.
#' @param same_sentence Logical, defaults to TRUE. If TRUE, before and after include only words found in the sentence including the matched string.
#' @param ignore_case Defaults to TRUE.
#' @param full_words_only Defaults to FALSE. If FALSE, string is counted even when the it is found in the middle of a word (e.g. if FALSE, "ratio" would be counted as match in the word "irrational"). Set to FALSE also if you want to use your own regex.
#' @param string_column_name Defaults to 'string'. The unquoted name of the column to be used for the word in the output.
#'
#' @return A data frame (a tibble), with the same columns as input, plus three columns: before, string, and after. Only rows where the string is found are included.
#' @export
#'
#' @examples
#'
#' cas_kwic(corpus = tifkremlinen::kremlin_en,
#'          string = "West")
#'
#'
cas_kwic_single_string <- function(corpus,
                     string,
                     text = text,
                     words_before = 5,
                     words_after = 5,
                     same_sentence = TRUE,
                     ignore_case = TRUE,
                     full_words_only = FALSE,
                     string_column_name = string) {

  if (full_words_only==TRUE) {
    string <- purrr::map_chr(.x = string,
                             .f = function(x) {
                               stringr::str_c(
                                 "\\b",
                                 x,
                                 "\\b"
                               )
                             })
  }


  if (length(string)==1) {
    corpus <- corpus %>%
      dplyr::filter(stringr::str_detect(string = {{ text }},
                                        pattern = string)) %>%
      dplyr::filter(is.na({{ text }})==FALSE)
  } else {
    usethis::ui_stop("String must be a vector of length one. You may want to use `cas_kwic()` instead.")
  }

  if (same_sentence==TRUE) {
    corpus <- corpus %>%
      tidytext::unnest_tokens(output = {{ text }},
                              input = {{ text }},
                              to_lower = FALSE,
                              token = "sentences")
  }


  all_words_location_l <- stringr::str_locate_all(
    string = corpus %>% dplyr::pull({{ text }}),
    pattern = stringr::boundary(type = c("word"))
  )

  string_location_l <- stringr::str_locate_all(
    string = corpus %>% dplyr::pull({{ text }}),
    pattern = stringr::regex(pattern = string,
                             ignore_case = ignore_case)
  )

  pb <- progress::progress_bar$new(total = length(all_words_location_l))

  output <- purrr::pmap_dfr(.l = list(
    all_words_location_l,
    string_location_l,
    corpus %>% dplyr::pull({{ text }}),
    seq_along(corpus %>% dplyr::pull({{ text }}))
  ),
  .f = function(current_all_words_location_l,
                current_string_location_l,
                current_text,
                i) {
    pb$tick()
    if (nrow(current_string_location_l)==0) return(NULL)

    purrr::map_dfr(
      .x = 1:nrow(current_string_location_l),
      .f = function(j){
        current_match <- current_string_location_l[j,]

        current_match_row_number <- current_all_words_location_l %>%
          tibble::as_tibble() %>%
          dplyr::mutate(row_number = dplyr::row_number()) %>%
          dplyr::filter(current_match[1]>=start&current_match[1]<=end) %>%
          dplyr::pull(.data$row_number)


        dplyr::bind_cols(
          corpus %>% dplyr::slice(i),
          tibble::tibble(before = stringr::str_sub(
            string = current_text,
            start = current_all_words_location_l[max(1,current_match_row_number-words_before),1],
            end = current_string_location_l[1,1]-1
          ),
          {{ string_column_name }} := stringr::str_sub(
            string = current_text,
            start = current_match[1],
            end = current_match[2]
          ),
          after = stringr::str_sub(
            string = current_text,
            start = current_string_location_l[1,2]+1,
            end = current_all_words_location_l[min(length(current_all_words_location_l)/2, current_match_row_number+words_after),2]
          )
          )
        )
      })
  })

  output

}
