#' Summarise for a given time period word counts, typically calculatd with `cas_count()`
#'
#' @param count_df A data frame. Must include at least a column with a date or date-time column and a column with number of occurrences for the given time.
#' @param date Defaults to `date`. Unquoted name of a column having either date or date-time as class.
#' @param n Unquoted to `n`.  Unquoted name of a column having number of occurrences per time unit.
#' @param period Defaults to NULL. A string describing the time unit to be used for summarising. Possible values include "year", "quarter", "month", "day", "hour", "minute", "second", "millisecond".
#' @param f Defaults to `mean`. Function to be applied over n for all the values in a given time period. Common alterantives would be `mean` or `median`.
#' @param auto_convert Defaults to FALSE. If FALSE, the date column is returned using the same format as the input; the minimun vale in the given group is used for reference (e.g. all values for January 2022 are summarised as 2021-01-01 it the data were originally given as dates.). If TRUE, it tries to adapt the output to the most intuitive correspondent type; for year, a numeric column with only the year number, for quarter in the format 2022.1, for month in the format 2022-01.
#' @param every Check `?slider::slide_period()` for details.
#' @param before Check `?slider::slide_period()` for details.
#' @param after Check `?slider::slide_period()` for details.
#' @param complete Check `?slider::slide_period()` for details.
#'
#' @return A data frame with two columns: the name of the period, and the same name originally used for `n`.
#' @export
#'
#' @examples
#' \dontrun{
#' # this assumes dates are provided in a column called date
#' corpus_df %>%
#'  cas_count(words = "example",
#'            group_by = date) %>%
#'  cas_summarise(period = "year")
#' }
#'
cas_summarise <- function(
  count_df,
  date = date,
  n = n,
  period = NULL,
  f = mean,
  every = 1L,
  before = 0L,
  after = 0L,
  complete = FALSE,
  auto_convert = FALSE) {

  if (is.null(period)) {
    summarised <- count_df %>%
      dplyr::mutate({{ n }} := slider::slide_index_dbl(.x = {{ n }},
                                                       .i = {{ date }},
                                                       .f = f,
                                                       .before = before,
                                                       .after = after))

  } else {
    summarised <- count_df %>%
      dplyr::mutate({{ date }} := lubridate::floor_date(x = {{ date }},
                                                       unit = period)) %>%
      dplyr::group_by({{ date }}) %>%
      dplyr::summarise({{ n }} := sum({{ n }}),
                       .groups = "drop") %>%
      dplyr::mutate(n = slider::slide_period_dbl(.x = {{ n }},
                                                 .i = {{ date }},
                                                 .period = period,
                                                 .f = f,
                                                 .before = before,
                                                 .after = after))
  }

  if (auto_convert == TRUE) {
    if (period=="year") {
      summarised %>%
        dplyr::transmute(year = lubridate::year({{ date }}), {{ n }})
    } else if (period == "quarter") {
      summarised %>%
        dplyr::transmute(
          quarter = lubridate::quarter(x = {{ date }},
                                       with_year = TRUE) %>%
            as.character(),
          {{ n }})
    } else if (period == "month") {
      summarised %>%
        dplyr::transmute(
          month = stringr::str_extract(string = {{ date }},
                                       pattern = "[:digit:]{4}-[:digit:]{2}"),
          {{ n }})
    } else if (period == "day") {
      summarised %>%
        dplyr::transmute(
          date = as.Date({{ date }}),
          {{ n }} )
    } else {
      summarised
    }
  } else {
    summarised
  }

}











cas_summarise_legacy <- function(
  count_df,
  date = date,
  n = n,
  period = "year",
  f = sum,
  auto_convert = FALSE,
  every = 1L,
  before = 0L,
  after = 0L,
  complete = FALSE) {
  summarised <- slider::slide_period_dfr(
    .x = count_df,
    .i = count_df %>%
      dplyr::pull({{ date }}),
    .period = period,
    .f = ~tibble::tibble(
      {{ date }} := lubridate::floor_date(x = .x %>%
                                            dplyr::pull({{ date }}),
                                          unit = period) %>%
        unique(),
      {{ n }} := f(.x %>%
                     dplyr::pull({{ n }}))
    ),
    .every = every,
    .before = before,
    .after = after,
    .complete = complete
  )

  if (auto_convert == TRUE) {
    if (period=="year") {
      summarised %>%
        dplyr::mutate({{ date }} := lubridate::year({{ date }}))
    } else if (period == "quarter") {
      summarised %>%
        dplyr::mutate(
          quarter = lubridate::quarter(x = quarter, with_year = TRUE) %>%
            as.character())
    } else if (period == "month") {
      summarised %>%
        dplyr::mutate(
          month = stringr::str_extract(string = month,
                                       pattern = "[:digit:]{4}-[:digit:]{2}"))
    } else if (period == "day") {
      summarised %>%
        dplyr::mutate(
          date = stringr::str_extract(string = day,
                                      pattern = "[:digit:]{4}-[:digit:]{2}"))
    } else {
      summarised
    }
  } else {
    summarised
  }

}
