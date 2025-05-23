#' Summarise for a given time period word counts, typically calculatd with
#' `cas_count()`
#'
#' @param count_df A data frame. Must include at least a column with a date or
#'   date-time column and a column with number of occurrences for the given
#'   time.
#' @param date Defaults to `date`. Unquoted name of a column having either date
#'   or date-time as class.
#' @param n Unquoted to `n`.  Unquoted name of a column having number of
#'   occurrences per time unit.
#' @param period Defaults to NULL. A string describing the time unit to be used
#'   for summarising. Possible values include "year", "quarter", "month", "day",
#'   "hour", "minute", "second", "millisecond".
#' @param f Defaults to `mean`. Function to be applied over n for all the values
#'   in a given time period. Common alternatives would be `mean` or `median`.
#' @param period_summary_function Defaults to `sum`. This is applied when
#'   grouping by period (e.g. when `period` is set to year). When calculating
#'   absolute word frequency, the default (`sum`) is fine. When calculating
#'   relative frequencies, then `mean` would be more appropriate, but extra
#'   consideration should be given to the implications if then a rolling average
#'   is applied.
#' @param auto_convert Defaults to FALSE. If FALSE, the date column is returned
#'   using the same format as the input; the minimun vale in the given group is
#'   used for reference (e.g. all values for January 2022 are summarised as
#'   2021-01-01 it the data were originally given as dates.). If TRUE, it tries
#'   to adapt the output to the most intuitive correspondent type; for year, a
#'   numeric column with only the year number, for quarter in the format 2022.1,
#'   for month in the format 2022-01.
#'
#' @inheritParams slider::slide_period
#'
#' @return A data frame with two columns: the name of the period, and the same name originally used for `n`.
#' @export
#'
#' @examples
#' \dontrun{
#' # this assumes dates are provided in a column called date
#' corpus_df %>%
#'   cas_count(
#'     pattern = "example",
#'     group_by = date
#'   ) %>%
#'   cas_summarise(period = "year")
#' }
#'
cas_summarise <- function(count_df,
                          date_column_name = date,
                          n_column_name = n,
                          pattern_column_name = pattern,
                          period = NULL,
                          f = mean,
                          period_summary_function = sum,
                          every = 1L,
                          before = 0L,
                          after = 0L,
                          complete = FALSE,
                          auto_convert = FALSE) {
  if (nrow(count_df) == 0) {
    return(tibble::tibble(
      {{ date_column_name }} := character(),
      {{ pattern_column_name }} := character(),
      {{ n_column_name }} := double()
    ))
  }

  if (is.null(period)) {
    summarised <- count_df %>%
      dplyr::group_by({{ pattern_column_name }}, .drop = TRUE) %>%
      dplyr::mutate({{ n_column_name }} := slider::slide_index_dbl(
        .x = {{ n_column_name }},
        .i = {{ date_column_name }},
        .f = f,
        .before = before,
        .after = after
      )) %>%
      dplyr::ungroup()
  } else {
    summarised <- count_df %>%
      dplyr::mutate({{ date_column_name }} := lubridate::as_date({{ date_column_name }})) %>%
      dplyr::mutate({{ date_column_name }} := lubridate::floor_date(
        x = {{ date_column_name }},
        unit = period
      )) %>%
      dplyr::group_by({{ pattern_column_name }}, {{ date_column_name }}) %>%
      dplyr::summarise({{ n_column_name }} := period_summary_function({{ n_column_name }}, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(
        {{ date_column_name }} := seq.Date(
          from = min({{ date_column_name }}),
          to = max({{ date_column_name }}),
          by = period
        ),
        {{ pattern_column_name }},
        fill = rlang::list2({{ n_column_name }} := 0)
      ) %>%
      dplyr::group_by({{ pattern_column_name }}) %>%
      dplyr::mutate(n = slider::slide_period_dbl(
        .x = {{ n_column_name }},
        .i = {{ date_column_name }},
        .period = period,
        .f = f,
        .before = before,
        .after = after
      ))
  }

  summarised_output_df <- summarised

  if (auto_convert == TRUE) {
    if (period == "year") {
      summarised_output_df <- summarised %>%
        dplyr::transmute(
          {{ date_column_name }} := lubridate::year({{ date_column_name }}),
          {{ pattern_column_name }},
          {{ n_column_name }}
        )
    } else if (period == "quarter") {
      summarised_output_df <- summarised %>%
        dplyr::transmute(
          {{ date_column_name }} := lubridate::quarter(
            x = {{ date_column_name }},
            with_year = TRUE
          ) %>%
            as.character(),
          {{ pattern_column_name }},
          {{ n_column_name }}
        )
    } else if (period == "month") {
      summarised_output_df <- summarised %>%
        dplyr::transmute(
          {{ date_column_name }} := stringr::str_extract(
            string = {{ date_column_name }},
            pattern = "[:digit:]{4}-[:digit:]{2}"
          ),
          {{ pattern_column_name }},
          {{ n_column_name }}
        )
    } else if (period == "day") {
      summarised_output_df <- summarised %>%
        dplyr::transmute(
          {{ date_column_name }} := as.Date({{ date_column_name }}),
          {{ pattern_column_name }},
          {{ n_column_name }}
        )
    }
  }
  summarised_output_df |>
    dplyr::ungroup()
}











cas_summarise_legacy <- function(count_df,
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
    .f = ~ tibble::tibble(
      {{ date }} := lubridate::floor_date(
        x = .x %>%
          dplyr::pull({{ date }}),
        unit = period
      ) %>%
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
    if (period == "year") {
      summarised %>%
        dplyr::mutate({{ date }} := lubridate::year({{ date }}))
    } else if (period == "quarter") {
      summarised %>%
        dplyr::mutate(
          quarter = lubridate::quarter(x = quarter, with_year = TRUE) %>%
            as.character()
        )
    } else if (period == "month") {
      summarised %>%
        dplyr::mutate(
          month = stringr::str_extract(
            string = month,
            pattern = "[:digit:]{4}-[:digit:]{2}"
          )
        )
    } else if (period == "day") {
      summarised %>%
        dplyr::mutate(
          date = stringr::str_extract(
            string = day,
            pattern = "[:digit:]{4}-[:digit:]{2}"
          )
        )
    } else {
      summarised
    }
  } else {
    summarised
  }
}
