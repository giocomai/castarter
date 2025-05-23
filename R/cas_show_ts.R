#' Create dygraphs based on a data frame typically generated with [cas_count()]
#'
#' @param count_df A data frame with word frequency counts, typically generated
#'   with [cas_count()] (and possibly [cas_summarise()])
#'
#' @return A `dygraph` object.
#' @export
#'
#' @examples
#'
#' count_df <- cas_count(
#'   corpus = cas_demo_corpus,
#'   pattern = c("russia", "moscow")
#' ) |>
#'   cas_summarise(before = 15, after = 15)
#' cas_show_ts_dygraph(count_df)
cas_show_ts_dygraph <- function(
  count_df,
  date_column_name = date,
  n_column_name = n,
  pattern_column_name = pattern,
  range_selector = TRUE
) {
  if (is.data.frame(count_df) == FALSE) {
    return(NULL)
  }
  xts_df <- count_df |>
    tidyr::pivot_wider(
      id_cols = {{ date_column_name }},
      names_from = {{ pattern_column_name }},
      values_from = {{ n_column_name }}
    ) |>
    tbl2xts::tbl_xts()

  dy <- xts_df |>
    dygraphs::dygraph()

  if (isTRUE(range_selector)) {
    dy <- dy |>
      dygraphs::dyRangeSelector()
  }
  dy
}
