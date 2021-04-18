#' Creates base ggplot2 object to be used by ggplot or ggiraph
#'
#' @param count_df
#' @param group_by Defaults to NULL. If given, the unquoted name of the column to be used for grouping (e.g. date, or doc_id, or source, etc.)
#' @param n_column_name Defaults to 'n'. The unquoted name of the column to be used for the count in the output.
#' @param word_column_name Defaults to 'word'. The unquoted name of the column to be used for the word in the output (if `word_column` is set to TRUE, as per default).
#' @param group_as_factor Defaults to FALSE. If TRUE, the grouping column is forced into a vector, otherwise it is kept in its current format (e.g. date, or numeric).
#'
#' @return A ggplot2 object with aesthetics set, but no geometry.
#' @export
#'
#' @examples
cas_show_barchart_gg_base <- function(count_df,
                                      group_by = date,
                                      n_column_name = n,
                                      word_column_name = word,
                                      group_as_factor = FALSE) {
  if (isTRUE(group_as_factor)) {
    count_df <- count_df %>%
      dplyr::mutate({{ group_by }} := factor({{ group_by }}))
  }
  count_df %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = {{ group_by }},
                                           y = {{ n_column_name }},
                                           fill = {{ word_column_name }}))
}


#' Creates interacative barchart with ggiraph
#'
#' For detail on parameters, see https://davidgohel.github.io/ggiraph/articles/offcran/using_ggiraph.html
#'
#' @param ggobj A ggplot2 object, typically generated with `cas_show_barchart_gg_base()`
#' @param data_id Defaults to NULL. If given, unquoted name of column, passed to ggiraph.
#' @param tooltip Defaults to NULL. If given, unquoted name of column, passed to ggiraph.
#'
#' @return A girage/htmlwidget object
#' @export
#'
#' @examples
cas_show_barchart_ggiraph <- function(ggobj,
                                      data_id = NULL,
                                      tooltip = NULL) {
  ggiraph::girafe(ggobj = ggobj +
                    ggiraph::geom_col_interactive(mapping = ggplot2::aes(data_id = data_id,
                                                                         tooltip = tooltip)))
}
