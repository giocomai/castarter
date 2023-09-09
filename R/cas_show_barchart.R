#' Creates base ggplot2 object to be used by ggplot or ggiraph
#'
#' @param count_df
#' @param group_by Defaults to NULL. If given, the unquoted name of the column to be used for grouping (e.g. date, or doc_id, or source, etc.)
#' @param n_column_name Defaults to 'n'. The unquoted name of the column to be used for the count in the output.
#' @param pattern_column_name Defaults to 'pattern'. The unquoted name of the column to be used for the word in the output.
#' @param group_as_factor Defaults to FALSE. If TRUE, the grouping column is forced into a factor, otherwise it is kept in its current format (e.g. date, or numeric).
#'
#' @return A ggplot2 object with aesthetics set, but no geometry.
#' @export
#'
#' @examples
cas_show_gg_base <- function(count_df,
                             group_by = date,
                             n_column_name = n,
                             pattern_column_name = pattern,
                             group_as_factor = FALSE) {
  if (isTRUE(group_as_factor)) {
    count_df <- count_df %>%
      dplyr::mutate({{ group_by }} := factor({{ group_by }}))
  }
  count_df %>%
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = {{ group_by }},
      y = {{ n_column_name }},
      fill = {{ pattern_column_name }}
    )) +
    ggplot2::scale_y_continuous(name = NULL, labels = scales::number) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}


#' Creates interacative barchart with ggiraph
#'
#' For detail on parameters, see https://davidgohel.github.io/ggiraph/articles/offcran/using_ggiraph.html
#'
#' @param ggobj A ggplot2 object, typically generated with `cas_show_gg_base()`
#' @param data_id Defaults to NULL. If given, unquoted name of column, passed to ggiraph.
#' @param tooltip Defaults to NULL. If given, unquoted name of column, passed to ggiraph.
#' @param position Defaults to "stack". Available values include "dodge".
#'
#' @return A girafe/htmlwidget object
#' @export
#'
#' @examples
cas_show_barchart_ggiraph <- function(ggobj,
                                      data_id = NULL,
                                      tooltip = NULL,
                                      position = "stack") {
  if (position == "stack") {
    ggiraph::girafe(ggobj = ggobj +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(
          data_id = data_id,
          tooltip = tooltip
        ),
        position = ggplot2::position_stack()
      ))
  } else if (position == "dodge") {
    ggiraph::girafe(ggobj = ggobj +
      ggiraph::geom_col_interactive(
        mapping = ggplot2::aes(
          data_id = data_id,
          tooltip = tooltip
        ),
        position = ggplot2::position_dodge()
      ))
  }
}



#' Creates barchart with ggplot2
#'
#' @param ggobj A ggplot2 object, typically generated with `cas_show_gg_base()`
#' @param position Defaults to "stack". Available values include "dodge".
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
cas_show_barchart_ggplot2 <- function(ggobj,
                                      position = "stack") {
  if (position == "stack") {
    ggobj +
      ggplot2::geom_col(position = ggplot2::position_stack())
  } else if (position == "dodge") {
    ggobj +
      ggplot2::geom_col(position = ggplot2::position_dodge())
  }
}
