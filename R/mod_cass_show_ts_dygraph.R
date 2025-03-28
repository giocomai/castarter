#' cass_show_ts_dygraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cass_show_ts_dygraph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dygraphs::dygraphOutput(ns("dygraph"))
  )
}

#' cass_show_ts_dygraph Server Functions
#'
#' @noRd
mod_cass_show_ts_dygraph_server <- function(id, count_df, strokeWidth = 2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dygraph <- dygraphs::renderDygraph({
      cas_show_ts_dygraph(count_df = count_df) %>%
        dygraphs::dyOptions(strokeWidth = strokeWidth)
    })
  })
}

## To be copied in the UI
# mod_cass_show_ts_dygraph_ui("cass_show_ts_dygraph_ui_1")

## To be copied in the server
# mod_cass_show_ts_dygraph_server("cass_show_ts_dygraph_ui_1")

#' A minimal shiny app that demonstrates the functioning of related modules
#'
#' @param count_df A dataframe with three columns (`date`, `word`, and `n`), typically created with `cas_count()` and possibly processed with `cas_summarise()`.
#'
#' @return A shiny app
#' @export
#'
#' @examples
#'
#' count_df <- cas_count(
#'   corpus = cas_demo_corpus,
#'   pattern = c("russia", "moscow")
#' ) |> 
#'   cas_summarise(before = 15, after = 15)
#'
#' # cass_show_ts_dygraph_app(count_df)
cass_show_ts_dygraph_app <- function(count_df) {
  ui <- fluidPage(
    mod_cass_show_ts_dygraph_ui("cass_show_ts_dygraph_ui_1")
  )
  server <- function(input, output, session) {
    mod_cass_show_ts_dygraph_server("cass_show_ts_dygraph_ui_1",
      count_df = count_df
    )
  }
  shinyApp(ui, server)
}
