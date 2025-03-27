#' cass_show_barchart_ggiraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cass_show_barchart_ggiraph_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ggiraph::girafeOutput(ns("ggiraph_barchart"))
  )
}

#' cass_show_barchart_ggiraph Server Functions
#'
#' @noRd
mod_cass_show_barchart_ggiraph_server <- function(id, count_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$ggiraph_barchart <- ggiraph::renderGirafe({
      cas_show_gg_base(count_df = count_df) %>%
        cas_show_barchart_ggiraph()
    })
  })
}

## To be copied in the UI
# mod_cass_show_barchart_ggiraph_ui("cass_show_barchart_ggiraph_ui_1")

## To be copied in the server
# mod_cass_show_barchart_ggiraph_server("cass_show_barchart_ggiraph_ui_1")
# count_df <- castarter::cas_count(corpus = castarter::cas_demo_corpus,
#                                   string = c("putin", "medvedev")) %>%
#   cas_summarise(period = "year")
# cass_show_barchart_ggiraph_app(count_df)


cass_show_barchart_ggiraph_app <- function(count_df) {
  ui <- fluidPage(
    mod_cass_show_barchart_ggiraph_ui("cass_show_barchart_ggiraph_ui_1")
  )
  server <- function(input, output, session) {
    mod_cass_show_barchart_ggiraph_server("cass_show_barchart_ggiraph_ui_1",
      count_df = count_df
    )
  }
  shinyApp(ui, server)
}
