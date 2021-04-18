#' cass_show_barchart_ggiraph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cass_show_barchart_ggiraph_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' cass_show_barchart_ggiraph Server Functions
#'
#' @noRd 
mod_cass_show_barchart_ggiraph_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cass_show_barchart_ggiraph_ui("cass_show_barchart_ggiraph_ui_1")
    
## To be copied in the server
# mod_cass_show_barchart_ggiraph_server("cass_show_barchart_ggiraph_ui_1")
