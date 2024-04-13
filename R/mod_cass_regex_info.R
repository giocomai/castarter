#' Explain basics of how pattern matching works.
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cass_regex_info_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::actionLink(inputId = ns("show_regex_helpers"),
                      label = "Search tips",
                      icon = shiny::icon(name = "info"))
  )
}
    
#' cass_regex_info Server Functions
#'
#' @noRd 
mod_cass_regex_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    shiny::observeEvent(
      eventExpr = ns(input$show_regex_helpers),
      ignoreInit = TRUE,
      handlerExpr = {
        shiny::showModal(
          shiny::modalDialog(
            title = "How does search work here?",
            shiny::h2("The basics"),
            shiny::tags$ul(
              shiny::tags$li("Use comma-separated patterns for comparisons: e.g. searching for 'apple, orange' will match items that include either apple or oranges, and will report them separately in graphs."),
              shiny::tags$li("Use the `|` sign to combine patterns: e.g. searching for 'apple|pear, orange|lemon' will combine apple and pear (and orange and lemon) in results."),
              shiny::tags$li("By default, any match - not only full words - is counted: e.g. 'verb' would match also with 'proverb'. This can be useful, but requires some caution. For example, 'ukrain' would match 'ukraine', 'ukrainian', etc., which may or may not be desirable, depending on the use case."),
              shiny::tags$li("More advanced patterns can be found through regular expressions, see below for details.")
            ),
            shiny::h2("Regular expressions"),
            shiny::p("By default, this interface relies on regular expressions (also known as regex) in order to enable advanced search.",
                     "For more information, ", shiny::a(href = "https://stringr.tidyverse.org/articles/regular-expressions.html", "see this page introducing regular expressions.")),
            shiny::p("Here are a few examples of common use cases:"),
            shiny::tags$ul(
              shiny::tags$li("In order to make sure to catch only full words, you may use '\b', which stands for 'word boundary': e.g. '\bRussia\b', will match only instances of 'Russia', but not 'Russian' or 'Belorussian'."),
              shiny::tags$li("It is possible to match specific combinations. For example, if you both 'russian' and 'russians' would be relevant, you can search for 'russian[s]?`, which indicates that the final 's' may or may not be present, or simply 'russian|russians', if it's easier to remember."),
              shiny::tags$li("In particular for languages with declensions, it may be important to consider all possible terminations, especially when combining words: for example, in 'коллективн[а-я]+ Запад', '[а-я]+' would capture none, one or more letters of the Cyrillic alphabet (the corresponding for Latin would be '[a-z]?')")
            ),
            easyClose = TRUE,
          ))
      })
  })
}

## To be copied in the UI
# mod_cass_regex_info_ui("cass_regex_info_1")
    
## To be copied in the server
# mod_cass_regex_info_server("cass_regex_info_1")


cass_regex_info <- function() {
  ui <- shiny::fluidPage(
    mod_cass_regex_info_ui(id = "cass_regex_info_1")
  )
  
  server <- function(input, output, session) {
    mod_cass_regex_info_server("cass_regex_info_1")
  }
  shiny::shinyApp(ui, server)
}
