#' casm_build_urls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_casm_build_urls_ui <- function(id) {
  ns <- NS(id)
  shiny::sidebarLayout(
    sidebarPanel = shiny::sidebarPanel(
      shiny::radioButtons(
        inputId = ns("url_type"), label = "Type of index urls",
        choices = c(
          "Incremental",
          "Date-based"
        ),
        inline = TRUE
      ),
      shiny::uiOutput(outputId = ns("build_urls_parameters_ui"))
    ),
    mainPanel = shiny::mainPanel(
      reactable::reactableOutput(outputId = ns("urls_table"))
    )
  )
}

#' casm_build_urls Server Functions
#'
#' @noRd
mod_casm_build_urls_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Build ui ####

    output$build_urls_parameters_ui <- shiny::renderUI({
      if (input$url_type == "Incremental") {
        shiny::tagList(
          shiny::fluidRow(
            shiny::textInput(
              inputId = ns("url"),
              label = "Base url",
              value = "https://example.com/news/",
              placeholder = "Insert here the base url"
            ),
            shiny::textInput(
              inputId = ns("url_ending"),
              label = "Url ending",
              value = ""
            ),
            shiny::numericInput(
              inputId = ns("start_page"),
              label = "Start page",
              value = 1
            ),
            shiny::numericInput(
              inputId = ns("end_page"),
              label = "End page",
              value = 10
            ),
            shiny::numericInput(
              inputId = ns("increase_by"),
              label = "Increase by",
              value = 1
            ),
            shiny::textInput(
              inputId = ns("index_group"),
              label = "Index group",
              value = "index"
            )
          )
        )
      } else if (input$url_type == "Date-based") {
        shiny::tagList(
          shiny::fluidRow(
            shiny::textInput(
              inputId = ns("url"),
              label = "Base url",
              value = "https://example.com/news/",
              placeholder = "Insert here the base url"
            ),
            shiny::textInput(
              inputId = ns("url_ending"),
              label = "Url ending",
              value = ""
            ),
            shiny::dateRangeInput(
              inputId = ns("date_range"),
              label = "Date range",
              start = Sys.Date() - 31,
              end = Sys.Date() - 1,
              format = "yyyy-mm-dd",
              weekstart = 1
            ),
            shiny::selectInput(
              inputId = ns("increase_date_by"),
              label = "Increase date by",
              choices = c(
                "day",
                "week",
                "month",
                "quarter",
                "year"
              )
            ),
            shiny::textInput(
              inputId = ns("index_group"),
              label = "Index group",
              value = "index"
            )
          )
        )
      }
    })




    urls_df_r <- shiny::reactive({
      cas_build_urls(
        url = input$url,
        url_ending = input$url_ending,
        start_page = input$start_page,
        end_page = input$end_page,
        increase_by = input$increase_by,
        date_format = input$date_format,
        increase_date_by = input$increase_date_by
      )
    })

    output$urls_table <- reactable::renderReactable({
      reactable::reactable(data = urls_df_r())
    })
  })
}

## To be copied in the UI
# mod_casm_build_urls_ui("casm_build_urls_1")

## To be copied in the server
# mod_casm_build_urls_server("casm_build_urls_1")



mod_casm_build_urls <- function() {
  ui <- shiny::fluidPage(
    mod_casm_build_urls_ui(id = "casm_build_urls_1")
  )

  server <- function(input, output, session) {
    urls_r <- mod_casm_build_urls_server(id = "casm_build_urls_1")
  }
  shiny::shinyApp(ui, server)
}

# mod_casm_build_urls()
