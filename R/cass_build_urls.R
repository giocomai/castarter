#' casm_build_urls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cass_build_urls_ui <- function(id) {
  ns <- NS(id)
  shiny::sidebarLayout(
    sidebarPanel = shiny::sidebarPanel(
      shiny::radioButtons(
        inputId = ns("url_type"),
        label = "Type of index urls",
        choices = c(
          "Incremental",
          "Date-based"
        ),
        inline = TRUE
      ),
      shiny::uiOutput(outputId = ns("build_urls_parameters_ui"))
    ),
    mainPanel = shiny::mainPanel(
      shiny::h3("Code:"),
      shiny::verbatimTextOutput(ns("code")),
      shiny::h3("Output:"),
      reactable::reactableOutput(outputId = ns("urls_table")),
      shiny::downloadButton(ns("download_csv"), "Download urls as csv"),
      shiny::h4("Formatted code:"),
      shiny::verbatimTextOutput(outputId = ns("function_string"))
    )
  )
}

#' casm_build_urls Server Functions
#'
#' @noRd
mod_cass_build_urls_server <- function(id) {
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
            shiny::selectizeInput(
              inputId = ns("date_format"),
              label = "Date format",
              choices = c(
                "Ymd",
                "dmY",
                "mdY"
              ),
              options = list(create = TRUE)
            ),
            shiny::textInput(
              inputId = ns("date_separator"),
              label = "Date separator",
              value = "-",
              placeholder = "Insert date separator"
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


    urls_df_r <- shinymeta::metaReactive2(
      varname = "index_urls_df",
      {
        if (input$url_type == "Incremental") {
          shinymeta::metaExpr(
            cas_build_urls(
              url = ..(input$url),
              url_ending = ..(input$url_ending),
              start_page = ..(input$start_page),
              end_page = ..(input$end_page),
              increase_by = ..(input$increase_by)
            )
          )
        } else {
          shinymeta::metaExpr(
            cas_build_urls(
              url = ..(input$url),
              url_ending = ..(input$url_ending),
              increase_by = ..(input$increase_by),
              start_date = ..(as.character(input$date_range[[1]])),
              end_date = ..(as.character(input$date_range[[2]])),
              date_format = ..(input$date_format),
              date_separator = ..(input$date_separator),
              increase_date_by = ..(input$increase_date_by)
            )
          )
        }
      }
    )

    output$function_string <- shiny::renderPrint({
      if (input$url_type == "Incremental") {
        styler::style_text(
          stringr::str_c(
            "cas_build_urls(",
            "url = ",
            "\"", input$url, "\",",
            "\n",
            "url_ending = ",
            "\"", input$url_ending, "\",",
            "\n",
            "start_page = ",
            input$start_page, ",",
            "\n",
            "end_page = ",
            input$end_page, ",",
            "\n",
            "increase_by = ",
            "\"", input$increase_by, "\"",
            "\n",
            ")"
          )
        )
      } else {
        styler::style_text(
          stringr::str_c(
            "cas_build_urls(",
            "url = ",
            "\"", input$url, "\",",
            "\n",
            "url_ending = ",
            "\"", input$url_ending, "\",",
            "\n",
            "date_format = ",
            "\"", input$date_format, "\",",
            "\n",
            "date_separator = ",
            "\"", input$date_separator, "\",",
            "\n",
            "start_date = ",
            "\"", input$date_range[[1]], "\",",
            "\n",
            "end_date = ",
            "\"", input$date_range[[2]], "\",",
            "\n",
            "increase_date_by = ",
            "\"", input$increase_date_by, "\"",
            "\n",
            ")"
          )
        )
      }
    })

    output$urls_table <- reactable::renderReactable({
      reactable::reactable(data = urls_df_r())
    })

    ### download csv ####

    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        "index.csv"
      },
      content = function(file) {
        readr::write_csv(
          x = urls_df_r(),
          file = file
        )
      }
    )

    ### shinymeta ####

    output$code <- renderPrint({
      shinymeta::expandChain(
        quote(library("castarter")),
        urls_df_r()
      )
    })
  })
}

## To be copied in the UI
# mod_cass_build_urls_ui("casm_build_urls_1")

## To be copied in the server
# mod_cass_build_urls_server("casm_build_urls_1")



#' Helps you define the parameters you need for building index urls
#'
#' @return Nothing, but prints to the console the function call as created in the Shiny app.
#' @export
#'
#' @examples
#' \dontrun{
#' if (interactive) {
#'   cass_build_urls()
#' }
#' }
cass_build_urls <- function() {
  ui <- shiny::fluidPage(
    mod_cass_build_urls_ui(id = "index_urls_df")
  )

  server <- function(input, output, session) {
    urls_r <- mod_cass_build_urls_server(id = "index_urls_df")
  }
  shiny::shinyApp(ui, server)
}

# cass_build_urls()
