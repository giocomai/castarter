#' cass_download_csv UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cass_download_csv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(outputId = ns("download_data")),
  )
}

#' cass_download_csv Server Functions
#'
#' @noRd
mod_cass_download_csv_server <- function(
  id,
  df,
  type = "data",
  corpus = "corpus"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$download_data <- downloadHandler(
      filename = function() {
        stringr::str_c(
          Sys.Date(),
          "_castarter_",
          type,
          "_",
          corpus,
          "_",
          as.numeric(Sys.time()),
          ".csv"
        ) |>
          fs::path_sanitize()
      },
      content = function(con) {
        readr::write_csv(
          df |>
            dplyr::collect(),
          con
        )
      }
    )
  })
}

## To be copied in the UI
# mod_cass_download_csv_ui("mod_cass_download_csv_ui_1")

## To be copied in the server
# mod_cass_download_csv_server("mod_cass_download_csv_ui_1")

#' A minimal shiny app that demonstrates the functioning of related modules
#'
#' @param df A data frame to be exported as csv.
#'
#' @return A shiny app
#' @export
#'
#' @examples
#'
#' count_df <- castarter::cas_count(
#'   corpus = castarter::cas_demo_corpus,
#'   string = c("russia", "moscow")
#' ) |>
#'   cas_summarise(before = 15, after = 15)
#' if (interactive() {
#' cass_download_csv_app(count_df)
#' }
#'
cass_download_csv_app <- function(df, type = "data") {
  ui <- fluidPage(
    mod_cass_download_csv_ui("mod_cass_download_csv_ui_1")
  )
  server <- function(input, output, session) {
    mod_cass_download_csv_server(
      "mod_cass_download_csv_ui_1",
      df = df,
      type = type
    )
  }
  shinyApp(ui, server)
}
