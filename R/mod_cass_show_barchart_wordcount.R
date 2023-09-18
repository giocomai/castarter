#' cass_show_barchart_wordcount UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cass_show_barchart_wordcount_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::plotOutput(ns("ggplot2_barchart"))
  )
}

#' cass_show_barchart_wordcount Server Functions
#'
#' @noRd
mod_cass_show_barchart_wordcount_server <- function(id,
                                                    count_df,
                                                    period = "year",
                                                    position = "stack") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (is.null(position)) {
      position <- "stack"
    }

    if (is.null(count_df)) {
      return(NULL)
    }

    counted_patterns <- unique(count_df$pattern)

    if (length(counted_patterns) == 1) {
      if (counted_patterns == "") {
        empty_pattern <- TRUE
      } else {
        empty_pattern <- FALSE
      }
    } else {
      empty_pattern <- FALSE
    }
    
    if (length(counted_patterns) == 0) {
      gg_base_year_gg <- cas_show_gg_base(count_df = count_df) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                       title = ggplot2::element_text(colour = "#800000")) +
        ggplot2::labs(title = "No match found in this corpus.\nCheck your pattern and try again.")
    } else if (empty_pattern == TRUE) {
      gg_base_year_gg <- count_df %>%
        dplyr::mutate(date = factor(date)) %>%
        cas_show_gg_base() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::scale_fill_manual(values = NA) +
        ggplot2::guides(fill = "none") +
        ggplot2::labs(title = stringr::str_c("Total number of words per ", period))
    } else {
      gg_base_year_gg <- count_df %>%
        dplyr::mutate(date = factor(date)) %>%
        cas_show_gg_base() +
        ggplot2::scale_x_discrete(name = NULL) +
        ggplot2::labs(title = stringr::str_c("Total number of mentions per ", period))
    }


    output$ggplot2_barchart <- shiny::renderPlot(cas_show_barchart_ggplot2(ggobj = gg_base_year_gg,
                                                                           position = position))
  })
}

## To be copied in the UI
# mod_cass_show_barchart_wordcount_ui("cass_show_barchart_wordcount_ui_1")

## To be copied in the server
# mod_cass_show_barchart_wordcount_server("cass_show_barchart_wordcount_ui_1")
# count_df <- castarter::cas_count(corpus = tifkremlinen::kremlin_en,
#                                   string = c("putin", "medvedev")) %>%
#   cas_summarise(period = "year")
# cass_show_barchart_wordcount_app(count_df)


cass_show_barchart_wordcount_app <- function(count_df,
                                             type = "ggplot2",
                                             period = "year") {
  ui <- fluidPage(
    mod_cass_show_barchart_wordcount_ui("cass_show_barchart_wordcount_ui_1")
  )
  server <- function(input, output, session) {
    mod_cass_show_barchart_wordcount_server("cass_show_barchart_wordcount_ui_1",
      count_df = count_df
    )
  }
  shinyApp(ui, server)
}
