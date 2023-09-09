#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
cass_explorer_app_server <- function(input, output, session) {
  ### Help ####

  observeEvent(input$cicerone,
    {
      cass_cicerone_help$init()$start()
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  ### Date range ####

  output$date_range_input_UI <- renderUI({
    shiny::dateRangeInput(
      inputId = "date_range",
      label = "Date range",
      start = min(golem::get_golem_options("corpus")[["date"]]),
      end = max(golem::get_golem_options("corpus")[["date"]]),
      weekstart = 1
    )
  })


  ### Advanced UI ####

  ##### Column selector UI   ######

  if (golem::get_golem_options("advanced")) {
    output$column_selector_UI <- renderUI({
      shiny::tagList(
        shiny::selectInput(
          inputId = "text_column",
          label = "Select text column",
          choices = c("", names(golem::get_golem_options("corpus"))),
          selected = dplyr::if_else(is.element(
            "text",
            names(golem::get_golem_options("corpus"))
          ), "text", "")
        ),
        shiny::selectInput(
          inputId = "group_by_column",
          label = "Select date column or column used for aggregation",
          choices = c("", names(golem::get_golem_options("corpus"))),
          selected = dplyr::if_else(is.element(
            "date",
            names(golem::get_golem_options("corpus"))
          ), "date", "")
        ),
        shiny::selectInput(
          inputId = "summarise_by",
          label = "If time/date column selected, aggregate by the following time unit:",
          choices = c("", "year", "quarter", "month", "day", "hour", "minute"),
          selected = dplyr::if_else(is.element(
            "date",
            names(golem::get_golem_options("corpus"))
          ), "day", "")
        ),
        conditionalPanel(condition = "input.summarise_by != ''", {
          shiny::checkboxInput(
            inputId = "summarise_auto_convert_checkbox",
            label = "Autoconvert date/time format in output?",
            value = FALSE
          )
        })
      )
    })
  }


  ### Word counting  reactives ####

  word_count_df_r <- shiny::eventReactive(input$go, {
    if (golem::get_golem_options("advanced")) {
      corpus_df <- corpus_df %>%
        dplyr::rename(
          text = .data[[input$text_column]],
          date = .data[[input$group_by_column]]
        )
    }

    if (input$freq == "Absolute frequency") {
      count_df <- corpus_df %>%
        castarter::cas_count(string = castarter:::cass_split(input$string))
    } else if (input$freq == "Relative frequency") {
      count_df <- corpus_df %>%
        castarter::cas_count_relative(string = castarter:::cass_split(input$string))
    }
    count_df
  })


  word_count_summarised_df_r <- shiny::eventReactive(
    list(
      input$go,
      input$moving_type_selector,
      input$moving_units_total,
      input$moving_units_before,
      input$moving_units_after
    ),
    {
      if (input$string == "") {
        return(cas_count_total_words(corpus = golem::get_golem_options("corpus")) %>%
          dplyr::mutate(pattern = "") %>%
          cas_summarise(
            period = input$summarise_by,
            f = sum,
            # before = units_before,
            # after = units_after,
            # f = foo,
            auto_convert = TRUE
          ))
      }


      if (is.null(input$summarise_by) == FALSE & input$summarise_by != "") {
        if (input$moving_type_selector == "average") {
          foo <- mean
        } else if (input$moving_type_selector == "median") {
          foo <- median
        } else if (input$moving_type_selector == "sum") {
          foo <- sum
        } else {
          return(NULL)
        }


        if (input$moving_length_type_selector == "centred") {
          units_before <- units_after <- round((input$moving_units_total - 1) / 2)
        } else {
          units_before <- input$moving_units_before
          units_after <- input$moving_units_after
        }


        count_summarised_df <- word_count_df_r() %>%
          castarter::cas_summarise(
            period = input$summarise_by,
            before = units_before,
            after = units_after,
            f = foo,
            auto_convert = input$summarise_auto_convert_checkbox
          ) %>%
          dplyr::rename(!!input$group_by_column := date)
      } else {
        count_summarised_df <- count_df %>%
          dplyr::rename(!!input$group_by_column := date)
      }
      count_summarised_df
    },
    ignoreNULL = TRUE
  )

  ##### modules #####

  ##### graph modules #####
  shiny::observeEvent(input$go,
    mod_cass_show_barchart_ggiraph_server(
      id = "cass_show_barchart_ggiraph_ui_1",
      count_df = word_count_summarised_df_r()
    ),
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )


  shiny::observeEvent(
    input$go,
    mod_cass_show_barchart_wordcount_server("cass_show_barchart_wordcount_ui_1",
      count_df = word_count_summarised_df_r(),
      type = "ggplot2",
      period = "year"
    ),
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )
}
