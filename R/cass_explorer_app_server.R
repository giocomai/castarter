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
    date_range_s <- golem::get_golem_options("corpus") %>%
      dplyr::distinct(date) %>%
      dplyr::pull(date, as_vector = FALSE)

    shiny::dateRangeInput(
      inputId = "date_range",
      label = "Date range",
      start = as.character(min(date_range_s)),
      end = as.character(max(date_range_s)),
      weekstart = 1
    )
  })

  ### Corpus ####

  corpus_active_r <- shiny::eventReactive(
    eventExpr = list(
      input$date_range,
      input$go
    ),
    valueExpr = ({
      active_corpus_df <- golem::get_golem_options("corpus") %>%
        dplyr::filter(
          is.na(date) == FALSE,
          date >= lubridate::as_date(input$date_range[[1]]),
          date <= lubridate::as_date(input$date_range[[2]])
        )


      if (is.null(input$pattern) == FALSE) {
        if (input$pattern != "") {
          if (golem::get_golem_options("collect") == TRUE) {
            active_corpus_df <- active_corpus_df %>%
              dplyr::collect()
          }

          current_pattern <- cass_split_string(
            string = input$pattern,
            to_regex = TRUE
          )

          active_corpus_df <- active_corpus_df %>%
            dplyr::filter(
              stringr::str_detect(
                string = text,
                pattern = stringr::regex(
                  pattern = current_pattern,
                  ignore_case = TRUE
                )
              )
            )
        }
      }
      active_corpus_df
    })
  )

  #### KWIC #####

  kwic_df_r <- shiny::eventReactive(
    eventExpr = list(
      input$date_range,
      input$go
    ),
    valueExpr = ({
      if (is.null(input$pattern)) {
        return(NULL)
      } else if (input$pattern == "") {
        return(NULL)
      }

      corpus_active_r() %>%
        dplyr::collect() %>%
        dplyr::select(id, date, url, title, text) %>%
        dplyr::collect() %>%
        cas_kwic(pattern = stringr::str_flatten(c(
          "(?i)",
          cass_split_string(
            string = input$pattern,
            to_regex = TRUE
          )
        )))
    })
  )

  output$kwic_reactable <- reactable::renderReactable({
    if (is.null(kwic_df_r())) {
      return(NULL)
    }

    if (is.null(input$pattern)) {
      return(NULL)
    }

    if (input$pattern == "") {
      return(NULL)
    }

    max_pattern_length <- kwic_df_r() %>%
      dplyr::distinct(pattern) %>%
      dplyr::filter(is.na(pattern) == FALSE) %>%
      dplyr::mutate(nchar_pattern = nchar(pattern)) %>%
      dplyr::slice_max(nchar_pattern, with_ties = FALSE) %>%
      dplyr::pull(nchar_pattern)

    kwic_df_r() %>%
      dplyr::mutate(source = paste0("<a target='_blank' href='", url, "'>", title, "</a><br />")) %>%
      dplyr::select(date, source, before, pattern, after) %>%
      dplyr::arrange(date) %>%
      reactable::reactable(
        resizable = TRUE,
        filterable = TRUE,
        showSortIcon = TRUE,
        showSortable = TRUE,
        defaultPageSize = 5,
        showPageSizeOptions = TRUE,
        wrap = FALSE,
        pageSizeOptions = c(5, 10, 20, 50, 100, 500),
        compact = TRUE,
        columns = list(
          date = reactable::colDef(maxWidth = 110),
          source = reactable::colDef(html = TRUE),
          before = reactable::colDef(align = "right"),
          pattern = reactable::colDef(maxWidth = max_pattern_length * 10)
        )
      )
  })


  kwic_sentences_df_r <- shiny::eventReactive(
    eventExpr = list(
      input$go,
      input$kwic_switch
    ),
    valueExpr = {
      if (is.null(input$pattern)) {
        return(NULL)
      }

      if (input$kwic_switch == FALSE) {
        return(NULL)
      }

      corpus_active_r() %>%
        dplyr::collect() %>%
        tidytext::unnest_tokens(
          output = sentence,
          input = text,
          to_lower = FALSE,
          token = "sentences"
        ) %>%
        dplyr::filter(stringr::str_detect(
          string = sentence,
          pattern = stringr::regex(
            cass_split_string(input$pattern,
              to_regex = TRUE
            ),
            ignore_case = TRUE
          )
        )) %>%
        dplyr::mutate(Source = paste0("<a target='_blank' href='", url, "'>", title, "</a><br />")) %>%
        dplyr::mutate(sentence = cass_highlight(
          sentence,
          cass_split_string(input$pattern,
            to_regex = TRUE
          )
        )) %>%
        dplyr::rename(Sentence = sentence, Date = date) %>%
        dplyr::select(Date, Source, Sentence) %>%
        dplyr::arrange(Date)
    }
  )

  output$kwic_sentences_reactable <- reactable::renderReactable({
    if (is.null(kwic_sentences_df_r())) {
      return(NULL)
    }

    if (is.null(input$pattern)) {
      return(NULL)
    }

    if (input$pattern == "") {
      return(NULL)
    }



    kwic_sentences_df_r() %>%
      reactable::reactable(
        resizable = TRUE,
        filterable = TRUE,
        showSortIcon = TRUE,
        showSortable = TRUE,
        defaultPageSize = 5,
        showPageSizeOptions = TRUE,
        wrap = TRUE,
        pageSizeOptions = c(5, 10, 20, 50, 100, 500),
        compact = TRUE,
        columns = list(
          Date = reactable::colDef(maxWidth = 110),
          Source = reactable::colDef(html = TRUE, maxWidth = 300),
          Sentence = reactable::colDef(html = TRUE)
        )
      )
  })

  ### Barchart main card

  output$barchart_main_card_UI <- renderUI({
    bslib::card(
      full_screen = TRUE,
      max_height = "400px",
      fill = TRUE,
      bslib::card_header("Word count"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          position = "right",
          shiny::selectInput(
            inputId = "summarise_by",
            label = "Aggregate by:",
            choices = c("year", "quarter", "month", "day"),
            selected = "year"
          ),
          shiny::tagList({
            if (length(cass_split_string(string = input$pattern)) > 1) {
              shiny::radioButtons(
                inputId = "barchart_position",
                label = "Bars should be:",
                choices = c(
                  stacked = "stack",
                  dodged = "dodge"
                )
              )
            }
          }),
        ),
        mod_cass_show_barchart_wordcount_ui("cass_show_barchart_wordcount_ui_1")
        # ,
        # mod_cass_show_barchart_ggiraph_ui("cass_show_barchart_ggiraph_ui_1")
      )
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

  word_count_summarised_df_r <- shiny::eventReactive(
    list(
      input$go,
      input$summarise_by
    ),
    {
      if (is.null(input$pattern) | is.null(input$summarise_by)) {
        return(NULL)
      }

      if (input$pattern == "") {
        count_summarised_df <- cas_count_total_words(
          corpus = corpus_active_r()
        ) %>%
          dplyr::mutate(pattern = "") %>%
          cas_summarise(
            period = input$summarise_by,
            f = sum,
            auto_convert = TRUE
          )
      } else {
        count_summarised_df <- cas_count(
          corpus = corpus_active_r(),
          pattern = cass_split_string(input$pattern)
        ) %>%
          cas_summarise(
            period = input$summarise_by,
            f = sum,
            auto_convert = TRUE
          )
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
    eventExpr = list(
      input$go,
      input$summarise_by,
      input$barchart_position
    ),
    {
      if (is.null(input$summarise_by)) {
        return(NULL)
      }

      mod_cass_show_barchart_wordcount_server("cass_show_barchart_wordcount_ui_1",
        count_df = word_count_summarised_df_r(),
        period = input$summarise_by,
        position = input$barchart_position
      )
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
}
