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
      castarter::cass_cicerone_help$init()$start()
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  ### Date range ####

  output$date_range_input_UI <- renderUI({
    date_range_s <- golem::get_golem_options("corpus") %>%
      dplyr::distinct(date) %>%
      dplyr::collect() |>
      dplyr::pull(date)

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
          is.na(text) == FALSE,
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
      input$go,
      input$kwic_switch
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
      input$date_range,
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
                  dodged = "dodge",
                  stacked = "stack"
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

  #### modules #####

  mod_cass_regex_info_server("cass_regex_info_1")


  ### export modules #####

  shiny::observeEvent(eventExpr = list(
    input$page_navbar,
    input$go
  ), handlerExpr = {
    output$export_cards_UI <- shiny::renderUI({
      if (input$kwic_switch) {
        kwic_export_taglist <- shiny::tagList(
          bslib::layout_column_wrap(
            # title = "Export keywords in context",
            bslib::card(
              bslib::card_header("Export filtered sentences"),
              bslib::card_body(
                shiny::p("Export only sentences where there is a match with the current pattern."),
                castarter:::mod_cass_download_csv_ui("mod_download_kwic_sentences")
              )
            ),
            bslib::card(
              bslib::card_header("Export keywords in context"),
              bslib::card_body(
                shiny::p("Export keywords in context for the current pattern (the matched pattern is in its own column, with words just before and just after it in separate columns.)"),
                castarter:::mod_cass_download_csv_ui("mod_download_kwic_df")
              )
            )
          )
        )
      } else {
        kwic_export_taglist <- shiny::tagList(
          bslib::layout_column_wrap(
            # title = "Export keywords in context",
            bslib::card(
              bslib::card_header("Export keywords in context"),
              bslib::card_body(
                shiny::p("In order to export keywords in context or only the setences where the given pattern is matched, enable the 'Show key words in context' switch in the sidebar.")
              )
            )
          )
        )
      }

      shiny::tagList(
        bslib::layout_column_wrap(
          # title = "Export active corpus",
          bslib::card(
            bslib::card_header("Export full text"),
            bslib::card_body(
              shiny::p("Export all available columns for the active corpus (i.e. after filtering based on pattern and dates)."),
              castarter:::mod_cass_download_csv_ui("mod_download_full_text")
            )
          ),
          bslib::card(
            bslib::card_header("Export aggregated count"),
            bslib::card_body(
              shiny::p("Export the count of matches, aggregated in the same form as used for the graph on the main tab."),
              castarter:::mod_cass_download_csv_ui("mod_download_word_count")
            )
          )
        ),
        kwic_export_taglist,
        bslib::layout_column_wrap(
          # title = "Export original corpus",
          bslib::card(
            bslib::card_header("Export original corpus"),
            bslib::card_body(
              shiny::p("Export the corpus in full in its original form."),
              castarter:::mod_cass_download_csv_ui("mod_download_original_corpus")
            )
          )
        )
      )
    })


    castarter:::mod_cass_download_csv_server(
      id = "mod_download_original_corpus",
      df = golem::get_golem_options("corpus"),
      type = "original_corpus",
      corpus = golem::get_golem_options("title")
    )

    castarter:::mod_cass_download_csv_server(
      id = "mod_download_full_text",
      df = corpus_active_r(),
      type = stringr::str_c("full_text", ifelse(input$pattern != "",
        stringr::str_c("_", input$pattern),
        ""
      )),
      corpus = golem::get_golem_options("title")
    )

    castarter:::mod_cass_download_csv_server(
      id = "mod_download_word_count",
      df = word_count_summarised_df_r(),
      type = stringr::str_c("count", ifelse(input$pattern != "",
        stringr::str_c("_", input$pattern),
        ""
      )),
      corpus = golem::get_golem_options("title")
    )

    castarter:::mod_cass_download_csv_server(
      id = "mod_download_kwic_sentences",
      df = kwic_sentences_df_r(),
      type = stringr::str_c("kwic", ifelse(input$pattern != "",
        stringr::str_c("_", input$pattern),
        ""
      )),
      corpus = golem::get_golem_options("title")
    )

    castarter:::mod_cass_download_csv_server(
      id = "mod_download_kwic_df",
      df = kwic_df_r(),
      type = stringr::str_c("kwic", ifelse(input$pattern != "",
        stringr::str_c("_", input$pattern),
        ""
      )),
      corpus = golem::get_golem_options("title")
    )
  })


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
