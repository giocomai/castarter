#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
cass_explorer_app_server <- function(input, output, session) {
  # Your application server logic

  guide <- cicerone::Cicerone$
    new()$
    step(
    el = "string",
    title = "Type the text you want to search here",
    description = "You can use commas to compare frequency of different terms"
  )$
    step(
    el = "go",
    title = "Click here!",
    description = "Press 'enter' or click on the 'Go!' button to update graphs"
  )$
    step(
    el = "column_selector_UI",
    title = "Select the columns of the corpus to be used for each operation",
    description = "Should the string be matched in the text column, or some other column? Do you want to aggregate by date or something else? Do you want to further aggregate dates by month or year?"
  )$
    step(
    el = "freq",
    title = "Absolute or relative frequency?",
    description = "In a corpus, the quantity of text available may vary over time. While 'absolute frequency' is a plain count of matches, 'relative frequency' divides the result by the total number of words in the same unit."
  )$
    step(
    el = "moving_type_selector_UI",
    title = "For time-based corpora, to increase readability it is common to show results with moving (or rolling) averages",
    description = "A 31-day moving average means that the data point shown for each day corresponds to the average of that day, and the 15 days before and after it. For lengthy datasets covering many years, 91 days may work best."
  )$
    step(
    el = "date_range_input_UI",
    title = "For time-based corpora, filter the dataset in order to include only contents from a given period",
    description = "This affects all parts of this interface, including graph, tables, etc. "
  )$
    step(
    el = "pre_submit_help_text_UI",
    title = "This text summarises in narrative form the choices you have selected. Remember to click on 'Go!' to apply updated settings.",
    description = "It should help ensuring you fully understand what the graphs and tables mean"
  )




  corpus_df <- golem::get_golem_options("corpus") %>%
    dplyr::filter(is.na(date) == FALSE)

  sentences_df <- corpus_df %>%
    tidytext::unnest_tokens(
      output = sentence,
      input = text,
      token = "sentences",
      to_lower = FALSE
    )

  sentences_df <- corpus_df %>%
    tidytext::unnest_tokens(
      output = sentence,
      input = text,
      token = "sentences",
      to_lower = FALSE
    )

  tsGG <- shiny::eventReactive(input$go, {
    if (input$freq == "Absolute frequency") {

    } else if (input$freq == "Relative frequency") {

    }
  })

  output$word_frequency_gg <- shiny::renderPlot({
    if (input$go == 0) {

    } else {
      tsGG()
    }
  })

  kwic_df_r <- shiny::eventReactive(input$go, {
    temp <- sentences_df %>%
      dplyr::filter(date > input$date_range[1], date < input$date_range[2]) %>%
      dplyr::filter(stringr::str_detect(
        string = sentence,
        pattern = stringr::regex(
          ignore_case = TRUE,
          pattern = paste(
            as.character(stringr::str_to_lower(trimws(stringr::str_split(
              string = input$string,
              pattern = ",",
              simplify = TRUE
            )))),
            collapse = "|"
          )
        )
      )) %>%
      dplyr::mutate(Source = paste0("<a target='_blank' href='", link, "'>", title, "</a><br />")) %>%
      dplyr::rename(Sentence = sentence, Date = date) %>%
      dplyr::select(Date, Source, Sentence) %>%
      dplyr::arrange(dplyr::desc(Date))
    #
    #     if (length(as.character(stringr::str_to_lower(trimws(stringr::str_split(string = input$string,
    #                                                                             pattern = ",",
    #                                                                             simplify = TRUE)))))==1) {
    #       temp$Sentence <- purrr::map_chr(.x = temp$Sentence,
    #                                       .f = function (x)
    #                                         paste(c(rbind(as.character(stringr::str_split(string = x,
    #                                                                                       pattern = stringr::regex(pattern = as.character(input$string), ignore_case = TRUE), simplify = TRUE)),
    #                                                       c(paste0("<span style='background-color: #FFFF00'>",
    #                                                                as.character(stringr::str_extract_all(string = x,
    #                                                                                                      pattern = stringr::regex(as.character(input$string),
    #                                                                                                                               ignore_case = TRUE),
    #                                                                                                      simplify = TRUE)),
    #                                                                "</span>"), ""))),
    #                                               collapse = ""))
    #     }
    temp
  })

  ###### highlight searched keywords ######
  marker_kwic <- marker::marker$new("#kwic_DT")

  observeEvent(input$kwic_DT_rows_current,
    {
      marker_kwic$
        unmark(className = "green")$
        mark(trimws(as.character(stringr::str_split(input$string, pattern = ",", simplify = TRUE))), className = "green")
    },
    priority = -1
  )

  ###### Word counting  reactives######

  word_count_df_r <- shiny::eventReactive(input$go, {
    if (input$freq == "Absolute frequency") {
      count_df <- corpus_df %>%
        dplyr::rename(
          text = .data[[input$text_column]],
          date = .data[[input$group_by_column]]
        ) %>%
        castarter::cas_count(string = castarter:::cass_split(input$string))
    } else if (input$freq == "Relative frequency") {
      count_df <- corpus_df %>%
        dplyr::rename(
          text = .data[[input$text_column]],
          date = .data[[input$group_by_column]]
        ) %>%
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


  output$total_matches_DT_UI <- DT::renderDataTable(
    {
      DT::datatable(
        data = word_count_df_r(),
        options = list(
          dom = "lipt",
          pageLength = 3,
          lengthMenu = c(
            3,
            5,
            10,
            15,
            20
          )
        ),
        escape = FALSE,
        rownames = FALSE
      )
    },
    server = TRUE
  )


  output$word_count_summarised_DT_UI <- DT::renderDataTable(
    {
      DT::datatable(
        data = word_count_summarised_df_r(),
        options = list(
          dom = "lipt",
          pageLength = 3,
          lengthMenu = c(
            3,
            5,
            10,
            15,
            20
          )
        ),
        escape = FALSE,
        rownames = FALSE
      )
    },
    server = TRUE
  )


  ###### Column selector UI   ######

  output$column_selector_UI <- renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = "text_column",
        label = "Select text column",
        choices = c("", names(corpus_df)),
        selected = dplyr::if_else(is.element(
          "text",
          names(corpus_df)
        ), "text", "")
      ),
      shiny::selectInput(
        inputId = "group_by_column",
        label = "Select date column or column used for aggregation",
        choices = c("", names(corpus_df)),
        selected = dplyr::if_else(is.element(
          "date",
          names(corpus_df)
        ), "date", "")
      ),
      shiny::selectInput(
        inputId = "summarise_by",
        label = "If time/date column selected, aggregate by the following time unit:",
        choices = c("", "year", "quarter", "month", "day", "hour", "minute"),
        selected = dplyr::if_else(is.element(
          "date",
          names(corpus_df)
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

  output$moving_type_selector_UI <- selector_UI <- shiny::renderUI({
    shiny::radioButtons(
      inputId = "moving_type_selector",
      label = "Calculate moving...",
      choices = c(
        "average",
        "median",
        "sum",
        "Keep as is"
      ),
      selected = dplyr::if_else(condition = input$summarise_by == "day",
        true = "average",
        false = "average"
      ),
      inline = TRUE
    )
  })

  output$moving_selector_UI <- shiny::renderUI({
    shiny::tagList(
      shiny::numericInput(
        inputId = "moving_units_total",
        label = dplyr::if_else(condition = input$summarise_by == "",
          true = paste0(
            "Calculate moving ",
            input$moving_type_selector,
            " over how many units?"
          ),
          false = paste0(
            "Calculate moving ",
            input$moving_type_selector,
            " over how many ",
            input$summarise_by, "s?"
          )
        ),
        value = dplyr::if_else(input$summarise_by == "day",
          31L,
          1L
        ),
        min = 1L,
        step = 2
      ),
      shiny::radioButtons(
        inputId = "moving_length_type_selector",
        label = paste0(
          "Moving ",
          input$moving_type_selector,
          ", centred or non-centred?"
        ),
        choices = c(
          "centred",
          "non-centred"
        )
      ),
      conditionalPanel(condition = "input.moving_length_type_selector == 'non-centred'&input.moving_type_selector != 'Keep as is'", {
        shiny::tagList(
          shiny::numericInput(
            inputId = "moving_units_before",
            label = "Before units",
            value = 0L
          ),
          shiny::numericInput(
            inputId = "moving_units_after",
            label = "After units",
            value = 0L
          )
        )
      }),
      conditionalPanel(condition = "input.moving_length_type_selector == 'centred'&input.moving_type_selector != 'Keep as is'", {


      })
    )
  })

  output$pre_submit_help_text_UI <- renderUI({
    if (is.null(input$moving_type_selector) | is.null(input$summarise_by)) {
      return(NULL)
    }

    if (input$moving_type_selector == "Keep as is" & input$summarise_by != "") {
      text <- paste0(
        "With the current settings, the figure for each ",
        input$summarise_by,
        " corresponds to the ",
        stringr::str_to_lower(input$freq),
        " of matches found within that timeframe."
      )
    } else if (input$summarise_by != "") {
      text <- paste0(
        "With the current settings, the figure for each ",
        input$summarise_by,
        " corresponds to the moving ",
        input$moving_type_selector,
        " of the ",
        stringr::str_to_lower(input$freq),
        " calculated over ",
        input$moving_units_total,
        " ",
        input$summarise_by,
        "s, including the given ",
        input$summarise_by,
        ", and ",
        round((input$moving_units_total - 1) / 2),
        " ",
        input$summarise_by,
        dplyr::if_else((round((input$moving_units_total - 1) / 2)) > 1, "s", ""),
        " before and after it."
      )
    } else {
      text <- ""
    }
    shiny::helpText(text)
  })


  ##### renders summary table at the bottom of left column ######


  output$summary_tables_left_UI <- shiny::renderUI({
    if (is.null(kwic_df_r()) == FALSE & nrow(kwic_df_r()) > 0) {
      shiny::tagList(
        shiny::hr(),
        tabsetPanel(
          tabPanel(
            title = "Total matches",
            DT::DTOutput(outputId = "total_matches_DT_UI"),
            mod_cass_download_csv_ui("total_matches_download")
          ),
          tabPanel(
            title = "Processed data",
            DT::DTOutput(outputId = "word_count_summarised_DT_UI"),
            mod_cass_download_csv_ui("word_count_summarised_download")
          )
        )
      )
    }
  })

  ###### Renders table at the bottom of the main tab   ######
  output$kwic_DT <- DT::renderDataTable(
    {
      DT::datatable(
        data = kwic_df_r(),
        options = list(
          dom = "lipt",
          pageLength = 5,
          lengthMenu = c(
            3,
            5,
            10,
            15,
            20
          )
        ),
        escape = FALSE,
        rownames = FALSE
      )
    },
    server = TRUE
  )

  output$date_range_input_UI <- renderUI({
    shiny::dateRangeInput(
      inputId = "date_range",
      label = "Date range",
      start = min(corpus_df$date),
      end = max(corpus_df$date),
      weekstart = 1
    )
  })

  ##### modules #####

  ##### graph modules #####
  shiny::observeEvent(input$go,
    {
      if (input$graph_type == "Line chart") {
        castarter:::mod_cass_show_ts_dygraph_server(
          id = "cass_show_ts_dygraph_ui_1",
          count_df = word_count_summarised_df_r()
        )
      } else if (input$graph_type == "Bar chart") {
        castarter:::mod_cass_show_barchart_ggiraph_server(
          id = "cass_show_barchart_ggiraph_ui_1",
          count_df = word_count_summarised_df_r()
        )
      }
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  observeEvent(input$cicerone,
    {
      guide$init()$start()
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  mod_cass_download_csv_server(
    id = "total_matches_download",
    df = word_count_df_r()
  )
  mod_cass_download_csv_server(
    id = "word_count_summarised_download",
    df = word_count_summarised_df_r(),
    type = "processed"
  )
}
