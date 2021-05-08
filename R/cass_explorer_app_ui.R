#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
cass_explorer_app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(version = 4,
                              bootswatch = "journal",
                              primary = "#3B9AB2"),
      tags$head(tags$style(
        ".red{background-color:#FFB8C3;}.blue{background-color:#6ECFEA;}.green{background-color:#a6ce39;}"
      )),
      tags$head(shiny::HTML(golem::get_golem_options("custom_head_html"))),
      tags$head(tags$script(HTML('$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#go").click();
}});'))),
      marker::use_marker(), # include marker dependencies
      waiter::use_waiter(), # include waiter dependencies
      cicerone::use_cicerone(), # include cicerone dependencies
      waiter::waiter_preloader(html = waiter::spin_fading_circles()),
      sidebarLayout(sidebarPanel(
        shiny::textInput(inputId = 'string',
                         label = 'String to be matched',
                         value = ifelse(test = is.null(golem::get_golem_options("default_string")),
                                        yes = "",
                                        no = golem::get_golem_options("default_string"))),

        shiny::uiOutput(outputId = "column_selector_UI"),

        shiny::radioButtons(inputId = "freq",
                            label = NULL,
                            choices = c("Absolute frequency",
                                        "Relative frequency")),

        shiny::uiOutput(outputId = "moving_type_selector_UI"),


        conditionalPanel(condition = "input.moving_type_selector != 'Keep as is'", {
          shiny::uiOutput(outputId = "moving_selector_UI")}),

        shiny::uiOutput(outputId = "date_range_input_UI")
        ,
        shiny::uiOutput(outputId = "pre_submit_help_text_UI"),
        shiny::actionButton("go", "Go!"),
        shiny::uiOutput(outputId = "summary_tables_left_UI")
      ),

      mainPanel(
        fluidRow(
          column(4,
                 h3("Select graph type"),
                 inputPanel(shiny::radioButtons(inputId = "graph_type",
                                                label = "Type of graph",
                                                choices = c("Line chart",
                                                            "Bar chart"))),
                 h3("About"),
                 shiny::actionButton(inputId = "cicerone",
                                     label = "Click for a guided tour",
                                     icon = icon("info-circle"),
                                     width = "100%"),
          ),
          column(8,
                 h3("Graph"),
                 shiny::conditionalPanel(condition = "input.graph_type == 'Line chart'", {
                   castarter2:::mod_cass_show_ts_dygraph_ui("cass_show_ts_dygraph_ui_1")
                 }),
                 shiny::conditionalPanel(condition = "input.graph_type == 'Bar chart'", {
                   castarter2:::mod_cass_show_barchart_ggiraph_ui("cass_show_barchart_ggiraph_ui_1")
                 })


          )
        ),
       # shiny::plotOutput("word_frequency_gg"),
        DT::dataTableOutput("kwic_DT")
      ))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'castarter2'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

