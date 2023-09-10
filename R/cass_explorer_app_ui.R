#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
cass_explorer_app_ui <- function(request) {
  bslib::page_navbar(
    title = "castarter",
    theme = bslib::bs_theme(
      version = "5",
      preset = "sandstone",
    ) %>%
      bslib::bs_add_rules(rules = "
.navbar.navbar-inverse {
  background-color: $dark !important;
}"),
    header = shiny::tagList(
      ### headers ####
      waiter::waiter_preloader(
        html = waiter::spin_fading_circles(),
        fadeout = TRUE
      ),
      golem_add_external_resources(),
      tags$head(shiny::HTML(golem::get_golem_options("custom_head_html"))),
      tags$head(tags$script(HTML('$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#go").click();
}});')))
    ),
    sidebar = bslib::sidebar(
      width = 300,
      shiny::textInput(
        inputId = "pattern",
        label = "Pattern to be matched",
        value = ifelse(test = is.null(golem::get_golem_options("default_pattern")),
          yes = "",
          no = golem::get_golem_options("default_pattern")
        )
      ),
      shiny::actionButton(
        inputId = "go",
        label = "Go!",
        icon = shiny::icon("search"),
        width = "100%"
      ),
      shiny::uiOutput(outputId = "date_range_input_UI"),
      bslib::input_switch(
        id = "kwic_switch",
        label = "Show pattern in context",
        value = FALSE
      ),
      shiny::tagList({
        if (golem::get_golem_options("advanced")) {
          shiny::tagList(
            shiny::h3("Additional settings"),
            shiny::uiOutput(outputId = "column_selector_UI")
          )
        }
      })
    ),

    ### Barchart ####

    bslib::nav_panel(
      title = "Barcharts",
      shiny::uiOutput(outputId = "barchart_main_card_UI")
    ),

    ### Time series ####

    bslib::nav_panel(
      title = "Time series",
      "Time series goes here"
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::actionLink(
        inputId = "cicerone",
        label = "Click for a guided tour",
        icon = icon("info-circle"),
        width = "100%"
      )
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "castarter"
    ),
    waiter::use_waiter(), # include waiter dependencies
    cicerone::use_cicerone(), # include cicerone dependencies
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
