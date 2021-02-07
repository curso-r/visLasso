#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      shiny::titlePanel("Lasso"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h3("Dados"),
          shiny::sliderInput("beta1", "Beta 1", -5, 5, 3, 0.5),
          shiny::sliderInput("beta2", "Beta 2", -5, 5, 4, 0.5),
          shiny::actionButton("gerar", "Gerar nova amostra"),
          shiny::hr(),
          shiny::h3("Restri\u00e7\u00e3o"),
          shiny::sliderInput("restricao", "Restri\u00e7\u00e3o", 0, 10, 1, 0.5),

        ),
        shiny::mainPanel(
          plotly::plotlyOutput("grafico", height = 600),
          shiny::hr(),
          shiny::h4("Valor \u00f3timo com restri\u00e7\u00e3o:"),
          shiny::tableOutput("tab_lasso")
        )
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
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'visLasso'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

