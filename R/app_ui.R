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
    # Your application UI logic
    fluidPage(
      titlePanel("Monero Consensus Status"),
      shiny::h4(shiny::HTML('The open source code for this web app is available <a href="https://github.com/Rucknium/xmrconsensus">here</a>.')),
      shiny::br(),
      # golem::golem_welcome_page() # Remove this line to start building your UI
      plotOutput("plot1", inline = TRUE),
      # inline = TRUE means that it displays the plot with dimensions specified on the server side
      shiny::hr(),
      shiny::h4(shiny::HTML('Created by <a href="https://github.com/Rucknium">Rucknium</a> at the <a href="https://github.com/monero-project/research-lab">Monero Research Lab</a>')),
      shiny::h5(shiny::HTML('Pool mining data collected by <a href="https://git.gammaspectra.live/WeebDataHoarder/monero-blocks">monero-blocks</a>, developed by DataHoarder.'))
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
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "xmrconsensus"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
