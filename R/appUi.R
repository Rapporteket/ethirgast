#' Client (ui) for the ethirgast app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  shiny::addResourcePath('rap', system.file('www', package='rapbase'))
  regTitle = "ETHIRGAST"

  # Define UI for application
  ui <- shiny::navbarPage(
    id = "ethirgast_app_id",

    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    shiny::tabPanel(
      "Startpage",
      shinyjs::useShinyjs(),
      shiny::tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
      ethirgast::startpage_ui("startpage")
    )#,

  #   shiny::tabPanel(
  #     "Distributions",
  #     ethirgast::fordelingsfig_UI(id = "fordelingsfig_id")
  #   ),
  #
  #
  #   shiny::tabPanel(
  #     "Funnel plot",
  #     ethirgast::traktplot_UI(id = "traktplot_id")
  #   ),
  #
  #   shiny::navbarMenu(
  #     "Tidsvisning",
  #     shiny::tabPanel("Andeler over tid",
  #                     ethirgast::tidsvisning_UI(id = "tidsvisning_id")
  #     ),
  #     shiny::tabPanel("Sammenlign andeler",
  #                     ethirgast::saml_andeler_UI(id = "saml_andeler_id")
  #     )
  #   ),
  #
  #   shiny::tabPanel("Quality indikators",
  #                   ethirgast::indikatorfig_UI(id = "indikator_id")
  #   ),
  #
  #   shiny::tabPanel("Survival",
  #                   ethirgast::overlevelse_UI(id = "overlevelse_id")
  #   ),
  #
  #   shiny::tabPanel("Aggregated reports",
  #                   shiny::h2("Samledokumenter", align='center'),
  #                   shiny::h4("When you select ", strong("Download report"),
  #                             " a document consisting of text, tables and figures
  #                             is generated. The report is based on the latest
  #                             available data in the registry.", align='center'),
  #                   shiny::br(),
  #                   shiny::br(),
  #                   ethirgast::samledok_UI(id = "samledok_id")
  #   ),
  #
  #   shiny::tabPanel(
  #     "Datadump",
  #     shiny::h2("Datadump", align='center'),
  #     shiny::h4("Here you can download the raw data tables from ETHIRGAST.",
  #               align='center'),
  #     shiny::br(),
  #     shiny::br(),
  #     ethirgast::datadump_UI(id = "datadump_id")
  #   ),
  #
  #   shiny::tabPanel("Administrative tables",
  #                   ethirgast::admtab_UI(id = "admtab_id")
  #   ),
  #
  #   shiny::tabPanel("Datakvalitet",
  #                   ethirgast::datakval_ui("datakval_id")
  #   ),
  #
  #   # shiny::tabPanel(
  #   #   shiny::span("Abonnement",
  #   #               title="Bestill tilsending av rapporter på e-post"),
  #   #   shiny::sidebarLayout(
  #   #     shiny::sidebarPanel(
  #   #       rapbase::autoReportInput("ethirgastSubscription")
  #   #     ),
  #   #     shiny::mainPanel(
  #   #       rapbase::autoReportUI("ethirgastSubscription")
  #   #     )
  #   #   )
  #   # ),
  #
  #   shiny::navbarMenu(
  #     "Tools",
  #
  #     shiny::tabPanel(
  #       "Export",
  #       shiny::sidebarLayout(
  #         shiny::sidebarPanel(
  #           rapbase::exportUCInput("ethirgastExport")
  #         ),
  #         shiny::mainPanel(
  #           rapbase::exportGuideUI("ethirgastExportGuide")
  #         )
  #       )
  #     ),
  #
  #     shiny::tabPanel(
  #       "Bruksstatistikk",
  #       shiny::sidebarLayout(
  #         shiny::sidebarPanel(rapbase::statsInput("ethirgastStats")),
  #         shiny::mainPanel(
  #           rapbase::statsUI("ethirgastStats"),
  #           rapbase::statsGuideUI("ethirgastStatsGuide")
  #         )
  #       )
  #     )
  #   )
  )

}
