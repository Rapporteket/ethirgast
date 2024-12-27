#' Server logic for the ETHIRGAST app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  # RegData <- ethirgast::loadEthirgastData()

  ethirgast::startpage_server("startpage")

  ##############################################################################
  ################ Subscription, Dispatchment and Stats ########################

  ## Objects currently shared among subscription and dispathcment
  # orgs <- as.list(BrValg$sykehus)
  # reports <- list(
  #   Kvartalsrapport = list(
  #     synopsis = "NORGAST: Kvartalsrapport",
  #     fun = "abonnement_kvartal_norgast",
  #     paramNames = c("baseName", "reshID"),
  #     paramValues = c("NorgastKvartalsrapport_abonnement", reshID)
  #   )
  # )
  #
  # ## Subscription
  # rapbase::autoReportServer(
  #   id = "norgastSubscription", registryName = "norgast",
  #   type = "subscription", reports = reports, orgs = orgs, freq = "quarter"
  # )
  #
  # ## Dispatchment
  # org <- rapbase::autoReportOrgServer("norgastDispatch", orgs)
  #
  # paramNames <- shiny::reactive(c("reshID"))
  # paramValues <- shiny::reactive(c(org$value()))
  #
  # rapbase::autoReportServer(
  #   id = "norgastDispatch", registryName = "norgast",
  #   type = "dispatchment", org = org$value, paramNames = paramNames,
  #   paramValues = paramValues, reports = reports, orgs = orgs,
  #   eligible = (userRole == "SC"), freq = "quarter"
  # )
  #
  # ## Stats
  # rapbase::statsServer("norgastStats", registryName = "norgast",
  #                      eligible = (userRole == "SC"))
  # rapbase::statsGuideServer("norgastStatsGuide", registryName = "norgast")
  #
  #
  # #Navbarwidget
  # output$appUserName <- renderText(rapbase::getUserFullName(session))
  # output$appOrgName <-
  #   shiny::renderText(
  #     names(BrValg$sykehus[BrValg$sykehus == rapbase::getUserReshId(session)])
  #   )

  # # Brukerinformasjon
  # userInfo <- rapbase::howWeDealWithPersonalData(session)
  # shiny::observeEvent(input$userInfo, {
  #   shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
  #                          type = "", imageUrl = "rap/logo.svg",
  #                          closeOnEsc = TRUE, closeOnClickOutside = TRUE,
  #                          html = TRUE, confirmButtonText = "Den er grei!")
  # })


  ##############################################################################
  # Eksport  ###################################################################
  # brukerkontroller
  # rapbase::exportUCServer("norgastExport", "norgast")
  #
  # ## veileding
  # rapbase::exportGuideServer("norgastExportGuide", "norgast")
  #
  ##############################################################################



}
