#' UI-part av startpage for ETHIRGAST's shiny application
#'
#' @export
startpage_ui <- function(id){
  ns <- NS(id)
  shiny::bootstrapPage(
    div(class = "container",
        div(class = "panel panel-default",
            div(class = "panel-heading" , style = "background-color : #E0E0E0 ",
                h2('Welcome to the report service for ETHIRGAST', align='center')),
            div(class = "panel-body",style = "background-color:#F0F0F0",
                div(class="panel-text",
                    br(),
                    h4('You have now entered the report service for ETHIRGAST.
                    Here you will find a collection of figures and tables which
                    display relevant results based on the data collected in
                    ETHIRGAST.'),
                    br(),
                    h4(tags$b(tags$u('Content in tabs:'))),
                    div(class = "container", style ="margin-right:(@gutter / 10)" ,
                        h4(tags$b('Distributions '), 'displays the distribution of some
                        central variables from ETHIRGAST.')
                    )
                )
            )
        )
    )
  )
}

#' Server-part av startpage for ETHIRGAST's Shiny-application
#'
#' @export
startpage_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      print("Printing something")
    }
  )
}
