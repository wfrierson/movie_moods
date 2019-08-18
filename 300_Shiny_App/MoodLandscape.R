#' Mood Landscape plot module user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
moodLandscapeUi <- function(id, width, height) {
  ns <- shiny::NS(id)
  
  elements <- tagList(
    shiny::plotOutput(ns("plot"), width, height)
  )
  
  return(elements)
}



#' Mood Landscape plot module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param dataset data frame (non-reactive) with variables \code{x} and \code {y}
moodLandscapeServer <- function(input, output, session, dataset) {
  plot_obj <- reactive({
    p <- graphics::plot(dataset$x, dataset$y)
    return(p)
  })

  output$plot <- renderPlot({plot_obj()})
}
