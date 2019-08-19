library(plotly)

#' Mood Star plot module user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
moodStarUi <- function(id, width, height) {
  ns <- shiny::NS(id)
  
  elements <- shiny::tagList(
    plotly::plotlyOutput(
      ns("plot"),
      width,
      height
    )
  )
  
  return(elements)
}



#' Mood Star plot module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param dataset a list of numeric values with names for each mood
moodStarServer <- function(input, output, session, dataset) {
  plot_obj <- shiny::reactive({
    labs <- c("anger", "fear", "anticipation", "trust", "surprise", "sadness", "joy", "disgust")
    p <- plotly::plot_ly(
      type = "scatterpolar",
      fill = "toself"
    ) %>%
      plotly::add_trace(
        r = c(39, 28, 8, 7, 28, 39, 10, 33),
        theta = labs,
        name = "Aliens",
        showlegend = FALSE
      ) %>%
      plotly::add_trace(
        r = c(1.5, 10, 39, 31, 15, 1.5, 5, 22),
        theta = labs,
        name = "Reservior Dogs",
        showlegend = FALSE
      ) %>%
      plotly::add_trace(
        r = c(5, 6, 20, 4, 30, 3, 10, 10),
        theta = labs,
        name = "Up",
        showlegend = FALSE
      ) %>%
      plotly::layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0,50)
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})
}
