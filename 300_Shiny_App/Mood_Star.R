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
#' @param dataset a (reactive) data-frame of values
#' @param nameCol the column to use as series name
#' @param moodCols the columns to use for plotting the mood star
moodStarServer <- function(input, output, session, dataset, nameCol, moodCols) {
  plot_obj <- shiny::reactive({
    p <- plotly::plot_ly(
      dataset(),
      type = "scatterpolar",
      fill = "toself",
      mode = "markers"
    ) %>%
      plotly::layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 0.15)
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    names <- dataset()[[nameCol]]
    for (name in names) {
      row <- dataset() %>%
        dplyr::filter(get(nameCol) == name)
      print(row)
      p <- plotly::add_trace(
        p,
        r = abs(array(row[,moodCols])),
        theta = moodCols,
        name = name,
        showlegend = FALSE
      )
    }

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})

}
