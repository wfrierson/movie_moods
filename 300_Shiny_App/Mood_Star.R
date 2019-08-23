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
#' @param rLim radius Limit for the visual
moodStarServer <- function(input,
                           output,
                           session,
                           dataset,
                           idCol,
                           nameCol,
                           moodCols,
                           moodLabels = NULL,
                           rLim = NA) {
  if (is.null(moodLabels)) {
    moodLabels <- moodCols
  }

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
            visible = FALSE,
            range = c(0, rLim)
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    ids <- dataset()[[idCol]]
    names <- dataset()[[nameCol]]
    for (index in seq_along(ids)) {
      row <- dataset() %>%
        dplyr::filter(get(idCol) == ids[index])
      p <- plotly::add_trace(
        p,
        r = abs(array(row[,moodCols])),
        theta = moodLabels,
        name = names[index],
        showlegend = FALSE
      )
    }

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})

}
