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
    ),
    shiny::uiOutput(ns("comment"))
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

  output$comment <- shiny::renderUI({
    if (nrow(dataset()) > 0) {
      p <- shiny::p(
        paste(
          "A point further from the center to a direction means more",
          "association with that mood."
        ),
        style = "padding: 4px"
      )
    } else {
      p <- shiny::p()
    }
    return(p)
  })

  plot_obj <- shiny::reactive({
    shiny::validate(
      shiny::need(
        nrow(dataset()) > 0,
        paste0(
          "Please select at least 1 ", nameCol,
          " to show the radar plot of aggregated moods for the selected ",
          nameCol, '(s).'
        )
      )
    )
    
    p <- plotly::plot_ly(
      dataset(),
      type = "scatterpolar",
      fill = "toself",
      mode = "markers"
    ) %>%
      plotly::layout(
        margin = list(l = 0, r = 0),
        polar = list(
          radialaxis = list(
            visible = FALSE,
            range = c(0, rLim)
          ),
          plot_bgcolor = "F0F0F0"
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
        showlegend = FALSE,
        hoverinfo = "text",
        hovertemplate = "%{theta}: %{r:.2f}"
      )
    }

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})

}
