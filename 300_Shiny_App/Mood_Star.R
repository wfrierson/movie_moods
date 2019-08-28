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
                           rLim = NA) {
  output$comment <- shiny::renderUI({
    if (nrow(dataset()) > 0) {
      p <- shiny::p(
        paste(
          "A point further from the center to a direction means more",
          "association with that mood.",
          "Moods are represented here as percentiles. E.g., a ", nameCol,
          "with a mood percentile above 50% means that mood is",
          "abnormally frequent for that", nameCol, "."
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
    
    # Melting table 
    dataset.melt <- data.table::melt(
      data.table::as.data.table(dataset()),
      id.vars = c(idCol, nameCol),
      measure.vars = moodCols,
      variable.name = 'mood',
      value.name = 'moodPercentile',
      variable.factor = FALSE
    )[
      # Clean up mood label for plotting
      , mood := gsub('Percentile', '', mood) %>% tools::toTitleCase(.)
    ]
    
    # Due to plotly bug, categorical line plots can only be connected if the
    # final factor values are duplicated. For the EmoLex, the final mood
    # category is fear.
    dataset.melt <- data.table::rbindlist(list(
      dataset.melt,
      dataset.melt[mood == 'Fear']
    ))
    
    p <- plotly::plot_ly(
      dataset.melt,
      type = "scatterpolar",
      mode = "lines",
      # fill = 'toself'
      hoverinfo = "text",
      hovertemplate = "%{theta} Percentile: %{r:.2f}",
      r = ~moodPercentile,
      theta = ~mood,
      name = ~get(nameCol)
    ) %>%
      plotly::layout(
        margin = list(l = 0, r = 0),
        polar = list(
          radialaxis = list(
            visible = FALSE,
            range = c(0, rLim)
          ),
          plot_bgcolor = "F0F0F0"
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})

}
