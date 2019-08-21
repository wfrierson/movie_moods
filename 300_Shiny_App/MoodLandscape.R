library(ggplot2)

#' Mood Landscape plot module user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
moodLandscapeUi <- function(id, width, height) {
  ns <- shiny::NS(id)
  
  elements <- shiny::tagList(
    htmlOutput(ns("searchUi")),
    plotly::plotlyOutput(
      ns("plot"),
      width,
      height
    )
  )
  
  return(elements)
}



#' Mood Landscape plot module debug user interface for interactions
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
moodLandscapeIxDebugUi <- function(id) {
  ns <- shiny::NS(id)

  elements <- shiny::tagList(
    shiny::h3(paste("Debug info for", id)),
    shiny::fluidRow(
      shiny::column(width = 6, shiny::verbatimTextOutput(ns("hover_info"))),
      shiny::column(width = 6, shiny::verbatimTextOutput(ns("click_info")))
    ),
    shiny::fluidRow(
      shiny::column(width = 6, shiny::verbatimTextOutput(ns("brush_info"))),
      shiny::column(width = 6, shiny::verbatimTextOutput(ns("search_info")))
      
    )
  )
  
  return(elements)
}


#' Mood Landscape plot module server-side processing
#'
#' @param input, output, session standard \code{shiny} boilerplate
#' @param dataset data frame (reactive)
#' @param searchHighlightCol (non-reactive) variable for the name of column to
#' use when filtering for highlight in the visual
moodLandscapeServer <- function(input,
                                output,
                                session,
                                dataset,
                                xCol,
                                yCol,
                                searchHighlightCol,
                                text) {
  ns = session$ns
  
  # Dynamically render the selectizeInput UI
  output$searchUi <- shiny::renderUI({ 
    selectizeInput(
      ns("search"),
      "Search by Title",
      choices = c("Select up to 5" = "", dataset()[[searchHighlightCol]]),
      multiple = TRUE,
      options = list(maxItems = 5)
    )
  })
  
  # Render the plot UI
  cleanAxis <- list(
    title = NA,
    showticklabels = FALSE
  )

  plot_obj <- shiny::reactive({
    p <- plotly::plot_ly(
      data = dataset(),
      x = ~get(xCol),
      y = ~get(yCol),
      type = "scatter",
      mode = "markers",
      text = text,
      marker = list(
        color = 'rgba(31, 119, 180, 0.75)',
        size = 5
      )
    ) %>%
      plotly::layout(xaxis = cleanAxis, yaxis = cleanAxis) %>%
      plotly::config(displayModeBar = FALSE)

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})
  
  # Show debug outputs
  output$hover_info <- shiny::renderPrint({
    cat("Hover:\n")
    str(input$plot_hover)
  })
  output$click_info <- shiny::renderPrint({
    cat("Click:\n")
    str(input$plot_click)
  })
  output$brush_info <- shiny::renderPrint({
    cat("Brush:\n")
    str(input$plot_brush)
  })
  # Show debug output for search filter
  output$search_info <- shiny::renderPrint({
    cat("Search:\n")
    str(input$search)
  })

  # Return reactiveValues for downstream use
  vals <- reactiveValues()
  observe({
    
  })

  return(vals)
}
