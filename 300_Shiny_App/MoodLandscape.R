library(ggplot2)

#' Mood Landscape plot module user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
moodLandscapeUi <- function(id, width, height) {
  ns <- shiny::NS(id)
  
  elements <- shiny::tagList(
    htmlOutput(ns("search")),
    shiny::plotOutput(
      ns("plot"),
      width,
      height,
      hover = ns("plot_hover"),
      click = ns("plot_click"),
      brush = ns("plot_brush")
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
#' @param dataset data frame (non-reactive) with variables \code{x} and \code {y}
#' @param searchHighlightCol (non-reactive) variable for the name of column to
#' use when filtering for highlight in the visual
moodLandscapeServer <- function(input,
                                output,
                                session,
                                dataset,
                                searchHighlightCol) {
  
  # Dynamically render the selectizeInput UI
  output$search <- renderUI({ 
    selectizeInput(
      "search",
      "Search by Title",
      choices = c("Select up to 5" = "", dataset[[searchHighlightCol]]),
      multiple = TRUE,
      options = list(maxItems = 5)
    )
  })
  
  # Render the plot UI
  plot_obj <- shiny::reactive({
    p <- ggplot2::ggplot(dataset, aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::geom_vline(xintercept = 0) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        legend.position = "none")
    return(p)
  })

  output$plot <- shiny::renderPlot({plot_obj()})
  
  
  
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
    vals$brushedPoints <- shiny::brushedPoints(dataset, input$plot_brush)
  })

  return(vals)
}
