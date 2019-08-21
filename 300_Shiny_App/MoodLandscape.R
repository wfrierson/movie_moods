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
    shiny::selectizeInput(
      ns("search"),
      "Search by Title",
      choices = c("Select up to 5" = "", dataset()[[searchHighlightCol]]),
      selected = input$search,
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
    selected = sapply(dataset()[[searchHighlightCol]], function(x) {
      return(x %in% input$search)
    })
    
    if (sum(selected) == 0) {
      p <- plotly::plot_ly(
        data = dataset(),
        x = ~get(xCol),
        y = ~get(yCol),
        type = "scatter",
        mode = "markers",
        text = text
      ) %>%
        plotly::layout(xaxis = cleanAxis, yaxis = cleanAxis) %>%
        plotly::config(displayModeBar = FALSE)
    } else if (sum(selected) > 0) {
      p <- plotly::plot_ly() %>%
        add_trace(
          data = dataset() %>% 
            filter((!!as.name(searchHighlightCol)) %in% input$search),
          x = ~get(xCol),
          y = ~get(yCol),
          type = "scatter",
          mode = "markers",
          text = text,
          marker = list(
            color = 'rgb(31, 119, 180)'
          ),
          name = paste0('Selected ', searchHighlightCol, '(s)')
      ) %>%
        add_trace(
          data = dataset() %>% 
            filter(!((!!as.name(searchHighlightCol)) %in% input$search)),
          x = ~get(xCol),
          y = ~get(yCol),
          type = "scatter",
          mode = "markers",
          text = text,
          marker = list(
            color = 'rgba(31, 119, 180, 0.1)'
          ),
          name = paste0('Unselected ', searchHighlightCol, '(s)')
        ) %>% 
        plotly::layout(
          xaxis = cleanAxis,
          yaxis = cleanAxis,
          legend = list(
            orientation = 'h',
            xanchor = 'center',
            x = 0.5
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    }
    
    

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})
  
  # Return reactiveValues for downstream use
  vals <- reactiveValues()
  observe({
    
  })

  return(vals)
}
