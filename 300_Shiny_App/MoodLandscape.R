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
                                searchDisplayCol,
                                text,
                                searchResultName,
                                xlim,
                                ylim) {
  ns = session$ns
  MAX_SELECT = 3
  
  # Dynamically render the selectizeInput UI if dataset changes
  output$searchUi <- shiny::renderUI({
    data <- dataset()
    searchChoices <- data[[searchHighlightCol]]
    names(searchChoices) <- data[[searchDisplayCol]]
    
    # Remove selected movie if no longer in dataset
    existingSelection <- shiny::isolate(input$search)
    newSelection <- existingSelection[existingSelection %in% searchChoices]

    shiny::selectizeInput(
      ns("search"),
      "Search:",
      choices = searchChoices,
      selected = newSelection,
      multiple = TRUE,
      options = list(
        placeholder = paste("Choose up to", MAX_SELECT),
        maxItems = MAX_SELECT
      )
    )
  })
  
  # Render the plot UI
  xAxisOptions <- list(
    title = NA,
    showticklabels = FALSE,
    range = xlim
  )
  
  yAxisOptions <- list(
    title = NA,
    showticklabels = FALSE,
    range = ylim
  )

  plot_obj <- shiny::reactive({
    shiny::validate(
      shiny::need(
        nrow(dataset()) > 0,
        "Please select at least 1 movie to show the mood space of characters in the selected movie(s)."
      )
    )
    
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
        text = text,
        hoverinfo = "text",
        source = ns("A"),
        customdata = ~get(searchHighlightCol)
      )
    } else if (sum(selected) > 0) {
      p <- plotly::plot_ly(source = ns("A")) %>%
        add_trace(
          data = dataset() %>% 
            filter((!!as.name(searchHighlightCol)) %in% input$search),
          x = ~get(xCol),
          y = ~get(yCol),
          type = "scatter",
          mode = "markers",
          text = text,
          hoverinfo = "text",
          marker = list(
            color = 'rgb(31, 119, 180)'
          ),
          customdata = ~id
      ) %>%
        add_trace(
          data = dataset() %>% 
            filter(!((!!as.name(searchHighlightCol)) %in% input$search)),
          x = ~get(xCol),
          y = ~get(yCol),
          type = "scatter",
          mode = "markers",
          text = text,
          hoverinfo = "text",
          marker = list(
            color = 'rgba(31, 119, 180, 0.1)'
          ),
          customdata = ~id
        )
    }
    
    p <- p %>% 
      plotly::layout(
        margin = list(t = 1, l = 1, b = 1, r = 1),
        xaxis = xAxisOptions,
        yaxis = yAxisOptions,
        showlegend = FALSE,
        plot_bgcolor = "F0F0F0"
      ) %>%
      plotly::config(displayModeBar = FALSE)

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})
  
  # Return reactiveValues for downstream use
  vals <- reactiveValues()
  shiny::observe({
    vals$idSelected <- input$search
    vals$valueSelected <- (dataset() %>% 
                            filter(
                              (!!as.name(searchHighlightCol)) %in% input$search
                            ) %>% 
                             select_at(searchDisplayCol))[[searchDisplayCol]]
  })

  shiny::observe({
    clickData <- plotly::event_data("plotly_click", source = ns("A"))
    if (is.null(clickData)) {
      return(NULL)
    }

    output$searchUi <- shiny::renderUI({
      data <- dataset()
      searchChoices <- data[[searchHighlightCol]]
      names(searchChoices) <- data[[searchDisplayCol]]
      
      # Remove a selected movie if not in dataset
      existingSelection <- shiny::isolate(input$search)
      newSelection <- existingSelection[existingSelection %in% searchChoices]

      if (clickData$customdata %in% existingSelection) {
        # If the clicked point was already selected
        newSelection <- newSelection[newSelection != clickData$customdata]
      } else {
        # Otherwise, add it.
        newSelection <- c(clickData$customdata, newSelection)
      }

      
      shiny::selectizeInput(
        ns("search"),
        "Search:",
        choices = searchChoices,
        selected = newSelection,
        multiple = TRUE,
        options = list(
          placeholder = paste("Choose up to", MAX_SELECT),
          maxItems = MAX_SELECT
        )
      )
    })
  })

  return(vals)
}
