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
#' @param dataset a data-frame of values with row names as movie titles and
#' column names for moods
moodStarServer <- function(input, output, session, dataset) {
  
  #this doesnt work yet
  filtered <- reactive( { subset(dataset, movie %in% c("12monkeys","8mm")) })
  #filtered <- reactive( { subset(dataset, movie %in% output$searchUi) })
  
  moodLabels <- c("anger", "fear", "anticipation", "trust", "surprise", "sadness", "joy", "disgust")
  moodcols <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")
  movies <- reactive({rownames(filtered())})
  #movies_labs <- dataset[movie,]
  
  plot_obj <- shiny::reactive({
    p <- plotly::plot_ly(
      filtered()[,moodcols],
      type = "scatterpolar",
      fill = "toself"
    ) %>%
      plotly::layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0,0.2)
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    for (movie in movies()) {
      p <- plotly::add_trace(
        p,
        r = abs(array(filtered()[movie,moodcols])),
        theta = moodLabels,
        #name = movie,
        showlegend = FALSE
      )
    }

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})
}
