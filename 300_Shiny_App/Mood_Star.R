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
moodStarServer <- function(input, output, session, filtered) {
  
  #set up elements independent of filtering
  #afraid|amused|angry|annoyed|dont_care|happy|inspired|sad|
  moodLabels <- c("afraid", "amused", "angry", "annoyed", "dont_care", "happy", "inspired", "sad")
  moodcols <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")
  movies <- reactive({filtered()$movie})
  movie_nums <- reactive({rownames(filtered())})
      #movies_labs <- dataset[movie,]
  
  plot_obj <- shiny::reactive({
    p <- plotly::plot_ly(
      filtered()[,moodLabels],
      type = "scatterpolar",
      fill = "toself"
    ) %>%
      plotly::layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0,0.15)
          )
        )
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    for (movie_num in movie_nums()) {
      p <- plotly::add_trace(
        p,
        r = abs(array(filtered()[movie_num,moodLabels])),
        theta = moodLabels,
        name = filtered()[movie_num,1],
        showlegend = FALSE
      )
    }

    return(p)
  })

  output$plot <- plotly::renderPlotly({plot_obj()})

}
