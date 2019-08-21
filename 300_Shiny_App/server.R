library(shiny)
library(tidyverse)
library(data.table)
library(plotly)

moodStarDummyData <- data.frame(
  row.names = c("Aliens", "Reservior Dogs", "Up"),
  anger = c(39, 1.5, 5),
  fear = c(28, 10, 6),
  anticipation = c(8, 39, 20),
  trust = c(7, 31, 4),
  surprise = c(28, 15, 30),
  sadness = c(39, 1.5, 3),
  joy = c(10, 5, 10),
  disgust = c(33, 22, 10)
)

# Load processed screenplays data
folder.data <- "../100_Data"
folder.data.processed <- file.path(folder.data, "120_Processed_Data")

screenplayMoodProb.movieRotated <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "901_screenplayMoodProb.movieRotated.csv"
  ),
  sep = "|",
  quote = ""
)

screenplayMoodscores <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "702_screenplayMoodProb.movie.csv"
  ),
  sep = "|",
  quote = ""
)


screenplayMoodProb.characterRotated <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "902_screenplayMoodProb.characterRotated.csv"
  ),
  sep = "|",
  quote = ""
)

SelectMovies <- function(df, selection) {
  filtered <- df
  
  if (length(selection) > 0) {
    filtered <-  dplyr::filter(df, movie %in% selection)
  }
}
  
FilterMovies <- function(df, genres, numCharacters) {
  # Default: if no filters selected, show all movies
  filtered <- df

  if (length(genres) > 0) {
    filtered <-  dplyr::filter_at(
      filtered,
      vars(genres),
      dplyr::any_vars(. == 1)
    )
  }
  
  filtered <-  dplyr::filter(
    filtered,
    characterCount >= numCharacters[1] & characterCount <= numCharacters[2]
  )

  return(filtered)
}

shinyServer(function(input, output) {
  # First, filter the movies dataset according to the filter panel
  filteredMovies <- shiny::reactive({
    FilterMovies(
      screenplayMoodProb.movieRotated,
      input$genreFilter,
      input$numCharactersFilter)
  })
  
  # Start the server for the movieMoodLandscape module
  movieMoodLandscape <- shiny::callModule(
    moodLandscapeServer,
    "movieMoodLandscape",
    filteredMovies,
    xCol = "PC1",
    yCol = "PC2",
    searchHighlightCol = "movie",
    text = ~paste(
      "<b>", movie, "</b>",
      "<br>Word Count: ", tokenCount,
      "<br>Character Count: ", characterCount,
      "<br>Genres: ", genreList
    )
  )
  
  filteredCharacters <- shiny::reactive({
    screenplayMoodProb.characterRotated %>%
      dplyr::filter(movie %in% movieMoodLandscape$selected)
  })

  # And for characters. TODO: bring in real datasets and enable cross filtering
  shiny::callModule(
    moodLandscapeServer,
    "charactersMoodLandscape",
    filteredCharacters,
    xCol = "PC1",
    yCol = "PC2",
    searchHighlightCol = "character",
    text = ~paste(
      'Character: ', character,
      '</br>Movie: ', movie,
      '</br>Word Count: ', tokenCount
    )
  )
  
  filtered <- shiny::reactive({
    SelectMovies(screenplayMoodscores, input$searchUi)
  })
  #as.data.frame(screenplayMoodProb.movieRotated)
  # Start the server for the movies mood star module
  shiny::callModule(
    moodStarServer,
    "movieMoodStar",
    filtered
  )
})
