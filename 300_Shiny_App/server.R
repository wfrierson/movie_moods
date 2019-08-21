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

screenplayMoodProb.characterRotated <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "902_screenplayMoodProb.characterRotated.csv"
  ),
  sep = "|",
  quote = ""
)

FilterMovies <- function(df, genres, numCharacters) {
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

# Using "RV" to denote a reactive value
GetCharacters <- function(df, movies) {
  filtered <- df

  if (length(movies > 0)) {
    filtered <-  dplyr::filter(
      filtered,
      movie %in% movies
    )
  }
}

FilterCharacters <- function(df, characters) {
  filtered <- df

  if (length(characters) > 0) {
    filtered <-  dplyr::filter_at(
      filtered,
      vars(character),
      dplyr::any_vars(. %in% characters)
    )
  }
  
  

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
    ),
    searchResultName = 'movieSelection'
  )
  
  filteredCharacters <- shiny::reactive({
    GetCharacters(
      screenplayMoodProb.characterRotated,
      movieMoodLandscape[['movieSelection']]()
    )
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
    ),
    searchResultName = 'characterSelection'
  )
  
  # Start the server for the movies mood star module
  shiny::callModule(
    moodStarServer,
    "movieMoodStar",
    moodStarDummyData
  )
})
