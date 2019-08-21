library(shiny)
library(tidyverse)
library(data.table)
library(plotly)

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

screenplayMoodProb.character <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "703_screenplayMoodProb.character.csv"
  ),
  sep = "|",
  quote = ""
)

SelectMovies <- function(df, selection) {
  filtered <- head(df, 0)
  
  if (length(selection) > 0) {
    filtered <-  dplyr::filter(df, movie %in% selection)
  }
  
  return(filtered)
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

SelectCharacters <- function(df, selection) {
  filtered <- head(df, 0)
  
  if (length(selection) > 0) {
    filtered <-  dplyr::filter(df, character %in% selection)
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
    )
  )
  
  filteredCharacters <- shiny::reactive({
    screenplayMoodProb.characterRotated %>%
      dplyr::filter(movie %in% movieMoodLandscape$selected)
  })

  # And for characters
  charactersMoodLandscape <- shiny::callModule(
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

  moodCols <- c(
    "afraid",
    "amused",
    "angry",
    "annoyed",
    "dont_care",
    "happy",
    "inspired",
    "sad"
  )
  
  movieMoodStarData <- shiny::reactive({
    SelectMovies(screenplayMoodscores, movieMoodLandscape$selected)
  })
  
  # Start the server for the movies mood star module
  shiny::callModule(
    moodStarServer,
    "movieMoodStar",
    movieMoodStarData,
    nameCol = "movie",
    moodCols = moodCols
  )
  
  characterMoodStarData <- shiny::reactive({
    SelectCharacters(
      screenplayMoodProb.character,
      charactersMoodLandscape$selected
    )
  })
  
  # Start the server for the characters mood star module
  shiny::callModule(
    moodStarServer,
    "characterMoodStar",
    characterMoodStarData,
    nameCol = "character",
    moodCols = moodCols
  )
})
