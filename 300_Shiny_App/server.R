library(shiny)
library(tidyverse)
library(data.table)
library(plotly)

# Load processed screenplays data
folder.data <- "../100_Data"
folder.data.processed <- file.path(folder.data, "120_Processed_Data")

# Import lookup table for more readable movie names
movieNameLookup <- data.table::fread(
  file = file.path(
    folder.data.processed,
    'movie_name_lookup.csv'
  ),
  sep = ',',
  stringsAsFactors = FALSE,
  key = 'movie'
)

# Import data and remove records with fewer than 10 sections.
# This has the effect of not including very minor characters in the dashboard.
screenplayMoodProb.movieRotated <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "901_screenplayMoodProb.movieRotated.csv"
  ),
  sep = "|",
  quote = ""
)[
  # Append nicer, more readable movie name
  , movie := movieNameLookup[movie, movieFormatted]
][
  sectionCount > 10
]

plotLims.movie <- screenplayMoodProb.movieRotated[
  , .(
    PC1.min = min(PC1),
    PC1.max = max(PC1),
    PC2.min = min(PC2),
    PC2.max = max(PC2)
  )
]

screenplayMoodProb.movie <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "702_screenplayMoodProb.movie.csv"
  ),
  sep = "|",
  quote = ""
)[
  # Append nicer, more readable movie name
  , movie := movieNameLookup[movie, movieFormatted]
][
  sectionCount > 10
]

screenplayMoodProb.characterRotated <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "902_screenplayMoodProb.characterRotated.csv"
  ),
  sep = "|",
  quote = ""
)[
  # Append nicer, more readable movie name
  , movie := movieNameLookup[movie, movieFormatted]
][
  sectionCount > 10
]

plotLims.character <- screenplayMoodProb.characterRotated[
  , .(
    PC1.min = min(PC1),
    PC1.max = max(PC1),
    PC2.min = min(PC2),
    PC2.max = max(PC2)
  )
]

screenplayMoodProb.character <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "703_screenplayMoodProb.character.csv"
  ),
  sep = "|",
  quote = ""
)[
  # Append nicer, more readable movie name
  , movie := movieNameLookup[movie, movieFormatted]
][
  sectionCount > 10
]

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
    filtered <-  dplyr::filter(df, id %in% selection)
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
    searchHighlightCol = "id",
    searchDisplayCol = 'movie',
    text = ~paste0(
      "<b>", movie, "</b>",
      "<br>Word Count: ", tokenCount,
      "<br>Character Count: ", characterCount,
      "<br>Genres: ", genreList
    ),
    xlim = plotLims.movie[, c(PC1.min, PC1.max)] * 1.1,
    ylim = plotLims.movie[, c(PC2.min, PC2.max)] * 1.1
  )
  
  filteredCharacters <- shiny::reactive({
    screenplayMoodProb.characterRotated %>%
      dplyr::filter(movie %in% movieMoodLandscape$valueSelected)
  })

  # And for characters
  charactersMoodLandscape <- shiny::callModule(
    moodLandscapeServer,
    "charactersMoodLandscape",
    filteredCharacters,
    xCol = "PC1",
    yCol = "PC2",
    searchHighlightCol = "id",
    searchDisplayCol = 'character',
    text = ~paste0(
      "<b>", character, "</b>",
      "<br>Movie: ", movie,
      "<br>Word Count: ", tokenCount
    ),
    xlim = plotLims.character[, c(PC1.min, PC1.max)] * 1.1,
    ylim = plotLims.character[, c(PC2.min, PC2.max)] * 1.1
  )

  moodCols <- c(
    'fear', 'trust', 'joy', 'anticipation', 'anger', 'disgust', 'sadness',
    'surprise'
  )
  
  movieMoodStarData <- shiny::reactive({
    SelectMovies(screenplayMoodProb.movie, movieMoodLandscape$valueSelected)
  })
  
  # Start the server for the movies mood star module
  shiny::callModule(
    moodStarServer,
    "movieMoodStar",
    movieMoodStarData,
    idCol = 'id',
    nameCol = "movie",
    moodCols = paste0(moodCols, 'Percentile'),
    rLim = 1
  )
  
  characterMoodStarData <- shiny::reactive({
    SelectCharacters(
      screenplayMoodProb.character,
      charactersMoodLandscape$idSelected
    )
  })
  
  # Start the server for the characters mood star module
  shiny::callModule(
    moodStarServer,
    "characterMoodStar",
    characterMoodStarData,
    idCol = 'id',
    nameCol = "character",
    moodCols = paste0(moodCols, 'Percentile'),
    rLim = 1
  )
})
