library(shiny)
library(tidyverse)
library(data.table)
library(plotly)

# Load processed screenplays data
folder.data <- "../100_Data"
folder.data.processed <- file.path(folder.data, "120_Processed_Data")

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
  sectionCount > 10
]

screenplayMoodProb.movie <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "702_screenplayMoodProb.movie.csv"
  ),
  sep = "|",
  quote = ""
)[
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
  sectionCount > 10
]

screenplayMoodProb.character <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "703_screenplayMoodProb.character.csv"
  ),
  sep = "|",
  quote = ""
)[
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
    xlim = c(-0.18, 0.18), #c(-0.130182, 0.178242),
    ylim = c(-0.2, 0.2) #c(-0.194214, 0.169695)
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
    xlim = c(-0.18, 0.18), #c(-0.130182, 0.178242),
    ylim = c(-0.2, 0.2) #c(-0.194214, 0.169695)
  )

  moodCols <- c(
  'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise',
  'trust'
  )
  
  moodLabels <- tools::toTitleCase(moodCols)
  
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
    moodLabels = moodLabels,
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
    moodLabels = moodLabels,
    rLim = 1
  )
})
