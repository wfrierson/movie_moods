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

shinyServer(function(input, output) {
  # Start the server for the movieMoodLandscape module
  movieMoodLandscape <- shiny::callModule(
    moodLandscapeServer,
    "movieMoodLandscape",
    screenplayMoodProb.movieRotated,
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

  # And for characters. TODO: bring in real datasets and enable cross filtering
  shiny::callModule(
    moodLandscapeServer,
    "charactersMoodLandscape",
    dataset,
    xCol = "x",
    yCol = "y",
    searchHighlightCol = "Movie",
    text = ~paste(
      'Character: ', x,
      '</br>Movie: ', x,
      '</br>Word Count: ', x
    )
  )
  
  # Start the server for the movies mood star module
  shiny::callModule(
    moodStarServer,
    "movieMoodStar",
    as.data.frame(screenplayMoodProb.movieRotated[1:2])
  )
})
