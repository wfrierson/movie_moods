library(shiny)
library(tidyverse)

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

shinyServer(function(input, output) {
  # Prepare a dummy dataset
  dataset <- mtcars %>%
    tibble::rownames_to_column() %>%
    dplyr::transmute(
      Movie = rowname,
      Genre = cyl,
      x = wt - mean(wt),
      y = mpg - mean(mpg)
    )

  # Start the server for the movieMoodLandscape module
  movieMoodLandscape <- shiny::callModule(
    moodLandscapeServer,
    "movieMoodLandscape",
    dataset
  )

  # And for characters. TODO: bring in real datasets and enable cross filtering
  shiny::callModule(
    moodLandscapeServer,
    "charactersMoodLandscape",
    movieMoodLandscape$brushedPoints
  )
  
  # Start the server for the movies mood star module
  shiny::callModule(
    moodStarServer,
    "movieMoodStar",
    moodStarDummyData
  )
})
