library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
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
})
