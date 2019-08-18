library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
  # Prepare a dummy dataset
  utils::data("faithful")
  dataset <- faithful %>%
    transmute(x = eruptions, y = waiting)

  # Start the server for the movieMoodLandscape module
  shiny::callModule(moodLandscapeServer, "movieMoodLandscape", dataset)
})
