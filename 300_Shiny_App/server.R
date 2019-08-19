library(shiny)
library(tidyverse)

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
})
