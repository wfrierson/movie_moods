library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
  # Prepare a dummy dataset
  dataset <- mtcars %>%
    tibble::rownames_to_column() %>%
    dplyr::transmute(
      Movie = rowname,
      Genre = cyl,
      x = wt,
      y = mpg
    )

  # Start the server for the movieMoodLandscape module
  shiny::callModule(moodLandscapeServer, "movieMoodLandscape", dataset)
})
