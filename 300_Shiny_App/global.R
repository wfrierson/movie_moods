library(tidyverse)

# Prepare a dummy dataset
dataset <- mtcars %>%
  tibble::rownames_to_column() %>%
  dplyr::transmute(
    Movie = rowname,
    Genre = cyl,
    x = wt - mean(wt),
    y = mpg - mean(mpg)
  )
