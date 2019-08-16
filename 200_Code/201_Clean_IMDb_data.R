require(R.utils) # Needed for data.table::fread to work with gzipfiles
library(tidyverse)

folder.data <- "100_Data"
folder.data.imdb <- file.path(folder.data, "imdb")
name.imdb.basics <- "title.basics.tsv.gz"
name.imdb.akas <- "title.akas.tsv.gz"
name.imdb.ratings <- "title.ratings.tsv.gz"


compressed.path <- file.path(folder.data.imdb, name.imdb.basics)
basics <- data.table::fread(
  compressed.path,
  sep = "\t",
  sep2 = ",",
  quote = "",
  header = TRUE,
  na.strings = "\\N",
  colClasses = c(
    "tconst" = "character",
    "primaryTitle" = "character",
    "originalTitle" = "character"
  ),
  fill = FALSE
)

# Filter the IMDB dataset to just movies, not TV
imdbMovies <- basics %>%
  dplyr::filter(titleType %in% c("short", "movie", "tvMovie")) %>%
  # And transform the title like how IMSDb names the files
  dplyr::mutate(movie = stringr::str_remove_all(tolower(primaryTitle), "\\s"))

screenplayStatsSelection <-
  readRDS(file.path(folder.data, "screenplayStatsSelection.rds"))

selectedMovieNames <- screenplayStatsSelection %>%
  dplyr::select(movie) %>%
  dplyr::distinct()

# These are the IMSDb movie names where we did not have a match by IMDb title
unmatched <- selectedMovieNames %>%
  dplyr::anti_join(imdbMovies, by = "movie")

# These are the IMSDb movie names where we did have a match by IMDb title
matched <- selectedMovieNames %>%
  dplyr::semi_join(imdbMovies, by = "movie")

# For those that matched, did we get multiple matches?
matchesByMovie <- matched %>%
  dplyr::left_join(imdbMovies, by = "movie") %>%
  dplyr::group_by(movie) %>%
  dplyr::summarise(matches = dplyr::n())

summary(matchesByMovie$matches)
