require(R.utils) # Needed for data.table::fread to work with gzipfiles
library(tidyverse)

folder.data <- '100_Data'
folder.data.imdb <- file.path(folder.data, 'imdb')
name.imdb.basics <- 'title.basics.tsv.gz'
name.imdb.akas <- 'title.akas.tsv.gz'
name.imdb.ratings <- 'title.ratings.tsv.gz'


compressed.path <- file.path(folder.data.imdb, name.imdb.basics)
basics <- data.table::fread(compressed.path,
                            sep = '\t',
                            sep2 = ',',
                            quote = '',
                            header = TRUE,
                            na.strings = '\\N',
                            colClasses = c(
                              'tconst' = 'character',
                              'primaryTitle' = 'character',
                              'originalTitle' = 'character'
                            ),
                            fill = FALSE)



screenplayStatsSelection <- readRDS(file.path(folder.data, 'screenplayStatsSelection.rds'))

# Convert the  movie titles in the IMDb dataset into a format that
# should match how the imsdb files are organised
basics[, movie:=str_remove_all(tolower(primaryTitle), '\\s')]

# Try to match each of the movie names from the IMSDb dataset to
# the corresponding entry from IMDb
unmatched <- screenplayStatsSelection[!basics, on='movie', movie, by=movie]

# These are the ones where we did not have a match by IMDb title
movies <- unique(screenplayStatsSelection[, movie])
