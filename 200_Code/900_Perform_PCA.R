###############################################################################
# This script performs principal component analysis on the movie-level
# aggregations of moods.

###############################################################################
# SETUP

folder.data <- '100_Data'
folder.data.processed <- file.path(folder.data, '120_Processed_Data')
folder.data.raw <- file.path(folder.data, '110_Raw_Data')
folder.code <- '200_Code'

path.dependencies <- file.path(folder.code, '000_Dependencies.R')
path.pcaFunctions <- file.path(folder.code, '800_PCA_Functions.R')
source(path.dependencies)
source(path.pcaFunctions)

###############################################################################
# IMPORT DATA

screenplayPaths <- data.table::fread(
  file.path(folder.data.processed, '301_screenplayPaths.csv'),
  stringsAsFactors = FALSE,
  quote = "",
  sep = '|'
)

screenplayMoodProb.movie <- data.table::fread(
  file.path(folder.data.processed, '702_screenplayMoodProb.movie.csv'),
  stringsAsFactors = FALSE,
  quote = "",
  sep = '|',
  key = c('id', 'movie')
)

screenplayMoodProb.character <- data.table::fread(
  file.path(folder.data.processed, '703_screenplayMoodProb.character.csv'),
  stringsAsFactors = FALSE,
  quote = "",
  sep = '|',
  key = c('id', 'movie', 'character')
)

screenplayMoodProb.scene <- data.table::fread(
  file.path(folder.data.processed, '704_screenplayMoodProb.scene.csv'),
  stringsAsFactors = FALSE,
  quote = "",
  sep = '|',
  key = c('id', 'movie', 'sceneNumber')
)

screenplayMoodProb.sceneCharacter <- data.table::fread(
  file.path(folder.data.processed, '705_screenplayMoodProb.sceneCharacter.csv'),
  stringsAsFactors = FALSE,
  quote = "",
  sep = '|',
  key = c('id', 'movie', 'sceneNumber', 'character')
)

###############################################################################
# PERFORM PCA ON MOVIE-LEVEL MOOD AGGREGATIONS

# Create vector of NRC moods
moods.NRC <- c(
  'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise',
  'trust'
)

# Perform PCA at movie-level
pcaMovie <- prcomp(
  screenplayMoodProb.movie[, mget(moods.NRC)],
  center = TRUE,
  scale. = FALSE,
  retx = TRUE
)

# Rotate each mood aggregation table for visualizing downstream
screenplayMoodProb.aggregationsRot <- lapply(
  c(
    'screenplayMoodProb.movie', 'screenplayMoodProb.character', 
    'screenplayMoodProb.scene','screenplayMoodProb.sceneCharacter'
  ),
  RotateMoodAggregation
)

###############################################################################
# EXPORT RESULTS

data.table::fwrite(
  screenplayMoodProb.aggregationsRot[[1]],
  file = file.path(
            folder.data.processed,
            '901_screenplayMoodProb.movieRotated.csv'
         ),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)

data.table::fwrite(
  screenplayMoodProb.aggregationsRot[[2]],
  file = file.path(
            folder.data.processed,
            '902_screenplayMoodProb.characterRotated.csv'
         ),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)

data.table::fwrite(
  screenplayMoodProb.aggregationsRot[[3]],
  file = file.path(
            folder.data.processed,
            '903_screenplayMoodProb.sceneRotated.csv'
         ),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)

data.table::fwrite(
  screenplayMoodProb.aggregationsRot[[4]],
  file = file.path(
            folder.data.processed,
            '904_screenplayMoodProb.sceneCharacterRotated.csv'
         ),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)
