###############################################################################
# This script joins the tagged tokens of each screenplay to the DepecheMood++
# emotion lexicon to perform sentiment analysis.

###############################################################################
# SETUP

folder.data <- '100_Data'
folder.data.processed <- file.path(folder.data, '120_Processed_Data')
folder.data.raw <- file.path(folder.data, '110_Raw_Data')
folder.code <- '200_Code'

path.dependencies <- file.path(folder.code, '000_Dependencies.R')
path.sentimentFunctions <- file.path(
  folder.code,
  '600_Sentiment_Aggregation_Functions.R'
)
source(path.dependencies)
source(path.sentimentFunctions)

###############################################################################
# IMPORT DATA

screenplayTagged <- readRDS(
  file.path(folder.data.processed, '501_screenplayTagged.rds')
)

screenplayTransformed <- readRDS(
  file.path(folder.data.processed, '401_screenplayTransformed.rds')
)

dictionary.NRC <- data.table::fread(
  file.path(folder.data.processed, '101_dictionary.NRC.csv')
  , stringsAsFactors = FALSE
)

###############################################################################
# APPEND MOODS FROM EMOLEX

# Use lemmatized token unless it's NULL. Otherwise, use the original token.
screenplayTagged[
  , Term := ifelse(
    lemma == '<unknown>',
    token,
    stringi::stri_trans_tolower(lemma)
  )
]

moods.NRC = c(
  'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise',
  'trust'
)

# Append word-mood probabilities from EmoLex using exact match via Term
screenplayMoodProb <- dictionary.NRC[, mget(c('Term', moods.NRC))][
  screenplayTagged[!is.na(posLabel)]
  , on = .(Term)
][
  is.na(anger)
  , (moods.NRC) := FALSE
]

# Aggregate match rate of tokens from selected screenplays with EmoLex:
#
# nrow(screenplayMoodProb) / nrow(screenplayTagged[!is.na(posLabel)])
#
# 0.5371756

###############################################################################
# AGGREGATE MOODS AT DIFFERENT LEVELS

# Append character labels to screenplayMoodProb
screenplayMoodProb <- unique(screenplayTransformed[
  !is.na(character),
  .(movie, sectionNumber, sceneNumber, component, character)
])[
  screenplayMoodProb
  , on = .(movie, sectionNumber, sceneNumber, component)
]

# Aggregate to movie-level to compare mood probabilities
screenplayMoodProb.movie <- AggregateMoods(
  moodTable = screenplayMoodProb,
  aggregateString = 'movie'
)

# Aggregate to character-level to compare mood probabilities
screenplayMoodProb.character <- AggregateMoods(
  moodTable = screenplayMoodProb,
  aggregateString = c('movie', 'character')
)[!is.na(character)]

# Aggregate to scene-level to compare mood probabilities
screenplayMoodProb.scene <- AggregateMoods(
  moodTable = screenplayMoodProb,
  aggregateString = c('movie', 'sceneNumber')
)

# Aggregate to scene & character level to compare mood probabilities
screenplayMoodProb.sceneCharacter <- AggregateMoods(
  moodTable = screenplayMoodProb,
  aggregateString = c('movie', 'sceneNumber', 'character')
)[
  # Give better labeling for rows representing scene descriptions with no
  # dialogue
  is.na(character) & characterCount == 0
  , character := '(Scene Description)'
]

###############################################################################
# EXPORT RESULTS

saveRDS(
  screenplayMoodProb,
  file = file.path(folder.data.processed, '701_screenplayMoodProb.rds')
)

data.table::setorder(screenplayMoodProb.movie, movie)
data.table::fwrite(
  screenplayMoodProb.movie,
  file = file.path(folder.data.processed, '702_screenplayMoodProb.movie.csv'),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)

data.table::setorder(screenplayMoodProb.character, movie, -tokenCount)
data.table::fwrite(
  screenplayMoodProb.character,
  file = file.path(
            folder.data.processed,
            '703_screenplayMoodProb.character.csv'
         ),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)

data.table::setorder(screenplayMoodProb.scene, movie, sceneNumber)
data.table::fwrite(
  screenplayMoodProb.scene,
  file = file.path(
            folder.data.processed,
            '704_screenplayMoodProb.scene.csv'
         ),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)

data.table::setorder(
  screenplayMoodProb.sceneCharacter,
  movie, sceneNumber, -tokenCount
)
data.table::fwrite(
  screenplayMoodProb.sceneCharacter,
  file = file.path(
            folder.data.processed,
            '705_screenplayMoodProb.sceneCharacter.csv'
         ),
  row.names = FALSE,
  quote = FALSE,
  sep = '|'
)
