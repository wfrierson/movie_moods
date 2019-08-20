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

###############################################################################
# PREPARE EMOTIONAL LEXICON

file.DMpp <- file.path(
  'DepecheMood++',
  'DepecheMood_english_lemmapos_full.tsv'
)

# Create vector of DM++ moods and column names
moods.DMpp <- c(
  'afraid', 'amused', 'angry', 'annoyed', 'dont_care', 'happy', 'inspired',
  'sad'
)
names(moods.DMpp) <- moods.DMpp
fields.DMpp <- c('tokenPOS', moods.DMpp, 'freq')

# Import DM++ lexicon
dictionary.DMpp <- data.table::fread(
  file.path(folder.data.raw, file.DMpp)
  , header = TRUE
  , col.names = fields.DMpp
)

# Calculate overall tokenPOS probability
dictionary.DMpp[
  , prob := freq / dictionary.DMpp[, sum(freq)]
]

# Calculate overall probabilities for each mood
moodProb.DMpp <- dictionary.DMpp[
  , lapply(
      .SD,
      function(mood) sum(mood * prob)
  )
  , .SDcols = moods.DMpp
]

# Use Bayes' rule to calculate probability of getting word given each mood
# and append token part-of-speech to tokenPOS-mood probabilities
dictionary.DMpp <- cbind(
  dictionary.DMpp[, .(tokenPOS)],
  dictionary.DMpp[
    , lapply(
        moods.DMpp, function(mood) get(mood) * prob / moodProb.DMpp[, get(mood)]
      )
  ]
)

###############################################################################
# APPEND MOODS FROM DEPECHEMOOD++

# Append word-mood probabilities from DM++ using exact match via tokenPOS
screenplayMoodProb <- dictionary.DMpp[, mget(fields.DMpp[-10])][
  screenplayTagged[!is.na(tokenPOS)]
  , on = .(tokenPOS)
]

# Retry matching with DM++ for those tokens with no match in the first round. 
# This time match using the column, lemmaPOS, which uses the lemmatized
# version of the token to account for commoon words that by bad luck don't
# appear in DM++. 
#
# E.g., 'climbs' is not in DM++, but 'climb' is. Lemmatizing 'climbs' to 'climb'
# and using the tagged part-of-speech would append the correct mood 
# probabilities from DM++.
screenplayMoodProb.lemmaMatch <- dictionary.DMpp[, mget(fields.DMpp[-10])][
  screenplayMoodProb[is.na(afraid)]
  , on = .(tokenPOS = lemmaPOS)
  , nomatch = FALSE
][
  # Delete unneeded columns
  , paste0('i.', c('tokenPOS', moods.DMpp)) := NULL
]

# Combine tokens where tokenPOS matched, or tokenPOS didn't match but lemmaPOS
# did match
screenplayMoodProb <- rbindlist(list(
  screenplayMoodProb[!is.na(afraid), -'lemmaPOS'],
  screenplayMoodProb.lemmaMatch
))

# Remove partial matches via lemmaPOS
rm(screenplayMoodProb.lemmaMatch)

# Aggregate match rate of tokens from selected screenplays with DM++:
#
# nrow(screenplayMoodProb) / nrow(screenplayTagged[!is.na(tokenPOS)])
#
# 0.8876894

###############################################################################
# AGGREGATE MOODS AT DIFFERENT LEVELS

# Append character labels to screenplayMoodProb
screenplayMoodProb <- screenplayTransformed[
  !is.na(character),
  .N,
  keyby = .(movie, sectionNumber, sceneNumber, component, character)
][
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

data.table::fwrite(
  dictionary.DMpp,
  file = file.path(folder.data.processed, '701_dictionary.DMpp.csv'),
  row.names = FALSE,
  quote = FALSE
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
