folder.data <- '100_Data'
folder.code <- '200_Code'

path.dependencies <- file.path(folder.code, '000_Dependencies.R')
path.screenplayFunctions <- file.path(folder.code, '200_Screenplay_Functions.R')

source(path.dependencies)
source(path.screenplayFunctions)

pathIMSDB <- file.path(folder.data, 'imsdb_raw_nov_2015')

# Get paths to all scraped imsdb screenplays
screenplayPaths <- data.table(
  path = list.files(path = pathIMSDB, pattern = '*.txt', recursive = TRUE)
)

# Split out fields
screenplayPaths[, c('genre', 'filename') := tstrsplit(path, '/')]
screenplayGenres <- dcast(
  screenplayPaths, 
  filename ~ genre, 
  fun = function(x) sum(ifelse(!is.na(x), 1, 0)),
  value.var = 'filename'
)

# Restate files according to folders/genres containing them
screenplayPaths <- screenplayPaths[
  , .(path = min(path))
  , keyby = filename
][
  screenplayGenres, on = 'filename'
][
  # Remove following movie because it has some bad text encodings (for ease)
  # Not worth fixing...
  filename != 'mightymorphinpowerrangersthemovie.txt'
]

# Import all screenplays
screenplays <- lapply(
  seq(nrow(screenplayPaths)),
  function(index) {
    screenplay <- data.table::fread(
      file.path(pathIMSDB, screenplayPaths[index]$path)
      , sep = NULL
      , header = FALSE
      , col.names = 'string'
      , quote = ""
      , strip.white = FALSE
      , blank.lines.skip = TRUE
    )
    
    list(
      'screenplay' = screenplay,
      'movie' = gsub('.txt', '', screenplayPaths[index]$filename, fixed = TRUE)
    )
  }
)

# Calculate statistics for each line and each screenplay to later infer
# which lines can be used for the following screenplay components:
#    dialogue, character labels, scene changes, and scene descriptions
#
# These features are inferred by finding patterns in indentations for each
# screenplay. Usually, scene descriptions have the fewest indents. Dialogue
# has more indents than descriptions. Character labels have more indents than
# dialogue (to denote who's speaking). Lastly, scene changes often occur with
# scene descriptions or sometimes on their own indentation level.
#
# Note: This takes ~10 min, but you can monitor the progress by following
# the current movie being analyzed, which is done alphabetically.
#
# The following is a non-parallel version of creating screenplayStats, which
# takes ~10 min. The parallelized version takes ~5 min.
#
# screenplayStats <- rbindlist(lapply(
#   screenplays,
#   function(screenplayList) {
#     cat(paste0(screenplayList[[2]], '\n'))
# 
#     GetMovieTranscriptStats(
#       screenplayList[[1]]
#     )[
#       , movie := screenplayList[[2]]
#     ]
#   }
# ))

functionsToExport <- list(
  'GetMovieTranscriptStats',
  'CreateRegexFilter.token',
  'CreateRegexFilter'
)
cl <- snow::makeCluster(4, type = 'SOCK')
clusterCall(cl, function(x) library(data.table))
clusterCall(cl, function(x) library(magrittr))
clusterCall(cl, function(x) library(stringi))
clusterExport(
  cl,
  functionsToExport,
  environment()
)
start_time <- Sys.time()
screenplayStats <- rbindlist(parLapply(
  cl,
  screenplays,
  function(screenplayList) {
    GetMovieTranscriptStats(
      screenplayList[[1]]
    )[
      , movie := screenplayList[[2]]
    ]
  }
))
end_time <- Sys.time()
stopCluster(cl)

# Identify movies with regular indentation formatting in order to maximize
# confidence in well structured screenplay transformations.
screenplayIndentationStats <- screenplayStats[
  # For those not familiar with data.table, the following block does two
  # group-by's at once:
  , {
    # 1) For each movie, calculate the total number of records and tokens
    totalRecordCount = .N
    totalTokenCount = sum(tokenCount)
    
    .SD[
      # 2) For each distinct indentation (i.e., leftSpaceCount) in each movie,
      #    calculate various statistics at the token and record level.
      , .(
        pctTokens = sum(tokenCount) / totalTokenCount
        , pctRecords = .N / totalRecordCount
        , pctTokensI = sum(tokenCountI) / sum(tokenCount)
        , pctTokensQuestionMark = sum(tokenCountQuestionMark) / sum(tokenCount)
        , pctTokensQuestionWord = sum(tokenCountQuestionWord) / sum(tokenCount)
        , pctTokensYou = sum(tokenCountYou) / sum(tokenCount)
        , pctTokensWe = sum(tokenCountWe) / sum(tokenCount)
        , pctTokensCapitalized = sum(capitalizedTokenCount) / sum(tokenCount)
        , pctTokensCharacterMention = 
            sum(tokenCountCharacterMention) / sum(tokenCount)
        , pctTokensCharacterMentionCapitalized = 
            sum(tokenCountCharacterMentionCapitalized) / sum(tokenCount)
        , pctLinesCharacterMention = 
            sum(tokenCountCharacterMention) / sum(tokenCount)
        , pctLinesCharacterMentionCapitalized = 
            sum(tokenCountCharacterMentionCapitalized) / sum(tokenCount)
        , pctLinesCharacterDirection = sum(characterDirectionInd) / .N
        , pctLinesCapitalized = sum(allCapsInd) / .N
        , pctLinesSetting = sum(settingInd) / .N
      )
      , keyby = leftSpaceCount
    ]
  }
  , keyby = movie
][
  # Remove 14 screenplays where no characters could be inferred.
  #
  # Characters are inferred by identifying lines with at most 3 tokens and
  # keeping the distinct lines that appear at least 10 times in each screenplay.
  # See for GetMovieTranscriptStats for details.
  !is.na(pctTokensCharacterMention)
][
  # Assume that left-spacing with "consistent" usage/variations of I, you, and
  # questions denote dialogue (so long as at least 1% of tokens occur).
  # More specifically, assume that any pairs of pctRecords* fields with at least
  # 0.01 records denote dialogue.
  , dialogueIndentInd := ifelse(
      pctTokens > 0.01 & (
          (pctTokensI > 0.01 & pctTokensYou > 0.01) | 
          (pctTokensI > 0.01 & pctTokensQuestionMark > 0.01) |
          (pctTokensI > 0.01 & pctTokensQuestionWord > 0.01) |
          (pctTokensYou > 0.01 & pctTokensQuestionMark > 0.01) |
          (pctTokensYou > 0.01 & pctTokensQuestionWord > 0.01) |
          (pctTokensWe > 0.01 & pctTokensI > 0.01) |
          (pctTokensWe > 0.01 & pctTokensYou > 0.01) |
          (pctTokensWe > 0.01 & pctTokensQuestionMark > 0.01) |
          (pctTokensWe > 0.01 & pctTokensQuestionWord > 0.01)
      ),
      1,
      0
  )
][
  # Assume that left-spacing that doesn't appear to be dialogue but also has
  # at least 35% of tokens mentioning inferred characters denotes character 
  # headers.
  , characterIndentInd := ifelse(
      dialogueIndentInd == 0 &
        pctTokensCharacterMention > 0.35,
      1,
      0
  )
][
  # Assume that left-spacing that isn't inferred dialogue or character headers
  # represents scene descriptions (so long as it has at least 10% lines per 
  # screenplay).
  , descriptionIndentInd := ifelse(
      dialogueIndentInd == 0 &
        characterIndentInd == 0 &
        pctRecords > 0.1,
      1,
      0
  )
][
  # Assume that left-spacing that isn't inferred dialogue or character headers 
  # but also has elements appearing to describe settings (e.g., EXT. 
  # AMY'S HOUSE) denotes setting changes.
  # a setting
  , settingIndentInd := ifelse(
      dialogueIndentInd == 0 &
        characterIndentInd == 0 &
        pctLinesSetting > 0.01,
      1,
      0
  )
][
  # Redefine character direction indicator to only accept its value if it does 
  # not occur on an inferred description line. When this component appears in a
  # description line, it tends to be a genuine parenthetical note and not
  # a direction for an actor.
  , characterDirectionIndentInd := ifelse(
      pctLinesCharacterDirection > 0.35 &
        descriptionIndentInd == 0,
      1,
      0
  )
][
  # Attempt at identifying indents that are amibiguous, i.e., they look like
  # important components (and they are) but they are many components.
  #
  # Not too selective yet. Big offenders are:
  #        myweekwithmarilyn & romeojuliet
  , mixedCharacterWithDialogueIndentInd := ifelse(
      pctLinesCharacterMentionCapitalized > 0.02 &
        pctLinesCharacterMention > 0.02 &
        dialogueIndentInd == 1 &
        pctTokens > 0.3,
      1,
      0
  )
]

fieldsScreenplayComponents <- c(
  'dialogueIndentInd',
  'characterIndentInd',
  'descriptionIndentInd',
  'settingIndentInd',
  'characterDirectionIndentInd',
  'mixedCharacterWithDialogueIndentInd'
)
fieldsComponentCounts <- gsub(
  'IndentInd',
  'IndentCount',
  fieldsScreenplayComponents
)

# Count number of times each screenplay component occurs for each movie
screenplayIndentationStats[
  , (fieldsComponentCounts) := lapply(.SD, sum)
  , by = movie
  , .SDcols = fieldsScreenplayComponents
][
  # Calculate additional statistics to assist in selecting well-structured
  # screenplays.
  , `:=` (
    entropy = -sum(pctTokens * log(pctTokens, base = 2))
    , pctRecordsDropped = sum(ifelse(
        dialogueIndentInd == 0 &
          characterIndentInd == 0 &
          descriptionIndentInd == 0 &
          settingIndentInd == 0,
        pctRecords,
        0
    ))
    , pctTokensDropped = sum(ifelse(
        dialogueIndentInd == 0 &
          characterIndentInd == 0 &
          descriptionIndentInd == 0 &
          settingIndentInd == 0,
        pctTokens,
        0
    ))
  )
  , by = movie
]

# Create draft of well-structured screenplays where:
#    * Each screenplay component occurs at least once, and
#    * at most 5% of all tokens per screenplay are omitted by only keeping the
#      inferred components
fieldsMovieLevel <- c('movie', 'entropy', 'pctRecordsDropped',
                      'pctTokensDropped', fieldsComponentCounts)
screenplaySelectionDraft <- unique(screenplayIndentationStats[
  !(movie %in% c('romeojuliet', 'myweekwithmarilyn'))
][
  , mget(fieldsMovieLevel)
])[
  dialogueIndentCount > 0 & 
    characterIndentCount > 0 & 
    descriptionIndentCount > 0 &
    settingIndentCount > 0 &
    pctTokensDropped < 0.05
][
  order(entropy)
]

# Limit possible screenplays further by only keeping those that have exactly
# one inferred description and setting component, and at most 2 character
# header components and 3 dialogue components.
#
# Note: Some text screenplays appear to have been scanned from a typed document
# or transformed from a PDF. As a result, the indentation can be inconsistent.
# I account for this by adding a modest tolerance for dialogue and character
# header components. OTOH, I noticed that screenplays with many description
# or setting components tend to be poorly formatted overall, and so not worth
# transforming.
#
# There are 427 screenplays in screenplaySelection.
screenplaySelection <- screenplaySelectionDraft[
  dialogueIndentCount <= 3 & 
    characterIndentCount <= 2 & 
    descriptionIndentCount == 1
]

# Create subset of screenplayIndentationStats for selected screenplays.
screenplayIndentationStatsSelection <- screenplaySelection[,.(movie)][
  screenplayIndentationStats
  , on = .(movie)
  , nomatch = FALSE
]

# Create subset of screenplayStats for selected screenplays.
screenplayStatsSelection <- screenplaySelection[,.(movie)][
  screenplayStats
  , on = .(movie)
  , nomatch = FALSE
]

# Export results
data.table::fwrite(
  screenplaySelection
  , file.path(folder.data, 'screenplaySelection.csv')
  , quote = FALSE
  , row.names = FALSE
)

data.table::fwrite(
  screenplayIndentationStatsSelection
  , file.path(folder.data, 'screenplayIndentationStatsSelection.csv')
  , quote = FALSE
  , row.names = FALSE
)

# Exporting as compressed file to avoid github's 100 MB per file limit.
saveRDS(
  screenplayStatsSelection
  , file.path(folder.data, 'screenplayStatsSelection.rds')
)