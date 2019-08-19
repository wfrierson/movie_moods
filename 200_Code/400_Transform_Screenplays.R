###############################################################################
# This script produces the transformed versions of the selected screenplays. 
# Text in each distinct screenplay component is concatenated together in order
# to have a more structured form to facilitate sentiment analysis.

###############################################################################
# SETUP

folder.data <- '100_Data'
folder.data.raw <- file.path(folder.data, '110_Raw_Data')
folder.data.processed <- file.path(folder.data, '120_Processed_Data')
folder.code <- '200_Code'

path.dependencies <- file.path(folder.code, '000_Dependencies.R')
source(path.dependencies)

# Create filter to identify "character direction", which can pollute
# a character's label
# E.g., RIPLEY (angrily)
filterCharacterDirection <- "\\([^()]*\\)|\\([^()]*|[^()]*\\)"

###############################################################################
# IMPORT SELECTED SCREENPLAYS
screenplayIndentationStatsSelection <- fread(
  file.path(
    folder.data.processed,
    '303_screenplayIndentationStatsSelection.csv'
  )
  , stringsAsFactors = FALSE
)

screenplayStatsSelection <- readRDS(
  file.path(folder.data.processed, '304_screenplayStatsSelection.rds')
)

###############################################################################
# TRANSFORM SCREENPLAYS

# Retain indentations inferred as screenplay components
screenplayTransformed <- screenplayIndentationStatsSelection[
  # Only keep indentations that appear to have desired components for each
  # screenplay
  dialogueIndentInd == 1 |
    characterIndentInd == 1 |
    descriptionIndentInd == 1 |
    settingIndentInd == 1 |
    characterDirectionIndentInd == 1
][
  # Apply the selected indentations to each screenplay and remove unmatching
  # indentations
  screenplayStatsSelection
  , on = .(movie, leftSpaceCount)
  , nomatch = FALSE
][
  # Create indicator to better infer if an apparent character label is formatted
  # correctly by ignoring those that only have character direction
  , characterLabelLikelyInd := ifelse(
      characterIndentInd == 1 & characterDirectionOnlyInd == 0,
      1,
      0
  )
][
  # Create character label field using characterIndentInd
  # Remove any apparent character direction to provide cleaner character
  # labels
  characterLabelLikelyInd == 1
  , character := stringi::stri_replace_all_regex(
        string,
        filterCharacterDirection,
        ''
      ) %>%
      stringi::stri_replace_all_regex(
        .,
        '\\|',
        '/'
      ) %>%
      stringi::stri_trans_totitle(.) %>%
      stringi::stri_trim(.)
][
  character == ''
  , character := NA_character_
][
  # Rederive line number to denote the retained lines
  order(movie, lineNumber)
  , lineNumber := 1:.N
  , by = movie
][
  # The following two field derivations are used to ensure new setting 
  # descriptions will be recognized as their own component, instead of being
  # lumped in with generic scene descriptions
  , descriptionNoSettingIndentInd := ifelse(
      settingIndentInd == descriptionIndentInd &
        descriptionIndentInd == settingInd,
      0,
      descriptionIndentInd
    )
][
  order(movie, lineNumber)
  , descriptionSectionInd := ifelse(
                              lineNumber == 1 &
                                descriptionNoSettingIndentInd == 1, 1, ifelse(
                              descriptionNoSettingIndentInd == 1 & 
                                shift(descriptionNoSettingIndentInd) == 0,
                              1,
                              0
                            ))
][
  # Create section indicator to use later in concatenating lines for easier
  # text mining
  #
  # dialogueIndentInd is excluded to later group it with the associated 
  # character label that's assumed to precede dialogue.
  , sectionInd := pmax(
      characterLabelLikelyInd,
      settingInd,
      descriptionSectionInd
    )
][
  # Create section number and scene number to partition each screenplay
  order(movie, lineNumber)
  , `:=` (
    sectionNumber = cumsum(sectionInd)
    , sceneNumber = cumsum(settingInd)
  )
  , by = movie
][
  # Extend the character label into subsequent dialogue and character 
  # direction lines
  , character := max(character, na.rm = TRUE)
  , by = .(movie, sectionNumber)
][
  , stringTrimmed := stringi::stri_trim(string)
][
  # Concatenate lines for each section for easier text mining
  characterLabelLikelyInd == 0
  , lapply(.SD, paste0, collapse = ' ')
  , by = .(
      movie
      , sectionNumber
      , sceneNumber
      , character
      , settingInd
      , settingIndentInd
      , descriptionIndentInd
  )
  , .SDcols = 'stringTrimmed'
][
  # Create field that labels each screenplay component
  , component := ifelse(
      # Case 1
      !is.na(character), 'dialogue', ifelse(
      
      # Case 2  
      settingInd == 1, 'setting', ifelse(
      
      # Case 3
      descriptionIndentInd == 1 | settingIndentInd == 1, 'description',
      
      # Else
      NA_character_
  )))
][
  # Remove unneeded columns
  , `:=` (
    settingInd = NULL
    , descriptionIndentInd = NULL
    , settingIndentInd = NULL
  )
]

# Format layout for easier use
setnames(screenplayTransformed, 'stringTrimmed', 'text')
setcolorder(
  screenplayTransformed,
  c('movie', 'sectionNumber', 'sceneNumber', 'component', 'character', 'text')
)

# Remove 11 screenplays with unresolved issues:
#    beginners
#    blackswan
#    colorofnight
#    flight
#    hitchcock
#    idesofmarchthe
#    jennifereight
#    limitless
#    mud
#    wallstreetmoneyneversleeps
#    warrior
screenplaysToDrop <- screenplayTransformed[
  , .(
    pctBadComponents = sum(
      ifelse(
        is.na(component),
        stri_count_words(text),
        0
      )
    ) / sum(stri_count_words(text))
    
  )
  , keyby = movie
][
  pctBadComponents > 0.10
  , movie
]

screenplayTransformed <- screenplayTransformed[!(movie %in% screenplaysToDrop)]

###############################################################################
# STATISICS ON TRANSFORMED SCREENPLAYS

# Note: The current implementation only has ~0.5% of screenplay tokens that 
# come from unidentified components. These components had issues that prevented
# their inclusion.
#
# Code:
# screenplayTransformed[
#   , {
#     totalTokenCount = sum(stri_count_words(text))
#     .SD[
#       , .(pctTokens = sum(stri_count_words(text)) / totalTokenCount)
#       , keyby = component
#     ]
#   }
# ]
#
# Output:
#
#      component   pctTokens
# 1:        <NA> 0.005492789
# 2: description 0.557861260
# 3:    dialogue 0.406270144
# 4:     setting 0.030375807

###############################################################################
# EXPORT RESULTS

# Exporting as compressed file to avoid github's 100 MB per file limit.
saveRDS(
  screenplayTransformed
  , file.path(folder.data.processed, '401_screenplayTransformed.rds')
)