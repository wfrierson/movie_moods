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
  # Create character label field using characterInd 
  characterIndentInd == 1
  , character := stri_trim(stri_trans_totitle(string))
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
  , sectionInd := pmax(characterIndentInd, settingInd, descriptionSectionInd)
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
  , stringTrimmed := stri_trim(string)
][
  # Concatenate lines for each section for easier text mining
  characterIndentInd == 0
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

# Remove 9 screenplays with unresolved issues
screenplaysToDrop <- screenplayTransformed[
  , .(
    pctBadComponents = sum(ifelse(is.na(component), 1, 0)) / .N
  )
  , keyby = movie
][
  pctBadComponents > 0.1
  , movie
]

screenplayTransformed <- screenplayTransformed[!(movie %in% screenplaysToDrop)]

###############################################################################
# STATISICS ON TRANSFORMED SCREENPLAYS

# Note: The current implementation only has < 1% of screenplay components that 
# are not identified. These components had issues that prevented their 
# inclusion.
#
# Code:
# screenplayTransformed[
#   , .(
#     pctSections = .N / nrow(screenplayTransformed)
#   )
#   , keyby = component
# ]
#
# Output:
#
#      component pctSections
# 1:        <NA> 0.008622927
# 2: description 0.320978184
# 3:    dialogue 0.570731676
# 4:     setting 0.099667214

###############################################################################
# EXPORT RESULTS

# Exporting as compressed file to avoid github's 100 MB per file limit.
saveRDS(
  screenplayTransformed
  , file.path(folder.data.processed, '401_screenplayTransformed.rds')
)