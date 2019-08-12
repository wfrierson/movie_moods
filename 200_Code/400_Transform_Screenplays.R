# Import prior results here:
folder.data <- '100_Data'

# Retain indentations inferred as screenplay components
screenplayTransformed <- screenplayIndentationStatsSelection[
  dialogueIndentInd == 1 |
    characterIndentInd == 1 |
    descriptionIndentInd == 1 |
    settingIndentInd == 1 |
    characterDirectionIndentInd == 1
][
  screenplayStatsSelection
  , on = .(movie, leftSpaceCount)
  , nomatch = FALSE
][
  # Create character label field using characterInd 
  characterIndentInd == 1
  , character := stri_trim(stri_trans_totitle(string))
][
  order(movie, lineNumber)
  , lineNumber := 1:.N
  , by = movie
][
  , descriptionNoSettingIndentInd := ifelse(
      settingIndentInd == descriptionIndentInd &
        descriptionIndentInd == settingInd,
      0,
      descriptionIndentInd
    )
][
  order(movie, lineNumber)
  # NOTE: This can be NULL if descriptionNoSettingIndentInd is on first line
  # Consider redoing lineNumber to identify first line (after filtering)
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
  , `:=` (
    settingInd = NULL
    , descriptionIndentInd = NULL
  )
]

# To-do
setnames(screenplayTransformed, 'stringTrimmed', 'text')
setcolorder(
  screenplayTransformed,
  c('movie', 'sectionNumber', 'sceneNumber', 'component', 'character', 'text')
)

# Note: The current implementation only has ~1% of screenplay components that are not identified.
# Code:
# screenplayTransformed[, .N / nrow(screenplayTransformed), keyby = component]
#
# Output:
#
#      component          V1
# 1:        <NA> 0.009326365
# 2: description 0.321195316
# 3:    dialogue 0.569917497
# 4:     setting 0.099560822

# Exporting as compressed file to avoid github's 100 MB per file limit.
saveRDS(
  screenplayTransformed
  , file.path(folder.data, 'screenplayTransformed.rds')
)