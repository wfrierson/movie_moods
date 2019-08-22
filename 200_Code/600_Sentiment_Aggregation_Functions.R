#' Aggregate moods to a given level
#' 
#' Aggregating moods using the functon \code{GetTokenMoodContribution}.
#'
#' @param moodTable dat.table containing granular token info 
#' @param aggregateString character vector of fields to aggregate by
#'
#' @return
#' @export
#'
#' @examples
AggregateMoods <- function(
  moodTable,
  aggregateString = c('movie')
) {
  moodLevels = c(
    'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise',
    'trust'
  )
  
  moodTableAgg <- copy(moodTable)[
    # Calculate token frequency to use when aggregating moods
    , tokenFreq := .N
    , by = c(aggregateString, 'Term')
  ][
    # Calculate metadata at the aggregation level
    , `:=` (
      tokenCount = .N,
      characterCount = uniqueN(character, na.rm = TRUE),
      sectionCount = uniqueN(sectionNumber)
    )
    , by = aggregateString
  ][
    , (moodLevels) := lapply(.SD, as.numeric)
    , .SDcols = moodLevels
  ][
    # Aggregate mood probabilities to movie-level via method described in
    # getTokenMoodContribution function
    , (moodLevels) := lapply(.SD, mean)
    , by = aggregateString
    , .SDcols = moodLevels
  ][
    # Collapse metadata to aggregation level
    , .(
      tokenCount = max(tokenCount),
      characterCount = max(characterCount),
      sectionCount = max(sectionCount),
      anger = max(anger),
      anticipation = max(anticipation),
      disgust = max(disgust),
      fear = max(fear),
      joy = max(joy),
      sadness = max(sadness),
      surprise = max(surprise),
      trust = max(trust)
    )
    , keyby = aggregateString
  ]
  
  mood.ecdf <- lapply(
    moodLevels,
    function(mood) ecdf(moodTableAgg[[mood]])
  )
  
  moodLevelPercentiles <- paste0(moodLevels, 'Percentile')
  
  moodTableAgg[
    , (moodLevelPercentiles) := lapply(
        seq_along(moodLevels),
        function(moodIndex) mood.ecdf[[moodIndex]](get(moodLevels[moodIndex]))
    )
  ]
  
  moodTableAgg[
    , id := 1:.N
  ]
  
  setcolorder(
    moodTableAgg,
    c(
      'id',
      aggregateString,
      'tokenCount',
      'characterCount',
      'sectionCount',
      moodLevels,
      moodLevelPercentiles
    )
  )
  
  return(moodTableAgg)
}