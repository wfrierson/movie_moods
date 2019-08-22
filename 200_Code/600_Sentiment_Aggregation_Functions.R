#' Get contribution of token for a given mood
#' 
#' Function that aggregates word-mood probabilities to a more coarse
#' "document" level. This could be at the screenplay level or character level,
#' e.g. Note: This aggregation method was originally made with topic modeling
#' in mind, not sentiment analysis. However, I'm treating word-mood 
#' probabilities like word-topic probabilities, and moods like topics. For now, 
#' this seems like reasonable cowboy math.
#'
#' This method comes from:
#'
#' Li, H., Graesser, A., Cai, Z., & Hu, X. (2016). Can word probabilities from 
#' LDA be simply added up to represent documents?. Proceedings of the 9th 
#' International Conference on Educational Data Mining.
#'
#' @param termProb term-mood probability
#' @param termFreq term frequency
#'
#' @return numeric
#' @export
#'
#' @examples
GetTokenMoodContribution <- function(termProb, termFreq) {
  output <- termProb * log(1 + termFreq)
  return(output)
}

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
    'afraid', 'amused', 'angry', 'annoyed', 'dont_care', 'happy', 'inspired',
    'sad'
  )
  
  moodTableAgg <- copy(moodTable)[
    # Calculate token frequency to use when aggregating moods
    , tokenFreq := .N
    , by = c(aggregateString, 'tokenPOS')
  ][
    # Calculate metadata at the aggregation level
    , `:=` (
      tokenCount = .N,
      characterCount = uniqueN(character, na.rm = TRUE),
      sectionCount = uniqueN(sectionNumber)
    )
    , by = aggregateString
  ][
    # Aggregate mood probabilities to movie-level via method described in
    # getTokenMoodContribution function
    , (moodLevels) := lapply(
        .SD,
        function(mood) sum(GetTokenMoodContribution(mood, tokenFreq))
      )
    , by = aggregateString
    , .SDcols = moodLevels
  ][
    # Collapse metadata to aggregation level
    , .(
      tokenCount = max(tokenCount),
      characterCount = max(characterCount),
      sectionCount = max(sectionCount),
      afraid = max(afraid),
      amused = max(amused),
      angry = max(angry),
      annoyed = max(annoyed),
      dont_care = max(dont_care),
      happy = max(happy),
      inspired = max(inspired),
      sad = max(sad)
    )
    , keyby = aggregateString
  ][
    , totalProbSum := Reduce(`+`, .SD)
    , .SDcol = moodLevels
  ][
    , (moodLevels) := lapply(.SD, function(mood) mood / totalProbSum)
    , .SDcol = moodLevels
  ]
  
  mood.ecdf <- lapply(
    moodLevels,
    function(mood) ecdf(moodTableAgg[[mood]])
  )
  
  moodTableAgg[
    , paste0(moodLevels, 'Percentile') := lapply(
        seq_along(moodLevels),
        function(moodIndex) mood.ecdf[[moodIndex]](get(moodLevels[moodIndex]))
    )
  ]
  
  return(moodTableAgg)
}