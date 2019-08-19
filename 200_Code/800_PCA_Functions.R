#' Rotate data using rotation matrix from prcomp
#'
#' @param newData data.table to rotate
#' @param prcompOutput output from \code{prcomp}
#'
#' @return data.table of rotated data
#' @export
#'
#' @examples
RotateData <- function(newData, prcompOutput) {
  newDataRot <- scale(
    as.matrix(
      newData
    )
    , center = prcompOutput$center
    , scale = prcompOutput$scale
  ) %*% prcompOutput$rotation %>% as.data.table
  
  pcs <- prcompOutput$sdev * sqrt(nrow(newDataRot))
  names(pcs) <- paste0('PC',1:8)
  
  # Scale rotated variables using PCs
  newDataRot[
    , paste0('PC',1:8) := lapply(
        paste0('PC',1:8),
        function(pc) get(pc) / pcs[pc]
      )
  ]
  
  return(newDataRot)
}

#' Append genres to a data.table with a movie column
#'
#' @param movieTable 
#' @param genreLookupTable 
#' @param movieTableFieldsString 
#'
#' @return
#' @export
#'
#' @examples
AppendGenres <- function(
  movieTable,
  genreLookupTable,
  movieTableFieldsString = c('movie')
) {
  # Get vector of movie genres for reference
  genres <- colnames(genreLookupTable)[-c(1:2, 25)] %>% copy
  
  output <- genreLookupTable[
      movieTable
      , on = .(movie)
    ][, mget(c(movieTableFieldsString, genres))]
  
  return(output)
}

#' Rotate an aggregation of mood probabilities with a supplied rotation matrix
#'
#' @return
#' @export
#'
#' @examples
RotateMoodAggregation <- function(moodAggTable) {
  moodAggTable <- get(moodAggTable)
  
  moodAggTableRot <- rotateData(
    newData = moodAggTable[, mget(moods.DMpp)],
    prcompOutput = pcaMovie
  )
  
  fieldsToInclude <- c(
    key(moodAggTable),
    'tokenCount',
    'characterCount',
    'sectionCount'
  )
  moodAggTableRot <- cbind(
    appendGenres(
      movieTable = moodAggTable,
      genreLookupTable = screenplayPaths,
      movieTableFieldsString = fieldsToInclude
    ),
    moodAggTableRot
  )
  
  moodAggTableRot[
    , genreCount := Reduce(`+`, .SD)
    , by = movie
    , .SDcol = genres
  ]
  
  return(moodAggTableRot)
}