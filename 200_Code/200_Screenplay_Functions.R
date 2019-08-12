#' Create regular expression filter for individual token
#'
#' @param token single string to be inserted into \code{filterTemplate}
#' @param filterTemplate regular expression template for \code{sprintf}
#'
#' @return string to be used as pattern for regular expression
#' @export
#'
#' @examples
#' CreateRegexFilter.token('test')
CreateRegexFilter.token <- function(
  token, 
  filterTemplate = "\\b%s\\b|\\b%s\\.\\b|\\b%s,\\b"
) {
  repititions <- stri_count(filterTemplate, fixed = '%s')
  tokenRepeated <- rep(token, repititions)
  argsSprintf <- as.list(c(filterTemplate, tokenRepeated))
  
  output <- do.call(sprintf, argsSprintf)
  return(output)
}

#' Create regular expression filter for vector of tokens
#'
#' @param tokenVector vector of strings to be inserted into \code{filterTemplate}
#' @param filterTemplate regular expression template for \code{sprintf}
#'
#' @return string of concatenated output from \code{CreateRegexFilter.token}
#' @export
#'
#' @examples
#' CreateRegexFilter(c('this', 'is', 'a', 'test'))
CreateRegexFilter <- function(
  tokenVector, 
  filterTemplate = "\\b%s\\b|\\b%s\\.\\b|\\b%s,\\b"
) {
  output <- lapply(
    tokenVector, 
    function(token) CreateRegexFilter.token(token, filterTemplate)) %>% 
      unlist %>% 
      paste0(collapse = '|')
  
  return(output)
}

#' Calculate various statistics on an individual screenplay
#'
#' @param rawTranscript data.table with rows representing lines in a screenplay
#'
#' @return \code{rawTranscript} with various record-level statistics 
#' @export
#'
#' @examples
#' # See 300_Select_Screenplays for example application
#' GetMovieTranscriptStats(screenplays[[1]])
GetMovieTranscriptStats <- function(rawTranscript) {
  # Create filter to count specific types of tokens
  filterTokenCount <- "\\b[a-zA-Z0-9]{2,}\\b"
  
  # Create filters to identify variations of 'I' & 'you'
  filterI <- CreateRegexFilter(c('i', "i'm", "i'll", "i've", "i'd"))
  filterYou <- CreateRegexFilter(
    c('you', 'your', "you're", "you've", "you'd", "you'll")
  )
  filterWe <- CreateRegexFilter(
    c('we', "we'll", "we've", "we'd", 'our', 'ours')
  )
  
  # Create filters to identify question words including their contractions
  filterTemplateContractions <- paste0(
    '\\b',
    c('%s', "%s's", "%s'd", "%s've", "%s're", "%s'll"), 
    '\\b',
    collapse = '|'
  )
  filterQuestionWords <- CreateRegexFilter(
    c('who', 'what', 'when', 'where', 'why'),
    filterTemplateContractions
  )
  
  # Create filter to identify "character direction", which can pollute
  # a character's label
  # E.g., RIPLEY (angrily)
  filterCharacterDirection <- "\\([^()]*\\)|\\([^()]*|[^()]*\\)"
  
  # Create regex filters to identify setting descriptions
  # E.g., "EXT. TIMES SQUARE"
  filterSettingShort <- CreateRegexFilter(
    token = c('EXT', 'INT'), 
    filterTemplate = "^%s\\. |^%s | %s\\.| %s "
  )
  filterSettingLong <- CreateRegexFilter(
    tokenVector = c('INTERIOR', 'EXTERIOR'), 
    filterTemplate = "^%s | %s "
  )
  filterSetting <- paste0(filterSettingShort, '|', filterSettingLong)
  
  # Create regex filters to identify variations of "(continued)" and 
  # '(voice over)'  labels
  filterContinued <- CreateRegexFilter(
    c('continuing', 'continued', 'continue', 'cont', 'cont\\.', "cont'd"),
    "\\(%s\\)"
  )
  filterVoiceOver <- CreateRegexFilter(
    c('voice over', 'v\\.o\\.', 'vo', 'off screen', 'o\\.s\\.', 'os'),
    "\\(%s\\)"
  )
  
  # Create regex filter to remove html bold tags
  filterBold <- '<b>|</b>'
  
  # Create regex filter to remove artifacts from webscraping by NLDS of UCSD
  filterScraping <- 
    'if \\(window!= top\\)|top\\.location\\.href=location\\.href'
  
  # Create combined filter to remove the items below
  filterToRemove <- paste0(
    c(
      filterContinued,
      filterVoiceOver,
      filterBold,
      filterScraping
    ),
    collapse = '|'
  )
  
  # Create regex filter to find and remove apparent page numbers
  filterPageNumbersLeft <- 
    '^([:digit:])+([\\.-])?([:alpha:]+)?([\\.-])?(?=[ ]{2,}[:alnum:])'
  filterPageNumbersRight <- 
    '([:digit:])+([\\.-])?([:alpha:]+)?([\\.-])?([ ]{0,2})?$'
  
  # Create regex filter to find leading spaces to track indentation patterns
  filterLeftSpaces <- '^[ ]*(?=[^ ])'
  
  rawTranscript[
    # Convert any tabs to 4 spaces
    , `:=` (
      string = stri_replace_all_fixed(string, '\t', '    ')
      , lineNumber = 1:.N
    )
  ][
    # Remove "continued" & "voice over" labels since they often are adjacent
    # to character labels, and remove html bold tags
    , string := stri_replace_all_regex(
                  string,
                  filterToRemove,
                  '',
                  opts_regex = stri_opts_regex(case_insensitive = TRUE)
                )
  ][
    # Find any apparent page numbers that appear before the main content.
    # Extract these to get their character length to replace later with
    # spaces (to preserve indentation).
    , pageNumberStringLength := stri_extract_first_regex(
            string,
            filterPageNumbersLeft
         ) %>% nchar 
  ][
    !is.na(pageNumberStringLength)
    # Remove apparent page numbers on left hand sides of each line.
    , string := stri_replace_all_regex(
                  string,
                  filterPageNumbersLeft,
                  stri_pad_left('', pageNumberStringLength)
        )
  ][
    # Remove apparent page numbers on left hand sides of each line.
    , string := stri_replace_all_regex(
                  string,
                  filterPageNumbersRight,
                  ''
                )
  ]
  
  # Create token count per line and only keep lines with at least 1 token
  rawTranscript[, tokenCount := stri_count(string, regex = filterTokenCount)]
  rawTranscript <- rawTranscript[tokenCount > 0]
  
  rawTranscript[
    # Create many support indicators and counters
    , `:=` (
      leftSpaceCount = stri_extract_all_regex(
                            string,
                            filterLeftSpaces
                         ) %>% nchar
      , capitalizedTokenCount = stri_count(string, regex="[A-Z]{2,}")
      , characterDirectionInd = ifelse(
          stri_detect_regex(string, filterCharacterDirection),
          1,
          0
      )
      , tokenCountI = stri_count(stri_trans_tolower(string), regex = filterI)
      , tokenCountYou = stri_count(
                          stri_trans_tolower(string),
                          regex = filterYou
                        )
      , tokenCountWe = stri_count(stri_trans_tolower(string), regex = filterWe)
      , tokenCountQuestionMark = stri_count(string, regex = "\\?")
      , tokenCountQuestionWord = stri_count(string, regex = filterQuestionWords)
      , tokenCountOddPunctuation = stri_count(string, regex = '\\!|\\?|,|:')
    )
  ][
    , allCapsInd := ifelse(
        tokenCount > 0 &
          tokenCount == capitalizedTokenCount,
        1,
        0
    )
  ][
    # Create indicator for presence of "setting description"
    # E.g., EXT. TIMES SQUARE
    , settingInd := ifelse(
        !nzchar(string)
        , 0
        , ifelse(
            stri_detect_regex(stri_trim(string), filterSetting)
            , 1
            , 0
        )
    )
    , by = lineNumber
  ]
  
  # Assume that character labels to denote dialogue are always on their own line
  # So, a line with at most 3 tokens could be a character label if it occurs
  # frequently (i.e., at least 10 times)
  freqCharacters <- rawTranscript[
    tokenCount <= 3 & 
    settingInd == 0 & 
    characterDirectionInd == 0 & 
    tokenCountOddPunctuation == 0
    , .N
    , by = .(
      string = stri_trim(string) %>% stri_trans_tolower
    )
  ][order(-N)][N > 10, string] %>% paste0(collapse = '|')
  
  # Use inferred character names to count number of mentions for any frequent
  # character
  if(identical(freqCharacters, character(0))) {
    rawTranscript[, tokenCountCharacterMention := 0]
  } else {
    rawTranscript[
      , tokenCountCharacterMention := stri_count(
          stri_trim(string) %>% stri_trans_tolower,
          regex = freqCharacters
      )
    ]
    
    rawTranscript[
      , tokenCountCharacterMentionCapitalized := stri_count(
          stri_trim(string),
          regex = stri_trans_toupper(freqCharacters)
      )
    ]
  }
  
  return(rawTranscript)
}