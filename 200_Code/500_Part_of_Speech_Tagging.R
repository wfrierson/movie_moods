###############################################################################
# This script tags the part-of-speech of each token in each screenplay, so that
# the tokens can be matched the DepecheMood++ lexicon.
#
# Part-of-speech tagging is applied via:
#    * The part-of-speech TreeTagger (which is not a part of R),
#    * Perl, and
#    * The koRpus and koRpus.lang.en packages
#
# The output of this script used the 64-bit Windows version of TreeTagger v3.2.2
# found below:
#    https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/
#    tree-tagger-windows-3.2.2.zip
#
# Perl functionality came from ActivePerl community edition, v5.28:
#      https://www.activestate.com/products/activeperl/downloads/
#
# Once installed and its executable is included in PATH, ensure you restart R 
# for that new PATH location to be visible in R.

###############################################################################
# SETUP

folder.data <- '100_Data'
folder.data.processed <- file.path(folder.data, '120_Processed_Data')
folder.code <- '200_Code'

path.dependencies <- file.path(folder.code, '000_Dependencies.R')
source(path.dependencies)

###############################################################################
# IMPORT TRANSFORMED SCREENPLAYS

screenplayTransformed <- readRDS(
  file.path(folder.data.processed, '401_screenplayTransformed.rds')
)

# Add data.table key to screenplayTransformed to improve runtime for tokenizing
setkey(screenplayTransformed, movie, sectionNumber, sceneNumber, component)

###############################################################################
# TOKENIZE SCREENPLAYS

# Tokenize each record of screenplayTransformed
screenplayTokens <- screenplayTransformed[
  , .(
    token = unlist(data.table::tstrsplit(text, ' ', fixed = TRUE))
  )
  , keyby = .(movie, sectionNumber, sceneNumber, component)
]

# Apply light text cleaning to each token
#
# Convert to lowercase
screenplayTokens[, token := stringi::stri_trans_tolower(token)]

# Remove non-letters
screenplayTokens[
  , token := stringi::stri_replace_all_regex(token, '[^[:alpha:]]', '')
]

# Remove tokens that are blank after filtering
screenplayTokens <- screenplayTokens[token != '']

###############################################################################
# PART-OF-SPEECH TAGGING

treetagOutput <- koRpus::treetag(
  screenplayTokens[, token],
  format = 'obj',
  treetagger = "manual",
  lang = "en",
  TT.options = list(
    # Change the following absolute path to your local installation folder of
    # TreeTagger, if different.
    path = "C:\\TreeTagger",
    preset = "en"
  )
)

# Convert koRpus tagging object to data.table
screenplayTagged <- data.table::data.table(
   token = treetagOutput@TT.res$token
   , lemma = treetagOutput@TT.res$lemma
   , pos = treetagOutput@TT.res$wclass
   
)[
  # Create column that mimics the part-of-speech labels from the DM++ lexicon
  , posLabel := ifelse(
      pos == 'verb', 'v', ifelse(
      pos == 'adverb', 'r', ifelse(
      pos == 'noun', 'n', ifelse(
      pos == 'adjective', 'a', NA_character_
  ))))
][
  # Create a field that can match with the key in the DM++
  !is.na(posLabel)
  , `:=` (
    tokenPOS = paste0(token, '#', posLabel),
    lemmaPOS = paste0(lemma, '#', posLabel)
  )
]

# Append key to maintain screenplay structure
#
# Note: There's some risk in appending columns with cbind and not a join. 
# However, koRpus::treetag returns the same number of tokens as it's given and
# the order is the same. 
#
# E.g., the following returns TRUE:
#    all.equal(screenplayTagged$token, screenplayTokens[, token])
screenplayTagged <- cbind(
  screenplayTokens[, .(movie, sectionNumber, sceneNumber, component)]
  , screenplayTagged
)

###############################################################################
# EXPORT RESULTS

saveRDS(
  screenplayTagged,
  file.path(folder.data.processed, '501_screenplayTagged.rds')
)
