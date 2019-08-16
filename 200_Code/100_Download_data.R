###############################################################################
# SETUP
folder.data <- '100_Data'
folder.data.raw <- file.path(folder.data, '110_Raw_Data')
folder.data.processed <- file.path(folder.data, '120_Processed_Data')

###############################################################################
# SCREENPLAY CORPUS
#
# Downloading and unzipping a corpus of digitized screenplays from the National 
# Language and Dialogue Systems (NLDS) lab of UC Santa Cruz.
#
# From the readme of this dataset (https://nlds.soe.ucsc.edu/fc2):
#
# Overview: This corpus is an updated version of the Film Corpus 1.0. It 
# contains complete texts for the scripts of 1068 films in txt files, scraped 
# from imsdb.com on Nov, 2015 using scrapy. It also contains 960 film scripts 
# where the dialog in the film has been separated from the scene descriptions.
name.screenplays <- 'imsdb_raw_nov_2015.zip'

path.screenplays <- file.path(folder.data.raw, name.screenplays)

# Download the dataset ZIP file if we haven't done so already
if (!file.exists(path.screenplays)) {
  url.screenplays <- 
    'http://nldslab.soe.ucsc.edu/film_corpus_2/imsdb_raw_nov_2015.zip'
  download.file(url.screenplays, path.screenplays)
}

# Extract all screenplays
unzip(path.screenplays, exdir = folder.data.raw)

# Remove zip file and unnecessary "__MACOSX" folder
file.remove(path.screenplays)
unlink(file.path(folder.data.raw, '__MACOSX'), recursive = TRUE)

###############################################################################
# NRC EMOTION LEXICON
#
# The following section downloads the NRC emotinal lexicon and prepares it for
# use in sentiment analysis.

# Download Emotion Lexicon dataset from NRC website
url.nrc.sentiment <- 'http://sentiment.nrc.ca/lexicons-for-research'
name.emolex <- 'NRC-Emotion-Lexicon.zip'
file.emolex <- file.path(
  'NRC-Emotion-Lexicon-v0.92',
  'NRC-Emotion-Lexicon-Wordlevel-v0.92.txt'
)

destfile.emolex <- file.path(folder.data.raw, name.emolex)

# Download the dataset ZIP file if we haven't done so already
if (!file.exists(destfile.emolex)) {
  url.emolex <- file.path(url.nrc.sentiment, name.emolex)
  download.file(url.emolex, destfile.emolex)
}

# Extract the word-level emotion lexicon with word associations
unzip(
  destfile.emolex,
  files = file.emolex,
  overwrite = FALSE,
  exdir = folder.data.raw
)

emolex <- data.table::fread(
  file.path(folder.data.raw, file.emolex),
  sep = '\t',
  header = FALSE,
  skip = 1,
  colClasses = c(
    'character',
    'factor',
    'logical'
  ),
  col.names = c(
    'Term',
    'AffectCategory',
    'AssociationFlag'
  ),
  logical01 = TRUE
)

# Build a data dictionary suitable for sentiment analysis
dictionary.NRC <- data.table::dcast(
  emolex,
  Term ~ AffectCategory,
  value.var = 'AssociationFlag'
)

# Export NRC emotion lexicon
data.table::fwrite(
  dictionary.NRC
  , file.path(folder.data.processed, 'dictionary.NRC.csv')
  , quote = FALSE
  , row.names = FALSE
)

# Remove zip file and extracted folder with the raw NRC file
file.remove(file.path(folder.data.raw, name.emolex))
unlink(
  file.path(folder.data.raw, 'NRC-Emotion-Lexicon-v0.92'),
  recursive = TRUE
)

###############################################################################
# IMDB DATA
#
# Download IMDb dataset tables
# We don't need to extract them in advance since we can read directly from a 
# gzfile

folder.data.imdb <- file.path(folder.data, 'imdb')

url.imdb <- 'https://datasets.imdbws.com/'

name.imdb.basics <- 'title.basics.tsv.gz'
name.imdb.akas <- 'title.akas.tsv.gz'
name.imdb.ratings <- 'title.ratings.tsv.gz'

imdb.names <- c(name.imdb.basics, name.imdb.akas, name.imdb.ratings)

# Download the IMDB files if we don't already have them
sapply(imdb.names, function(name) {
  destfile <- file.path(folder.data.imdb, name)

  if (!file.exists(destfile)) {
    url <- paste0(url.imdb, name)
    download.file(url, destfile)
  }
})
