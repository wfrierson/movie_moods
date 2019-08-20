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
# DEPECHEMOOD++
#
# The following section downloads the DepecheMood++ emotional lexicon.
#
# Araque, O., Gatti, L., Staiano, J., and Guerini, M. (2018) 
# "DepecheMood++: a Bilingual Emotion Lexicon Built Through Simple Yet Powerful 
# Techniques". ArXiv preprint is available at https://arxiv.org/abs/1810.03660

name.DMpp <- 'DepecheMood_v2.0.zip'
path.DMpp <- file.path(folder.data.raw, name.DMpp)
file.DMpp <- file.path(
  'DepecheMood++',
  'DepecheMood_english_lemmapos_full.tsv'
)


if (!file.exists(path.DMpp)) {
  url.DMpp <- file.path(
    'https://github.com/marcoguerini/DepecheMood/releases/download/v2.0'
    , name.DMpp
  )
  download.file(url.DMpp, path.DMpp)
}

# Extract the relevant emotion lexicon
unzip(
  path.DMpp,
  files = file.DMpp,
  overwrite = FALSE,
  exdir = folder.data.raw
)

# Remove the zip file
file.remove(path.DMpp)

###############################################################################
# IMDB DATA
#
# Download IMDb dataset tables
# We don't need to extract them in advance since we can read directly from a 
# gzfile

folder.data.imdb <- file.path(folder.data, 'imdb')

if (!dir.exists(folder.data.imdb)) {
  dir.create(folder.data.imdb)
}

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
