# Downloading and unzipping a corpus of digitized screenplays from the National 
# Language and Dialogue Systems (NLDS) lab of UC Santa Cruz.
#
# From the readme of this dataset (https://nlds.soe.ucsc.edu/fc2):
#
# Overview: This corpus is an updated version of the Film Corpus 1.0. It 
# contains complete texts for the scripts of 1068 films in txt files, scraped 
# from imsdb.com on Nov, 2015 using scrapy. It also contains 960 film scripts 
# where the dialog in the film has been separated from the scene descriptions.
folder.data <- '100_Data'

name.screenplays <- 'imsdb_raw_nov_2015.zip'

path.screenplays <- file.path(folder.data, name.screenplays)

url.screenplays <- 'http://nldslab.soe.ucsc.edu/film_corpus_2/imsdb_raw_nov_2015.zip'

download.file(url.screenplays, path.screenplays)

unzip(path.screenplays, exdir = folder.data)

# Remove zip file and unnecessary "__MACOSX" folder
file.remove(path.screenplays)
unlink(file.path(folder.data, '__MACOSX'), recursive = TRUE)
