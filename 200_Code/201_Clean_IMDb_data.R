require(R.utils) # Needed for data.table::fread to work with gzipfiles

folder.data <- '100_Data'
folder.data.imdb <- file.path(folder.data, 'imdb')
name.imdb.basics <- 'title.basics.tsv.gz'
name.imdb.akas <- 'title.akas.tsv.gz'
name.imdb.ratings <- 'title.ratings.tsv.gz'


compressed.path <- file.path(folder.data.imdb, name.imdb.basics)
basics <- data.table::fread(compressed.path,
                            sep = '\t',
                            sep2 = ',',
                            quote = '',
                            header = TRUE,
                            na.strings = '\\N',
                            colClasses = c(
                              'tconst' = 'character',
                              'primaryTitle' = 'character',
                              'originalTitle' = 'character'
                            ),
                            fill = FALSE)
