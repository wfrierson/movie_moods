###############################################################################
# This script publishes the Shiny app to ShinyApps.io

###############################################################################
# DEPENDENCIES

library(rsconnect)

###############################################################################
# SETUP

folder.data <- "100_Data"
folder.data.processed <- file.path(folder.data, "120_Processed_Data")
folder.shinyapp <- "300_Shiny_App"
folder.shinyrelease <- "release"

###############################################################################
# ASSEMBLE APP FOLDER

# Create the release folder
if (!dir.exists(folder.shinyrelease)) {
  dir.create(folder.shinyrelease)
}

# Copy code files
app.files <- list.files(folder.shinyapp, full.names = TRUE, recursive = TRUE)
file.copy(app.files, folder.shinyrelease, overwrite = TRUE)

# Fix the folder paths
filesToFix <- file.path(folder.shinyrelease, c("server.R", "ui.R"))
sapply(filesToFix, function(filename) {
  txt <- gsub(
    pattern = '^folder.data.processed <- .*$',
    replace = 'folder.data.processed <- (".")',
    x = readLines(filename))
  writeLines(txt, con = filename)
})

# Copy data files
data.files <- c(
  "301_screenplayPaths.csv",
  "702_screenplayMoodProb.movie.csv",
  "703_screenplayMoodProb.character.csv",
  "901_screenplayMoodProb.movieRotated.csv",
  "902_screenplayMoodProb.characterRotated.csv",
  "movie_name_lookup.csv"
)
file.copy(file.path(folder.data.processed, data.files), folder.shinyrelease)

###############################################################################
# TEST THE APP PACKAGE

runApp(folder.shinyrelease)

###############################################################################
# PUBLISH TO SHINYAPPS.IO

rsconnect::deployApp(folder.shinyrelease, appName = "MovieMoods")
