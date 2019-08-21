library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
source("MoodLandscape.R")
source("Mood_Star.R")

# Load all genres from file
folder.data <- '../100_Data'
folder.data.processed <- file.path(folder.data, '120_Processed_Data')
screenplayPaths <- data.table::fread(
  file.path(folder.data.processed, '301_screenplayPaths.csv'),
  stringsAsFactors = FALSE,
  quote = "",
  sep = '|'
)
genres <- tail(colnames(screenplayPaths), 22)

screenplayMoodscores <- data.table::fread(
  file = file.path(
    folder.data.processed,
    "702_screenplayMoodProb.movie.csv"
  ),
  sep = "|",
  quote = ""
)

shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme("darkly"),

    shinyWidgets::useShinydashboard(),

    titlePanel("Movie Moods"),

    sidebarLayout(

      sidebarPanel(
        selectInput('genreFilter', 'Genre', genres),
        selectizeInput('searchUi',"Search by Title",
            choices = c("Select up to 2" = "", screenplayMoodscores$movie),
            multiple = FALSE,
            options = list(maxItems = 2)),
        sliderInput(
          "filter2",
          "Filter 2",
          min = 1,
          max = 50,
          value = c(35, 45)
        ),
        sliderInput(
          "filter3",
          "Filter 3",
          min = 1,
          max = 50,
          value = 30
        )
      ),

      mainPanel(
        fluidRow(
          box(
            title = "Movies",
            width = 6,
            height = 400,
            moodLandscapeUi(
              "movieMoodLandscape",
              width = "100%",
              height = "240"
            )
          ),
          box(
            title = "Mood star for selected movies",
            width = 6,
            height = 400,
            moodStarUi(
              "movieMoodStar",
              width = "100%",
              height = "240"
            )
          )
        ),
        hr(),
        fluidRow(
          box(
            title = "Characters",
            width = 6,
            height = 400,
            moodLandscapeUi(
              "charactersMoodLandscape",
              width = "100%",
              height = "240"
            )
          ),
          box(
            title = "Mood star for selected characters",
            width = 6,
            height = 300
          )
        ),
        fluidRow(
          box(
            title = "Character mood progression",
            width = 12,
            height = 300
          )
        ),
        hr(),
        fluidRow(
          column(width = 12, moodLandscapeIxDebugUi("movieMoodLandscape"))
        )
      )
    )
  )
)
