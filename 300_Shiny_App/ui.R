library(shiny)
library(shinydashboard)
library(shinythemes)
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

    titlePanel("Explore Movie by Moods"),

    sidebarLayout(

      sidebarPanel(
        width = 2,
        selectizeInput(
          "genreFilter",
          "Genre",
          c("Choose up to 5" = "", genres),
          multiple = TRUE,
          options = list(maxItems = 5)
        ),
        sliderInput(
          "numCharactersFilter",
          "Number of Characters",
          min = 1,
          max = 140,
          value = c(0, 140)
        )
      ),

      mainPanel(
        shiny::fluidRow(h3("Choose movies to compare")),
        fluidRow(
          shiny::column(
            width = 6,
            moodLandscapeUi(
              "movieMoodLandscape",
              width = "100%",
              height = "320"
            )
          ),
          shiny::column(
            width = 6,
            moodStarUi(
              "movieMoodStar",
              width = "100%",
              height = "400"
            )
          )
        ),
        hr(),
        shiny::fluidRow(h3("Choose characters to compare")),
        fluidRow(
          shiny::column(
            width = 6,
            moodLandscapeUi(
              "charactersMoodLandscape",
              width = "100%",
              height = "320"
            )
          ),
          shiny::column(
            width = 6,
            moodStarUi(
              "characterMoodStar",
              width = "100%",
              height = "400"
            )
          )
        )
      )
    )
  )
)
