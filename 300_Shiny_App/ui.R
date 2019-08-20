library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
source("MoodLandscape.R")
source("Mood_Star.R")

shinyUI(
  fluidPage(
    theme = shinythemes::shinytheme('darkly'),

    shinyWidgets::useShinydashboard(),

    titlePanel('Movie Moods'),

    sidebarLayout(
        
      sidebarPanel(
        selectInput('filter1', 'Filter 1', c('Choice')),
        sliderInput(
          'filter2',
          'Filter 2',
          min = 1,
          max = 50,
          value = c(35, 45)
        ),
        sliderInput(
          'filter3',
          'Filter 3',
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
            height = 300,
            moodLandscapeUi(
              "movieMoodLandscape",
              width = "100%",
              height = "240"
            )
          ),
          box(
            title = "Mood star for selected movies",
            width = 6,
            height = 300,
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
            height = 300,
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
