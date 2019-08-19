#install.packages("radarchart")
library(radarchart)


#Add data using list interface
labs <- c("anger", "fear", "anticipation", "trust",  "surprise", "sadness", "joy", "disgust")

scores <- list("Reservoir Dogs" = c(9, 7, 4, 5, 3, 7,8,9),
               "Aliens" = c(7, 6, 6, 2, 6, 9,4,5),
               "Up" = c(6, 5, 8, 4, 7, 6,1,3))

#Radar
chartJSRadar(scores=scores, labs=labs, maxScale=10, scaleStepWidth = 2, main = "Movie Moods (radarchart package)", showLegend = TRUE)

##########
library(plotly)

labs <- c("anger", "fear", "anticipation", "trust",  "surprise", "sadness", "joy", "disgust")

p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself'
) %>%
  add_trace(
    r = c(39, 28, 8, 7, 28, 39, 10, 33),
    theta = c('anger','fear','anticipation', 'trust', 'surprise', 'sadness', 'joy', 'disgust'),
    name = 'Aliens'
  ) %>%
  add_trace(
    r = c(1.5, 10, 39, 31, 15, 1.5, 5, 22),
    theta = c('anger','fear','anticipation', 'trust', 'surprise', 'sadness', 'joy', 'disgust'),
    name = 'Reservior Dogs'
  ) %>%
  add_trace(
    r = c(5, 6, 20, 4, 30, 3, 10, 10),
    theta = c('anger','fear','anticipation', 'trust', 'surprise', 'sadness', 'joy', 'disgust'),
    name = 'Up'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,50)
      )
    )
  )
p

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link <- api_create(p, filename = "radar-multiple")
chart_link





################
#install.packages("fmsb")
library(fmsb)

# NOT RUN {
# Data must be given as the data frame, where the first cases show maximum.
maxmin <- data.frame(
  total=c(5, 1),
  phys=c(15, 3),
  psycho=c(3, 0),
  social=c(5, 1),
  env=c(5, 1))
# data for radarchart function version 1 series, minimum value must be omitted from above.
RNGkind("Mersenne-Twister")
set.seed(123)
dat <- data.frame(
  total=runif(6, 1, 7),
  phys=rnorm(6, 10, 2),
  psycho=c(0.5,4,5,6,1,2),
  social=runif(6, 1, 7),
  env=c(5,1,3,4,5,6))
dat <- rbind(maxmin,dat)

op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 2))
radarchart(dat, axistype=1, seg=4, plty=1, vlabels=c("anger", "fear", "anticipation", "trust",  "surprise", "sadness", "joy", "disgust"), 
           title="Aliens", vlcex=0.5)
radarchart(dat, axistype=1, seg=5, plty=1, vlabels=c("Total\nQOL", "Physical\naspects", 
                                                     "Phychological\naspects", "Social\naspects", "Environmental\naspects"), 
           title="(Aliens)", vlcex=0.5)
radarchart(dat, axistype=1, seg=5, plty=1, vlabels=c("Total\nQOL", "Physical\naspects", 
                                                     "Phychological\naspects", "Social\naspects", "Environmental\naspects"), 
           title="(Aliens)", vlcex=0.5)
radarchart(dat, axistype=1, seg=5, plty=1, vlabels=c("Total\nQOL", "Physical\naspects", 
                                                     "Phychological\naspects", "Social\naspects", "Environmental\naspects"), 
           title="(Aliens)", vlcex=0.5)





radarchart(dat, axistype=2, pcol=topo.colors(3), plty=1, pdensity=c(5, 10, 30), 
           pangle=c(10, 45, 120), pfcol=topo.colors(3), 
           title="(topo.colors, fill, axis=2)")
radarchart(dat, axistype=3, pty=32, plty=1, axislabcol="grey", na.itp=FALSE,
           title="(no points, axis=3, na.itp=FALSE)")
radarchart(dat, axistype=1, plwd=1:5, pcol=1, centerzero=TRUE, 
           seg=4, caxislabels=c("worst", "", "", "", "best"),
           title="(use lty and lwd but b/w, axis=1,\n centerzero=TRUE, with centerlabels)")
par(op)
?par
# }