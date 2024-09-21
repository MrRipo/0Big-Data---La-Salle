library(shiny)
library(dplyr)
# install.packages("ISOweek")
# install.packages("lubridate")
install.packages("googleCharts")
library(ISOweek)
library(lubridate)
library(googleCharts)


setwd('C:/Users/alber/Desktop/0Big Data - La Salle/Estadística/7- Big data y shiny')
data <- read.table('flights_mini.txt',header=TRUE,sep="\t")
data$dest <- as.factor(data$dest)
data$dep_delay <- as.numeric(data$dep_delay)
data$arr_delay <- as.numeric(data$arr_delay)
View(data)


data$Date <- as.Date(paste(data$year, data$month, data$day, sep = "-"))
# Convert the Date column to the corresponding ISO week number
data$WeekNumber <- isoweek(data$Date)
data$WeekNumber <- wday(data$Date)


# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = min(data$dep_delay) -2,
  max = max(data$dep_delay) + 2
)
ylim <- list(
  min = min(data$arr_delay)-2,
  max = max(data$arr_delay) + 2
)

ui <- fluidPage(
  # This line loads the Google Charts JS library
  googleChartsInit(),
  
  # Use the Google webfont "Source Sans Pro"
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
             "body {font-family: 'Source Sans Pro'}"
  ),
  
  h2("Google Charts demo"),
  
  googleBubbleChart("chart",
                    width="100%", height = "475px",
                    # Set the default options for this chart; they can be
                    # overridden in server.R on a per-update basis. See
                    # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                    # for option documentation.
                    options = list(
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      # Set axis labels and ranges
                      hAxis = list(
                        title = "Departure Delay (min)",
                        viewWindow = xlim
                      ),
                      vAxis = list(
                        title = "Arrival Delay (min)",
                        viewWindow = ylim
                      ),
                      # The default padding is a little too spaced out
                      chartArea = list(
                        top = 50, left = 75,
                        height = "75%", width = "75%"
                      ),
                      # Allow pan/zoom
                      explorer = list(),
                      # Set bubble visual props
                      bubble = list(
                        opacity = 0.4, stroke = "none",
                        # Hide bubble label
                        textStyle = list(
                          color = "none"
                        )
                      ),
                      # Set fonts
                      titleTextStyle = list(
                        fontSize = 16
                      ),
                      tooltip = list(
                        textStyle = list(
                          fontSize = 12
                        )
                      )
                    )
  ),
  fluidRow(
    shiny::column(4, offset = 4,
                  sliderInput("Dia de Semana", "Dia de Semana",
                              min = min(data$WeekNumber), max = max(data$WeekNumber),
                              value = min(data$WeekNumber), animate = TRUE)
    )
  )
)


server <- function(input, output, session) {
  
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$dest)
  )
  
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data %>%
      filter(weeknum == input$WeekNumber) %>%
      select(dest,dep_delay, arr_delay,
             dest, year) %>%
      arrange(dest)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Departure delay vs. Arrival delay, %s",
          input$WeekNumber),
        series = series
      )
    )
  })
}

shinyApp(ui = ui, server = server)