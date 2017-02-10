library(shiny)
source("metalsFun.r")
source("metalsThresFuns.r")

## Load, format, subset, WQD File
wqDF <- read.csv("LabReport.txt",
                 stringsAsFactors=FALSE, sep=",", header=TRUE)
wqDF$Coldate <- as.POSIXct(wqDF$Coldate, format="%m/%d/%Y %H:%M:%S")
wqDF$Date <- as.Date(wqDF$Coldate)  ## If needed
wqDF <- wqDF[which(wqDF$Analyte %in%
                   c("Total Phosphorus", "Fecal Coliform", "Total Suspended Sediments",
                     "Copper", "Lead", "Zinc", "Cadmium", "Chromium",
                     "Beryllium", "Selenium", "Arscenic", "Silver")), ]
wqDF <<- wqDF[order(wqDF$Coldate), ]

## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="Exceedance Plots",
               selected="Data",
               br(),
               fluidRow(
                   h3("Percent Exceedance", align="center"),
                   wellPanel(
                       fluidRow(
                           column(6,
                                  selectInput(
                                      inputId= "sitecode",
                                      label="Select Site: ",
                                      choices=returnSiteGlob(unique(wqDF$Site)),
                                      multiple=FALSE,
                                      selectize=TRUE,
                                      selected="MC22A")
                                  ),
                           column(6,
                                  selectInput(
                                      inputId="analyte",
                                      label="Select Analyte: ",
                                      unique(wqDF$Analyte),
                                      selected="Copper")
                                  )
                       ),
                       fluidRow(
                           column(8, align="center",
                                  dateRangeInput(inputId="dates",
                                                 label="Filter Dates: ",
                                                 start="2010-01-01", end=Sys.Date(),
                                                 min="1986-11-03", max=Sys.Date(),
                                                 startview="year", weekstart=0)
                                  ),
                           column(4,
                                  sliderInput(inputId="Benchmark",
                                              label="Benchmark Conc.",
                                              min=0, max=500,
                                              value=10, step=1,
                                              round=FALSE,
                                              animate=FALSE)
                                  )
                       )
                   ),
                   plotlyOutput("exPlotQuick", height="450px"),
                   hr(),
                   h3("Table results, needs work..."),
                   tableOutput("summaryStuff")
               )
               )
)


