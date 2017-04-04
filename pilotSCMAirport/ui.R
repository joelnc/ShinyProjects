library(shiny)
#source("metalsFun.r")
#source("metalsThresFuns.r")

## Load, format, subset, WQD File
wqDF <- read.csv("LabReport.txt",
                 stringsAsFactors=FALSE, sep=",", header=TRUE)
wqDF$Coldate <- as.POSIXct(wqDF$Coldate, format="%m/%d/%Y %H:%M:%S")
wqDF$Date <- as.Date(wqDF$Coldate)  ## If needed
wqDF <- wqDF[which(wqDF$Analyte %in%
                   c("Total Phosphorus", "Fecal Coliform",
                     "Total Suspended Solids",
                     "Copper", "Lead", "Zinc", "Cadmium", "Chromium",
                     "Beryllium", "Selenium", "Arscenic", "Silver")), ]

wqDFInlet <<- wqDF[which(wqDF$Site %in%
                   c("APFGI", "APSCI", "BRRGI", "BRUN1",
                     "CGI", "CMCHUI", "CTBAYI", "CTCDSI",
                     "CTCRYI", "CTDDI", "CTSTCI", "CYTW1I",
                     "CYTW2I", "CYTW3I", "CYTW4I", "CYVORI",
                     "EBWI", "EGWI", "FLIDU",
                     "FS39I", "HMRGI", "IVEYI1", "LGVORI",
                     "LGVSI", "LLRSCI", "LOWESBAY", "LOWESCRYI",
                     "MHDDI", "MHPURE", "MPHSITI", "MPHSRGI")),]


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
                           column(4,
                                  selectInput(
                                      inputId= "sitecode1",
                                      label="Select Site 1: ",
                                      choices=unique(wqDF$Site),
                                      multiple=FALSE,
                                      selectize=TRUE)
                                      #selected="MC22A")
                                  ),
                           column(4,
                                  selectInput(
                                      inputId= "sitecode2",
                                      label="Select Site 2: ",
                                      choices=unique(wqDF$Site),
                                      multiple=FALSE,
                                      selectize=TRUE)
                                      #selected="MC22A")
                                  ),
                           column(4,
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
                                  sliderInput(inputId="nBins",
                                              label="Number of Bins",
                                              min=0, max=500,
                                              value=20, step=10,
                                              round=FALSE,
                                              animate=FALSE)
                                  )
                       )
                   ),
                   tabsetPanel(type="tabs",
                               tabPanel("TSS",
                                        plotOutput("tssPlot")
                                       ),
                               tabPanel("Hist",
                                        plotOutput("selHist")
                                        ),
                               tabPanel("Scatter",
                                        plotOutput("dataScatter")
                                       )
                               )
               )
               )
)
