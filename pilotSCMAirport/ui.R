library(shiny)
#source("metalsFun.r")
#source("metalsThresFuns.r")

## Load, format, subset, WQD File
wqDF <- read.csv("LabReport.txt",
                 stringsAsFactors=FALSE, sep=",", header=TRUE)
sites <<- read.csv("sites.csv", sep=",", header=TRUE,
                  stringsAsFactors=FALSE)

scmDF <- wqDF[which(wqDF$Site %in% sites$Site),]
scmDF$Coldate <- as.POSIXct(scmDF$Coldate, format="%m/%d/%Y %H:%M:%S")
scmDF$Date <- as.Date(scmDF$Coldate)  ## If needed
scmDF <<- scmDF[order(scmDF$Coldate), ]

## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="Pilot SCM",
               selected="Data",
               br(),
               fluidRow(
                   h3("Pilot SCM", align="center"),
                   wellPanel(
                       fluidRow(
                           column(6,
                                  selectInput(
                                      inputId="Alias",
                                      label="SCM Alias: ",
                                      choices=unique(sites$Alias),
                                      multiple=FALSE,
                                      selectize=TRUE,
                                      selected="PPS")
                                  ),
                           column(6,
                                  selectInput(
                                      inputId="analyte",
                                      label="Select Analyte: ",
                                      unique(scmDF$Analyte),
                                      selected="Total Suspended Solids")
                                  )
                       ),
                       fluidRow(
                           column(4,
                                  uiOutput("selectSite1")
                                  ),
                           column(4,
                                  uiOutput("selectSite2")
                                  )
                       ),
                       fluidRow(
                           column(8, align="center",
                                  dateRangeInput(inputId="dates",
                                                 label="Filter Dates: ",
                                                 start="2004-01-01", end=Sys.Date(),
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
                                        plotOutput("defScatter")
                                       )
                               )
               )
               )
)
