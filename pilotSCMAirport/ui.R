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
##cs <- unique( as.list(as.data.frame(t(sites[,c("Alias", "SiteTitle")]))))

## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="Pilot SCM",
               selected="Data",
               br(),
               fluidRow(
                   h3("Pilot SCM", align="center"),
                   wellPanel(
                       fluidRow(
                           column(8,
                                  selectInput(
                                      inputId="Alias",
                                      label="SCM Alias: ",
                                      choices=
                                          setNames(as.character(
                                              sites$Alias), sites$SiteTitle),
                                      multiple=FALSE,
                                      selectize=TRUE,
                                      selected="Providence Prep School")
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
                           column(8,
                                  selectInput(
                                      inputId="analyte",
                                      label="Select Analyte: ",
                                      unique(scmDF$Analyte),
                                      selected="Total Suspended Solids")
                                  )
                       ),
                       fluidRow(
                           column(8, align="center",
                                  dateRangeInput(inputId="dates",
                                                 label="Filter Dates: ",
                                                 start="2004-01-01", end=Sys.Date(),
                                                 min="1986-11-03", max=Sys.Date(),
                                                 startview="year", weekstart=0)
                                  )
                       )
                   ),
                   tabsetPanel(type="tabs",
                               tabPanel("Time Series",
                                        plotlyOutput("tsPlotly")
                                       ),
                               tabPanel("Hist",
                                        plotOutput("selHist")
                                        ),
                               tabPanel("Scatter",
                                        fluidRow(
                                            column(4,
                                                   plotOutput("defScatter")
                                                   ),
                                            column(8,
                                                   DT::dataTableOutput("matchedSamples")
                                                   )
                                        )
                                        ),
                               tabPanel("Analysis Plots",
                                        plotOutput("analPlots")
                                        )
                               )
               )
               )
)
