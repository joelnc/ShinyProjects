library(shiny)

f <- "wqdTestingText.dat"
con <- file(f, open="r")
t <- readLines(con)
t <- t[which(t!="")]
close(con)

## Load WQD File
wqDF <- read.csv("c:/users/95218/documents/r/shinystuff/LabReport.txt",
                 stringsAsFactors=FALSE,
                 sep=",", header=TRUE)
wqDF$Coldate <- as.POSIXct(wqDF$Coldate, format="%m/%d/%Y %H:%M:%S")
wqDF$Date <- as.Date(wqDF$Coldate)
wqDF <- wqDF[which(wqDF$Analyte %in% c("Cadmium", "Lead", "Zinc",
                                       "Copper")), ]
wqDF <<- wqDF[order(wqDF$Coldate), ]

## Define UI for applicaiton that draws a hist
shinyUI(
    fluidPage(
        ## Application title
        h1("Other Demo", align="center"),
        br(), p(t[1]),
        sidebarLayout(
            fluidRow(
                column(3,
                       selectInput(
                           inputId= "sitecode",
                           label="Choose Site: ",
                           unique(wqDF$Site))
                       ),
                column(3,
                       selectInput(
                           inputId="analyte",
                           label="Choose: ",
                           unique(wqDF$Analyte))
                       )
               ),
            mainPanel(
                tabsetPanel(type="tabs",
                            tabPanel("Interactive Plot", br(),
                                     h3("Time Series Plot of Selected Data"),
                                     plotlyOutput("tsPlot",
                                                  height="450px"),
                                     br()),
                            tabPanel("DiffPlot", br(),
                                     plotlyOutput("tDiffPlot",
                                                  height="300px")
                                     )
                            )
                )
            )
        )
    )
