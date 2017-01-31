library(shiny)
source("metalsFun.r")

## Load and Read Text File if needed
f <- "wqdTestingText.dat"
con <- file(f, open="r")
t <- readLines(con)
t <- t[which(t!="")]
close(con)

## Load, format, subset, WQD File
wqDF <- read.csv("LabReport.txt",
                 stringsAsFactors=FALSE, sep=",", header=TRUE)
wqDF$Coldate <- as.POSIXct(wqDF$Coldate, format="%m/%d/%Y %H:%M:%S")
##wqDF$Date <- as.Date(wqDF$Coldate)  ## If needed
wqDF <- wqDF[which(wqDF$Analyte %in%
                   c("Copper", "Lead", "Zinc", "Cadmium", "Chromium",
                     "Beryllium", "Selenium", "Arscenic", "Silver")), ]
wqDF <<- wqDF[order(wqDF$Coldate), ]

## Define UI for applicaiton that draws a hist
shinyUI(
    fluidPage(
        ## Application title
        h1("Other Demo", align="center"),
        br(), p(t[1]),
        sidebarLayout(fluid=TRUE,
            sidebarPanel(
                selectInput(
                    inputId= "sitecode",
                    label="Choose Site: ",
                    choices=returnSiteGlob(unique(wqDF$Site)),
                    multiple=FALSE,
                    selectize=TRUE,
                    selected="MC22A"),
                selectInput(
                    inputId="analyte",
                    label="Choose: ",
                    unique(wqDF$Analyte)
                )
            ),
            mainPanel(
                tabsetPanel(type="tabs",
                            tabPanel("Interactive Plot",
                                     fluidRow(
                                         column(12, h3("TS Plot of Data"),
                                                plotlyOutput("tsPlot")
                                                )
                                         )##,
                                     ## fluidRow(
                                     ##     column(12,
                                     ##            plotlyOutput("tDiffPlot"),
                                     ##            height="350px"
                                     ##            )
                                     ##     )
                                      )#,
                            ## tabPanel("DiffPlot", br(),
                            ##          plotlyOutput("tsPlot",
                            ##                       height="300px")
                            ##          )
                            )
                )
            )
        )
    )
