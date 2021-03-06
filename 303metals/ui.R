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
wqDF$Date <- as.Date(wqDF$Coldate)  ## If needed
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
                    selected="MC22A"
                ),
                selectInput(
                    inputId="analyte",
                    label="Choose: ",
                    unique(wqDF$Analyte)
                ),
                dateRangeInput(inputId="dates",
                               label="Filter Dates: ",
                               start="2010-01-01", end=Sys.Date(),
                               min="1986-11-03",
                               max=Sys.Date(),
                               startview="year", weekstart=0
                               )
                ),
            mainPanel(
                tabsetPanel(type="tabs",
                            tabPanel("Interactive Plot",
                                     fluidRow(
                                         column(12, h3("TS Plot of Data"),
                                                plotlyOutput("tsPlot"),
                                                height="500px"
                                                )
                                         )##,
                                     ## fluidRow(
                                     ##     column(12,
                                     ##            plotlyOutput("tDiffPlot"),
                                     ##            height="350px"
                                     ##            )
                                     ##     )
                                      ),
                            tabPanel("DiffPlot", br(),
                                     plotlyOutput("tDiffPlot",
                                                  height="500px")
                                     ),
                            tabPanel("exPlot", br(),
                                     fluidRow(
                                         column(5,
                                                radioButtons(inputId="radio",
                                                             label = h3("Hypothesis Tests"),
                                                             choices = list(
                                                                 "Listing (assuming the site is not 303(d) listed for this pollutant)" = "g",
                                                                 "Delisting (assuming the site is 303(d) listed for this pollutant)" = "l",
                                                                 "Two-sided!"= "t"),
                                                             selected = "g"
                                                             )
                                                ),
                                         column(7,
                                                sliderInput(inputId="conf",
                                                            label=
                                                                "Condidence Level (90% = 0.9)",
                                                            min=0, max=1,
                                                            value=.9, step=0.01,
                                                            round=FALSE,
                                                            animate=TRUE),
                                                sliderInput(inputId="cutoff",
                                                            label=
                                                                "Exceedance Level (10% = 0.10)",
                                                            min=0, max=1,
                                                            value=.1, step=0.01,
                                                            round=FALSE,
                                                            animate=TRUE)
                                                )
                                         ),

                                     plotOutput("excPlot",
                                                  height="500px")
                                     )

                            )
                )
            )
        )
    )
