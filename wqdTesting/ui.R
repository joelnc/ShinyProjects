library(shiny)

f <- "wqdTestingText.dat"
con <- file(f, open="r")
t <- readLines(con)
t <- t[which(t!="")]
close(con)

## Load WQD File
wqDF <- read.csv("LabReport.txt", stringsAsFactors=FALSE,
                 sep=",", header=TRUE)
sedDF <- wqDF
rm(wqDF)
sedDF$Coldate <- as.POSIXct(sedDF$Coldate, format="%m/%d/%Y %H:%M:%S")
sedDF$Date <- as.Date(sedDF$Coldate)
sedDF <<- sedDF[order(sedDF$Coldate), ]

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
                           unique(sedDF$Site))
                       ),
                column(3,
                       selectInput(
                           inputId="analyte",
                           label="Choose: ",
                           unique(sedDF$Analyte))
                       ),
                column(3,
                       selectInput(
                           inputId="element",
                           label="Choose: ",
                           choices=c("Fixed Interval"="ICS1.1",
                               "Pilot?"="ICS1.5",
                               "EPIC"="EPIC"),
                           selected="ICS1.1")
                       )
                ),

            mainPanel(
                tabsetPanel(type="tabs",
                            tabPanel("Interactive Plot", br(),
                                     h3("Time Series Plot of Selected Data"),
                                     checkboxInput("log", "Log Y Axis?",
                                                   value=FALSE),
                                     ## plotlyOutput("Plot1",
                                     ##              height="600px")),
                                     plotOutput("tsPlot")),
                            tabPanel("Background Docs", br(),
                                     a("EPA",
                                       href="https://www.epa.gov/"),
                                     br())#,
                            ## tabPanel("Shiny Stuff", br(),
                            ##          h3("Other Functionality (basic)"),
                            ##          fluidRow(
                            ##              column(4,
                            ##                     radioButtons("radio",
                            ##                                  label = h3("RB"),
                            ##                       choices = list("C 1" = 1,
                            ##                                      "C 2" = 2,
                            ##                                      "C 3" = 3),
                            ##                       selected = 1)
                            ##                     ),
                            ##              column(4,
                            ##                     sliderInput("slider",
                            ##                                 label=h3("Sldr",
                            ##                                     min=0,
                            ##                                     max=10,
                            ##                                     value=c(3,6))
                            ##                     )),
                            ##              column(4,
                            ##                     numericInput("num",
                            ##                                  label=h3("Num"),
                            ##                                  value=7)
                            ##                     )
                            ##              )
                            ##          ),
                            ##         hr()
                            )
                )
            )
        )
    )

    ##                                     )
    ##                                     )

    ##                                     #,
    ##                                  ## fluidRow(
    ##                                      ## column(4,
    ##                                      ##        verbatimTextOutput("radioV")),
    ##                                      column(4,
    ##                                             verbatimTextOutput("range")),
    ##                                      column(4,
    ##                                             verbatimTextOutput("value"))
    ##                                  )
    ##                         )
    ##             )
    ##         )
    ##     )
    ## )
