library(shiny)
source("metalsFun.r")
source("metalsThresFuns.r")

f <- "appText.dat"
con <- file(f, open="r")
t <- readLines(con)
t <- t[which(t!="")]
close(con)
textI <- 1

## Load, format, subset, WQD File
## wqDF <- read.csv("LabReport.txt",
##                  stringsAsFactors=FALSE, sep=",", header=TRUE)
## wqDF$Coldate <- as.POSIXct(wqDF$Coldate, format="%m/%d/%Y %H:%M:%S")
## wqDF$Date <- as.Date(wqDF$Coldate)  ## If needed
## wqDF <- wqDF[which(wqDF$Analyte %in%
##                    c("Copper", "Lead", "Zinc", "Cadmium", "Chromium",
##                      "Beryllium", "Selenium", "Arscenic", "Silver")), ]
## wqDF <<- wqDF[order(wqDF$Coldate), ]

## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="Exceedance Demos",
               selected="Hypothetical",
               br(),
               p(t[1]),
               fluidRow(
                   column(12, h3("Hypothetical Data Selection", align="center"),
                          wellPanel(
                              selectInput(
                                  inputId= "dataset",
                                  label="Select Dataset: ",
                                  choices=c("Slight (3 exceedances out of 13)"="data1",
                                            "Vast (3 exceedances out of 13)"="data2",
                                            "2 exceedances out of 13"="data3",
                                            "1 exceedance out of 13"="data4",
                                            "0 exceedances out of 13"="data5",
                                            "0 exceedances out of 23"="data6",
                                            "1 exceedance out of 38"="data7",
                                            "2 exceedances out of 52"="data8"),
                                  selected="data1")
                          )
                          )
               ),
               tabPanel("Hypothetical",
                        tabsetPanel(type="tabs",
                                    tabPanel(title="Time Series",
                                             h4("Hypothetical Time Series"),
                                             t[2], br(), br(), t[3], plotOutput("tsPlot")),
                                    tabPanel(title="Listing Analysis",
                                             h4("Hypothetical Time Series"),
                                             t[4],
                                             plotOutput("excPlotG")),
                                    tabPanel(title="Delisting Analysis*",
                                             h4("Hypothetical Time Series"),
                                             t[5], br(), br(), t[6],
                                             fluidRow(
                                                 column(12, align="center",
                                                        plotOutput("excPlotL")
                                                        )
                                                 )
                                             ),
                                    tabPanel(title="Two Sided Test",
                                             h4("Hypothetical Time Series"),
                                             t[7], br(), br(), t[8],
                                             plotOutput("excPlotT")),
                                    tabPanel(title="Toggleable",
                                             fluidRow(
                                                 column(5,
                                                        radioButtons(inputId="radio",
                                                                     label = h3("Hypothesis Tests"),
                                                                     choices = list(
                                                                         "Listing (assuming the site is not 303(d) listed for this pollutant)" = "g",
                                                                         "Delisting (assuming the site is 303(d) listed for this pollutant)" = "l",
                                                                         "Two-sided!"= "t"),
                                                                     selected = "g")
                                                        ),
                                                 column(7,
                                                        br(),
                                                        sliderInput(inputId="conf",
                                                                    label="Condidence Level (90% = 0.9)",
                                                                    min=0, max=1,
                                                                    value=.9, step=0.01,
                                                                    round=FALSE,
                                                                    animate=TRUE),
                                                        sliderInput(inputId="cutoff",
                                                                    label="Exceedance Level (10% = 0.10)",
                                                                    min=0, max=1,
                                                                    value=.1, step=0.01,
                                                                    round=FALSE,
                                                                    animate=TRUE)
                                                        )
                                             ),
                                             textOutput("confText"),
                                             fluidRow(
                                                 column(9,
                                                        plotOutput("excPlotTog", height="500px")
                                                        ),
                                                 column(3,
                                                        br(), br(),
                                                        img(src="stormy.jpg",
                                                            height="200", width="125")
                                                        )
                                             )
                                    ),
                                    tabPanel("'One in Three'",
                                             br(), t[9]),
                                    tabPanel("Backgroud Docs.",
                                              br(),
                                              h3("Related Documents"),
                                              a("EPA Public Notice Decision / 303(d) Listing Letter",
                                                href="https://www.epa.gov/sites/production/files/2016-12/documents/nc2016_303ddecisionpackage20161208_reduced.pdf"),
                                              br(), br(),
                                              a("Lin et al. 2000",
                                                href="http://infohouse.p2ric.org/ref/41/40276.pdf")
                                              )
                                    )
                        ),
               tabPanel("CharMeck Metals",
                        tabsetPanel(type="tabs",
                                    tabPanel(title="Time Series Conc."
                                             ),
                                    tabPanel(title="Time Series Exceedances"
                                             ),
                                    tabPanel(title="90% Confidence"
                                             )
                                    )
                        )
               )
)





