library(shiny)
source("metalsFun.r")
f <- "appText.dat"
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
    navbarPage(title="Exceedance Demos",
               selected="Hypothetical",
               br(),
               p(t[1]),
               fluidRow(
                   column(4, h3("Hypothetical Data Selection", align="center"),
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
                                            "1 exceedance out of 37"="data7",
                                            "2 exceedances out of 52"="data8"),
                                  selected="data1")
                          )
                          ),
                   column(8, h3("CharMeck Metals Data", align="center"),
                          wellPanel(
                              fluidRow(
                                  column(12,
                                         fluidRow(
                                             column(6,
                                                    selectInput(
                                                        inputId= "sitecode",
                                                        label="Select Site: ",
                                                        choices=returnSiteGlob(unique(wqDF$Site)),
                                                        multiple=FALSE, selectize=TRUE,
                                                        selected="MC22A")
                                                    ),
                                             column(6,
                                                    selectInput(
                                                        inputId="analyte", label="Select Analyte: ",
                                                        unique(wqDF$Analyte))
                                                    )
                                         ),
                                         fluidRow(
                                             column(12, align="center",
                                                    dateRangeInput(inputId="dates",
                                                                   label="Filter Dates: ",
                                                                   start="2010-01-01", end=Sys.Date(),
                                                                   min="1986-11-03", max=Sys.Date(),
                                                                   startview="year", weekstart=0)
                                                    )
                                         )
                                         )
                              )
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
                                                        plotOutput("excPlotL")),
                                               tabPanel(title="Two Sided Test",
                                                        h4("Hypothetical Time Series"),
                                                        t[7], br(), br(), t[8],
                                                        plotOutput("excPlotT")),
                                               tabPanel("'One in Three'",
                                                        br(), t[9])
                                               )
                        ),
               tabPanel("CharMeck Metals",
                        tabsetPanel(type="tabs",
                                    tabPanel(title="Time Series Conc.",
                                             plotlyOutput("cmTsPlot")),
                                    tabPanel(title="Time Series Exceedances",
                                             plotlyOutput("tDiffPlot")),
                                    tabPanel(title="90% Confidence",
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
                                             plotOutput("excPlot", height="500px")
                                             )
                                    )
                        )
               )
)






        ## sidebarLayout(fluid=TRUE,
##                       sidebarPanel(
##                           h4("Hypothetical Data (Blue Tabs)",
##                              style="color: #4d3a7d;"),


##                       ),
##                       mainPanel(
##                           tabsetPanel(type="tabs",
##                                                br(), t[7], br(), br(), t[8],
##                                                plotOutput("excPlotT")),
##                                       tabPanel("Background Docs", br(),
##                                                a("EPA Public Notice Decision / 303(d) Listing Letter",
##                                                  href="https://www.epa.gov/sites/production/files/2016-12/documents/nc2016_303ddecisionpackage20161208_reduced.pdf"),
##                                                br(),
##                                                a("Lin et al. 2000",
##                                                  href="http://infohouse.p2ric.org/ref/41/40276.pdf")
##                                                )
##                                       )
##                       )
##                       )
##     )
## )


