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
##wqDF$Date <- as.Date(wqDF$Coldate)  ## If needed
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
               wellPanel(
               fluidRow(
                   column(12, h2("Data Selection", align="center"), hr(),
                          fluidRow(
                              column(4, h3("Hypothetical Data Selection"),
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
                                     ),
                              column(8,
                                     fluidRow(
                                         column(12, h3("CharMeck Metals Data", align="center"),
                                                hr(),
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
                                                           ),
                                                    fluidRow(
                                                    column(12, align="center",
                                                           dateRangeInput(inputId="dates",
                                                                          label="Filter Dates: ",
                                                                          start="2010-01-01", end=Sys.Date(),
                                                                          min="1986-11-03", max=Sys.Date(),
                                                                          startview="year", weekstart=0)
                                                           ))
                                                ))
                                         )
                                     )
                          ))
                  )),

               tabPanel("Hypothetical",
                                   tabsetPanel(type="tabs",
                                               tabPanel(title="Time Series",
                                                        h3("Hypothetical Time Series"),
                                                        t[2], plotOutput("tsPlot")),
                                               tabPanel(title="Histogram",
                                                        br(), t[3],
                                                        plotOutput("tsHist", width="100%")),
                                               tabPanel(title="Listing Analysis",
                                                        h3("Hypothetical . Time series Stuff"),
                                                        br(), t[4],
                                                        plotOutput("excPlotG")),
                                               tabPanel(title="Delisting Analysis*",
                                                        br(), t[5], br(), br(), t[6],
                                                        plotOutput("excPlotL")),
                                               tabPanel(title="Two Sided Test",
                                                        br(), t[7], br(), br(), t[8],
                                             plotOutput("excPlotT")),
                                    tabPanel("'One in Three'",
                                             br(), p("text about EPA 1 in 3"))
                                    )
                        ),
               tabPanel("CharMeck Metals",
                        tabsetPanel(type="tabs",
                                    tabPanel(title="TS",
                                             h3("Real Time series Stuff")),
                                    tabPanel(title="RealHist.",
                                             h3("Hyp. hist.."))
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


