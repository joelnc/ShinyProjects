library(shiny)

f <- "app3Text.dat"
con <- file(f, open="r")
t <- readLines(con)
t <- t[which(t!="")]
close(con)

## Define UI for applicaiton that draws a hist
shinyUI(
    fluidPage(
        ## Application title
        h1("Exceedance Demo", align="center"),
        br(), p(t[1]),
        sidebarLayout(
            selectInput(
                inputId= "dataset",
                label="Choose Dataset: ",
                choices=c("Slight (3 exceedances out of 13)"="data1",
                          "Vast (3 exceedances out of 13)"="data2",
                          "2 exceedances out of 13"="data3",
                          "1 exceedance out of 13"="data4",
                          "0 exceedances out of 13"="data5",
                          "0 exceedances out of 23"="data6",
                          "1 exceedance out of 37"="data7",
                          "2 exceedances out of 52"="data8"),
                selected="data1"),

            mainPanel(
                tabsetPanel(type="tabs",
                            tabPanel("Timeseries", br(),
                                     h3("Time Series Plot of Selected Data"),
                                     t[2], plotOutput("tsPlot")),
                            tabPanel("Histogram", br(), t[3],
                                     plotOutput("tsHist", width="100%")),
                            tabPanel("Listing", br(), t[4],
                                     plotOutput("excPlotG")),
                            tabPanel("Delisting", br(),
                                     t[5], br(), br(), t[6],
                                     plotOutput("excPlotL")),
                            tabPanel("Since You Asked...", br(),
                                     t[7], br(), br(), t[8],
                                     plotOutput("excPlotT")),
                            tabPanel("Background Docs", br(),
                                     a("EPA Public Notice Decision / 303(d) Listing Letter",
                                       href="https://www.epa.gov/sites/production/files/2016-12/documents/nc2016_303ddecisionpackage20161208_reduced.pdf"),
                                     br(),
                                     a("Lin et al. 2000",
                                       href="http://infohouse.p2ric.org/ref/41/40276.pdf")),
                            tabPanel("Arc", br(),
                                     htmlOutput("arcFrame")),
                            tabPanel("Plotly", br(),
                                     htmlOutput("plotlyFrame")),
                            tabPanel("Shiny Stuff", br(),
                                     h3("Other Functionality (basic)"),
                                     fluidRow(
                                         column(4,
                                                radioButtons("radio", label = h3("Radio buttons"),
                                                  choices = list("Choice 1" = 1,
                                                                 "Choice 2" = 2,
                                                                 "Choice 3" = 3),
                                                  selected = 1)
                                                ),
                                         column(4,
                                                sliderInput("slider", label=h3("Slider Range"),
                                                            min=0, max=100, value=c(30,60))
                                                ),
                                         column(4,
                                                numericInput("num", label=h3("Numeric Input"),
                                                            value=7)
                                                )
                                     ),
                                     hr(),
                                     fluidRow(
                                         column(4, verbatimTextOutput("radioV")),
                                         column(4, verbatimTextOutput("range")),
                                         column(4, verbatimTextOutput("value"))
                                     )
                                     )
                            )
            )
        )
    )
)
