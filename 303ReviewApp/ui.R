library(shiny)

f <- "appText.dat"
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
        sidebarLayout(fluid=TRUE,
                      sidebarPanel(
                          h4("Hypothetical Data (Blue Tabs)",
                             style="color: #4d3a7d;"),

                                 selectInput(
                                     inputId= "dataset",
                                     label="Choose Hypothetical Dataset: ",
                                     choices=c("Slight (3 exceedances out of 13)"="data1",
                                               "Vast (3 exceedances out of 13)"="data2",
                                               "2 exceedances out of 13"="data3",
                                               "1 exceedance out of 13"="data4",
                                               "0 exceedances out of 13"="data5",
                                               "0 exceedances out of 23"="data6",
                                               "1 exceedance out of 37"="data7",
                                               "2 exceedances out of 52"="data8"),
                                     selected="data1"),
                          hr(), h4("CharMeck Metals Data")

                      ),
                      mainPanel(
                          tabsetPanel(type="tabs",
                                      tabPanel(title=p("Timeseries", style="color: #4d3a7d;"),
                                               br(),
                                               h3("Time Series Plot of Selected Data"),
                                               t[2], plotOutput("tsPlot")),
                                      tabPanel(title=p("Histogram",style="color: #4d3a7d;"),
                                               br(), t[3],
                                               plotOutput("tsHist", width="100%")),
                                      tabPanel(title=p("Listing", style="color: #4d3a7d;"),
                                               br(), t[4],
                                               plotOutput("excPlotG")),
                                      tabPanel(title=p("Delisting", style="color: #4d3a7d;"),
                                               br(), t[5], br(), br(), t[6],
                                               plotOutput("excPlotL")),
                                      tabPanel(title=p("Two Tailed Test", style="color: #4d3a7d;"),
                                               br(), t[7], br(), br(), t[8],
                                               plotOutput("excPlotT")),
                                      tabPanel("Background Docs", br(),
                                               a("EPA Public Notice Decision / 303(d) Listing Letter",
                                                 href="https://www.epa.gov/sites/production/files/2016-12/documents/nc2016_303ddecisionpackage20161208_reduced.pdf"),
                                               br(),
                                               a("Lin et al. 2000",
                                                 href="http://infohouse.p2ric.org/ref/41/40276.pdf")
                                               )
                                      )
                      )
                      )
    )
)


