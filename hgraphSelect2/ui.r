library(shiny)

## Load format PP eff for testing
## ppRain <- read.csv("ppRain.csv", stringsAsFactors=FALSE,
##                    sep=",", header=TRUE)
## ppRain$dt <- as.POSIXct(ppRain$dt, format="%m/%d/%Y %H:%M")
## ppRain <<- ppRain
## ppEff <- read.csv("ppseEff.csv", stringsAsFactors=FALSE,
##                   sep=",", header=TRUE)
## ppEff$dt <- as.POSIXct(ppEff$dt, format="%m/%d/%Y %H:%M")
## ppEff <<- ppEff

## Bartlett
precipBar <- read.csv(file="bartlettPrecip2011.csv", header=TRUE,
                      sep=",", stringsAsFactors=FALSE)
precipBar$dt <- as.POSIXct(precipBar$dt)
precipBar <<- precipBar

flowBar <-  read.csv(file="USGSBartlett2011.csv", header=TRUE,
                      sep=",", stringsAsFactors=FALSE)
flowBar$date.time <- as.POSIXct(flowBar$date.time, format="%m/%d/%Y %H:%M")
colnames(flowBar) <- c("dt", "ft", "cfs")
flowBar <<- flowBar

starts <<- as.POSIXct(character())
ends <<- as.POSIXct(character())

## Start UI
ui <- fluidPage(
    verticalLayout(
        h1("Visualize Selected Year"),
        plotlyOutput("plot1"),
        br(),
        h1("Interactive Event Selection"),
        plotOutput("plot2",
                   brush=brushOpts(id="plot_brush",
                                   direction="x")
                   ),
        fluidRow(
            column(12,
            wellPanel(
                actionButton("minusSmall", "Minus 0.5 Days"),
                actionButton("minusLarge", "Minus 3 Days"),
                actionButton("plusLarge", "Plus 3 Days"),
                actionButton("plusSmall", "Plus 0.5 Days")
                )
        )),
        checkboxInput("log", "Log Y?",
                      value=FALSE),
        verbatimTextOutput("info"),
        hr(),
        actionButton("addData", "Add Event"),
        actionButton("done", "Finished, Save"),
        p(starts),
        p(ends)
    )
)


