library(shiny)

## Load, format, subset, WQD File
## llData <- read.csv("LLI&E.csv", stringsAsFactors=FALSE,
##     sep=",", header=TRUE)
## llData$dt <- as.POSIXct(llData$dt)
## llData <<- llData

## Load format PP eff for testing
ppRain <- read.csv("ppRain.csv", stringsAsFactors=FALSE,
                   sep=",", header=TRUE)
ppRain$dt <- as.POSIXct(ppRain$dt, format="%m/%d/%Y %H:%M")
ppRain <<- ppRain
ppEff <- read.csv("ppseEff.csv", stringsAsFactors=FALSE,
                  sep=",", header=TRUE)
ppEff$dt <- as.POSIXct(ppEff$dt, format="%m/%d/%Y %H:%M")
ppEff <<- ppEff

## kp <<- data.frame()
## keepers <<- data.frame(starts=as.POSIXct(character()),
##                        ends=as.POSIXct(character()))

starts <<- as.POSIXct(character())
ends <<- as.POSIXct(character())

## Start UI
ui <- basicPage(
    plotOutput("plot1",
               brush=brushOpts(id="plot_brush",
                               direction="x")
               ),
    plotOutput("plot2",
               brush=brushOpts(id="plot_brush",
                               direction="x")
               ),


##    plotlyOutput("plotly1",
##                 brush=brushOpts(id="plot_brush2")
    ##              ),
    checkboxInput("log", "Log Y?",
                 value=FALSE),
    verbatimTextOutput("info"),
    hr(),
    actionButton("addData", "Add Event"),

    actionButton("done", "Finished, Save"),
    p(starts),
    p(ends)
)


