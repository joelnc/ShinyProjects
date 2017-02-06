library(shiny)

## Load, format, subset, WQD File
llData <- read.csv("LLI&E.csv",stringsAsFactors=FALSE, sep=",", header=TRUE)
llData$dt <- as.POSIXct(llData$dt)
llData <<- llData

kp <<- data.frame()

keepers <<- data.frame(starts=as.POSIXct(character()),
                       ends=as.POSIXct(character()))
starts <<- as.POSIXct(character())
ends <<- as.POSIXct(character())

## Start UI
ui <- basicPage(
    plotOutput("plot1",
               brush=brushOpts(id="plot_brush",
                               direction="x")
               ),
    verbatimTextOutput("info"),
    hr(),
    actionButton("addData", "Add Event"),

    actionButton("done", "Finished, Save"),
    p(starts),
    p(ends)
)


