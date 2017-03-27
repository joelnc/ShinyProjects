library(shiny)

## This is a magical container that tells other shiny functions to check for
## updated value.  Put subset of data in it, and go....
liveVals <- reactiveValues()

## Initialize temporal indexes
startDT <- max(c(min(precipBar$dt,na.rm=TRUE), min(flowBar$dt, na.rm=TRUE)), na.rm=TRUE)
endDT <- startDT + (60*60*24*3)

## Put initial data subsets in the reactive container, based on intial 7 day index
liveVals$effData <- flowBar[which(flowBar$dt>startDT & flowBar$dt<endDT), ]
liveVals$pData <- precipBar[which(precipBar$dt>startDT & precipBar$dt<endDT), ]


server <- function(input, output) {

    ## Create plotly of the full time series, as reference
    output$plot1 <- renderPlotly({

            plot_ly() %>%
                add_trace(data=flowBar,
                          x=~dt, y=~cfs,
                          type="scatter", mode="lines")
            ## Add rainfall
    })

    ## Weekly plot
    output$plot2 <- renderPlot({
        ## Look at other examples, the log statement can get wrapped into
        ## a single function call.
        if (input$log==TRUE) {
            plot(liveVals$effData$dt, liveVals$effData$cfs, log="y",
                 type="l")
            par(new=TRUE)
            plot(liveVals$pData$dt, liveVals$pData$precip, col="blue",
                 type="h", ylim=rev(range(liveVals$pData$precip,
                                          na.rm=TRUE)))
        } else {
            plot(liveVals$effData$dt, liveVals$effData$cfs, type="l")
            par(new=TRUE)
            plot(liveVals$pData$dt, liveVals$pData$precip, col="blue",
                 type="h", ylim=rev(range(liveVals$pData$precip,
                                          na.rm=TRUE)))
        }
    })

    ## Add 3 Button- if clicked, do this
    observeEvent(input$plusLarge, {
        ## Update indices
        startDT <- max(c(min(liveVals$effData$dt, na.rm=TRUE) + (60*60*24*2.75),
                         ))

        endDT <- min(c(max(liveVals$effData$dt, na.rm=TRUE) + (60*60*24*2.75),
                     max(flowBar$dt, na.rm=TRUE)))

        ## Update data subset to global (possibly unnecessary)
        liveVals$effData <<- flowBar[which(flowBar$dt>startDT & flowBar$dt<endDT), ]
        liveVals$pData <<- precipBar[which(precipBar$dt>startDT & precipBar$dt<endDT), ]
    })

    ## Add 0.5 Button- if clicked, do this
    observeEvent(input$plusSmall, {
        ## Update indices
        startDT <- min(liveVals$effData$dt, na.rm=TRUE) + (60*60*24*0.5)
        endDT <- min((max(liveVals$effData$dt, na.rm=TRUE) + (60*60*24*0.5)),
                     max(flowBar$dt, na.rm=TRUE))

        ## Update data subset to global (possibly unnecessary)
        liveVals$effData <<- flowBar[which(flowBar$dt>startDT & flowBar$dt<endDT), ]
        liveVals$pData <<- precipBar[which(precipBar$dt>startDT & precipBar$dt<endDT), ]
    })

    ## Minus 0.5 Button- if clicked, do this
    observeEvent(input$minusSmall, {
        ## Update indices
        startDT <- min(liveVals$effData$dt, na.rm=TRUE) - (60*60*24*0.5)
        endDT <- max(liveVals$effData$dt, na.rm=TRUE) - (60*60*24*0.5)

        ## Update data subset to global (possibly unnecessary)
        liveVals$effData <<- flowBar[which(flowBar$dt>startDT & flowBar$dt<endDT), ]
        liveVals$pData <<- precipBar[which(precipBar$dt>startDT & precipBar$dt<endDT), ]
    })

    ## Minus 3 Button- if clicked, do this
    observeEvent(input$minusLarge, {
        ## Update indices
        startDT <- min(liveVals$effData$dt, na.rm=TRUE) - (60*60*24*2.75)
        endDT <- max(liveVals$effData$dt, na.rm=TRUE) - (60*60*24*2.75)

        ## Update data subset to global (possibly unnecessary)
        liveVals$effData <<- flowBar[which(flowBar$dt>startDT & flowBar$dt<endDT), ]
        liveVals$pData <<- precipBar[which(precipBar$dt>startDT & precipBar$dt<endDT), ]
    })

    ## Text output box
    output$info <- renderText({
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste(as.POSIXct(e$xmin, origin="1970-01-01"), " to ",
                  as.POSIXct(e$xmax, origin="1970-01-01"))
        }
        paste0("Event Date Range: ",
               xy_range_str(input$plot_brush)
        )
    })

    ## Add Data Button, if clicked, write dates to
    observeEvent(input$addData, {
        starts <<- rbind(starts, input$plot_brush$xmin)
        ends <<- rbind(ends, input$plot_brush$xmax)
        print(starts)
    })

    ## Done button- if clicked write text files
    observeEvent(input$done, {
        write(starts, "starts.txt")
        write(ends, "ends.txt")
    })
}
