library(shiny)
library(plotly)

## NEED TO
##    1) Fix the "if no data meeting criteria", in plotly context
##    2) Check for consistent units, flag plots where needed
##    3) Return site name function
##    4) 2004 rect does not always behave as expected
##    5) hover text.  Done.
##
##    xx5) hover text.  Done.
##    6) Make sure works with full WQD Export File
##    7) Add interactive regression/trending

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

        ## Reactive plot one of TS concentrations
        output$tsPlot <- renderPlotly({

            if (length(which(sedDF$Site==input$sitecode &
                           sedDF$Analyte==input$analyte &
                           sedDF$Element==input$element))==0) {
                plot(x=NA, y=NA, axes=FALSE, xlab=NA, ylab=NA,
                     xlim=c(0,1), ylim=c(0,1))
                text(x=.5, y=.8, labels="No Data Meeting Criteria",
                     font=2, cex=2)
            } else {


                ## Subset data
                index <- which(sedDF$Site==input$sitecode &
                           sedDF$Analyte==input$analyte &
                           sedDF$Element==input$element)

                x <- sedDF$Coldate[index]
                y <- sedDF$Result[index]

                yAx <- list(
                    title=sedDF$Aunit[index][1],
                    titlefont=list(size=25),
                    type=ifelse(input$log, "log", "linear")
                    )

                plot_ly() %>%
                    ## Plot data series
                    add_trace(
                        x=~x, y=~y,
                        type="scatter", mode="markers",
                        hoverinfo="text",
                        text=paste('Storm-ish: ', sedDF$Storm[index],
                            '</br> Date: ', sedDF$Coldate[index])) %>%
                    ## Add line at approx. QAQC date thresh
                    add_trace(
                        x=~rep(as.POSIXct("2004-01-01"),2),
                        y=~c(0.001,1.2*max(sedDF$Result[index])),
                        type="scatter", mode="lines"
                        ) %>%

                    layout(
                        yaxis=yAx,
                        margin=list(l=90, r=60, b=80, t=150, pad=0),
                        title=input$analyte,
                        titlefont=list(size=25),
                        shapes = list( ## This is shaded rect- need to scale
                            type = "rect", fillcolor = "blue",
                            line = list(color = "blue"), opacity = 0.1,
                            x0 = min(sedDF$Coldate[index])-(2*365*60*60*24),
                            x1 = "2004-01-01", xref = "x",
                            y0 = 0.001, y1 = 1.2*max(sedDF$Result[index]),
                            yref = "y")
                    )

            }

                ## ## Add labels
                ## mtext(3, line=1.25, cex=2, font=2,
                ##       text=input$analyte)
                ## mtext(2, line=3.25, cex=1.75, font=2,
                ##       text=sedDF$Aunit[index][1])


                ## ## Log plot vs not
                ## if (input$log==FALSE) {
                ##     plot(x, y, pch=18, ylab=NA)
                ## } else {
                ##     plot(x, y, pch=18, ylab=NA, log="y")
                ## }

                ## ## Add labels
                ## mtext(3, line=1.25, cex=2, font=2,
                ##       text=input$analyte)
                ## mtext(2, line=3.25, cex=1.75, font=2,
                ##       text=sedDF$Aunit[index][1])

        })
    }) ## Done
