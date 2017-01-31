library(shiny)
library(plotly)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

        ## Reactive plot one of TS concentrations
        output$tsPlot <- renderPlot({

            if (length(which(sedDF$Site==input$sitecode &
                           sedDF$Analyte==input$analyte &
                           sedDF$Element==input$element))==0) {
                plot(x=NA, y=NA, axes=FALSE, xlab=NA, ylab=NA,
                     xlim=c(0,1), ylim=c(0,1))
                text(x=.5, y=.8, labels="No Data Meeting Criteria",
                     font=2, cex=2)
            } else {
                ## Data time series
                par(xaxs="i", yaxs="i", mar=c(4,8,3.5,5))

                index <- which(sedDF$Site==input$sitecode &
                               sedDF$Analyte==input$analyte &
                               sedDF$Element==input$element)

                ## Subset data
                x <- sedDF$Coldate[index]
                y <- sedDF$Result[index]


                ## Add labels
                mtext(3, line=1.25, cex=2, font=2,
                      text=input$analyte)
                mtext(2, line=3.25, cex=1.75, font=2,
                      text=sedDF$Aunit[index][1])


                ## Log plot vs not
                if (input$log==FALSE) {
                    plot(x, y, pch=18, ylab=NA)
                } else {
                    plot(x, y, pch=18, ylab=NA, log="y")
                }

                ## Add labels
                mtext(3, line=1.25, cex=2, font=2,
                      text=input$analyte)
                mtext(2, line=3.25, cex=1.75, font=2,
                      text=sedDF$Aunit[index][1])
            }

        })
    }) ## Done
