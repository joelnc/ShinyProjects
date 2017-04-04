library(shiny)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

#############################################################################
######################### CharMeck Data Functions ###########################
#############################################################################
        output$tssPlot <- renderPlot({
            tssIndx <- which(wqDF$Analyte==input$analyte &
                             wqDF$Site==input$sitecode &
                             wqDF$Date > input$dates[1] &
                             wqDF$Date < input$dates[2])
            tssData <- wqDF[tssIndx, c("Coldate", "Result")]
##browser()
            plot(tssData$Coldate, tssData$Result, type="l",
                 col="blue")
        })

        output$selHist <- renderPlot({
            histIndx <- which(wqDF$Analyte==input$analyte)
            histData <- wqDF[histIndx, c("Coldate", "Result")]

            hist(histData$Result, col="grey",
                 breaks=input$nBins)
        })



    }) ## Done

