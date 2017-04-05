library(shiny)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

        output$tssPlot <- renderPlot({
            ## pull out alias' site codes
            useSites <- unique(sites$Site[which(sites$Alias==input$Alias)])
            print(useSites)

            tssIndx <- which(scmDF$Analyte==input$analyte &
                             scmDF$Site==useSites[1] &
                             scmDF$Date > input$dates[1] &
                             scmDF$Date < input$dates[2])

            tssData <- scmDF[tssIndx, c("Coldate", "Result")]
            yMax <- max(scmDF$Result[which(scmDF$Site %in% useSites &
                                           scmDF$Analyte==input$analyte)])
            plot(tssData$Coldate, tssData$Result, pch=16,
                 col=1, ylim=c(0,yMax))

            if (length(useSites) > 1) {
                for (i in 2:length(useSites)) {
                    tssIndx <- which(scmDF$Analyte==input$analyte &
                             scmDF$Site==useSites[i] &
                             scmDF$Date > input$dates[1] &
                             scmDF$Date < input$dates[2])
                    tssData <- scmDF[tssIndx, c("Coldate", "Result")]
                    points(tssData$Coldate, tssData$Result, pch=16,
                         col=i)
                }

            }
            legend("topright", useSites, col=seq(1,length(useSites)),
                   pch=16, pt.cex=1.5, bty="n")
        })

        output$selectSite1 <- renderUI({
            selectInput(inputId="sitecode1",
                        label="Select Site 1: ",
                        choices=sites$Site[which(sites$Alias==input$Alias)],
                        multiple=FALSE,
                        selectize=TRUE)
        })

        output$selectSite2 <- renderUI({
            selectInput(inputId="sitecode2",
                        label="Select Site 2: ",
                        choices=sites$Site[which(sites$Alias==input$Alias)],
                        multiple=FALSE,
                        selectize=TRUE)
        })

        output$defScatter <- renderPlot({
            mSet <- merge(scmDF[which(scmDF$Analyte==input$analyte &
                                      scmDF$Site==input$sitecode1 &
                                      scmDF$Date > input$dates[1] &
                                      scmDF$Date < input$dates[2]),
                                c("Date", "Result", "Site")],
                          scmDF[which(scmDF$Analyte==input$analyte &
                                      scmDF$Site==input$sitecode2 &
                                      scmDF$Date > input$dates[1] &
                                      scmDF$Date < input$dates[2]),
                                c("Date", "Result", "Site")],
                          by="Date")
            par(pty="s")
            plot(mSet$Result.x, mSet$Result.y, pch=16,
                 xlim=c(0,max(mSet$Result.x, mSet$Result.y)),
                 ylim=c(0,max(mSet$Result.x, mSet$Result.y)),
                 xlab=mSet$Site.x[1], ylab=mSet$Site.y[1])
        })

        output$selHist <- renderPlot({
            histIndx <- which(scmDF$Analyte==input$analyte)
            histData <- scmDF[histIndx, c("Coldate", "Result")]

            hist(histData$Result, col="grey",
                 breaks=input$nBins)
        })

        ## Display the subset
        output$matchedSamples <- DT::renderDataTable(
            DT::datatable(
                merge(scmDF[which(scmDF$Analyte==input$analyte &
                                  scmDF$Site==input$sitecode1 &
                                  scmDF$Date > input$dates[1] &
                                  scmDF$Date < input$dates[2]),
                            c("Date", "Result", "Site")],
                      scmDF[which(scmDF$Analyte==input$analyte &
                                  scmDF$Site==input$sitecode2 &
                                  scmDF$Date > input$dates[1] &
                                  scmDF$Date < input$dates[2]),
                            c("Date", "Result", "Site")],
                      by="Date"),
                ##dataSubset(),
                options(list(pageLength = 25))
            )
        )


    }) ## Done

