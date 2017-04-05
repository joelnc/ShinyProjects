library(shiny)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

        output$tsPlotly <- renderPlotly({
            useSites <- unique(sites$Site[which(sites$Alias==input$Alias)])

            tsIndx <- which(scmDF$Analyte==input$analyte &
                             scmDF$Site==useSites[1] &
                             scmDF$Date > input$dates[1] &
                             scmDF$Date < input$dates[2])

            tsData <- scmDF[tsIndx, c("Coldate", "Result")]
            yMax <- max(scmDF$Result[which(scmDF$Site %in% useSites &
                                           scmDF$Analyte==input$analyte)])

##            browser()
            tsP <- plot_ly(data=tsData, x=~Coldate, y=~Result,
                           type="scatter", mode="markers",
                           name=useSites[1])

            if (length(useSites) > 1) {
                for (i in 2:length(useSites)) {
                    tsIndx <- which(scmDF$Analyte==input$analyte &
                             scmDF$Site==useSites[i] &
                             scmDF$Date > input$dates[1] &
                             scmDF$Date < input$dates[2])
                    tsData <- scmDF[tsIndx, c("Coldate", "Result")]
                    tsP <- add_trace(tsP, data=tsData, x=~Coldate, y=~Result,
                           type="scatter", mode="markers", name=useSites[i])
                    }
            }
            tsP
        })


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
                        choices=setNames(as.character(
                            sites$Site[which(sites$Alias==input$Alias)]),
                            sites$Description[which(sites$Alias==input$Alias)]),
                        multiple=FALSE,
                        selectize=TRUE)
        })

        output$selectSite2 <- renderUI({
            selectInput(inputId="sitecode2",
                        label="Select Site 2: ",
                        choices=setNames(as.character(
                            sites$Site[which(sites$Alias==input$Alias)]),
                            sites$Description[which(sites$Alias==input$Alias)]),
                        ## choices=sites$Site[
                        ##     which(sites$Alias==input$Alias)],
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
            browser
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

        output$analPlots <- renderPlot({
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


            bSet <- data.frame(result=c(mSet$Result.x, mSet$Result.y),
                               site=c(mSet$Site.x, mSet$Site.y))
##browser()
            par(xaxs="i", yaxs="i", mai=c(1.1, 1.1, .7, .25), omi=c(0,0,0,0),
                font=2, cex.axis=1.5, family="serif")

            boxplot(result~site, data=bSet, ylim=c(0, max(bSet$result)*1.25),
                    axes=TRUE, lwd=2, pch=8, horizontal=TRUE, xlab=FALSE)
            rect(xleft=par("usr")[1], ybottom=0, xright=par("usr")[2],
                 ytop=0.05,col="white")
            lines(x=par("usr")[1:2],y=c(0.05,0.05),col="blue")
            box()

            ## Axes
            axis(2, at=c(1,2), labels=c(input$sitecode1,input$sitecode2),
                 font=2,las=2)
            axis(1,at=seq(0,2,0.5),
                 labels=seq(0,2,0.5), las=1, font=2)
            axis(1,at=seq(0,2,0.1), labels=FALSE, tck=-0.02)

            ## Labels
            ## mtext(paste("Concentration ","(",data$units[1],")",sep=""), line=3,
            ##       side=1, font=2, cex=1.8)
            mtext(input$analyte, side=3, line=1, font=2, cex=2)
            ## mtext(text="DL = 0.05", side=4, line=0.15, at=0.05, las=2,
            ##       col="blue", cex=1.2)


        })



    }) ## Done

