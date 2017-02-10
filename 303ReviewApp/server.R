library(shiny)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

#############################################################################
######################### Hypothetical Data Functions #######################
#############################################################################

        ## Set test data
        data1 <- c(11,6,2,11,3,7,3,4,12,4,3,5,1)
        data2 <- c(9,8,85,8,9,74,9,8,92,7,8,9,8)
        data3 <- c(9,6,2,11,3,7,3,4,12,4,3,5,1)
        data4 <- c(9,6,2,11,3,7,3,4,6,4,3,5,1)
        data5 <- c(9,6,2,4,3,7,3,4,6,4,3,5,1)
        data6 <- c(rep(c(4,3,7,1,5,4),3), 4,3,7,1,5)
        data7 <- c(rep(c(4,3,7,1,5,4),3), 12, rep(c(4,3,7,1,5,4),3))
        data8 <- c(rep(c(4,3,7,1,5,4),3), 12,4,3,7,14, rep(c(4,3,7,1,5,4),5))
        dataList <- list(data1=data1, data2=data2, data3=data3, data4=data4,
                         data5=data5, data6=data6, data7=data7, data8=data8)

        ## Reactive plot one of TS concentrations
        output$tsPlot <- renderPlot({

            ## Data time series- use the pass in input$dataset to select column
            par(xaxs="i", yaxs="i", mar=c(5,8,2,2),
                fig=c(0,.75,0,1))
            dt <- seq.Date(from=as.Date("2015-01-01"), by="month",
                           length.out=length(dataList[[input$dataset]]))
            plot(dt, dataList[[input$dataset]], ylim=c(0,100), cex=1.2,
                 pch=18, xlab=NA, ylab=NA)
            lines(x=dt,y=rep(10,length(dt)), col="red")
            mtext(2, text="Conc. (mg/L)", cex=1.4, font=2,
                  line=3)

            par(xaxs="i", yaxs="i", mar=c(5,4 ,2,2),
                fig=c(0.75,1,0,1), new=TRUE)

            hData <- data.frame(exc=0, notExc=0)
            hData$exc <- hData$exc+table(dataList[[input$dataset]]>10)[2]
            hData$notExc <- hData$notExc+table(dataList[[input$dataset]]>10)[1]

            barplot(c(hData[1,1], hData[1,2]), space=0, axes=FALSE,
                    col="blue",
                    ylim=c(0, length(dataList[[input$dataset]]+2)))
            box()
            axis(1,at=c(0.5, 1.5), labels=c("Yes", "No"))
            axis(2,at=seq(0,55,5))
            mtext(side=1, text="Exceeds Standard?", line=3,font=2, cex=1.25)

        })

        ## Plot fun
        excPlotFun <- function(biTest, alt) {
            plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
                 pch=NA, xlab=NA, ylab=NA, axes=FALSE)
            if (alt=="g"){
                rect(xleft=0, xright=0.1, ybottom=0, ytop=1,
                     col="grey90", border=NA)
            } else if (alt=="l") {
                rect(xleft=0.10, xright=1, ybottom=0, ytop=1,
                     col="grey90", border=NA)
            }
            points(x=biTest$estimate, y=0.5, pch=18, cex=2)
            lines(x=rep(0.10,2), y=c(0,1), col="red",lty=2)
            lines(x=rep(biTest$conf.int[1],2), y=c(0.4, 0.6), lwd=2)
            lines(x=rep(biTest$conf.int[2],2), y=c(0.4, 0.6), lwd=2)
            lines(x=c(biTest$conf.int[1], biTest$conf.int[2]),
                  y=c(0.5, 0.5), lwd=2)
            box()
            axis(1, at=seq(0,1,0.1), labels=seq(0,100,10))
            mtext(side=1, text="Exceedance Percent (%)",
                  line=3, font=2, cex=1.25)
        }

        ## ... Repeat yourself
        output$excPlotG <- renderPlot({
            ## Do the binomial tests (maybe move to above)
            useTestG <- binom.test(x=sum(dataList[[input$dataset]]>=10),
                                  n=length(dataList[[input$dataset]]), p=0.10,
                                  alternative="g", conf.level=0.90)

            ## Plot function for binomial plots
            par(xaxs="i", yaxs="i", fig=c(0,1,0.5,1))

            ## Call it, 2x
            excPlotFun(biTest=useTestG, alt="g")
        })

        ## ... Repeat yourself
        output$excPlotL <- renderPlot({
            ## Do the binomial tests (maybe move to above)
            useTestL <- binom.test(x=sum(dataList[[input$dataset]]>=10),
                                  n=length(dataList[[input$dataset]]), p=0.10,
                                  alternative="l", conf.level=0.90)

            ## Plot function for binomial plots
            par(xaxs="i", yaxs="i", fig=c(0,1,0.5,1))

            ## Call it, 2x
            excPlotFun(biTest=useTestL, alt="l")
        })

        ## ... Repeat yourself
        output$excPlotT <- renderPlot({
            ## Do the binomial tests (maybe move to above)
            useTestT <- binom.test(x=sum(dataList[[input$dataset]]>=10),
                                  n=length(dataList[[input$dataset]]), p=0.10,
                                  alternative="t", conf.level=0.90)

            ## Plot function for binomial plots
            par(xaxs="i", yaxs="i", fig=c(0,1,0.5,1))

            ## Call it, 2x
            excPlotFun(biTest=useTestT, alt="t")
        })

#############################################################################
######################### CharMeck Data Functions ###########################
#############################################################################

        ## Make a blank axis to pass in the 'No Data' case
        ax <- list(
            title = "", zeroline = FALSE,
            showline = FALSE, showticklabels = FALSE,
            showgrid = FALSE
        )

        ## CharMeck TS Plot
        output$cmTsPlot <- renderPlotly({
            tsData <- returnData(site=input$sitecode,
                                 metal=input$analyte,
                                 dates=input$dates)
            if (nrow(tsData)==0) {
                plot_ly() %>%
                    add_trace(
                        x=NULL, y=NULL,
                        type="scatter", mode="markers") %>%
                    layout(
                        xaxis=ax, yaxis=ax,
                        annotations=list(
                            x=0.5, y=0.95, xref="paper", yref="paper",
                            text="No Data Meeting Selection Criteria",
                            showarrow=FALSE, font=list(size=25))
                    )
            } else {
                ## Name data
                x <- tsData$Coldate
                y <- tsData$Result
                grp <- tsData$Site

                yAx <- list(
                    title=tsData$Aunit[1], titlefont=list(size=25),
                    type=ifelse(input$log, "log", "linear")
                )

                plot_ly() %>%
                    ## Plot data series
                    add_trace(
                        x=~x, y=~y, symbol=~grp,
                        type="scatter", mode="markers",
                        symbols='circle', name="Total"
                    ) %>%
                    layout(
                        yaxis=yAx,
                        margin=list(l=90, r=60, b=80, t=150, pad=0),
                        title=input$analyte, titlefont=list(size=25)
                    )
            }
        })

        ## CharMeck Time Diff Plot
        output$tDiffPlot <- renderPlotly({
            tDiffData <- returnDiffData(site=input$sitecode,
                                     metal=input$analyte,
                                        dates=input$dates)
            ## No metals data
            if (nrow(tDiffData[["tData"]])==0) {
                plot_ly() %>%
                    add_trace(x=NULL, y=NULL,
                        type="scatter", mode="markers") %>%
                    layout(xaxis=ax, yaxis=ax,
                           annotations=list(
                               x=0.5, y=0.95, xref="paper", yref="paper",
                               text="No Data Meeting Selection Criteria",
                               showarrow=FALSE, font=list(size=25))
                        )

            ## Total Metals only
            } else if (nrow(tDiffData[["dData"]])==0) {
                yMax <-
                    1.2*max(abs(tDiffData[["tData"]]["Result.x"]-
                    tDiffData[["tData"]]["aStan"]))

                ## Pull x, y and grouping info from returned subset of data
                xT <- tDiffData[["tData"]]["Coldate"]
                yT <- tDiffData[["tData"]]["Result.x"]-
                    tDiffData[["tData"]]["aStan"]

                ## For hover
                grp <- tDiffData[["tData"]]["Qualifier.x"]
                resT <- tDiffData[["tData"]]["Result.x"]

                ## Plot total metals data
                plot_ly() %>%
                    layout(
                       yaxis = list(
                           range = c(-yMax, yMax),
                           tickfont = list(size = 14),
                           title = "Sample Result - Ac., Diss. Std. (ug/L)"
                       ),
                       shapes = list(
                            list(fillcolor = "green",
                                line = list(color = "white"),
                                opacity = 0.3, type = "rect",
                                x0=input$dates[1], x1=input$dates[2],
                                xref = "x", y0 = -yMax, y1 = 0, yref = "y"),
                            list(fillcolor = "red",
                                line = list(color = "white"),
                                opacity = .20, type = "rect",
                                x0=input$dates[1], x1=input$dates[2],
                                xref = "x", y0 = 0, y1 = yMax, yref = "y")
                        ),
                        showlegend=TRUE
                    ) %>%

                    add_trace(
                        x=~xT$Coldate, y=~yT$Result.x,
                        type="scatter", mode="markers",
                        marker = list(size=4), color=I('black'),
                        name="Total      ",
                        hoverinfo="text",
                        text=paste("Qualifier: ", grp$Qualifier.x,
                                   '</br> Metal Conc.: ', resT$Result.x)
                        )

            ## Total and dissolved
            } else if (nrow(tDiffData[["tData"]])>0 &
                       nrow(tDiffData[["tData"]])>0) {
                yMax <-
                    1.2*max(abs(tDiffData[["tData"]]["Result.x"]-
                        tDiffData[["tData"]]["aStan"]),
                        abs(tDiffData[["dData"]]["Result.x"]-
                        tDiffData[["dData"]]["aStan"]))

                ## Pull x, y and grouping info from returned subset of data
                ##   for both total and dissolved metals data
                xT <- tDiffData[["tData"]]["Coldate"]
                yT <- tDiffData[["tData"]]["Result.x"]-
                    tDiffData[["tData"]]["aStan"]
                xD <- tDiffData[["dData"]]["Coldate"]
                yD <- tDiffData[["dData"]]["Result.x"]-
                    tDiffData[["dData"]]["aStan"]

                ## For Hover
                grpT <- tDiffData[["tData"]]["Qualifier.x"]
                grpD <- tDiffData[["dData"]]["Qualifier.x"]
                resT <- tDiffData[["tData"]]["Result.x"]
                resD <- tDiffData[["dData"]]["Result.x"]

                plot_ly() %>%
                    ## Plot data series
                    add_trace(
                        x=~xT$Coldate, y=~yT$Result.x,
                        type="scatter", mode="markers",
                        marker = list(size = 4),
                        color=I('black'), name="Total",
                        hoverinfo="text",
                        text=paste('Qualifier: ', grpT$Qualifier.x,
                                   '</br> Metal Conc.: ', resT$Result.x)
                        ) %>%
                    add_trace(
                        x=~xD$Coldate, y=~yD$Result.x,
                        type="scatter", mode="markers",
                        marker = list(size = 6),
                        color=I('red'), name="Dissolved",
                        text=paste("Qualifier: ", grpD$Qualifier.x,
                                   '</br> Metal Conc.: ', resD$Result.x)
                    ) %>%
                    layout(
                        yaxis=list(range=c(-yMax, yMax)),
                        shapes = list(
                            list(fillcolor = "green",
                                line = list(color = "white"),
                                opacity = 0.3, type = "rect",
                                x0=input$dates[1], x1=input$dates[2],
                                xref = "x", y0 = -yMax, y1 = 0, yref = "y"),
                            list(fillcolor = "red",
                                line = list(color = "white"),
                                opacity = .20, type = "rect",
                                x0=input$dates[1], x1=input$dates[2],
                                xref = "x", y0 = 0, y1 = yMax, yref = "y")
                        )
                    )
            }
        })

        ## Function to plot the 90% confidence (toggleable) fig
        excPlotFun <- function(biTest, alt) {
            plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
                 pch=NA, xlab=NA, ylab=NA, axes=FALSE)
            if (alt=="g"){
                rect(xleft=0, xright=input$cutoff, ybottom=0, ytop=1,
                     col="grey90", border=NA)
            } else if (alt=="l") {
                rect(xleft=input$cutoff, xright=1, ybottom=0, ytop=1,
                     col="grey90", border=NA)
            }
            points(x=biTest$estimate, y=0.5, pch=18, cex=2)
            lines(x=rep(input$cutoff,2), y=c(0,1), col="red",lty=2)
            lines(x=rep(biTest$conf.int[1],2), y=c(0.4, 0.6), lwd=2)
            lines(x=rep(biTest$conf.int[2],2), y=c(0.4, 0.6), lwd=2)
            lines(x=c(biTest$conf.int[1], biTest$conf.int[2]),
                  y=c(0.5, 0.5), lwd=2)
            box()
            axis(1, at=seq(0,1,0.1), labels=seq(0,100,10))
            mtext(side=1, text="Exceedance Percent (%)",
                  line=3, font=2, cex=1.25)
        }

        ## ... Repeat yourself
        output$excPlot <- renderPlot({

            ## Get metal data, hardness, standard at that hardness
            tDiffData <- returnDiffData(site=input$sitecode,
                                     metal=input$analyte,
                                        dates=input$dates)

            binomData <- tDiffData[["aData"]]["Result.x"]-
                    tDiffData[["aData"]]["aStan"]

            useTest <- binom.test(x=sum(binomData[,1]>=0),
                                   n=length(binomData[,1]),
                                   p=input$cutoff,
                                  alternative=input$radio, conf.level=input$conf)

            ## Plot function for binomial plots
            par(xaxs="i", yaxs="i", fig=c(0,1,0.5,1))

            ## Call it, 2x
            excPlotFun(biTest=useTest, alt=input$radio)
        })

        ## Last Plot-- selectize mashup
        output$lastTSPlot <- renderPlotly({
            tsFinalData <- returnData2(site2=input$sitecode2,
                                       metal2=input$analyte2,
                                       dates2=input$dates2,
                                       element2=input$element2,
                                       sj=input$sj)
            if (nrow(tsFinalData)==0) {
                plot_ly() %>%
                    add_trace(
                        x=NULL, y=NULL,
                        type="scatter", mode="markers") %>%
                    layout(
                        xaxis=ax, yaxis=ax,
                        annotations=list(
                            x=0.5, y=0.95, xref="paper", yref="paper",
                            text="No Data Meeting Selection Criteria",
                            showarrow=FALSE, font=list(size=25))
                    )
            } else {
                ## Name data
                x <- tsFinalData$Coldate
                y <- tsFinalData$Result
                grp <- tsFinalData$Site

                yAx <- list(
                    title=tsFinalData$Aunit[1], titlefont=list(size=25),
                    type=ifelse(input$log, "log", "linear")
                )

                plot_ly() %>%
                    ## Plot data series
                    add_trace(
                        x=~x, y=~y, symbol=~grp,
                        type="scatter", mode="markers",
                        symbols='circle', name="Total"
                    ) %>%
                    layout(
                        yaxis=yAx,
                        margin=list(l=90, r=60, b=80, t=150, pad=0),
                        title=input$analyte2, titlefont=list(size=25)
                    )
            }



        })

        output$confText <- renderText({
            tDiffData <- returnDiffData(site=input$sitecode,
                                        metal=input$analyte,
                                        dates=input$dates)

            binomData <- tDiffData[["aData"]]["Result.x"]-
                    tDiffData[["aData"]]["aStan"]

            useTest <- binom.test(x=sum(binomData[,1]>=0),
                                   n=length(binomData[,1]),
                                   p=input$cutoff,
                                  alternative=input$radio, conf.level=input$conf)

            paste0("Confidence interval is ",
                  format(useTest$conf.int[1]*100, digits=3), "% to ",
                  format(useTest$conf.int[2]*100, digits=3), "%.")
        })


    }) ## Done

