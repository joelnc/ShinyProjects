library(shiny)
library(plotly)
source("metalsFun.r")
source("metalsThresFuns.r")

## NEED TO
##    1) Fix the "if no data meeting criteria", in plotly context
##    2) Check for consistent units, flag plots where needed
##    3) Return site name function DONE
##    4) 2004 rect does not always behave as expected
##    xx5) hover text.  Done.
##    6) Make sure works with full WQD Export File
##    7) Add interactive regression/trending
##    8) Maps

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

        ## Make a blank axis to pass in the 'No Data' case
        ax <- list(
            title = "", zeroline = FALSE,
            showline = FALSE, showticklabels = FALSE,
            showgrid = FALSE
        )

        ## Reactive plot one of TS concentrations
        output$tsPlot <- renderPlotly({

            tsData <- returnData(site=input$sitecode,
                             metal=input$analyte)

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
                        ##marker = list(color="black", size = 4),
                        symbols='circle',
                        name="Total") %>%
                    layout(
                        yaxis=yAx,
                        margin=list(l=90, r=60, b=80, t=150, pad=0),
                        title=input$analyte, titlefont=list(size=25)
                    )
            }
        })


        ## TDiff Plot
        output$tDiffPlot <- renderPlotly({
            tDiffData <- returnDiffData(site=input$sitecode,
                                     metal=input$analyte)

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
                grp <- tDiffData[["tData"]]["Site.x"]

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
                                x0 = "2010-01-01", x1 = "2017-02-01",
                                xref = "x", y0 = -yMax, y1 = 0, yref = "y"),
                            list(fillcolor = "red",
                                line = list(color = "white"),
                                opacity = .20, type = "rect",
                                x0 = "2010-01-01", x1 = "2017-02-01",
                                xref = "x", y0 = 0, y1 = yMax, yref = "y")
                        ),
                        showlegend=TRUE
                    ) %>%
                    add_trace(
                        x=~xT$Coldate, y=~yT$Result.x,
                        type="scatter", mode="markers",
                        marker = list(size=4),
                        symbol=~grp$Site,
                        color=I('black'),
                        name="Total      ")

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
                grpT <- tDiffData[["tData"]]["Site.x"]
                grpD <- tDiffData[["dData"]]["Site.x"]

                plot_ly() %>%
                    ## Plot data series
                    add_trace(
                        x=~xT$Coldate, y=~yT$Result.x,
                        symbol=grpT$Site,
                        type="scatter", mode="markers",
                        marker = list(size = 4),
                        color=I('black'),
                        name="Total") %>%
                    add_trace(
                        x=~xD$Coldate, y=~yD$Result.x,
                        symbol=grpD$Site,
                        type="scatter", mode="markers",
                        marker = list(size = 6),
                        color=I('red'),
                        name="Dissolved") %>%
                    layout(
                        yaxis=list(range=c(-yMax, yMax)),
                        shapes = list(
                            list(fillcolor = "green",
                                line = list(color = "white"),
                                opacity = 0.3, type = "rect",
                                x0 = "2010-01-01", x1 = "2017-02-01",
                                xref = "x", y0 = -yMax, y1 = 0, yref = "y"),
                            list(fillcolor = "red",
                                line = list(color = "white"),
                                opacity = .20, type = "rect",
                                x0 = "2010-01-01", x1 = "2017-02-01",
                                xref = "x", y0 = 0, y1 = yMax, yref = "y")
                        )
                    )
            }
        })

    }) ## Done
