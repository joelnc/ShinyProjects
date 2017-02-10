library(shiny)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

#############################################################################
######################### CharMeck Data Functions ###########################
#############################################################################

        ## Make a blank axis to pass in the 'No Data' case
        ax <- list(
            title = "", zeroline = FALSE,
            showline = FALSE, showticklabels = FALSE,
            showgrid = FALSE
        )

        ## CharMeck Exc Plot
        output$exPlotQuick <- renderPlotly({
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
                concs <<- tsData$Result[order(tsData$Result)]
                ranks <- seq(1,length(concs))
                exc <- ranks/length(concs)
                grp1 <- as.POSIXlt(tsData$Coldate[order(order(tsData$Result))])$year
                grpYrs <- vector(mode="character", length=length(concs))
                grpYrs[which(grp1<105)] <- "2004 and earlier"
                grpYrs[which(grp1>104 & grp1<107)] <- "2005-2006"
                grpYrs[which(grp1>106 & grp1<109)] <- "2007-2008"
                grpYrs[which(grp1>108 & grp1<111)] <- "2009-2010"
                grpYrs[which(grp1>110 & grp1<113)] <- "2011-2012"
                grpYrs[which(grp1>112 & grp1<115)] <- "2013-2014"
                grpYrs[which(grp1>114 & grp1<117)] <- "2015-2016"
                grpYrs[which(grp1>116 & grp1<119)] <- "2017-2018"

                yAx <- list(
                    title=tsData$Aunit[1], titlefont=list(size=25),
                    type=ifelse(input$log, "log", "linear")
                )

                plot_ly() %>%
                    ## Plot data series
                    add_trace(
                        x=~concs, y=~exc, ##symbol=~grp,
                        type="scatter", mode="lines",
                        name="Percentiles",
                        line=list(color="black", width=0.5)
                    ) %>%
                    add_trace(
                        x=~concs, y=~exc, ##symbol=~grp,
                        type="scatter", mode="markers",
                        symbol=~grpYrs,
                        name="Percentiles",
                        showlegend=TRUE
                    ) %>%

                    add_trace(
                        x=rep(input$Benchmark,2), y=c(0,1),
                        type="scatter", mode="lines",
                        name="Benchmark Conc."
                    ) %>%
                    add_trace(
                        x=c(0, max(concs)),
                        y=rep(0.9, 2),
                        type="scatter", mode="lines",
                        name="10% Exceedance Limit"
                    ) %>%
                    layout(
                        yaxis=yAx,
                        margin=list(l=90, r=60, b=80, t=150, pad=0),
                        title=input$analyte, titlefont=list(size=25)
                    )
            }
        })

        output$summaryStuff <- renderTable({
            tsDataAgain <- returnData(site=input$sitecode,
                                 metal=input$analyte,
                                 dates=input$dates)
            summary(tsDataAgain[,c("Coldate", "Result")])
        })

        ##
 ##        observe({
##             tsDataSlid <- returnData(site=input$sitecode,
##                                  metal=input$analyte,
##                                  dates=input$dates)
## browser()
##             sMin <- min(tsDataSlid$Result)
##             sMax <- max(tsDataSlid$Result)
##             updateSliderInput("session",
##                               "Benchmark",
##                               min=~sMin, max=~sMax,
##                               step=((sMax-sMin)/50)
##                               )
##         })
    }) ## Done

