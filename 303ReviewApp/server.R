library(shiny)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

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
            par(xaxs="i", yaxs="i", mar=c(5,8,2,5))
            dt <- seq.Date(from=as.Date("2015-01-01"), by="month",
                           length.out=length(dataList[[input$dataset]]))
            plot(dt, dataList[[input$dataset]], ylim=c(0,100), cex=1.2,
                 pch=18, xlab=NA, ylab="Conc. (mg/l)")
            lines(x=dt,y=rep(10,length(dt)), col="red")
        })

        output$tsHist <- renderPlot({
            ## Data time series- use the pass in input$dataset to select column
            par(xaxs="i", yaxs="i", fig=c(0.3, 0.7, 0, 1))
            hData <- c(rep(0,sum(dataList[[input$dataset]]<10)),
                       rep(1, sum(dataList[[input$dataset]]>=10)))
            hist(hData, breaks=2, ylim=c(0,length(dataList[[input$dataset]])),
                 col="blue", axes=FALSE, xlab=NA, main=NA)
            box()
            axis(1,at=c(0.25, .75), labels=c("No", "Yes"))
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

        output$arcFrame <- renderUI({
            ##theme="arc.css"
            my_ifrm <- tags$iframe(src="//charlotte.maps.arcgis.com/apps/Embed/index.html?webmap=19da1da27f8a4a6ea508bdd9b10e44a4&amp;extent=-80.7825,35.1246,-80.5669,35.3738&amp;zoom=true&amp;scale=true&amp;disable_scroll=true&amp;theme=light", height=600, width=500)
            print(my_ifrm)
            my_ifrm
        })

        ## output$arcFrame <- renderUI({
        ##     my_ifrm <- tags$iframe(width="500", height="400", frameborder="0", scrolling="no",
        ##                            marginheight="0", marginwidth="0", title="provPrepTest",
        ##                            src="//charlotte.maps.arcgis.com/apps/Embed/index.html?webmap=19da1da27f8a4a6ea508bdd9b10e44a4&amp;extent=-80.7825,35.1246,-80.5669,35.3738&amp;zoom=true&amp;scale=true&amp;disable_scroll=true&amp;theme=light")
        ##     my_ifrm <- tags$div(class="embed-container")
        ##     ##src="http://arcg.is/2jwKdHm",
        ##     print(my_ifrm)
        ##     my_ifrm
        ## })

        output$plotlyFrame <- renderUI({
            my_plty <- tags$iframe(
                src="https://plot.ly/~jnipper/141.embed",
                height=700, width=700)
            print(my_plty)
            my_plty
        })

        output$radioV <- renderPrint({ input$radio })
        output$range <- renderPrint({ input$slider })
        output$value <- renderPrint({ input$num })
    }) ## Done

## '<style>.embed-container {position: relative; padding-bottom: 80%; height: 0; max-width: 100%;} .embed-container iframe, .embed-container object, .embed-container iframe{position: absolute; top: 0; left: 0; width: 100%; height: 100%;} small{position: absolute; z-index: 40; bottom: 0; margin-bottom: -15px;}</style>
## <div class="embed-container">

## <iframe width="500" height="400" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" title="provPrepTest" src="//charlotte.maps.arcgis.com/apps/Embed/index.html?webmap=19da1da27f8a4a6ea508bdd9b10e44a4&amp;extent=-80.7825,35.1246,-80.5669,35.3738&amp;zoom=true&amp;scale=true&amp;disable_scroll=true&amp;theme=light"></iframe></div>'

## <style>.embed-container {position: relative; padding-bottom: 80%; height: 0; max-width: 100%;} .embed-container iframe, .embed-container object, .embed-container iframe{position: absolute; top: 0; left: 0; width: 100%; height: 100%;} small{position: absolute; z-index: 40; bottom: 0; margin-bottom: -15px;}</style>
##                                                                                                                                                                                                                     <div class="embed-container">
##                                                                                                                                                                                                                     <iframe width="500" height="400" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" title="provPrepTest" src="//charlotte.maps.arcgis.com/apps/Embed/index.html?webmap=19da1da27f8a4a6ea508bdd9b10e44a4&amp;extent=-80.7825,35.1246,-80.5669,35.3738&amp;zoom=true&amp;scale=true&amp;disable_scroll=true&amp;theme=light"></iframe></div>
