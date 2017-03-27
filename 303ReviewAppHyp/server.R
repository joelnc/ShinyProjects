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
        data7 <- c(rep(c(4,3,7,1,5,4),3), 12, 7,rep(c(4,3,7,1,5,4),3))
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


        ## Function to plot the 90% confidence (toggleable) fig
        excPlotFunTog <- function(biTest, alt) {
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
            axis(1, at=seq(0,1,0.1), labels=seq(0,100,10),
                 cex.axis=1.25)
            mtext(side=1, text="Exceedance Percent (%)",
                  line=3, font=2, cex=1.5)
        }

        ## ... Repeat yourself
        output$excPlotTog <- renderPlot({
##browser()
            ## Get metal data, hardness, standard at that hardness
            useData <- dataList[[input$dataset]]
                ## returnDiffData(site=input$sitecode,
                ##                      metal=input$analyte,
                ##                         dates=input$dates)

            binomData <- useData-10

            useTest <- binom.test(x=sum(binomData>=0),
                                   n=length(binomData),
                                   p=input$cutoff,
                                  alternative=input$radio, conf.level=input$conf)

            ## Plot function for binomial plots
            par(xaxs="i", yaxs="i", fig=c(0,1,0.5,1))

            ## Call it, 2x
            excPlotFunTog(biTest=useTest, alt=input$radio)
        })


    }) ## Done

