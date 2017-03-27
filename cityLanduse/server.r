##load("c:/Users/95218/Documents/R/ShinyProjects/cityLanduse/luTable.Rdata")
load("luTable.Rdata")
load("parcelData.Rdata")
urlList <- read.csv("maps.csv", header=FALSE,
                             sep=",", stringsAsFactors=FALSE)
function(input, output) {
    output$frame <- renderUI({
        useUrl <- urlList[which(urlList$V1==input$wshed2),2]
        myTest <- withTags({
            #link(rel = "stylesheet", type="text/css", href="arc.css")
            div(class="embed-container")
            iframe(width="500", height="400", frameborder="0", scrolling="no",
                   marginheight="0", marginwidth="0", title="provPrepTest",
                   src=useUrl)
            })
        print(myTest)
        myTest
    })


    ## output$frame <- renderUI({
    ##     useUrl <- urlList[which(urlList$V1==input$wshed2),2]
    ##     myTest <- withTags({
    ##         link(rel = "stylesheet", type="text/css", href="arc.css")
    ##         div(class="embed-container")
    ##         iframe(width="500", height="400", frameborder="0", scrolling="no",
    ##                marginheight="0", marginwidth="0", title="provPrepTest",
    ##                src="//charlotte.maps.arcgis.com/apps/Embed/index.html?webmap=19da1da27f8a4a6ea508bdd9b10e44a4&amp;extent=-80.7462,35.1868,-80.6211,35.3114&amp;zoom=true&amp;scale=true&amp;disable_scroll=true&amp;theme=light")
    ##         })
    ##     print(myTest)
    ##     myTest
    ## })


    output$wshedPlot <- renderPlot({
        par(mfrow=c(1,2), mai=c(4.5, 1, 0.1, 0.5),
            xaxs="i", yaxs="i", font.lab=2)
        if (input$checkbox==TRUE) {
            thing1 <- which(names(luTable)==input$wshed1)
            bp <- barplot(luTable[-7,thing1], ylab="Number of Parcels")
            ##box()
            axis(1, at=bp, labels=rownames(luTable[-7,]), las=2)

            thing2 <- which(names(luTable)==input$wshed2)
            barplot(luTable[-7,thing2], ylab="Number of Parcels")
            ##box()
            axis(1, at=bp, labels=rownames(luTable[-7,]), las=2)
        } else {
            thing1 <- which(names(luTable)==input$wshed1)
            bp <- barplot(luTable[,thing1], ylab="Number of Parcels")
            ##box()
            axis(1, at=bp, labels=rownames(luTable), las=2)

            thing2 <- which(names(luTable)==input$wshed2)
            barplot(luTable[,thing2], ylab="Number of Parcels")
            ##box()
            axis(1, at=bp, labels=rownames(luTable), las=2)
        }

    })

    output$pie1 <- renderPlot({
        par(mai=c(0.0, 1 , 0.0, 0.5),
            oma=rep(0,4))
        if (input$checkbox==FALSE) {
            uDP1 <- parcelData[[input$wshed1]]
        } else {
            uDP1 <- parcelData[[input$wshed1]][which(parcelData[[input$wshed1]][,"lu"]!="Single Family - Detached"),]
        }

        pie(x=c((sum(uDP1$area)-sum(uDP1$ic))/sum(uDP1$area),
                (sum(uDP1$ic)-sum(uDP1$buildings))/sum(uDP1$area),
                sum(uDP1$buildings)/sum(uDP1$area),
                sum(uDP1$area[which(uDP1$lu=="Water")])/sum(uDP1$area)),
            labels=c("Pervious", "Non-Building IC", "Building IC", "Water"),
            col=c("olivedrab3", "grey60", "grey80", "blue"))
    })

    output$pie2 <- renderPlot({
        par(mai=c(0, 0.5 , 0, 1),
            oma=rep(0,4))
        if (input$checkbox==FALSE) {
            uDP2 <- parcelData[[input$wshed2]]
        } else {
            uDP2 <- parcelData[[input$wshed2]][which(parcelData[[input$wshed2]][,"lu"]!="Single Family - Detached"),]
        }

        pie(x=c((sum(uDP2$area)-sum(uDP2$ic))/sum(uDP2$area),
                (sum(uDP2$ic)-sum(uDP2$buildings))/sum(uDP2$area),
                sum(uDP2$buildings)/sum(uDP2$area),
                sum(uDP2$area[which(uDP2$lu=="Water")])/sum(uDP2$area)),
            labels=c("Pervious", "Non-Building IC", "Building IC", "Water"),
            col=c("olivedrab3", "grey60", "grey90", "blue"))

    })


    ## output$pie <- renderPlot({
    ##     par(mfrow=c(1,2))
    ##     if (input$checkbox==FALSE) {
    ##         uDP1 <- parcelData[[input$wshed1]]
    ##         uDP2 <- parcelData[[input$wshed2]]
    ##     } else {
    ##         uDP1 <- parcelData[[input$wshed1]][which(parcelData[[input$wshed1]][,"lu"]!="Single Family - Detached"),]
    ##         uDP2 <- parcelData[[input$wshed2]][which(parcelData[[input$wshed2]][,"lu"]!="Single Family - Detached"),]
    ##     }

    ##     pie(x=c((sum(uDP1$area)-sum(uDP1$ic))/sum(uDP1$area),
    ##             (sum(uDP1$ic)-sum(uDP1$buildings))/sum(uDP1$area),
    ##             sum(uDP1$buildings)/sum(uDP1$area),
    ##             sum(uDP1$area[which(uDP1$lu=="Water")])/sum(uDP1$area)),
    ##         labels=c("Pervious", "Non-Building IC", "Building IC", "Water"),
    ##         col=c("olivedrab3", "grey60", "grey80", "blue"))

    ##     pie(x=c((sum(uDP2$area)-sum(uDP2$ic))/sum(uDP2$area),
    ##             (sum(uDP2$ic)-sum(uDP2$buildings))/sum(uDP2$area),
    ##             sum(uDP2$buildings)/sum(uDP2$area),
    ##             sum(uDP2$area[which(uDP2$lu=="Water")])/sum(uDP2$area)),
    ##         labels=c("Pervious", "Non-Building IC", "Building IC", "Water"),
    ##         col=c("olivedrab3", "grey60", "grey90", "blue"))

    ## })

    output$icPlot1 <- renderPlot({
        areaLim <- c(0,43560*45)
        par(mfrow=c(1,2), xaxs="i", yaxs="i")
        if (input$checkbox==TRUE) {
            useDataA1 <- parcelData[[input$wshed1]][which(parcelData[[input$wshed1]][,"lu"]!="Single Family - Detached"),]
            useDataA2 <- parcelData[[input$wshed2]][which(parcelData[[input$wshed2]][,"lu"]!="Single Family - Detached"),]
        }
        else {
            useDataA1 <- parcelData[[input$wshed1]]
            useDataA2 <- parcelData[[input$wshed2]]
        }

        hist(useDataA1[,"area"],
             col="grey80", xlim=areaLim, xlab="", main="",
             ylab="Number of Parcels")
        hist(useDataA2[,"area"],
             col="grey80", xlim=areaLim, xlab="", main="",
        ylab="Number of Parcels")
    })

    output$icPlot2 <- renderPlot({
        par(mfrow=c(1,2), xaxs="i", yaxs="i",
            oma=c(0, 0, 1.5, 0), font.lab=2)
        ##browser()

        if (input$checkbox==TRUE) {
            useDataB1 <- parcelData[[input$wshed1]][which(parcelData[[input$wshed1]][,"lu"]!="Single Family - Detached"),]
            useDataB2 <- parcelData[[input$wshed2]][which(parcelData[[input$wshed2]][,"lu"]!="Single Family - Detached"),]
        }
        else {
            useDataB1 <- parcelData[[input$wshed1]]
            useDataB2 <- parcelData[[input$wshed2]]
        }

        hist(useDataB1[,"ic"]/useDataB1[,"area"],
             col="grey", xlim=c(0,1), xlab="", main="",
             ylab="Number of Parcels")
        mtext(side=1,"Watershed 1", line=2.5,
              font=2, cex=1.5)
        mtext(side=1,"Parcel Impervious Fraction", line=4,
              font=2, cex=1.5)
        hist(useDataB2[,"ic"]/useDataB2[,"area"],
             col="grey", xlim=c(0,1), xlab="", main="",
             ylab="Number of Parcels")
        mtext(side=1,"Watershed 2", line=2.5,
              font=2, cex=1.5)
        mtext(side=1,"Parcel Impervious Fraction", line=4,
              font=2, cex=1.5)
        title(cex.main=2, font=2, line=0, outer=TRUE,
              main="Distributions of Impervious Fractions Across")
        title(cex.main=2, font=2, line=-1.80, outer=TRUE,
              main="All Parcels in Selected Watersheds")

    })

    output$icPlot3 <- renderPlot({
        par(mfrow=c(1,2), xaxs="i", yaxs="i",
            oma=c(0, 0, 1.5, 0), font.lab=2)
        if (input$checkbox==TRUE) {
            useDataC1 <- parcelData[[input$wshed1]][which(parcelData[[input$wshed1]][,"lu"]!="Single Family - Detached"),]
            useDataC2 <- parcelData[[input$wshed2]][which(parcelData[[input$wshed2]][,"lu"]!="Single Family - Detached"),]
        }
        else {
            useDataC1 <- parcelData[[input$wshed1]]
            useDataC2 <- parcelData[[input$wshed2]]
        }

        hist(useDataC1[,"buildings"]/useDataC1[,"ic"],
             col="grey50", xlim=c(0,1), xlab="", main="",
             ylab="Number of Parcels")
        mtext(side=1, text="Parcel Buildings / Parcel",
              line=2.5, font=2, cex=1.5)
        mtext(side=1, text="Total Impervious",
              line=4, font=2, cex=1.5)
        hist(useDataC2[,"buildings"]/useDataC2[,"ic"],
             col="grey50", xlim=c(0,1), xlab="", main="",
             ylab="Number of Parcels")
        mtext(side=1, text="Parcel Buildings / Parcel",
              line=2.5, font=2, cex=1.5)
        mtext(side=1, text="Total Impervious",
              line=4, font=2, cex=1.5)
        title(cex.main=2, font=2, line=0, outer=TRUE,
              main="Area of Building Rooftops Relative to Total Impervious")
        title(cex.main=2, font=2, line=-1.8, outer=TRUE,
              main="Across All Parcels in Selected Watersheds")

    })
}
