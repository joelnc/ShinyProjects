library(shiny)

server <- function(input, output) {

    vals <- reactiveValues(
        selRows = rep(TRUE, nrow(ppEff))
        )

    output$plot1 <- renderPlotly({ ## make this one plotly

            plot_ly() %>%
                add_trace(data=ppEff,
                          x=~dt, y=~cts,
                          type="scatter", mode="lines")


            ## plot(ppEff$dt, ppEff$cts, log="y",
            ##      type="l")
            ## par(new=TRUE)
            ## plot(ppRain$dt, ppRain$rain15, col="blue",
            ##      type="h", ylim=rev(range(ppRain$rain15,
            ##                               na.rm=TRUE)))
    })

    ## output$plot1 <- renderPlot({ ## make this one plotly

    ##     if (input$log==TRUE) {
    ##         plot(ppEff$dt, ppEff$cts, log="y",
    ##              type="l")
    ##         par(new=TRUE)
    ##         plot(ppRain$dt, ppRain$rain15, col="blue",
    ##              type="h", ylim=rev(range(ppRain$rain15,
    ##                                       na.rm=TRUE)))
    ##     } else {
    ##         plot(ppEff$dt, ppEff$cts, type="l")
    ##         par(new=TRUE)
    ##         plot(ppRain$dt, ppRain$rain15, col="blue",
    ##              type="h", ylim=rev(range(ppRain$rain15,
    ##                                       na.rm=TRUE)))
    ##     }
    ## })


    output$plot2 <- renderPlot({
##browser()
        effData <- ppEff[which(ppEff$dt>min(ppEff$dt) &
                               ppEff$dt<(min(ppEff$dt)+(60*60*24*7))), ]
        ppData <- ppRain[which(ppRain$dt>min(ppEff$dt) &
                               ppRain$dt<(min(ppEff$dt)+(60*60*24*7))), ]

        if (input$log==TRUE) {
            plot(effData$dt, effData$cts, log="y",
                 type="l")
            par(new=TRUE)
            plot(ppData$dt, ppData$rain15, col="blue",
                 type="h", ylim=rev(range(ppData$rain15,
                                          na.rm=TRUE)))
        } else {
            plot(effData$dt, effData$cts, type="l")
            par(new=TRUE)
            plot(ppData$dt, ppData$rain15, col="blue",
                 type="h", ylim=rev(range(ppData$rain15,
                                          na.rm=TRUE)))
        }

    })

    output$info <- renderText({
        xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }
        xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste(as.POSIXct(e$xmin, origin="1970-01-01"),
                  as.POSIXct(e$xmax, origin="1970-01-01"))
        }

        paste0(
            "click: ", xy_str(input$plot_click),
            "dblclick: ", xy_str(input$plot_dblclick),
            "hover: ", xy_str(input$plot_hover),
            "brush: ", xy_range_str(input$plot_brush)
        )
    })

    observeEvent(input$addData, {
        starts <<- rbind(starts, input$plot_brush$xmin)
        ends <<- rbind(ends, input$plot_brush$xmax)
        print(starts)
    })

    observeEvent(input$done, {
        write(starts, "starts.txt")
        write(ends, "ends.txt")


    })


}
