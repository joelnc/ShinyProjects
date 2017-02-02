library(shiny)

server <- function(input, output) {
    output$plot1 <- renderPlot({
        plot(llData$dt, llData$cfs)
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
        rbind(starts, input$plot_brush$xmin)
        rbind(ends, input$plot_brush$xmax)
        print(starts)
    })

}
