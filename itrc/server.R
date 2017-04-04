library(shiny)
library(tuple)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

#########################################################################
######################### CharMeck Data Functions #######################
#########################################################################

        ## 'Flag' df, holds T/F values indicating which data to show
        ## Initialize as all False
        fl <- data.frame(ThirdPartyLab=FALSE, ThirdPartyField=FALSE, IDB=FALSE,
                         RefJournal=FALSE, VendorData=FALSE)

        ## Data subset function
        dataSubset <- reactive({

            ## If nothing selected, RETURN ALL
            if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
                      input$Vend)==FALSE)) {
                a <- itrcData

            ## Else, do a conditional subset, display results
            } else {

                ## (Re-)Set flag values whenever inputs change
                if (input$TPL==TRUE) fl$ThirdPartyLab <- TRUE
                else  fl$ThirdPartyLab <- FALSE

                if (input$TPF==TRUE)  fl$ThirdPartyField <- TRUE
                else fl$ThirdPartyField <- FALSE

                if (input$IDB==TRUE) fl$IDB <- TRUE
                else fl$IDB <- FALSE

                if (input$refJ==TRUE) fl$RefJournal <- TRUE
                else fl$RefJournal <- FALSE

                if (input$Vend==TRUE) fl$VendorData <- TRUE
                else fl$VendorData <- FALSE

                ## Extract colnames for conditional TRUE
                showThese <- colnames(fl[which(fl==TRUE)])

                ## If just one box is checked, simple subset
                if (length(showThese)==1) {
                    a <- itrcData[which(itrcData[ ,showThese]==TRUE), ]

                ## More than one box, attrocious loop
                } else {
                    temp <- NULL ## init. container
                    print(showThese) ## diagn. output
                    for (i in 1:length(showThese)) { ## loop over cols. to filter
                        for (j in 1:nrow(itrcData)) { ## loop over rows in given col.
                            ## If given row is a TRUE, record its row index
                            if (itrcData[j, showThese[i]]==TRUE) {
                                temp <- c(temp, j)
                            }
                        }
                    }
                    print(temp)
                    print(temp[tuplicated(temp, n=length(showThese))])
                    ## Return subset of practices where all checkbox values = TRUE
                    a <- itrcData[temp[tuplicated(temp, n=length(showThese))], ]
                }
            }
            })

        ## Display the subset
        output$results2 <- DT::renderDataTable(
                                   DT::datatable(
                                           dataSubset(),
                                           options(list(pageLength = 25))
                                       )
                               )


        output$results <- DT::renderDataTable(DT::datatable({
            data <- itrcData

            ##browser()
            if (input$TPL==TRUE) {
                holder[12] <- TRUE
            } else {
                holder[12] <- FALSE
            }

            if (input$TPF==TRUE) {
                holder[13] <- TRUE
            } else {
                holder[13] <- FALSE
            }
            if (input$IDB==TRUE) {
                holder[14] <- TRUE
            } else {
                holder[14] <- FALSE
            }

            if (input$refJ==TRUE) {
                holder[15] <- TRUE
            } else {
                holder[15] <- FALSE
            }
            if (input$Vend==TRUE) {
                holder[16] <- TRUE
            } else {
                holder[16] <- FALSE
            }
##browser()
            ## Just need to get his index working....
            ##data2 <- data[which(holder==TRUE)=="TRUE", ]


            ## if (input$refJ==TRUE) {
            ##     data <- data
            ## } else {
            ##     data <- data[which(data$RefJournal=="T"), ]
            ## }

            data

        })
        )

    }) ## Done

