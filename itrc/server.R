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
        flags1 <- itrcData[1,2:4]
        flags1[flags1!=FALSE] <- FALSE

        flags2 <- data.frame(ThirdPartyLab=FALSE, ThirdPartyField=FALSE, IDB=FALSE,
                         RefJournal=FALSE, VendorData=FALSE)

        ## Pollutant subset
        dataSubset1 <- reactive({
##browser()
            ## If nothing selected, RETURN ALL
            if (is.null(input$pollutants)) {
                a0 <- itrcData
            } else {

                ## If just one box is checked, simple subset
                if (length(input$pollutants)==1) {
                    a0 <- itrcData[which(itrcData[ ,input$pollutants]!='X'), ]
                }

                else {
                    temp0 <- NULL ## init. container
                    print(input$pollutants) ## diagn. output
                    for (i0 in 1:length(input$pollutants)) { ## loop over cols. to filter
                        for (j0 in 1:nrow(itrcData)) { ## loop over rows in given col.
                            ## If given row is a TRUE, record its row index
                            if (itrcData[j0, input$pollutants[i0]]!='X') {
                                temp0 <- c(temp0, j0)
                            }
                        }
                    }
                    print(temp0)
                    print(temp0[tuplicated(temp0, n=length(input$pollutants))])
                    ## Return subset of practices where all checkbox values = TRUE
                    a0 <- itrcData[temp0[tuplicated(temp0, n=length(input$pollutants))], ]

                }

            }
        })


        ## Data subset function
        dataSubset2 <- reactive({

            ## If nothing selected, RETURN ALL
            if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
                      input$Vend)==FALSE)) {
                ##a1 <- itrcData
                a1 <- dataSubset1()

            ## Else, do a conditional subset, display results
            } else {

                ## (Re-)Set flag values whenever inputs change
                if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
                else  flags2$ThirdPartyLab <- FALSE

                if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
                else flags2$ThirdPartyField <- FALSE

                if (input$IDB==TRUE) flags2$IDB <- TRUE
                else flags2$IDB <- FALSE

                if (input$refJ==TRUE) flags2$RefJournal <- TRUE
                else flags2$RefJournal <- FALSE

                if (input$Vend==TRUE) flags2$VendorData <- TRUE
                else flags2$VendorData <- FALSE

                ## Extract colnames for conditional TRUE
                showThese <- colnames(flags2[which(flags2==TRUE)])

                ## If just one box is checked, simple subset
                if (length(showThese)==1) {
                    a1 <- itrcData[which(itrcData[ ,showThese]==TRUE), ]

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
                    a1 <- itrcData[temp[tuplicated(temp, n=length(showThese))], ]
                }
            }
            })

        ## Display the subset
        output$results2 <- DT::renderDataTable(
                                   DT::datatable(
                                           dataSubset2(),
                                           options(list(pageLength = 25)), escape =FALSE
                                       )
                               )


    }) ## Done

