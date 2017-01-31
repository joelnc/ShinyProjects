## <- function(site, metal, dates1, dates2){
returnData <- function(site, metal){
    ## Create a list with a dissovled and a total metals
    data <- wqDF[which(wqDF$Site==site &
                           wqDF$Analyte==metal &
                           wqDF$Element=="ICS1.1"), ]
    return(data)

}

returnDiffData <- function(site, metal) {
    ## Return a list with a dissolved and a total df

    ## Subset data
    data <- wqDF[which(wqDF$Site==site &
                       wqDF$Analyte==metal &
                       wqDF$Element=="ICS1.1"), ]
    hd <- wqDF[which(wqDF$Site==site &
                       wqDF$Analyte=="Hardness" &
                       wqDF$Element=="ICS1.1"), ]


    metalHd <- merge(data, hd, by="Coldate", all=TRUE)

    fun <- match.fun(paste0(data$Analyte[1], "Fun"))
    funOut <- lapply(X=as.list(metalHd$Result.x), FUN=fun)
    funOut2 <- do.call("rbind", funOut)

    metalHd$cStan <- unlist(funOut2[,"chron"])
    metalHd$aStan <- unlist(funOut2[,"ac"])
    metalHd$cStanValid <- unlist(funOut2[,"chronV"])
    metalHd$aStanValid <- unlist(funOut2[,"acV"])
    metalHd$cStanTot <- unlist(funOut2[,"chronT"])

    FIMD <- which(metalHd$Type.x=="Grab - Filtered" &
              metalHd$aStanValid!="no" &
              metalHd$Coldate>as.POSIXct("2010-01-01 0:00"))
    FIMT <- which(metalHd$Element.x=="ICS1.1" &
              metalHd$Type.x!="Grab - Filtered" &
              metalHd$aStanValid=="yes" &
              metalHd$Coldate>as.POSIXct("2010-01-01 0:00"))

    return(list(tData=metalHd[FIMT, ], dData=metalHd[FIMD, ]))
    ## plot_ly() %>%
    ##     add_trace(data=metalHd[FIMT],
    ##         x=~Coldate,
    ##         y=~(Result.x-aStan)
    ##         mode= "markers", type = "scatter",
    ##         marker = list(color="black", size = 4),
    ##         name="Total"
    ##         ) %>%
    ##     add_trace(data=metalHd[FIMD],
    ##         x=~Coldate,
    ##         y=~(Result.x-aStan)
    ##         mode= "markers", type = "scatter",
    ##         marker = list(color="red", size = 6),
    ##         name="Dissolved") ##%>%
        ##layout()
}

