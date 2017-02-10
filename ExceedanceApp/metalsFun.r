## <- function(site, metal, dates1, dates2){
returnData <- function(site, metal, dates){
    ## Create a list with a dissovled and a total metals
    data <- wqDF[which(wqDF$Site %in% site &
                       wqDF$Analyte==metal &
                       wqDF$Element=="ICS1.1" &
                       wqDF$Date>=dates[1] &
                       wqDF$Date<=dates[2]), ]
    return(data)
}

## <- function(site, metal, dates1, dates2){
returnData2 <- function(site2, metal2, dates2, element2, sj){
    ## Create a list with a dissovled and a total metals
    if (sj!=TRUE) {
        data2 <- wqDF[which(wqDF$Site %in% site2 &
                       wqDF$Analyte==metal2 &
                       wqDF$Element %in% element2 &
                       wqDF$Date>=dates2[1] &
                       wqDF$Date<=dates2[2]), ]
    } else {
        data2 <- wqDF[which(wqDF$Site %in% site2 &
                       wqDF$Analyte==metal2 &
                       wqDF$Element %in% element2 &
                       wqDF$Date>=dates2[1] &
                       wqDF$Date<=dates2[2] &
                            wqDF$CollectedBy=="Steve Jadlocki"), ]
    }

    return(data2)
}


returnDiffData <- function(site, metal, dates) {
    ## Return a list with a dissolved and a total df

    ## Subset data
    data <- wqDF[which(wqDF$Site %in% site &
                       wqDF$Analyte==metal &
                       wqDF$Element=="ICS1.1"&
                       wqDF$Date>=dates[1] &
                       wqDF$Date<=dates[2]), ]
    hd <- wqDF[which(wqDF$Site %in% site &
                       wqDF$Analyte=="Hardness" &
                       wqDF$Element=="ICS1.1" &
                       wqDF$Date>=dates[1] &
                       wqDF$Date<=dates[2]), ]


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

    return(list(tData=metalHd[FIMT, ], dData=metalHd[FIMD, ],
                aData=metalHd[c(FIMT,FIMD), ]))
}

returnSiteName <- function(siteAbr) {
    load("sitesLookup.Rdata")
    if (length(sitesLookup$name[which(sitesLookup$code==siteAbr)])<1) {
        return("Site Not on File")
    } else {
        return(sitesLookup$name[which(sitesLookup$code==siteAbr)])
    }
}

## To have drop down menus include the descriptive name and site code...
returnSiteGlob <- function(siteCodes) {
    ## load the lookup DF
    load("sitesLookup.Rdata")
    ## Function to handle single sitecode/long name combo
    nameDrop <- function(siteCode) {
        tNames <- paste0(
            sitesLookup$name[which(sitesLookup$code==siteCode)],
            " (", siteCode,")")
        return(tNames)
    }
    ## Apply function, wrangle into a sorted named char for
    ## direct input to selectInput(choices=  )
    makeNames <- sapply(siteCodes, nameDrop)
    shouldWork <- setNames(siteCodes, makeNames)
    shouldWorkNow <- unlist(shouldWork)
    return(shouldWorkNow[order(names(shouldWorkNow))])
}
