## consider bundlign with log plotting pars, a

## sitesLookup <- read.csv("c:/Users/95218/Documents/R/siteNames.csv",
##                         stringsAsFactors=FALSE, sep=",", header=FALSE)
## names(sitesLookup) <- c("code","name")
## save(sitesLookup, file="sitesLookup.Rdata")

returnSiteName <- function(siteAbr) {
    load("c:/Users/95218/Documents/R/sitesLookup.Rdata")
    if (length(sitesLookup$name[which(sitesLookup$code==siteAbr)])<1) {
        return("Site Not on File")
    } else {
        return(sitesLookup$name[which(sitesLookup$code==siteAbr)])
    }
}

