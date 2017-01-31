rm(list=ls())

allSites <- c("A","B","C")

df <- data.frame(site=c(rep(allSites,3)), x=c(rep(1,3), rep(2,3), rep(3,3)),
                 y=c(2,3,2,4,5,5,7,4,9))

sites <- as.list(allSites[1:3])



plot_ly(data=df,
        x=~x[which(df$site=="A")], y=~y[which(df$site=="A")],
        type="scatter", mode="markers")



pFun <- function(sites) {
    bbPlot <- plot_ly() %>%
        add_trace(data=df,
                  x=~(x[which(df$site==sites)]),
                  y=~(y[which(df$site==sites)]),
                  type="scatter", mode="markers")
    return(bbPlot)
}

qq <- sapply(X=sites, FUN=pFun)




