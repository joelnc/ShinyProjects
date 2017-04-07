library(shiny)

shinyUI(
    fluidPage(
        mainPanel(
            tabsetPanel(
                type="tabs",
                tabPanel("Arc",
                         br(),
                         htmlOutput("arcFrame")
                         )
            )
        )
    )
)


## tags$style(
##                 HTML('<style>.embed-container {position: relative; padding-bottom: 80%; height: 0; max-width: 100%;} .embed-container iframe, .embed-container object, .embed-container iframe{position: absolute; top: 0; left: 0; width: 100%; height: 100%;} small{position: absolute; z-index: 40; bottom: 0; margin-bottom: -15px;}</style><div class="embed-container"><iframe width="500" height="400" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" title="provPrepTest" src="//charlotte.maps.arcgis.com/apps/Embed/index.html?webmap=19da1da27f8a4a6ea508bdd9b10e44a4&amp;extent=-80.7557,35.1872,-80.6,35.3118&amp;zoom=true&amp;scale=true&amp;disable_scroll=true&amp;theme=light"></iframe></div>')
##             ),





## shinyUI(
## fluidPage(
##     mainPanel(
##         tabsetPanel(
##             type="tabs",
##             tabPanel("Arc",
##                      br(),
##                      htmlOutput("arcFrame")
##                      )
##         )
##     )
## )
## )
