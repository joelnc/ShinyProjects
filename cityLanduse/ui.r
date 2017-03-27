load("luTable.Rdata")
load("parcelData.Rdata")
list2env(parcelData, .GlobalEnv)

fluidPage(
    h1("Compare Land Cover and Impervious Attributes"),
    h5("* Note: All analysis using parcel based data, which excludes roadways."),
    verticalLayout(
        fluidRow(
            column(6,
                   wellPanel(
                       selectInput("wshed1", "Watershed 1",
                                   choices=names(parcelData))
                   )
                   ),
            column(6,
                   wellPanel(
                       selectInput("wshed2", "Watershed 2",
                                   choices=names(parcelData),
                                   selected="Irwin")
                   )
                   )
        ),
        checkboxInput("checkbox", label = "Exclude Single Family Detached?", value = FALSE),
        tabsetPanel(type="tabs",
                    tabPanel("Area",
                             br(),
                             h3("Distribution of Surface Types Across All Parcels",
                                           align="center"),
                             fluidRow(
                                 column(6,
                                        plotOutput("pie1")
                                        ),
                                 column(6,
                                        plotOutput("pie2")
                                        )
                             ),
                             hr(),
                             fluidRow(
                                 column(12,
                                        h3('"Existing Land Use" Distribution', align="center"),
                                        plotOutput("wshedPlot", height="550px")
                                 )
                             )
                             ),
                    tabPanel("IC",
                             fluidRow(
                                 ##h3("Distribtuions of Impervious Fractions Across", align="center"),
                                 ##h3("All Parcels in Selected Watersheds", align="center"),
                                 br(),
                                 plotOutput("icPlot2"),
                                 hr()
                             ),
                             fluidRow(
                                 br(),
                                 plotOutput("icPlot3"),
                                 hr()
                             )
                             ),
                    tabPanel("Map(s)",
                             h4("*Currently, maps for Irwin and ULSugar only, via the Watershed 2 dropdown"),
                             ##includeCSS("C:/Users/95218/Documents/R/ShinyProjects/cityLanduse/www/arc.css"),
                             htmlOutput("frame")
                             )
                    )
    )
)

