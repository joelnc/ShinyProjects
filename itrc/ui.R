library(shiny)
#source("metalsFun.r")
#source("metalsThresFuns.r")

## Load, format, subset, WQD File
itrcDataFull <- read.csv("toolExData.csv", stringsAsFactors=FALSE,
                     sep=",", header=TRUE, check.names=FALSE)
itrcData <<- itrcDataFull[,c(1,2,5,8,11,12,13,14,15,16)]
#itrcData$link <<- paste0("<a href=", itrcData$doc, " target='blank' >MyFiles</a>")
itrcData$Practice <<- paste0("<a href=", itrcData$doc, " target='blank' >",
                         itrcData$Practice,"</a>")
itrcData <<- subset(itrcData, select=-c(doc))

print(names(itrcData))

## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="ITRC Eval. Tool",
               selected="Data",
               br(),
               h2("**DRAFT/PROTOTYPE**", align="center"),
               h2("ITRC Stormwater BMP Applicability / Evaulation Tool"),
               fluidRow(
                   column(6,
                          h3("Select Criteria", align="center"),
                          wellPanel(style="background-color: #D2DDE4",
                                    selectInput(
                                    inputId="pollutants",
                                    label="Select Pollutant(s): ",
                                    choices=names(itrcData[2:4]),
                                    multiple=TRUE,
                                    selectize=TRUE)
                                    )
                          ),
                   column(6,
                          wellPanel(style="background-color: #D2DDE4;",
                                    includeCSS("styles.css"),
                                    h4("Does data need to meet these conditions? (Select...)"),
                                    h5("(*If none selected, all practices will be shown)"),
                                    checkboxInput(inputId="TPL",
                                                  label="3rd Party Lab?",
                                                  value=FALSE),
                                    checkboxInput(inputId="TPF",
                                                  label="3rd Party Field",
                                                  value=FALSE),
                                    checkboxInput(inputId="IDB",
                                                  label="Int. DB? ",
                                                  value=FALSE),
                                    checkboxInput(inputId="refJ",
                                                  label="Ref. Journ?",
                                                  value=FALSE),
                                    checkboxInput(inputId="Vend",
                                                  label="Vendor Data?",
                                                  value=FALSE)
                                    )
                          )
               ),
               tabsetPanel(type="tabs",
                           tabPanel("Pot. Applicable",
                                    DT::dataTableOutput("results2")
                                    )
                           )
               )
)


