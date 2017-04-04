library(shiny)
#source("metalsFun.r")
#source("metalsThresFuns.r")

## Load, format, subset, WQD File
itrcData <- read.csv("toolExData.csv", stringsAsFactors=FALSE,
                     sep=",", header=TRUE, check.names=FALSE)

itrcData <<- itrcData[,c(1,2,5,8,11,12,13,14,15)]

## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="ITRC Eval. Tool",
               selected="Data",
               br(),
               h2("**DRAFT/PROTOTYPE**", align="center"),
               h2("ITRC Stormwater BMP Applicability / Evaulation Tool"),
               fluidRow(
                   h3("Select Criteria", align="center"),
                   wellPanel(style="background-color: #D2DDE4",

                       fluidRow(
                           column(12,
                                  selectInput(
                                      inputId= "sitecode1",
                                      label="Select Pollutant(s)*: ",
                                      choices=names(itrcData[3:11]),
                                      multiple=TRUE,
                                      selectize=TRUE)
                                  ),
                           h5("*Not functional at present. Need to discuss binary vs categorical ratings.")
                                  )
                  )
               ),
               fluidRow(
                   wellPanel(style="background-color: #D2DDE4;",
                       fluidRow(
                           includeCSS("styles.css"),
                           h4("Does data need to meet these conditions? (Select...)"),
                           h5("(*If none selected, all practices will be shown)"),
                           column(4,
                              checkboxInput(inputId="TPL",
                                            label="3rd Party Lab?",
                                            value=FALSE)
                              ),
                           column(4,
                              checkboxInput(inputId="TPF",
                                            label="3rd Party Field",
                                            value=FALSE)
                              ),
                           column(4,
                                  checkboxInput(inputId="IDB",
                                            label="Int. DB? ",
                                            value=FALSE)
                                  )
                       ),
                       fluidRow(
                           column(4,
                              checkboxInput(inputId="refJ",
                                            label="Ref. Journ?",
                                            value=FALSE)
                              ),
                       column(4,
                              checkboxInput(inputId="Vend",
                                            label="Vendor Data?",
                                            value=FALSE)
                              )
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


