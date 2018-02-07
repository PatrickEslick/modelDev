library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Information",
      includeHTML("README.html")
    ),
    tabPanel("Upload/Examine Data",
      fluidRow(
        column(1,
          textInput("dVar", "Response variable:"),
          helpText("The name of the column with the response variable")
        ),       
        column(2, 
          fileInput("mergedFile","Merged Sample Data"),
          selectInput("sdateformat","Date Format:", c(
            "mm/dd/yyyy hh:mm" = "%m/%d/%Y %H:%M",
            "yyyy-mm-dd hh:mm" = "%Y-%m-%d %H:%M",
            "yyyymmdd hhmm" = "%Y%m%d %H%M",
            "mm/dd/yy hh:mm" = "%m/%d/%y %H:%M",
            "yyyymmddhhmm" = "%Y%m%d%H%M",
            "yyyymmddhhmmss" = "%Y%m%d%H%M%S"))
        ),
        column(2,
          fileInput("contFile", "Continuous Data"),
          selectInput("cdateformat","Date Format:", c(
            "mm/dd/yyyy hh:mm" = "%m/%d/%Y %H:%M",
            "yyyy-mm-dd hh:mm" = "%Y-%m-%d %H:%M",
            "yyyymmdd hhmm" = "%Y%m%d %H%M",
            "mm/dd/yy hh:mm" = "%m/%d/%y %H:%M",
            "yyyymmddhhmm" = "%Y%m%d%H%M",
            "yyyymmddhhmmss" = "%Y%m%d%H%M%S"))  
        ),
        column(2,
          helpText("Fill out all fields on this tab and then press below before proceeding"),
          actionButton("loadButton","Upload Data")  
        ),
        column(3,
          tableOutput("periodOfRecord"),
          textOutput("censorWarning")
        )
      ),
      fluidRow(
        column(1, checkboxInput("limitDates", "Limit calibration data to date range"), value=FALSE),
        column(3, 
          dateRangeInput("uploadDateRange", label=""),
          helpText("Data after this range can be used for validation")     
        )
      ),
      tabsetPanel(
        tabPanel("Time series",
          fluidRow(
            column(2, uiOutput("timeSeriesChoice")),
            column(1, helpText("-"), actionButton("tsPlotButton", "Plot")),
            column(1, helpText("-"), checkboxInput("tsLogScale", "Log scale", value=FALSE))
          ),
          fluidRow(
            plotOutput("timeSeries1", brush = brushOpts(id = "tsBrush", direction = 'x', resetOnNew = TRUE))
          ),
          fluidRow(
            plotOutput("timeSeries2")
          )
        ),
        tabPanel("Duration curves",
          fluidRow(
            column(2,uiOutput("durationChoice")),
            column(1,helpText("-"), actionButton("plotButton", "Plot"))
          ),
          fluidRow(
            plotOutput("wholeYear")
          ),
          fluidRow(
            column(6, plotOutput("quarter1")),
            column(6, plotOutput("quarter2"))
          ),
          fluidRow(
            column(6, plotOutput("quarter3")),
            column(6, plotOutput("quarter4"))
          )
        ),
        tabPanel("Box plots",
          fluidRow(
            column(8,
              helpText("Samples taken by month"),
              plotOutput("monthlyBoxplots")
            )
          ),
          fluidRow(
            column(8,
              helpText("Boxplots of continuous measurements and sample data"),
              checkboxInput("boxLog2", "Log Scale", value=FALSE),
              plotOutput("contVsSampled")
            )
          )
        )
      )
    ),
    tabPanel("Correlations/Transformations",
      fluidRow(
        column(7,
          uiOutput("showPairs"),
          fluidRow(
            column(3, checkboxInput("sin2piD", "Add sin/cos(2*pi*D)")),
            column(3, checkboxInput("sin4piD", "Add sin/cos(4*pi*D)"))
          ),
          plotOutput("pairsPlot")
        ),
        column(1,
          fluidRow(
            uiOutput("cubeChoices"),
            helpText("---------------"),
            uiOutput("squareChoices"),
            helpText("---------------"),
            uiOutput("logChoices"),
            helpText("---------------"),
            uiOutput("sqrtChoices")
          )    
        ),
        column(4,
          helpText("Correlation matrix"),
          box(width=NULL,
               div(style='overflow-x: scroll', tableOutput("corMatrix"))),
          helpText("All subsets regression (3 best N-Variable Models)"),
          box(width=NULL,
              div(style='overflow-x: scroll', tableOutput("allRegTable")))
        )
      )
    ),
    tabPanel("Compare Models",
      fluidRow(
        column(4,
          uiOutput("modelForm1UI"),
          textOutput("model1varCheck"),
          tabsetPanel(
            tabPanel("Model",
              helpText("\n"),
              textOutput("model1eq"),
              helpText("\n"),
              tableOutput("model1summary"),
              plotOutput("model1ovc"),
              plotOutput("model1cvo"),
              plotOutput("model1dvr"),
              plotOutput("model1nqvr"),
              plotOutput("model1qq"),
              plotOutput("model1monthResidBox"),
              plotOutput("model1yearResidBox"),
              plotOutput("model1cvalFolds"),
              fluidRow(
                column(6, plotOutput("model1cvalBox")),
                column(6, 
                  tableOutput("model1cvalTable"), 
                  helpText("<-- Red line: model MSE; Blue line: mean MSE of folds")
                )
              )
            ),
            tabPanel("Data/Outliers",
              fluidRow(
                column(6,
                  textInput("model1remove", "Remove points:")
                ),
                column(6,
                  radioButtons("model1FlagOn", label="Flag based on",
                               choices=c("None" = "N", 
                                         "Cook's D" = "C", 
                                         "DFFITS" = "D", 
                                         "Leverage" = "L", 
                                         "Residual" = "R"))       
                )
              ),
              textOutput("model1outcount"),
              box(width=NULL,
                  div(style='overflow-x: scroll', dataTableOutput("model1outliers"))
              )
            )
          )
        ),
        column(4,
          uiOutput("modelForm2UI"),
          tabsetPanel(
            tabPanel("Model",
              helpText("\n"),
              textOutput("model2eq"),
              helpText("\n"),
              tableOutput("model2summary"),
              plotOutput("model2ovc"),
              plotOutput("model2cvo"),
              plotOutput("model2dvr"),
              plotOutput("model2nqvr"),
              plotOutput("model2qq"),
              plotOutput("model2monthResidBox"),
              plotOutput("model2yearResidBox"),
              plotOutput("model2cvalFolds"),
              fluidRow(
                column(6, plotOutput("model2cvalBox")),
                column(6, 
                  tableOutput("model2cvalTable"),
                  helpText("<-- Red line: model MSE; Blue line: mean MSE of folds"))
              )
            ),
            tabPanel("Data/Outliers",
              fluidRow(
                column(6,
                  textInput("model2remove", "Remove points:")    
                ),
                column(6,
                  radioButtons("model2FlagOn", label="Flag based on",
                                choices=c("None" = "N", 
                                          "Cook's D" = "C", 
                                          "DFFITS" = "D", 
                                          "Leverage" = "L", 
                                          "Residual" = "R"))           
                )
              ),
              textOutput("model2outcount"),
              box(width=NULL,
                  div(style='overflow-x: scroll', dataTableOutput("model2outliers"))
              )
            )
          )
        ),
        column(4,
          uiOutput("modelForm3UI"),
          tabsetPanel(
            tabPanel("Model",
              helpText("\n"),
              textOutput("model3eq"),
              helpText("\n"),
              tableOutput("model3summary"),
              plotOutput("model3ovc"),
              plotOutput("model3cvo"),
              plotOutput("model3dvr"),
              plotOutput("model3nqvr"),
              plotOutput("model3qq"),
              plotOutput("model3monthResidBox"),
              plotOutput("model3yearResidBox"),
              plotOutput("model3cvalFolds"),
              fluidRow(
                column(6, plotOutput("model3cvalBox")),
                column(6, 
                  tableOutput("model3cvalTable"), 
                  helpText("<-- Red line: model MSE; Blue line: mean MSE of folds")
                )
              )
            ),
            tabPanel("Data/Outliers",
              fluidRow(
                column(6,
                  textInput("model3remove", "Remove points:")
                ),
                column(6,
                  radioButtons("model3FlagOn", label="Flag based on",
                                choices=c("None" = "N", 
                                          "Cook's D" = "C", 
                                          "DFFITS" = "D", 
                                          "Leverage" = "L", 
                                          "Residual" = "R"))        
                )
              ),
              textOutput("model3outcount"),
              box(width=NULL,
                  div(style='overflow-x: scroll', dataTableOutput("model3outliers"))
              )
            )
          )
        )
      )
    ),
    tabPanel("Prediction Time Series",
      tableOutput("modelReminder"),
      radioButtons("native", label = "Predict in:", choices=c("transformed units", "native units"), inline=TRUE),
      tabsetPanel(
        tabPanel("Plot",
          plotOutput("predictedSeries", brush = brushOpts(id = "predictBrush", direction = 'x', resetOnNew = TRUE)),
          plotOutput("predictedSeriesZoomed")
        ),
        tabPanel("Table",
          dataTableOutput("predictTable")         
        )
      )
    ),
    tabPanel("Validation",
      fluidRow(
        column(2, uiOutput("valModelSelectUI")),
        column(1, checkboxInput("valNativeUnits", label="Native units", value=TRUE))
      ),
      fluidRow(
        plotOutput("valPlot", width="100%", height="400px", 
                   brush = brushOpts(id = "valBrush", resetOnNew = FALSE, direction="x"))
      ),
      fluidRow(
        plotOutput("valPlotZoomed")
      )
    )
  )
))
