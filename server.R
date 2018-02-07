library(shiny) 
library(shinydashboard)
library(rmarkdown) 
library(scales) 
library(labeling)
library(ggplot2) 
library(car) 
library(dataRetrieval) 
library(lubridate) 
library(smwrQW) 
library(smwrStats) 
library(grid) 
library(XML)
library(DAAG)
library(leaps)
library(reshape2)
library(MASS)
source("helpers.R")

options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input,output) {
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  rangesPredict <- reactiveValues(x = NULL, y = NULL)
  
  rangesVal <- reactiveValues(x = NULL, y = NULL)
  
  #Raw continuous data - all points in the file
  allContData <- eventReactive(input$loadButton, {
    withProgress(message="Loading Continuous Data...", value=0.1, {
      cInFile <- input$contFile
      if(is.null(cInFile))
        return(NULL)
      incProgress(0.1, detail="Reading file")
      cTemp <- read.csv(cInFile$datapath, stringsAsFactors = FALSE)
      #Try this line
      incProgress(0.3, detail="Converting date")
      cTemp$datetime <- as.POSIXct(cTemp$datetime, format=input$cdateformat)
      incProgress(0.7, detail="Adding variables")
      cTemp$pdectime <- decimal_date(cTemp$datetime)
      cTemp$year <- year(cTemp$datetime)
      cTemp$month <- month(cTemp$datetime)
      cTemp$day <- day(cTemp$datetime)
      cTemp$hour <- hour(cTemp$datetime)
    })
    cTemp
  })
  
  #All the continuous data including any transformations selected
  allContDataTrans <- reactive({
    Data <- allContData()
    #Add seasonality variables, if selected
    withProgress(message="Generating data...", value=0.1, {
      incProgress(0.2, "Adding seasonality...")
      if(input$sin2piD) {
        Data$sin2piD <- sin(2*pi*(Data$day/365))
        Data$cos2piD <- cos(2*pi*(Data$day/365))
      }
      if(input$sin4piD) {
        Data$sin4piD <- sin(4*pi*(Data$day/365))
        Data$cos4piD <- cos(4*pi*(Data$day/365))
      }
      #Drop all the time variables except datetime
      Data <- Data[,!(names(Data) %in% c("dectime", "year", "month", "day", "hour"))]
      #ONly include variables checked in the second tab
      datetimes <- Data$datetime
      incProgress(0.7, "Transforming data...")
      Data <- Data[,names(Data)[names(Data) %in% c(input$useP, "sin2piD", "cos2piD", "sin4piD", "cos4piD")]]
      #Add transformations, if selected
      for(i in names(Data[,!(names(Data) %in% c("datetime","sin2piD", "cos2piD", "sin4piD", "cos4piD"))])) {
        if(input[[paste("logp", i, sep="")]]) {
          Data[,i] <- log10(Data[,i])
          names(Data)[names(Data) == i] <- paste("log", i, sep="")
        }
        if(input[[paste("sqrp", i, sep="")]]) {
          Data[,i] <- (Data[,i])^2
          names(Data)[names(Data) == i] <- paste("sqr", i, sep="")
        }
        if(input[[paste("sqrtp", i, sep="")]]) {
          Data[,i] <- (Data[,i])^0.5
          names(Data)[names(Data) == i] <- paste("sqrt", i, sep="")
        }
        if(input[[paste("cubep", i, sep="")]]) {
          Data[,i] <- (Data[,i])^3
          names(Data)[names(Data) == i] <- paste("cube", i, sep="")
        }
      }
      Data$datetime <- datetimes
      Data
    })
    
  })
  
  #Just the calibration data (if limited by dates on the first tab)
  contData <- reactive({
    cTemp <- allContData()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      cTemp <- cTemp[cTemp$datetime > start & cTemp$datetime < end,]
    }
    cTemp
  })
  
  #Just the validation data (outside of the range selected on the first tab)
  valContData <- reactive({
    cTemp <- allContData()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      cTemp <- cTemp[cTemp$datetime < start | cTemp$datetime > end,]
    } else {
      cTemp <- cTemp[0,]
    }
  })
  
  #Just the transformed continuous calibration data (inside of the range selected on the first tab)
  contDataTrans <- reactive({
    cTemp <- allContDataTrans()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      cTemp <- cTemp[cTemp$datetime > start & cTemp$datetime < end,]
    }
    cTemp
  })
  
  #Just the transformed continuous validation data (outside of the range selected on the first tab)
  valContDataTrans <- reactive({
    cTemp <- allContDataTrans()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      cTemp <- cTemp[cTemp$datetime < start | cTemp$datetime > end,]
    } else {
      cTemp <- cTemp[0,]
    }
    cTemp
  })
  
  #Output from the readCens function - a numerical data frame and a logical frame indicating
  #which, if any samples are censored
  allCensSample <- eventReactive(input$loadButton, {
    
    sInFile <- input$mergedFile
    start <- NA
    end <- NA
    data <- readCensored(sInFile$datapath, start, end, dateformat=input$sdateformat)
    data
    
  })
  
  #Just the data part from censSample
  allSampleData <- reactive({
    sTemp <- allCensSample()[["censData"]]
    sTemp$datetime <- as.POSIXct(sTemp$datetime, format=input$sdateformat)
    sTemp$dectime <- decimal_date(sTemp$datetime)
    sTemp$year <- year(sTemp$datetime)
    sTemp$month <- month(sTemp$datetime)
    sTemp$day <- day(sTemp$datetime)
    sTemp$hour <- hour(sTemp$datetime)
    sTemp
  })
  
  #The data part of the sample data, inside of the date range selected for calibration
  sampleData <- reactive({
    sTemp <- allSampleData()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      sTemp <- sTemp[sTemp$datetime > start & sTemp$datetime < end,]
    }
    sTemp
  })
  
  #The data part of the sample data, outside of the date range selected for calibration
  valSampleData <- reactive({
    sTemp <- allSampleData()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      sTemp <- sTemp[sTemp$datetime > end,]
    } else {
      sTemp <- sTemp[0,]
    }
    sTemp
  })
  
  #The qualifier part of the sample data
  allIsCensored <- reactive({
    temp <- allCensSample()[["isCensored"]]
    temp$datetime <- as.POSIXct(temp$datetime, format=input$sdateformat)
    temp
  })
  
  #The qualifer part of the sample data, inside of the date range selected on the first tab
  isCensored <- reactive({
    temp <- allIsCensored()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      temp <- temp[temp$datetime > start & temp$datetime < end,]
    }
    temp
  })
  
  #The qualifer part of the sample data, outside of the date range selected on the first tab
  valIsCensored <- reactive({
    temp <- allIsCensored()
    if(input$limitDates) {
      start <- as.POSIXct(input$uploadDateRange[1], format="%Y-%m-%d")
      end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
      temp <- temp[temp$datetime < start | temp$datetime > end,]
    } else {
      temp <- temp[0,]
    }
    temp
  })
  
  #A data frame containing only the response and candidate variables, 
  #As subset by the useP input variable, and transformed according to the
  #ladder of powers section
  filtData <- reactive({
    Data <- sampleData()
    #Drop all the time variables except datetime
    Data <- Data[,!(names(Data) %in% c("dectime","datetime","year", "month", "day", "hour"))]
    #ONly include variables checked in the second tab
    Data <- Data[,input$useP]
    #Add transformations, if selected
    for(i in names(Data)) {
      if(input[[paste("logp", i, sep="")]]) {
        Data[,i] <- log10(Data[,i])
        names(Data)[names(Data) == i] <- paste("log", i, sep="")
      }
      if(input[[paste("sqrp", i, sep="")]]) {
        Data[,i] <- (Data[,i])^2
        names(Data)[names(Data) == i] <- paste("sqr", i, sep="")
      }
      if(input[[paste("sqrtp", i, sep="")]]) {
        Data[,i] <- (Data[,i])^0.5
        names(Data)[names(Data) == i] <- paste("sqrt", i, sep="")
      }
      if(input[[paste("cubep", i, sep="")]]) {
        Data[,i] <- (Data[,i])^3
        names(Data)[names(Data) == i] <- paste("cube", i, sep="")
      }
    }
    Data
  })
  
  #A data frame with all date variables left in
  filtDatawD <- reactive({
    Data <- sampleData()
    #Add seasonality variables, if selected
    if(input$sin2piD) {
      Data$sin2piD <- sin(2*pi*(Data$day/365))
      Data$cos2piD <- cos(2*pi*(Data$day/365))
    }
    if(input$sin4piD) {
      Data$sin4piD <- sin(4*pi*(Data$day/365))
      Data$cos4piD <- cos(4*pi*(Data$day/365))
    }
    #Drop all the time variables except datetime
    Data <- Data[,!(names(Data) %in% c("dectime", "year", "month", "day", "hour"))]
    #ONly include variables checked in the second tab
    datetimes <- Data$datetime
    Data <- Data[,names(Data)[names(Data) %in% c(input$useP, "sin2piD", "cos2piD", "sin4piD", "cos4piD")]]
    #Add transformations, if selected
    for(i in names(Data[,!(names(Data) %in% c("datetime","sin2piD", "cos2piD", "sin4piD", "cos4piD"))])) {
      if(input[[paste("logp", i, sep="")]]) {
        Data[,i] <- log10(Data[,i])
        names(Data)[names(Data) == i] <- paste("log", i, sep="")
      }
      if(input[[paste("sqrp", i, sep="")]]) {
        Data[,i] <- (Data[,i])^2
        names(Data)[names(Data) == i] <- paste("sqr", i, sep="")
      }
      if(input[[paste("sqrtp", i, sep="")]]) {
        Data[,i] <- (Data[,i])^0.5
        names(Data)[names(Data) == i] <- paste("sqrt", i, sep="")
      }
      if(input[[paste("cubep", i, sep="")]]) {
        Data[,i] <- (Data[,i])^3
        names(Data)[names(Data) == i] <- paste("cube", i, sep="")
      }
    }
    
    Data$datetime <- datetimes
    Data
  })
  
  #Output a table showing the period of record for each parameter
  output$periodOfRecord <- renderTable({
    #Get the data
    cont <- contData()
    samp <- na.omit(sampleData()[,c("datetime", input$dVar)])
    isCens <- isCensored()
    
    #Find the start and end dates for all the continuous variables not called datetime
    cont <- cont[,!(names(cont) %in% c("pdectime", "dectime", "year", "month", "day", "hour","X"))]
    namesNotDate <- names(cont)[names(cont) != "datetime"]
    variable <- vector()
    start <- vector()
    end <- vector()
    censored <- vector()
    for(i in namesNotDate) {
      temp <- na.omit(cont[,c("datetime", i)])
      cens <- isCens[,i]
      variable[length(variable) + 1] <- i
      start[length(start) + 1] <- as.character(as.Date(min(temp$datetime)))
      end[length(end) + 1] <- as.character(as.Date(max(temp$datetime)))
      censored[length(censored) + 1] <- paste0(round((sum(cens)/length(cens))*100, 1), "%")
    }
    
    #Find the period of record for the response variable
    cens <- isCens[,input$dVar]
    variable[length(variable) + 1] <- input$dVar
    start[length(start) + 1] <- as.character(as.Date(min(samp$datetime)))
    end[length(end) + 1] <- as.character(as.Date(max(samp$datetime)))
    censored[length(censored) + 1] <- paste0(round((sum(cens)/length(cens))*100, 1), "%")
      
    #Make a data frame, rename and output
    pOR <- data.frame(variable, start, end, censored)
    names(pOR) <- c("Parameter", "Beginning", "End", "Censored")
    pOR
  })
  
  #Output a warning if any of the varibles have too much censored dta
  output$censorWarning <- renderText({
    
    isCens <- isCensored()
    notDate <- names(isCens)[names(isCens) != "datetime"]
    percentCensored <- vector()
    for(i in notDate) {
      cens <- isCens[,i]
      percentCensored[length(percentCensored) + 1] <- sum(cens)/length(cens)
    }
    if(any(percentCensored > 0.05)) {
      text <- "WARNING: Too much censored data to procede!! Consider other regression methods."
    } else {
      text <- "Censored points are replaced with half the detection limit."
    }
    return(text)
    
  })
  
  #Select which parameter to plot the duration plot for
  output$durationChoice <- renderUI({
    ch <- names(contData())
    ch <- ch[!(ch %in% c("pdectime", "year", "month", "day", "hour", "X", "datetime"))]
    selectInput("durChoice", "Base duration plot on: ", choices = ch)
  })
  
  #Select which continuous parameter to plot on the time series
  output$timeSeriesChoice <- renderUI({
    ch <- names(contData())
    ch <- ch[!ch %in% c("pdectime", "year", "month", "day", "hour", "X", "datetime")]
    selectInput("tsChoice", "View time series plot of: ", choices = ch)
  })
  
  output$timeSeries1 <- renderPlot({
    
    if(input$tsPlotButton > 0) {
      isolate({
        data <- contData()
        tsChoice <- input$tsChoice
      })
      data <- contData()
      data <- data[,c("datetime", tsChoice)]
      names(data) <- c("datetime", "Continuous")
      p <- ggplot() +
        geom_point(data=data, aes(x=datetime, y=Continuous), color="blue4") +
        scale_x_datetime(date_labels="%m-%d-%Y")
      if(input$tsLogScale) {
        p <- p + scale_y_log10()
      }
      p
    }
  })
  
  observe({
    brush <- input$tsBrush
    if (!is.null(brush)) {
      ranges$x <- c(as.POSIXct(brush$xmin, origin="1970-01-01"), as.POSIXct(brush$xmax, origin="1970-01-01"))
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$timeSeries2 <- renderPlot({
    if(input$tsPlotButton > 0) {
      isolate({
        data <- contData()
        tsChoice <- input$tsChoice
      })
      data <- contData()
      data <- data[,c("datetime", tsChoice)]
      data <- data[data$datetime>ranges$x[1] & data$datetime<ranges$x[2],]
      names(data) <- c("datetime", "Continuous")
      p <- ggplot() +
        geom_point(data=data, aes(x=datetime, y=Continuous), color="blue4") +
        scale_x_datetime(date_labels="%m-%d-%Y")
      if(input$tsLogScale) {
        p <- p + scale_y_log10()
      }
      p
    }
  })
  
  #Duration curve with sample points for the year
  output$wholeYear <- renderPlot({
    
    if(input$plotButton > 0) {
      isolate({
        pred1 <- contData()
        cal1 <- sampleData()
        durVar <- input$durChoice
        respVar <- input$dVar
      })
      
      pred1 <- pred1[pred1[,durVar]>0.0 & !is.na(pred1[,durVar]),]
      cal1 <- cal1[cal1[,durVar]>0.0 & !is.na(cal1[,durVar]),]

      withProgress(message = "Making overall plot...", value=0.5, {
        durationCurve(continuousData = pred1, sampleData = cal1, durVariable = durVar, respVariable = respVar, quarter=0)
      })

    }
  })
  
  #Duration curve with sample points for the first quarter
  output$quarter1 <- renderPlot({
    
    if(input$plotButton > 0) {
      isolate({
        pred1 <- contData()
        cal1 <- sampleData()
        durVar <- input$durChoice
        respVar <- input$dVar
      })

      pred1 <- pred1[pred1[,durVar]>0.0 & !is.na(pred1[,durVar]),]
      cal1 <- cal1[cal1[,durVar]>0.0 & !is.na(cal1[,durVar]),]
      
      withProgress(message = "Making first quarter plot...", value=0.5, {
        durationCurve(continuousData = pred1, sampleData = cal1, durVariable = durVar, respVariable = respVar, quarter=1)
      })
    }
  })
  
  #Duration curve with sample points for the second quarter
  output$quarter2 <- renderPlot({
    
    if(input$plotButton > 0) {
      isolate({
        pred1 <- contData()
        cal1 <- sampleData()
        durVar <- input$durChoice
        respVar <- input$dVar
      })

      pred1 <- pred1[pred1[,durVar]>0.0 & !is.na(pred1[,durVar]),]
      cal1 <- cal1[cal1[,durVar]>0.0 & !is.na(cal1[,durVar]),]
      
      withProgress(message = "Making second quarter plot...", value=0.5, {
        durationCurve(continuousData = pred1, sampleData = cal1, durVariable = durVar, respVariable = respVar, quarter=2)
      })
    }
  })
  
  #Duration curve with sample points for the third quarter
  output$quarter3 <- renderPlot({
    
    if(input$plotButton > 0) {
      isolate({
        pred1 <- contData()
        cal1 <- sampleData()
        durVar <- input$durChoice
        respVar <- input$dVar
      })
      
      pred1 <- pred1[pred1[,durVar]>0.0 & !is.na(pred1[,durVar]),]
      cal1 <- cal1[cal1[,durVar]>0.0 & !is.na(cal1[,durVar]),]
      
      withProgress(message = "Making third quarter plot...", value=0.5, {
        durationCurve(continuousData = pred1, sampleData = cal1, durVariable = durVar, respVariable = respVar, quarter=3)
      })
    }
  })
  
  #Duration curve with sample points for the fourth quarter
  output$quarter4 <- renderPlot({
    
    if(input$plotButton > 0) {
      isolate({
        pred1 <- contData()
        cal1 <- sampleData()
        durVar <- input$durChoice
        respVar <- input$dVar
      })

      pred1 <- pred1[pred1[,durVar]>0.0 & !is.na(pred1[,durVar]),]
      cal1 <- cal1[cal1[,durVar]>0.0 & !is.na(cal1[,durVar]),]
      
      withProgress(message = "Making fourth quarter plot...", value=0.5, {
        durationCurve(continuousData = pred1, sampleData = cal1, durVariable = durVar, respVariable = respVar, quarter=4)
      })
    }
    
  })
  
  #Boxplots of samples by month
  output$monthlyBoxplots <- renderPlot({
    
    input$plotButton
    
    isolate({
      bplotData <- sampleData()
      bplotData <- bplotData[,c("month", input$dVar)]
    })
    
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monWData <- months[as.numeric(levels(factor(bplotData$month)))]
    bplotData$month <- months[bplotData$month]
    bplotData$month <- factor(bplotData$month, monWData)
    boxplot(formula(paste(input$dVar, "~ month")), data=bplotData, varwidth=TRUE)
    for(xi in 1:12) {
      text(x=xi,y=boxplot.stats(bplotData[bplotData$month==months[xi],2])$stats[5], 
           as.character(nrow(bplotData[bplotData$month==months[xi],])), pos=3)
    }
  })
  
  output$contVsSampled <- renderPlot({
    
    cont <- contData()
    samp <- sampleData()
    #Find which variables are in both data sets
    sharedNames <- names(cont)[names(cont) %in% names(samp)]
    sharedNames <- sharedNames[!(sharedNames %in% c("datetime","year","month","day","hour"))]
    #Only keep the shared variables
    cont <- cont[,sharedNames]
    samp <- samp[,sharedNames]
    #Flatten the data frames
    cont <- melt(cont, na.rm=TRUE)
    samp <- melt(samp, na.rm=TRUE)
    #Add a grouping variable
    cont$group <- "Continuous"
    samp$group <- "Sampled"
    data <- rbind(cont, samp)
    rows <- ceiling(length(sharedNames)/4)
    if(input$boxLog2) {
      logOpt <- "y"
      data <- data[data$value>0,]
    } else {
      logOpt <- ""
    }
    par(mfrow=c(rows,4), cex=1.1)
    for(i in sharedNames) {
      #Subset the frames by variable
      temp_data <- data[data$variable==i, c("value", "group")]
      boxplot(value~group, data=temp_data, boxwex=0.5, col=c("coral2", "steelblue2"), 
              main=i, las=1, log=logOpt)
    }
  }, height=800)
  
  #Generate a series of checkbox inputs to log-transform variables
  output$logChoices <- renderUI({
    Data <- sampleData()
    Data <- Data[,!(names(Data) %in% c("datetime", "dectime", "year", "month", "day", "hour"))]
    Data <- Data[,names(Data) %in% input$useP]
    lapply(names(Data), function(i) {
      checkboxInput(inputId = paste("logp", i, sep=""), label = paste("Log", i))
    })
  })
  
  #Generate a series of checkbox inputs to square variables
  output$squareChoices <- renderUI({
    Data <- sampleData()
    Data <- Data[,!(names(Data) %in% c("datetime", "dectime", "year", "month", "day", "hour"))]
    Data <- Data[,names(Data) %in% input$useP]
    lapply(names(Data), function(i) {
      checkboxInput(inputId = paste("sqrp", i, sep=""), label=paste("Square", i))
    })
  })
  
  #Generate a series of checkbox inputs to square root variables
  output$sqrtChoices <- renderUI({
    Data <- sampleData()
    Data <- Data[,!(names(Data) %in% c("datetime", "dectime", "year", "month", "day", "hour"))]
    Data <- Data[,names(Data) %in% input$useP]
    lapply(names(Data), function(i) {
      checkboxInput(inputId = paste("sqrtp", i, sep=""), label=paste("Square root", i))
    })
  })
  
  #Generate a series of checkbox inputs to cube variables
  output$cubeChoices <- renderUI({
    Data <- sampleData()
    Data <- Data[,!(names(Data) %in% c("datetime", "dectime", "year", "month", "day", "hour"))]
    Data <- Data[,names(Data) %in% input$useP]
    lapply(names(Data), function(i) {
      checkboxInput(inputId = paste("cubep", i, sep=""), label=paste("Cube", i))
    })
  })
  
  #Choose what parameters to include in the pairs plot, ladder of powers,
  #and data frame for the model comparisons
  output$showPairs <- renderUI({
    Data <- sampleData()
    ch <- names(Data)[!(names(Data) %in% c("datetime", "dectime", "year", "month", "day", "hour"))]
    checkboxGroupInput("useP", "Use:", choices=ch, selected=ch, inline=TRUE)
  })
  
  #Generate a scatter plot matrix of the variables
  output$pairsPlot <- renderPlot({
    Data <- filtData()
    for(i in names(Data)) {
      print(class(Data[,i]))
    }
    pairs(Data, panel=panel.smooth)}, height=800)
  
  #Generate a table of correlation coefficients
  output$corMatrix <- renderTable({
    Data <- filtData()
    cov.mat <- matrix(data=NA, nrow=ncol(Data), ncol=ncol(Data))
    for(i in 1:ncol(Data)) {
      for(j in 1:ncol(Data)) {
        t <- Data[,c(i,j)]
        t <- na.omit(t)
        cov.mat[i,j] <- cor(t[,1], t[,2])
      }
    }
    row.names(cov.mat) <- names(Data)
    colnames(cov.mat) <- names(Data)
    cov.mat
  }, rownames = TRUE)
  
  #Generate a table of best subsets
  output$allRegTable <- renderTable({
    #A vector of possible names of the response variable
    responseVars <- paste0(c("", "log", "cube", "square", "sqrt"), input$dVar)
    #Get the fitlered and transformed data, and drop the datetime column
    data <- filtDatawD()
    data <- data[,names(data) != "datetime"]
    #Get a frame of just the predictors (anything not having a response variable name)
    predictors <- data[,!(names(data) %in% responseVars)]
    #Get the response variable (anything with a name in the list)
    response <- data[,names(data) %in% responseVars]
    rName <- names(data)[names(data) %in% responseVars]
    #Find the best models
    models <- allReg(predictors, response, nmax=5, lin.dep=0)
    #Rename "response" in the formulas with the name of the response variable
    models$model.formula <- sub("response", rName, models$model.formula)
    models
  })
  
  #Generate a selectize input for selecting variables for each model
  output$modelForm1UI <- renderUI({
    variables <- names(filtDatawD())
    prefixes <- c("cube", "sqr", "sqrt", "log")
    respVars <- c(input$dVar, paste(prefixes, input$dVar, sep=""))
    variables <- variables[!(variables %in% c(respVars, "datetime", "X", "day", "month", "year", "hour"))]
    selectizeInput("modelForm1", "Model 1", choices=variables, selected=NULL, multiple=TRUE)
  })
  
  output$modelForm2UI <- renderUI({
    variables <- names(filtDatawD())
    prefixes <- c("cube", "sqr", "sqrt", "log")
    respVars <- c(input$dVar, paste(prefixes, input$dVar, sep=""))
    variables <- variables[!(variables %in% c(respVars, "datetime", "X", "day", "month", "year", "hour"))]
    selectizeInput("modelForm2", "Model 2", choices=variables, selected=NULL, multiple=TRUE)
  })
  
  output$modelForm3UI <- renderUI({
    variables <- names(filtDatawD())
    prefixes <- c("cube", "sqr", "sqrt", "log")
    respVars <- c(input$dVar, paste(prefixes, input$dVar, sep=""))
    variables <- variables[!(variables %in% c(respVars, "datetime", "X", "day", "month", "year", "hour"))]
    selectizeInput("modelForm3", "Model 3", choices=variables, selected=NULL, multiple=TRUE)
  })
  
  #Paste a model form together from the selected variables for each model
  model1Form <- reactive({
    if(!(is.null(input$modelForm1))) {
      variables <- names(filtDatawD())
      prefixes <- c("cube", "sqr", "sqrt", "log")
      respVars <- c(input$dVar, paste(prefixes, input$dVar, sep=""))
      resp <- variables[variables %in% respVars]
      resp <- resp[1]
      form <- paste(resp, " ~ ", input$modelForm1[1])
      if(length(input$modelForm1) > 1) {
        form <- paste(c(form, input$modelForm1[-1]), collapse= " + ")
      }
    } else {
      form <- ""
    }
    form
  })
  
  model2Form <- reactive({
    if(!(is.null(input$modelForm2))) {
      variables <- names(filtDatawD())
      prefixes <- c("cube", "sqr", "sqrt", "log")
      respVars <- c(input$dVar, paste(prefixes, input$dVar, sep=""))
      resp <- variables[variables %in% respVars]
      resp <- resp[1]
      form <- paste(resp, " ~ ", input$modelForm2[1])
      if(length(input$modelForm2) > 1) {
        form <- paste(c(form, input$modelForm2[-1]), collapse= " + ")
      }
    } else {
      form <- ""
    }
    form
  })
  
  model3Form <- reactive({
    if(!(is.null(input$modelForm3))) {
      variables <- names(filtDatawD())
      prefixes <- c("cube", "sqr", "sqrt", "log")
      respVars <- c(input$dVar, paste(prefixes, input$dVar, sep=""))
      resp <- variables[variables %in% respVars]
      resp <- resp[1]
      form <- paste(resp, " ~ ", input$modelForm3[1])
      if(length(input$modelForm3) > 1) {
        form <- paste(c(form, input$modelForm3[-1]), collapse= " + ")
      }
    } else {
      form <- ""
    }
    form
  })
  
  #Data used to calculate each model
  model1Data <- reactive({
    
    form <- strsplit(model1Form(), "[^[:alnum:]]")[[1]]
    form <- form[!(form %in% c("", " "))]
    
    Data <- filtDatawD()
    Data <- Data[!(rownames(Data) %in% as.numeric(strsplit(input$model1remove, "[^[:alnum:]]")[[1]])),]
    Data <- Data[,c("datetime", form)]
    Data <- na.omit(Data)
    Data
  })
  
  model2Data <- reactive({
    
    form <- strsplit(model2Form(), "[^[:alnum:]]")[[1]]
    form <- form[!(form %in% c("", " "))]
    
    Data <- filtDatawD()
    Data <- Data[!(rownames(Data) %in% as.numeric(strsplit(input$model2remove, "[^[:alnum:]]")[[1]])),]
    Data <- Data[,c("datetime", form)]
    Data <- na.omit(Data)
    Data
  })
  
  model3Data <- reactive({
    
    form <- strsplit(model3Form(), "[^[:alnum:]]")[[1]]
    form <- form[!(form %in% c("", " "))]
    
    Data <- filtDatawD()
    Data <- Data[!(rownames(Data) %in% as.numeric(strsplit(input$model3remove, "[^[:alnum:]]")[[1]])),]
    Data <- Data[,c("datetime", form)]
    Data <- na.omit(Data)
    Data
  })
  
  #Check if each model has enough observations for its number of variables
  output$model1varCheck <- renderText({
    
    if(model1Form() == "")
      return(NULL)
    
    terms <- length(input$modelForm1)
    observations <- nrow(model1Data())
    score <- round(observations/terms, 1)
    if(score < 10 & score > 6) { 
      warning <- paste("It's better to have more 10 observations per variable, you have", score)
    } else if (score <= 6) {
      warning <- paste("Warning: Not enough observations for this model!")
    } else {
      warning <- ""
    }
    
    warning
    
  })
  
  output$model2varCheck <- renderText({
    
    if(model2Form() == "")
      return(NULL)
    
    terms <- length(input$modelForm2)
    observations <- nrow(model2Data())
    score <- round(observations/terms, 1)
    if(score < 10 & score > 6) { 
      warning <- paste("It's better to have more 10 observations per variable, you have", score)
    } else if (score <= 6) {
      warning <- paste("Warning: Not enough observations for this model!")
    } else {
      warning <- ""
    }
    
    warning
    
  })
  
  output$model3varCheck <- renderText({
    
    if(model3Form() == "")
      return(NULL)
    
    terms <- length(input$modelForm3)
    observations <- nrow(model3Data())
    score <- round(observations/terms, 1)
    if(score < 10 & score > 6) { 
      warning <- paste("It's better to have more 10 observations per variable, you have", score)
    } else if (score <= 6) {
      warning <- paste("Warning: Not enough observations for this model!")
    } else {
      warning <- ""
    }
    
    warning
    
  })
  
  #Calculate the models
  model1lm <- reactive({
    
    form <- strsplit(model1Form(), "[^[:alnum:]]")[[1]]
    form <- form[!(form %in% c("", " "))]
    f <- formula(model1Form())
    
    Data <- model1Data()
    
    model1 <- lm(f, data=Data)
    model1
    
  })
  
  model2lm <- reactive({
    
    form <- strsplit(model2Form(), "[^[:alnum:]]")[[1]]
    form <- form[!(form %in% c("", " "))]
    f <- formula(model2Form())
    
    Data <- model2Data()
    
    model2 <- lm(f, data=Data)
    model2
    
  })
  
  model3lm <- reactive({
    
    form <- strsplit(model3Form(), "[^[:alnum:]]")[[1]]
    form <- form[!(form %in% c("", " "))]
    f <- formula(model3Form())
    
    Data <- model3Data()
    
    model3 <- lm(f, data=Data)
    model3
    
  })
  
  #Find the antifunction of the transformation of the response variable
  model1antiFunction <- reactive({
    
    form <- model1Form()
    if(substr(form, 1, 4) == "cube") {
      f <- function(x) {x ^ (1/3)}
    } else if (substr(form, 1, 3) == "sqr") {
      f <- sqrt
    } else if(substr(form, 1, 3) == "log") {
      f <- function(x) {10 ^ x}
    } else if(substr(form, 1, 4) == "sqrt") {
      f <- function(x) {x ^ 2}
    } else {
      f <- function(x) {x} 
    }
    
    f
    
  })
  
  model2antiFunction <- reactive({
    
    form <- model2Form()
    if(substr(form, 1, 4) == "cube") {
      f <- function(x) {x ^ (1/3)}
    } else if (substr(form, 1, 3) == "sqr") {
      f <- sqrt
    } else if(substr(form, 1, 3) == "log") {
      f <- function(x) {10 ^ x}
    } else if(substr(form, 1, 4) == "sqrt") {
      f <- function(x) {x ^ 2}
    } else {
      f <- function(x) {x} 
    }
    
    f
    
  })
  
  model3antiFunction <- reactive({
    
    form <- model3Form()
    if(substr(form, 1, 4) == "cube") {
      f <- function(x) {x ^ (1/3)}
    } else if (substr(form, 1, 3) == "sqr") {
      f <- sqrt
    } else if(substr(form, 1, 3) == "log") {
      f <- function(x) {10 ^ x}
    } else if(substr(form, 1, 4) == "sqrt") {
      f <- function(x) {x ^ 2}
    } else {
      f <- function(x) {x} 
    }
    
    f
    
  })
  
  #Find the transformation function of the of the response variable
  model1Function <- reactive({
    
    form <- model1Form()
    if(substr(form, 1, 4) == "cube") {
      f <- function(x) {x ^ 3}
    } else if (substr(form, 1, 3) == "sqr") {
      f <- function(x) {x ^ 2}
    } else if(substr(form, 1, 3) == "log") {
      f <- log10
    } else if(substr(form, 1, 4) == "sqrt") {
      f <- function(x) {x ^ (0.5)}
    } else {
      f <- function(x) {x} 
    }
    
    f
    
  })
  
  model2Function <- reactive({
    
    form <- model2Form()
    if(substr(form, 1, 4) == "cube") {
      f <- function(x) {x ^ 3}
    } else if (substr(form, 1, 3) == "sqr") {
      f <- function(x) {x ^ 2}
    } else if(substr(form, 1, 3) == "log") {
      f <- log10
    } else if(substr(form, 1, 4) == "sqrt") {
      f <- function(x) {x ^ (0.5)}
    } else {
      f <- function(x) {x} 
    }
    
    f
    
  })
  
  model3Function <- reactive({
    
    form <- model3Form()
    if(substr(form, 1, 4) == "cube") {
      f <- function(x) {x ^ 3}
    } else if (substr(form, 1, 3) == "sqr") {
      f <- function(x) {x ^ 2}
    } else if(substr(form, 1, 3) == "log") {
      f <- log10
    } else if(substr(form, 1, 4) == "sqrt") {
      f <- function(x) {x ^ (0.5)}
    } else {
      f <- function(x) {x} 
    }
    
    f
    
  })
  
  #Find the bias correction factor of the models
  model1bcf <- reactive({

    
    if(model1Form() != "") {
      if(input$dVar %in% names(model1Data())) {
        bcf <- 1
      } else {
        antiF <- model1antiFunction()
        residuals <- resid(model1lm())
        bcf <- sum(antiF(residuals))/length(residuals)
      }
    } else {
      bcf <- 1
    }
    bcf
  })
  
  model2bcf <- reactive({
    if(model2Form() != "") {
      if(input$dVar %in% names(model2Data())) {
        bcf <- 1
      } else {
        antiF <- model2antiFunction()
        residuals <- resid(model2lm())
        bcf <- sum(antiF(residuals))/length(residuals)
      }
    } else {
      bcf <- 1
    }
    bcf
  })
  
  model3bcf <- reactive({
    if(model3Form() != "") {
      if(input$dVar %in% names(model3Data())) {
        bcf <- 1
      } else {
        antiF <- model3antiFunction()
        residuals <- resid(model3lm())
        bcf <- sum(antiF(residuals))/length(residuals)
      }
    } else {
      bcf <- 1
    }
    bcf
  })
  
  #Output a summary table of each model
  output$model1summary <- renderTable({

    if(model1Form() == "")
      return(NULL)
    
    modelXSummary(model1Form(), filtDatawD(), input$model1remove, model1bcf())

    
  })
  
  output$model2summary <- renderTable({

    if(model2Form() == "")
      return(NULL)
    
    modelXSummary(model2Form(), filtDatawD(), input$model2remove, model2bcf())
    
  })
  
  output$model3summary <- renderTable({

    if(model3Form() == "")
      return(NULL)
    
    modelXSummary(model3Form(), filtDatawD(), input$model3remove, model3bcf())

    
  })
  
  #Generate a table of outlier test statistics for each observation in for the three models
  output$model1outliers <- renderDataTable({
    
    respVars <- paste0(c("", "cube", "sqr", "log", "sqrt"), input$dVar)
    data <- model1Data()
    data <- data[,names(data) %in% c("datetime", respVars)]
    
    modelXOutliers(model1lm(), data)
    
  })
  
  output$model2outliers <- renderDataTable({
    
    respVars <- paste0(c("", "cube", "sqr", "log", "sqrt"), input$dVar)
    data <- model2Data()
    data <- data[,names(data) %in% c("datetime", respVars)]
    
    modelXOutliers(model2lm(), data)
    
  })
  
  output$model3outliers <- renderDataTable({
    
    respVars <- paste0(c("", "cube", "sqr", "log", "sqrt"),input$dVar)
    data <- model3Data()
    data <- data[,names(data) %in% c("datetime", respVars)]
    
    modelXOutliers(model3lm(), data)
    
  })
  
  #data frame indicating which points to flag based on
  model1isFlag <- reactive({
    
    flagOn <- input$model1FlagOn
    
    respVars <- paste0(c("", "cube", "sqr", "log", "sqrt"), input$dVar)
    data <- model1Data()
    data <- data[,names(data) %in% c("datetime", respVars)]
    
    outliers <- modelXOutliers(model1lm(), data)
    outliers <- outliers[,c("datetime", "Flag")]
    
    outliers$Flag <- grepl(flagOn, outliers$Flag)
    outliers
    
  })
  
  model2isFlag <- reactive({
    
    flagOn <- input$model2FlagOn
    
    respVars <- paste0(c("", "cube", "sqr", "log", "sqrt"), input$dVar)
    data <- model2Data()
    data <- data[,names(data) %in% c("datetime", respVars)]
    
    outliers <- modelXOutliers(model2lm(), data)
    outliers <- outliers[,c("datetime", "Flag")]
    
    outliers$Flag <- grepl(flagOn, outliers$Flag)
    outliers
    
  })
  
  model3isFlag <- reactive({
    
    flagOn <- input$model3FlagOn
    
    respVars <- paste0(c("", "cube", "sqr", "log", "sqrt"), input$dVar)
    data <- model3Data()
    data <- data[,names(data) %in% c("datetime", respVars)]
    
    outliers <- modelXOutliers(model3lm(), data)
    outliers <- outliers[,c("datetime", "Flag")]
    
    outliers$Flag <- grepl(flagOn, outliers$Flag)
    outliers
    
  })
  
  #Output how many points have been taken out of each model
  output$model1outcount <- renderText({
    as.character(length(strsplit(input$model1remove, "[^[:alnum:]]")[[1]]))
  })
  
  output$model2outcount <- renderText({
    as.character(length(strsplit(input$model2remove, "[^[:alnum:]]")[[1]]))
  })
  
  output$model3outcount <- renderText({
    as.character(length(strsplit(input$model3remove, "[^[:alnum:]]")[[1]]))
  })
  
  #Output a text equations for each model 
  output$model1eq <- renderText({
    
    if(model1Form() == "")
      return(NULL)
    
    model_form <- modelXeq(model1Form(), model1lm())
    model_form
    
  })
  
  output$model2eq <- renderText({
    
    if(model2Form() == "")
      return(NULL)
    
    model_form <- modelXeq(model2Form(), model2lm())
    model_form
    
  })
  
  output$model3eq <- renderText({
    
    if(model3Form() == "")
      return(NULL)

    model_form <- modelXeq(model3Form(), model3lm())
    model_form
    
  })
  
  #Output plots of residual vs calculated value for each model
  output$model1ovc <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    
    respVars <- paste0(c("cube", "sqr", "log", "sqrt"), input$dVar)
    modelData <- model1Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model1isFlag()
    
    modelXovc(model1Form(), model1lm(), model1Function(), model1antiFunction(), isCens, isFlag)
    
  })
  
  output$model2ovc <- renderPlot({
    
    if(model2Form() == "")
      return(NULL)
    
    respVars <- paste0(c("cube", "sqr", "log", "sqrt"), input$dVar)
    modelData <- model2Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model2isFlag()

    modelXovc(model2Form(), model2lm(), model2Function(), model2antiFunction(), isCens, isFlag)
    
  })
  
  output$model3ovc <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    respVars <- paste0(c("cube", "sqr", "log", "sqrt"), input$dVar)
    modelData <- model3Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model3isFlag()
    
    modelXovc(model3Form(), model3lm(), model3Function(), model3antiFunction(), isCens, isFlag)
    
  })
  
  #Output computed vs observed plots for each model
  output$model1cvo <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    print("modelData")
    modelData <- model1Data()
    print(head(modelData))
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model1isFlag()
    
    print(model1Function())
    print(model1antiFunction())
    print(model1bcf())
    
    modelXcvo(model1Form(), model1lm(), model1Data(), model1Function(), model1antiFunction(), model1bcf(), isCens, isFlag)
      
  })
  
  output$model2cvo <- renderPlot({
    
    if(model2Form() == "")
      return(NULL)
    
    modelData <- model2Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model2isFlag()

    modelXcvo(model2Form(), model2lm(), model2Data(), model2Function(), model2antiFunction(), model2bcf(), isCens, isFlag)
    
  })
  
  output$model3cvo <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    modelData <- model3Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model3isFlag()
    
    modelXcvo(model3Form(), model3lm(), model3Data(), model3Function(), model3antiFunction(), model3bcf(), isCens, isFlag)
    
  })
  
  #Output date vs residual plots for each model
  output$model1dvr <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    
    modelData <- model1Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model1isFlag()
    
    modelXdvr(modelData, model1lm(), isCens, isFlag)
    
  })
  
  output$model2dvr <- renderPlot({

    if(model2Form() == "")
      return(NULL)
    
    modelData <- model2Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model2isFlag()
    
    modelXdvr(modelData, model2lm(), isCens, isFlag)
    
  })
  
  output$model3dvr <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    modelData <- model3Data()
    isCens <- isCensored()
    isCens <- isCens[isCens$datetime %in% modelData$datetime, c("datetime", input$dVar)]
    isFlag <- model3isFlag()
    
    modelXdvr(modelData, model3lm(), isCens, isFlag)
    
  })
  
  #Output normal quantiles of residuals for each model
  output$model1nqvr <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    
    isFlag <- model1isFlag()
    
    modelXnqvr(model1lm(), isFlag)
  })
  
  output$model2nqvr <- renderPlot({
    
    if(model2Form() == "")
      return(NULL)
    
    isFlag <- model2isFlag()
    
    modelXnqvr(model2lm(), isFlag)
  })
  
  output$model3nqvr <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    isFlag <- model3isFlag()
    
    modelXnqvr(model3lm(), isFlag)
    
  })
  
  #Output a qqplot for each model
  output$model1qq <- renderPlot({
    
    residuals <- resid(model1lm())
    qqnorm(residuals)
    qqline(residuals)
    
  })
  
  output$model2qq <- renderPlot({
    
    residuals <- resid(model2lm())
    qqnorm(residuals)
    qqline(residuals)
    
  })
  
  output$model3qq <- renderPlot({
    
    residuals <- resid(model3lm())
    qqnorm(residuals)
    qqline(residuals)
    
  })
  
  #Output boxplots of residual by month for each model
  output$model1monthResidBox <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    
    modelXmonthResidBox(model1Data(), model1lm())
    
  })
  
  output$model2monthResidBox <- renderPlot({
    
    if(model2Form() == "")
      return(NULL)
    
    modelXmonthResidBox(model2Data(), model2lm())
    
  })
  
  output$model3monthResidBox <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    modelXmonthResidBox(model3Data(), model3lm())
    
  })
  
  #Ouutput boxplots of residual by year for each model
  output$model1yearResidBox <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    
    modelXyearResidBox(model1Data(), model1lm())
    
  })
  
  output$model2yearResidBox <- renderPlot({
    
    if(model2Form() == "")
      return(NULL)
    
    modelXyearResidBox(model2Data(), model2lm())
    
  })
  
  output$model3yearResidBox <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    modelXyearResidBox(model3Data(), model3lm())
    
  })
  
  #Output a plot of 10 fold cross-validation for each model
  output$model1cvalFolds <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    
    f <- formula(model1Form())
    Data <- model1Data()
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10)
    
  })
  
  output$model2cvalFolds <- renderPlot({
    
    if(model2Form() == "")
      return(NULL)
    
    f <- formula(model2Form())
    Data <- model2Data()
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10)
    
  })
  
  output$model3cvalFolds <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    f <- formula(model3Form())
    Data <- model3Data()
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10)
    
  })
  
  #Output a boxplot of cross-validation folds for each model
  output$model1cvalBox <- renderPlot({
    
    if(model1Form() == "")
      return(NULL)
    
    f <- formula(model1Form())
    Data <- model1Data()
    model1 <- model1lm()
    b.sigma <- summary.lm(model1)$sigma
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10, plotit=FALSE)
    names <- c("Minimum MSE of folds: ", "Mean MSE of folds: ", "Median MSE of folds: ",
               "Maximum MSE of folds: ", "(Mean MSE of folds) / (Model MSE): ")
    boxplot(cvsum$MSE, ylab="MSE of folds", boxwex=0.4, range=100)
    abline(h=(b.sigma^2), col="red")
    abline(h=(mean(cvsum$MSE)), col="blue")
    
  })
  
  output$model2cvalBox <- renderPlot({
    
    if(model2Form() == "")
      return(NULL)
    
    f <- formula(model2Form())
    Data <- model2Data()
    model2 <- model2lm()
    b.sigma <- summary.lm(model2)$sigma
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10, plotit=FALSE)
    names <- c("Minimum MSE of folds: ", "Mean MSE of folds: ", "Median MSE of folds: ",
               "Maximum MSE of folds: ", "(Mean MSE of folds) / (Model MSE): ")
    boxplot(cvsum$MSE, ylab="MSE of folds", boxwex=0.4, range=100)
    abline(h=(b.sigma^2), col="red")
    abline(h=(mean(cvsum$MSE)), col="blue")

    
  })
  
  output$model3cvalBox <- renderPlot({
    
    if(model3Form() == "")
      return(NULL)
    
    f <- formula(model3Form())
    Data <- model3Data()
    model3 <- model3lm()
    model3 <- lm(f, data=Data)
    
    b.sigma <- summary.lm(model3)$sigma
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10, plotit=FALSE)
    names <- c("Minimum MSE of folds: ", "Mean MSE of folds: ", "Median MSE of folds: ",
               "Maximum MSE of folds: ", "(Mean MSE of folds) / (Model MSE): ")
    boxplot(cvsum$MSE, ylab="MSE of folds", boxwex=0.4, range=100)
    abline(h=(b.sigma^2), col="red")
    abline(h=(mean(cvsum$MSE)), col="blue")
    
  })
  
  #Output a table summarizing cross-validation folds for each model
  output$model1cvalTable <- renderTable({
    
    if(model1Form() == "")
      return(NULL)
    
    f <- formula(model1Form())
    Data <- model1Data()
    model1 <- model1lm()
    b.sigma <- summary.lm(model1)$sigma
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10, plotit=FALSE)
    stats <- c(min(cvsum$MSE), mean(cvsum$MSE), median(cvsum$MSE), max(cvsum$MSE), mean(cvsum$MSE)/(b.sigma^2))
    names <- c("Minimum MSE of folds: ", "Mean MSE of folds: ", "Median MSE of folds: ",
               "Maximum MSE of folds: ", "(Mean MSE of folds) / (Model MSE): ")
    cvout <- data.frame(names, signif(stats, 3))
    names(cvout) <- c(" ", "  ")
    cvout
    
  })
  
  output$model2cvalTable <- renderTable({
    
    if(model2Form() == "")
      return(NULL)
    
    f <- formula(model2Form())
    Data <- model2Data()
    model2 <- model2lm()
    b.sigma <- summary.lm(model2)$sigma
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10, plotit=FALSE)
    stats <- c(min(cvsum$MSE), mean(cvsum$MSE), median(cvsum$MSE), max(cvsum$MSE), mean(cvsum$MSE)/(b.sigma^2))
    names <- c("Minimum MSE of folds: ", "Mean MSE of folds: ", "Median MSE of folds: ",
               "Maximum MSE of folds: ", "(Mean MSE of folds) / (Model MSE): ")
    cvout <- data.frame(names, signif(stats, 3))
    names(cvout) <- c(" ", "  ")
    cvout
    
  })
  
  output$model3cvalTable <- renderTable({
    
    if(model3Form() == "")
      return(NULL)
    
    f <- formula(model3Form())
    Data <- model3Data()
    model3 <- model3lm()
    b.sigma <- summary.lm(model3)$sigma
    
    cvsum <- cvsummary(data=Data, form.lm=f, m=10, plotit=FALSE)
    stats <- c(min(cvsum$MSE), mean(cvsum$MSE), median(cvsum$MSE), max(cvsum$MSE), mean(cvsum$MSE)/(b.sigma^2))
    names <- c("Minimum MSE of folds: ", "Mean MSE of folds: ", "Median MSE of folds: ",
               "Maximum MSE of folds: ", "(Mean MSE of folds) / (Model MSE): ")
    cvout <- data.frame(names, signif(stats, 3))
    names(cvout) <- c(" ", "  ")
    cvout
    
  })
  
  #Generate a table of model forms, to remind the user what their selected models are
  output$modelReminder <- renderTable({
    Model <- c("Model 1", "Model 2", "Model 3")
    Form <- c(model1Form(), model2Form(), model3Form())
    table <- data.frame(Model, Form)
    table
  })

  predictedData <- reactive({
    
    if(all(c(model1Form(), model2Form(), model3Form())==""))
      return(NULL)
    
    cData <- contDataTrans()
    
    if(model1Form() != "") {
      model1Pred <- predictSeries(model1lm(), cData)
      model1Pred$Model <- rep("Model 1", nrow(model1Pred))
    } else {
      model1Pred <- data.frame(datetime=vector(), predicted=vector(), Model=vector())
    }
    if(model2Form() != "") {
      model2Pred <- predictSeries(model2lm(), cData)
      model2Pred$Model <- rep("Model 2", nrow(model2Pred))
    } else {
      model2Pred <- data.frame(datetime=vector(), predicted=vector(), Model=vector())
    }
    if(model3Form() != "") {
      model3Pred <- predictSeries(model3lm(), cData)
      model3Pred$Model <- rep("Model 3", nrow(model3Pred))
    } else {
      model3Pred <- data.frame(datetime=vector(), predicted=vector(), Model=vector())
    }
    
    predictions <- rbind(model1Pred, model2Pred, model3Pred)
    predictions
    
  })
  
  output$predictTable <- renderDataTable({
    
    data <- predictedData()
    #Pull out the predictions from each model
    model1 <- data[data$Model=="Model 1", -3]
    model2 <- data[data$Model=="Model 2", -3]
    model3 <- data[data$Model=="Model 3", -3]
    #Merge the predictions
    merged <- merge(model1, model2 , by="datetime", all=TRUE, suffixes=c(".1",".2"))
    merged <- merge(merged, model3, by="datetime", all=TRUE, suffixes=c("", ".3"))
    names(merged) <- c("datetime", "Model1", "Model2", "Model3")
    merged
    
  })
  
  output$predictedSeries <- renderPlot({
      
      sData <- filtDatawD()
      dVars <- paste0(c("", "cube", "sqr", "log", "sqrt"), input$dVar)
      sData <- sData[,names(sData) %in% c("datetime", dVars)]
      names(sData)[names(sData) != "datetime"] <- "Measured"
      
      predictions <- predictedData()

      antiF1 <- model1antiFunction()
      antiF2 <- model2antiFunction()
      antiF3 <- model3antiFunction()
      bcf1 <- model1bcf()
      bcf2 <- model2bcf()
      bcf3 <- model3bcf()
      
      if(input$native=="native units") {
        predictions[predictions$Model == "Model 1", "predicted"] <- 
          bcf1 * antiF1(predictions[predictions$Model == "Model 1", "predicted"])
        predictions[predictions$Model == "Model 2", "predicted"] <- 
          bcf2 * antiF2(predictions[predictions$Model == "Model 2", "predicted"])
        predictions[predictions$Model == "Model 3", "predicted"] <- 
          bcf3 * antiF3(predictions[predictions$Model == "Model 3", "predicted"])
        sData$Measured <- antiF1(sData$Measured)
      }
      
      colors <- c("#e41a1c", "#377eb8", "#4daf4a")
    

      p <- ggplot() +
        geom_line(mapping=aes(x=datetime, y=predicted, color=Model), data=predictions) +
        scale_x_datetime() +
        scale_color_manual(values=colors) +
        geom_point(mapping=aes(x=datetime, y=Measured), shape=17, size=3, data=sData)
      p
    
  })
  
  observe({
    brush <- input$predictBrush
    if (!is.null(brush)) {
      rangesPredict$x <- c(as.POSIXct(brush$xmin, origin="1970-01-01"), as.POSIXct(brush$xmax, origin="1970-01-01"))
    } else {
      rangesPredict$x <- NULL
      rangesPredict$y <- NULL
    }
  })
  
  output$predictedSeriesZoomed <- renderPlot({
    
    sData <- filtDatawD()
    dVars <- paste0(c("", "cube", "sqr", "log", "sqrt"), input$dVar)
    sData <- sData[,names(sData) %in% c("datetime", dVars)]
    names(sData)[names(sData) != "datetime"] <- "Measured"
    
    predictions <- predictedData()
    
    antiF1 <- model1antiFunction()
    antiF2 <- model2antiFunction()
    antiF3 <- model3antiFunction()
    bcf1 <- model1bcf()
    bcf2 <- model2bcf()
    bcf3 <- model3bcf()
    
    if(input$native=="native units") {
      predictions[predictions$Model == "Model 1", "predicted"] <- 
        bcf1 * antiF1(predictions[predictions$Model == "Model 1", "predicted"])
      predictions[predictions$Model == "Model 2", "predicted"] <- 
        bcf2 * antiF2(predictions[predictions$Model == "Model 2", "predicted"])
      predictions[predictions$Model == "Model 3", "predicted"] <- 
        bcf3 * antiF3(predictions[predictions$Model == "Model 3", "predicted"])
      sData$Measured <- antiF1(sData$Measured)
    }

      if(!is.null(rangesPredict$x)) {
        predictions <- predictions[predictions$datetime>rangesPredict$x[1] & predictions$datetime<rangesPredict$x[2],]
        sData <- sData[sData$datetime>rangesPredict$x[1] & sData$datetime<rangesPredict$x[2],]
      }
      colors <- c("#e41a1c", "#377eb8", "#4daf4a")
    
      p <- ggplot() +
        geom_line(mapping=aes(x=datetime, y=predicted, color=Model), data=predictions) +
        scale_x_datetime() +
        scale_color_manual(values=colors)+
        geom_point(mapping=aes(x=datetime, y=Measured), shape=17, size=3,data=sData)
      p
    
  })
  
  #UI to select which model to validate
  output$valModelSelectUI <- renderUI({
    
    ch <- vector()
    if(model1Form() != "") {
      ch <- c(ch, "Model 1")
    } 
    if(model2Form() != "") {
      ch <- c(ch, "Model 2")
    }
    if(model3Form() != "") {
      ch <- c(ch, "Model 3")
    }
    
    selectInput("valModel","Model to validate", choices = ch)
    
  })
  
  #Find which model to use for validation
  valModellm <- reactive({
    #Select the model based on the valModel input
    if(input$valModel == "Model 1") {
      model <- model1lm() 
    } else if(input$valModel == "Model 2") {
      model <- model2lm()
    } else if(input$valModel == "Model 3") {
      model <- model3lm()
    } else {
      model <- NULL
    }
    model
  })
  
  #Get predictions for the model thats being validated
  predictedDataVal <- reactive({
    
    cData <- valContDataTrans()
    acData <- allContDataTrans()
    print("acData")
    print(head(acData))
    print("cData")
    print(head(cData))
    model <- valModellm() 
    
    #Get the predictions, or return an empty data frame if no model is selected
    if(is.null(model)) {
      predictions <- data.frame()
    } else {
      predictions <- predictSeries(model, cData)
    }
    predictions
    
  })
  
  #Data used for thentwo validation plots - adding and subtracting sigma
  valPlotData <- reactive({
    print("prePreData")
    pData <- predictedDataVal()
    print("pData")
    print(head(pData))
    names(pData) <- c("datetime", "P")
    
    model <- valModellm()
    rmse <- summary.lm(model)
    rmse <- rmse$sigma #Calculate the rmse for the model
    
    pData$pUp1 <- pData$P + rmse #Predicted value plus one sigma
    pData$pDown1 <- pData$P - rmse #Predicted value minus one sigma
    pData$pUp2 <- pData$P + 2 * rmse #You get the idea
    pData$pDown2 <- pData$P - 2 * rmse
    pData$pUp3 <- pData$P + 3 * rmse
    pData$pDown3 <- pData$P - 3 * rmse
    
    pData
    
  })
  
  valPlotPoints <- reactive({
    
    #Find the transformation function
    if(input$valModel == "Model 1") {
      transFunction <- model1Function()
    } else if(input$valModel == "Model 2") {
      transFunction <- model2Function()
    } else if(input$valModel == "Model 3") {
      transFunction <- model3Function()
    } else {
      transFunction <- function(x) {x}
    }
    
    #Get the validation points and apply the transformation
    points <- valSampleData()
    cens <- valIsCensored()
    if(nrow(points) != 0) {
      points <- points[, c("datetime", input$dVar)]
      names(points)[2] <- "Measured"
      points$Measured <- transFunction(points$Measured)
      cens <- cens[, c("datetime", input$dVar)]
      points$cens <- cens[, 2]
    }
    points
    
  })
  
  #Plot the predicted value in the validation data range with sigma bands
  output$valPlot <- renderPlot({
    #Get the continuous time series data for the plot
    data <- valPlotData()
    end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
    data <- data[data$datetime > end,]
    #Get the points, if any
    points <- valPlotPoints()

    p <- ggplot(data = data) +
      geom_ribbon(aes(x=datetime, ymin=pDown3, ymax=pUp3), fill="#BFDBFF") +
      geom_ribbon(aes(x=datetime, ymin=pDown2, ymax=pUp2), fill="#CCE1FF") +
      geom_ribbon(aes(x=datetime, ymin=pDown1, ymax=pUp1), fill="#F0F4FC") +
      geom_line(aes(x = datetime, y = P), color="steelblue3") +
      geom_point(data=points, aes(x=datetime, y=Measured, shape=cens), color="orangered1", size=3) +
      scale_shape_manual(values = c(19, 1), guide=FALSE) +
      scale_x_datetime() +
      xlab("Datetime") + ylab("Modeled Value")
    p
  })
  
  #Observer for the brush on valPlot
  observe({
    brush <- input$valBrush
    if (!is.null(brush)) {
      rangesVal$x <- c(as.POSIXct(brush$xmin, origin="1970-01-01"), as.POSIXct(brush$xmax, origin="1970-01-01"))
    } else {
      rangesVal$x <- NULL
      rangesVal$y <- NULL
    }
  })

  #Zoomed region of the valPlot
  output$valPlotZoomed <- renderPlot({
    #Get the continuous data for the plot
    data <- valPlotData()
    end <- as.POSIXct(input$uploadDateRange[2], format="%Y-%m-%d")
    data <- data[data$datetime > end,]
    if(!is.null(rangesVal$x)) {
      data <- data[data$datetime > rangesVal$x[1] & data$datetime < rangesVal$x[2], ]
    }
    #Get the points, if any for the plot
    points <- valPlotPoints()
    if(!is.null(rangesVal$x)) {
      points <- points[points$datetime > rangesVal$x[1] & points$datetime < rangesVal$x[2], ]
    }
    
    p <- ggplot(data = data) +
      geom_ribbon(aes(x=datetime, ymin=pDown3, ymax=pUp3), fill="#BFDBFF") +
      geom_ribbon(aes(x=datetime, ymin=pDown2, ymax=pUp2), fill="#CCE1FF") +
      geom_ribbon(aes(x=datetime, ymin=pDown1, ymax=pUp1), fill="#F0F4FC") +
      geom_line(aes(x = datetime, y = P), color="steelblue3") +
      geom_point(data=points, aes(x=datetime, y=Measured, shape=cens), color="orangered1", size=3) +
      scale_shape_manual(values = c(19, 1), guide=FALSE) +
      scale_x_datetime() +
      xlab("Datetime") + ylab("Modeled Value")
    p
  })
  
})