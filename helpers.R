options(stringsAsFactors=FALSE)

cvsummary <- function(data, form.lm, m = 3, dots = FALSE, seed = 29, plotit = c("Observed", "Residual"), 
                      main = "Small symbols show cross-validation predicted values",  legend.pos = "topleft", printit = TRUE) {
  gphtype <- ""
  if (is.logical(plotit)) {
    if (plotit) 
      gphtype <- "Observed"
  }
  else if (is.character(plotit)) {
    if (!(plotit[1] %in% c("Observed", "Residual", ""))) 
      stop(paste("Illegal argument plotit =", plotit[1]))
    gphtype <- plotit[1]
    if (plotit[1] %in% c("Observed", "Residual")) 
      plotit <- TRUE
  }
  else stop("Argument plotit must be logical or character")
  if (class(form.lm) == "formula") 
    form <- form.lm
  else if (class(form.lm) %in% c("call", "lm")) 
    form <- formula(form.lm)
  else stop("form.lm must be formula or call or lm object")
  formtxt <- deparse(form)
  mf <- model.frame(form, data = data)
  ynam <- attr(mf, "names")[attr(attr(mf, "terms"), "response")]
  data.lm <- lm(mf)
  tm <- terms(mf)
  xcolumns <- labels(tm)
  n <- nrow(data)
  data[, ynam] <- model.response(mf)
  data[, "Predicted"] <- predict(data.lm)
  data[, "cvpred"] <- numeric(n)
  yval <- mf[, ynam]
  if (gphtype == "Residual") 
    yval <- yval - data[, "Predicted"]
  if (!is.null(seed)) 
    set.seed(seed)
  n <- dim(data)[1]
  rand <- sample(n)%%m + 1
  foldnum <- sort(unique(rand))
  for (i in foldnum) {
    rows.in <- rand != i
    rows.out <- rand == i
    subs.lm <- lm(form, data = data[rows.in, ])
    data[rows.out, "cvpred"] <- predict(subs.lm, newdata = data[rows.out, 
                                                                ])
  }
  if (length(xcolumns) == 1) {
    stline <- TRUE
    xnam <- xcolumns
  }
  else {
    stline <- FALSE
    xnam <- "Predicted"
  }
  if (printit) {
    # options(digits = 3)
    # print(anova(data.lm))
    # cat("\n")
  }
  if (plotit) {
    oldpar <- par(mar = par()$mar - c(1, 0, 2, 0))
    on.exit(par(oldpar))
    coltypes <- palette()[c(2, 3, 6, 1, 4:5, 7)]
    if (m > 7) 
      coltypes <- c(coltypes, rainbow(m - 7))
    ltypes <- 1:m
    ptypes <- 2:(m + 1)
    par(lwd = 2)
    if (stline) 
      xlab <- xnam
    else {
      xlab <- "Predicted (fit to all data)"
      cat("\n")
      warning(paste("\n\n As there is >1 explanatory variable, cross-validation\n", 
                    "predicted values for a fold are not a linear function\n", 
                    "of corresponding overall predicted values.  Lines that\n", 
                    "are shown for the different folds are approximate\n"))
    }
    ylab <- ynam
    if (gphtype == "Residual") 
      ylab <- paste(ynam, " (offset from predicted using all data)")
    plot(as.formula(paste("yval ~", xnam)), data = data, 
         ylab = ylab, type = "p", pch = ptypes[rand], col = coltypes[rand], 
         cex = 1.25, xlab = xlab)
    title(main = main, cex = 1.05)
    if (dots) {
      with(data, points(as.formula(paste("yval ~", xnam)), 
                        data = data, type = "p", pch = 16, col = coltypes[rand], 
                        cex = 1))
    }
  }
  
  fold_number <- vector()
  MSE <- vector()
  if (printit | plotit) {
    sumss <- 0
    sumdf <- 0
    for (i in foldnum) {
      rows.in <- rand != i
      rows.out <- rand == i
      n.out <- sum(rows.out)
      resid <- data[rows.out, ynam] - data[rows.out, "cvpred"]
      ss <- sum(resid^2)
      sumss <- sumss + ss
      if (printit) {
        fold_data <- t(cbind(data[rows.out, c(xnam, "cvpred", 
                                              ynam)], resid))
        rownames(fold_data) = c(xnam, "cvpred", ynam, 
                                "CV residual")
        #cat("\nfold", i, "\n")
        fold_number[length(fold_number)+1] <- i
        #cat("Observations in test set:", n.out, "\n")
        #print(fold_data, collab = rep("", n.out))
        #cat("\nSum of squares =", round(ss, 2), "   Mean square =", 
        #round(ss/n.out, 2), "   n =", n.out, "\n")
        MSE[length(MSE) + 1] <- signif((ss/n.out), 3)
      }
      if (plotit) {
        xval <- data[rows.out, xnam]
        nminmax <- c(which.min(xval), which.max(xval))
        cvpred <- data[rows.out, "cvpred"]
        if (gphtype == "Residual") 
          cvpred <- cvpred - data[rows.out, "Predicted"]
        points(xval, cvpred, col = coltypes[i], pch = ptypes[i], 
               cex = 0.75, lwd = 1)
        n1 <- which.min(xval)
        n2 <- which.max(xval)
        fold.lm <- lm(cvpred ~ xval)
        fold.b <- coef(fold.lm)
        lines(xval[c(n1, n2)], fold.b[1] + fold.b[2] * 
                xval[c(n1, n2)], col = coltypes[i], lty = ltypes[i])
        topleft <- par()$usr[c(1, 4)]
        par(lwd = 1, col = 1)
        legend(x = legend.pos, legend = paste("Fold", 
                                              1:m), pch = ptypes, lty = ltypes, col = coltypes, 
               cex = 0.75)
      }
    }
  }
  sumdf <- sum(!is.na(data[, "Predicted"]))
  if (printit) {
    # cat("\nOverall", "(Sum over all", n.out, "folds)", "\n")
    # print(c(ms = sumss/sumdf))
  }
  # attr(data, "ms") <- sumss/sumdf
  # attr(data, "df") <- sumdf
  data <- data.frame(fold_number, MSE)
  invisible(data)
}

scaleLog <- function(dataset) {
  dMin <- min(dataset)
  dMax <- max(dataset)
  lowerPower <- floor(log10(dMin))
  higherPower <- ceiling(log10(dMax))
  minLimit <- 10^lowerPower
  maxLimit <- 10^higherPower
  return(c(minLimit,maxLimit))
}

scaleLN <- function(dataset) {
  dMin <- min(dataset)
  dMax <- max(dataset)
  lowerPower <- floor(log(dMin))
  higherPower <- ceiling(log(dMax))
  breaks <- exp(lowerPower:higherPower)
  breaks <- signif(breaks,3)
  return(breaks)
} 

#Generate a model summary table, given a form as a string and data, and a list of observations
#to remove - given by a character string with integers separated by commas
modelXSummary <- function(modelForm, Data, removed, bcf) {
  if(modelForm == "")
    return(NULL)
  f <- formula(modelForm)
  
  Data <- Data[!(rownames(Data) %in% as.numeric(strsplit(removed, "[^[:alnum:]]")[[1]])),]
  
  # form <- strsplit(modelForm, "[^[:alnum:]]")[[1]]
  form <- strsplit(gsub(" ", "+", gsub("~", "+", modelForm)), "\\+")[[1]]
  form <- form[!(form %in% c("", " "))]
  logtrans <- (substr(form[1], 1, 3) == "log")
  
  model1 <- lm(f, data=Data)
  lm.stats <- summary.lm(model1)
  b.count <- nrow(na.omit(Data[,form]))
  b.sigma <- lm.stats$sigma
  summ <- summary(model1)
  b.rsquared <- summ$r.squared
  b.adjrsquared <- summ$adj.r.squared
  if(logtrans==FALSE) { #!!!
    b.uMSPE <- (lm.stats$sigma/mean(Data[,form[1]]))*100
    b.lMSPE <- (lm.stats$sigma/mean(Data[,form[1]]))*100
  } else {
    b.uMSPE <- ((10^b.sigma)-1)*100
    b.lMSPE <- (1-(10^(-1*b.sigma)))*100
  }
  cv_output <- CV(model1)
  print(cv_output)
  b.AIC <- cv_output["AIC"]
  b.BIC <- cv_output["BIC"]
  b.CV <- cv_output["CV"]
  b.mat <- data.frame(b.count, b.rsquared, b.adjrsquared, b.sigma, b.uMSPE, b.lMSPE, b.CV, b.AIC, b.BIC, bcf)
  rnames <- c("Observations", "R2", "adjR2", "RMSE", "Upper MSPE (90%)", "Lower MSPE (90%)", "CV", "AIC", "BIC", "BCF")
  b.mat <- t(b.mat)
  # row.names(b.mat) <- rnames
  b.mat <- data.frame(rnames, b.mat[,1])
  colnames(b.mat) <- c("","Model")
  return(b.mat)
}

#Generate a table of dates, values, and outlier test criteria, with a flag for those values
#exceding the test criteria. Takes the same arguments as modelXSummary, with an additional variable
#to indicate the sorting column for the output table
modelXOutliers <- function(model, data) {

  output <- data
  output$datetime <- as.character(output$datetime)
  #Calculate leverage, cook's D and dfits, and studentized residual for each observation
  output$CooksD <- cooks.distance(model)
  output$DFFITS <- dffits(model)
  output$Lev <- hatvalues(model)
  output$Resid <- studres(model)

  #Calculate the test criteria
  p <- length(model$coefficients)
  n <- nrow(model$model)
  cvlev <- ((3*p)/n)
  cvdfit <- 2 * sqrt(p/n)
  cvcook <- qf(0.1, p + 1, n - p)
  
  output$Flag <- ""
  #Apply a 'C' flag for cook's distance
  output[output$CooksD > cvcook,"Flag"] <- paste0(output[output$CooksD > cvcook,"Flag"], "C")
  #Apply a 'D' flag for dffits
  output[abs(output$DFFITS) > cvdfit,"Flag"] <- paste0(output[abs(output$DFFITS) > cvdfit,"Flag"], "D")
  #Apply a 'L' flag for leverage
  output[output$Lev > cvlev,"Flag"] <- paste0(output[output$Lev > cvlev,"Flag"], "L")
  #Apply a 'R' flag for studentized residual
  output[abs(output$Resid) > 3,"Flag"] <- paste0(output[abs(output$Resid) > 3,"Flag"], "R")
  
  nm <- names(output)
  output$point <- row.names(output)
  output <- output[,c("point", nm)]
  names(output)[1] <- ""
  output[,3:7] <- signif(output[,3:7], 3)
  
  return(output)
  
}

predictSeries <- function(model, timeSeriesData) {
  
  coefficients <- model$coefficients
  variables <- names(coefficients)

  timeSeriesData <- timeSeriesData[,names(timeSeriesData) %in% c("datetime", variables)]
  timeSeriesData <- na.omit(timeSeriesData)
  N <- nrow(timeSeriesData)
  datetime <- timeSeriesData$datetime
  timeSeriesData <- timeSeriesData[,names(timeSeriesData) != "datetime", drop=FALSE]
  
  if("(Intercept)" %in% variables) {
    predicted <- rep(coefficients[variables=="(Intercept)"], N)
  } else {
    predicted <- rep(0, nrow(timeSeriesData))
  }
  
  var <- variables[variables != "(Intercept)"]
  for(i in var) {
    coef <- coefficients[variables==i]
    predicted <- predicted + coef * timeSeriesData[,i]
  }
  
  predictedSeries <- data.frame(datetime, predicted)
  
  return(predictedSeries)
  
}

#Produce a duration curve
durationCurve <- function(continuousData, sampleData, durVariable, respVariable, quarter) {
  
  pred1 <- continuousData
  cal1 <- sampleData
  durVar <- durVariable
  sMonth <- (3 * quarter) - 2
  eMonth <- 3 * quarter
  if(quarter != 0) {
    label <- switch(quarter, "January-March", "April-June", "July-September", "October-December")
  } else {
    label <- "January-December"
  }
  
  #Merge the sample data (cal1) and the continuous data (pred)
  temp1 <- mergeNearest(pred1, dates.left="datetime", all.left=TRUE, suffix.left="", cal1,
                        dates.right="datetime", suffix.right="y", max.diff="2 hours")
  
  # Exclude years with population streamflow data outside the time span of sample data
  durVar <- paste(durVar, ".", sep="")
  caldatemin<-min(cal1$datetime)
  caldatemax<-max(cal1$datetime)
  predcal1<-subset(temp1, datetime. >= caldatemin & datetime. <= caldatemax)
  predcal2<-subset(predcal1,!(is.na(predcal1[,durVar])))
  
  # Compute y axis boundaries
  ymin<-floor(log10(min(predcal2[,durVar])))
  ymax<-ceiling(log10(max(predcal2[,durVar])))
  ystep<-1
  
  # Define x axis boundaries
  xmin<-0.0
  xmax<-100.0
  xstep<-10.0
  
  #Keep data from the relevant months(if quarter is 0, skip this - all months)
  if(quarter != 0) {
    predcal2<-subset(predcal2,!(is.na(predcal2[,durVar])) & predcal2$month.>=sMonth & predcal2$month.<=eMonth)
  }
  
  # Compute y axis boundaries
  ymin<-floor(log10(min(predcal2[,durVar])))
  ymax<-ceiling(log10(max(predcal2[,durVar])))
  ystep<-1
  
  # Define x axis boundaries
  xmin<-0.0
  xmax<-100.0
  xstep<-10.0
  
  # Compute cumulative frequency percent for population streamflow
  tab<-table(predcal2[,durVar])
  # The following statement gets the cumulative frequency percent
  cpct<-100*cumsum(tab)/length(predcal2[,durVar])
  # The following statement gets values associated with the cumulative frequency percent
  pQ<-as.numeric(as.character(names(tab)))
  temp<-data.frame(pQ,cpct)
  tempnames<-c(durVar,"cpct")
  names(temp)<-tempnames
  predcal3<-merge(predcal2,temp,by=durVar,all=TRUE)
  
  par(lwd=2,xaxs="i",yaxs="i",mgp=c(3,0.3,0),cex=1.00,font=2)
  # Plot cumulative frequency curve for population streamflow
  plot(predcal3$cpct,log10(predcal3[,durVar]),type="l",lty=1,lwd=1,axes=F,xlim=c(xmin,xmax),ylim=c(ymin,ymax),ylab="",xlab="")
  # Overlay when samples were collected on the cumulative frequency curve
  predcal4<-subset(predcal3,!(is.na(predcal3[respVariable])))
  predcal4 <- subset(predcal4, !(duplicated(predcal4$datetime.y)))
  points(predcal4$cpct,log10(predcal4[,durVar]),pch=1,mkh=0.07)
  
  # Draw axes on plot
  axis(side=2,labels=T,adj=1,las=2,tck=0.025,at=seq(ymin,ymax,ystep),lwd=2,cex=0.8)
  axis(side=2,labels=F,tck=0.015,at=seq(ymin,ymax,ystep/4),lwd=2)
  axis(side=1,labels=T,adj=0.5,tck=0.025,at=seq(xmin,xmax,xstep),lwd=2,cex=0.8)
  axis(side=1,labels=F,tck=0.015,at=seq(xmin,xmax,xstep/10),lwd=2)
  axis(side=4,labels=F,tck=0.025,at=seq(ymin,ymax,ystep),lwd=2)
  axis(side=4,labels=F,tck=0.015,at=seq(ymin,ymax,ystep/4),lwd=2)
  axis(side=3,labels=F,tck=0.025,at=seq(xmin,xmax,xstep),lwd=2)
  axis(side=3,labels=F,tck=0.015,at=seq(xmin,xmax,xstep/10),lwd=2)
  
  # Add title to plot
  mtext(label, outer=F,line=0.2,side=3,at=50,adj=0.5,cex=0.80)
}

#Look for < signs in all columns, remove them, convert them to numeric
readCensored <- function(file, start=NA, end=NA, dateformat="", action="replace") {
  
  censData <- read.csv(file, stringsAsFactors = FALSE)
  print("censData")
  print(head(censData))
  print(start)
  if(!(is.na(start))) {
    print("Converting start")
    start <- as.POSIXct(start, format="%Y-%m-%d")
    print("Converting end")
    end <- as.POSIXct(end, format="%Y-%m-%d")
    print("converting censdata$datetime")
    censData$datetime <- as.POSIXct(censData$datetime, format=dateformat)
    censData <- censData[censData$datetime > start & censData$datetime < end,]
    censData$datetime <- as.character(censData$datetime, format=dateformat)
  }
  isCensored <- censData
  notDate <- names(censData)[names(censData) != "datetime"]
  for(i in notDate) {
    isCensored[,i] <- grepl("<", isCensored[,i])
    censData[,i] <- gsub("<", "", censData[,i])
    censData[,i] <- as.numeric(censData[,i])
    censData[isCensored[,i],i] <- 0.5 * censData[isCensored[,i],i]
  }
  output <- list(censData = censData, isCensored = isCensored)
  return(output) 
  
}

modelXeq <- function(form, model) {

  form <- form <- strsplit(gsub(" ", "+", gsub("~", "+", form)), "\\+")[[1]]
  form <- form[!(form %in% c("", " "))]
  logtrans <- (substr(form, 1, 3) == "log")
  
  coeff <- coef(model)
  names(coeff) <- NULL
  model_form <- paste(form[1], " = ", sep="")
  for(x in 2:length(form)){
    if(coeff[x] < 0) {
      model_form <- paste(model_form, paste(as.character(signif(abs(coeff[x]), 3)), "*", form[x], sep=" "), sep=" - ")
    } else {
      model_form <- paste(model_form, paste(as.character(signif(abs(coeff[x]), 3)), "*", form[x], sep=" "), sep=" + ")
    }
  }
  if(coeff[1] < 0) {
    model_form <- paste(model_form, as.character(signif(abs(coeff[1]), 3)), sep=" - ")
  } else {
    model_form <- paste(model_form, as.character(signif(abs(coeff[1]), 3)), sep=" + ")
  }
  
 model_form 
 
}

#Function to generate residual vs computed value plot
modelXovc <- function(form, model, isCens, isFlag) {
  
  temp <- data.frame(predict(model), y=resid(model))
  names(temp) <- c("Regression.Computed","Residuals")
  res <- temp$Residuals
  temp$censored <- isCens[,2]
  temp$flagged <- isFlag[,2]
  
  pl2 <- ggplot(temp, aes(x=Regression.Computed, y=Residuals)) + 
    geom_point(aes(shape = censored, color = flagged), size=1.3) +
    scale_shape_manual(values=c(19, 1), guide=FALSE) +
    scale_color_manual(values=c("black", "red"), guide=FALSE) +
    scale_y_continuous() +
    scale_x_continuous("Regression Computed") +
    geom_smooth(method="loess", size=1.0, se=FALSE) +
    geom_hline(aes(yintercept=0))
  
  pl2
  
}

#Function to generate computed vs. observed plot
modelXcvo <- function(form, model, data, func, antiFunc, bcf, isCens, isFlag) {
  
  #Break up the form into a list of variables
  form <- form <- strsplit(gsub(" ", "+", gsub("~", "+", form)), "\\+")[[1]]
  form <- form[!(form %in% c("", " "))]
  
  x <- antiFunc(predict(model)) * bcf
  y <- antiFunc(data[,form[1]])
  trans <- trans_new("t", func, antiFunc)
  
  temp <- data.frame(x,y)
  names(temp) <- c("Regression.Computed","Observed")
  temp$censored <- isCens[, 2]
  temp$flagged <- isFlag[, 2]
  
  pl1 <- ggplot(temp, aes(x=Regression.Computed, y=Observed)) + 
    geom_point(aes(shape=censored, color=flagged), size=1.3) +
    scale_x_continuous("Computed", trans=trans, expand=c(0,0)) +
    scale_y_continuous(trans=trans) +
    scale_shape_manual(values = c(19, 1), guide=FALSE) +
    scale_color_manual(values = c("black", "red"), guide=FALSE) +
    geom_abline(intercept=0, slope=1) +
    geom_smooth(method="loess", size=1.0, se=FALSE) +
    theme(plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"))
  pl1
  
}

#Function to generate date vs residual plot
modelXdvr <- function(data, model, isCens, isFlag) {
  
  temp <- data.frame(as.Date(data$datetime), resid(model))
  names(temp) <- c("Date","Residuals")
  temp$censored <- isCens[,2]
  temp$flagged <- isFlag[,2]
  res <- temp$Residuals
  s <- max(abs(res))
  s <- 1.1*s
  wilk_y <- extended(-1*max(extended(-1*s,s,5,only.loose=TRUE)),max(extended(-1*s,s,5,only.loose=TRUE)),5,only.loose=TRUE)
  pl3 <- ggplot(temp, aes(x=Date, y=Residuals)) + 
    geom_point(aes(shape = censored, color = flagged), size=1.3) +
    scale_x_date(labels=date_format("%Y-%m")) +
    scale_y_continuous(limits=c(-1*max(wilk_y),max(wilk_y)),breaks=wilk_y,expand=c(0,0),labels=comma) +
    scale_shape_manual(values=c(19, 1), guide = FALSE) +
    scale_color_manual(values = c("black", "red"), guide=FALSE) +
    geom_hline(aes(yintercept=0)) +
    geom_smooth(method="loess", size=1.0, se=FALSE) +
    theme(plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"))
  pl3
  
}

#Function to generate normal quantile vs residual plot
modelXnqvr <- function(model, isFlag) {
  
  Residuals <- resid(model)
  
  #Calculate normal quantiles
  temp <- data.frame(1:length(Residuals),Residuals)
  names(temp) <- c("Original.Pos", "Residuals")
  n <- length(temp[[1]])                             #number of obs/ sample size
  temp <- temp[order(temp$Residuals),]               #Rank by residual
  
  #Calculate plotting position with the Cunnane formula (a=0.4) Helsel & Hirsch p.23
  temp$Rank <- 1:n
  temp$Cunnane <- (temp$Rank - 0.4)/(n + 0.2)
  temp$Normal.Quantiles <- qnorm(temp$Cunnane)
  temp <- temp[order(temp$Original.Pos),]
  temp$flagged <- isFlag[, 2]
  
  transient <- temp[,c("Residuals","Normal.Quantiles","flagged")]
  res <- transient$Residuals
  s <- max(abs(res))
  s <- 1.1*s
  wilk_y <- extended(-1*max(extended(-1*s,s,5,only.loose=TRUE)),max(extended(-1*s,s,5,only.loose=TRUE)),5,only.loose=TRUE)
  pl4 <- ggplot(transient, aes(x=Normal.Quantiles, y=Residuals)) + geom_point(aes(color = flagged), size=1.3) +
    scale_x_continuous("Normal Quantiles", limits=c(-3,3), expand=c(0,0),labels=comma) +
    scale_y_continuous("Residuals",limits=c(-1*max(wilk_y),max(wilk_y)),breaks=wilk_y, expand=c(0,0),labels=comma) +
    scale_color_manual(values=c("black", "red"), guide=FALSE) +
    geom_hline(aes(yintercept=0)) +
    stat_smooth(method="lm", se=FALSE) +
    theme(plot.margin = unit(c(0.3,0.5,0.3,0.3), "cm"))
  pl4
}

#Function to generate boxplots of residual by month
modelXmonthResidBox <- function(data, model) {

  Residuals <- resid(model)
  
  # #Calculate extents of the boxplot plotting window
  s <- max(abs(Residuals))
  s <- 1.3*s
  wilk_y <- extended(-1*max(extended(-1*s,s,5,only.loose=TRUE)),max(extended(-1*s,s,5,only.loose=TRUE)),5,only.loose=TRUE)
  
  #Boxplot of residuals by month
  bplotData <- data.frame(as.numeric(month(data$datetime)), Residuals)
  names(bplotData) <- c("month", "Residual")
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  monWData <- months[as.numeric(levels(factor(bplotData$month)))]
  bplotData$month <- months[bplotData$month]
  bplotData$month <- factor(bplotData$month, monWData)
  boxplot(Residual ~ month, data=bplotData, xlab="Month", ylab="Residual",
          range=100, outline=FALSE, varwidth=TRUE, yaxt="n",yaxs="i", ylim=c(-1*max(wilk_y),max(wilk_y)), 
          labels=format(wilk_y,format="d",big.mark=","))
  for(xi in 1:length(monWData)) {
    text(x=xi,y=boxplot.stats(bplotData[bplotData$month==monWData[xi],2], coef=0)$stats[5],
         as.character(nrow(bplotData[bplotData$month==monWData[xi],])), pos=3)
  }
  axis(2,at=wilk_y,labels=format(wilk_y,format="d",big.mark=","),las=2)
  abline(h=0, col="gray60")
  
}

#Function to generate boxplots of residual by year
modelXyearResidBox <- function(data, model) {
  
  residuals_value <- resid(model)
  datetime <- data$datetime
  
  #Box plots of residuals by year
  resid_yr <- factor(year(datetime)) 
  s <- max(abs(residuals_value))
  s <- 1.2 * s
  wilk_y <- extended(-1 * max(extended(-1 * s,s, 5, only.loose=TRUE)), 
                     max(extended(-1 * s, s, 5, only.loose=TRUE)), 5, only.loose=FALSE)
  
  boxplot(residuals_value ~ resid_yr, xlab="Year", ylab="Residual", 
          range=100, outline=FALSE, horizontal=FALSE, names = levels(resid_yr), 
          ylim=c(-1*max(wilk_y),max(wilk_y)),yaxt="n",yaxs="i", varwidth=TRUE, las=2, boxwex=0.4)
  axis(2,at=wilk_y,labels=format(wilk_y,format="d",big.mark=","),las=2)
  abline(h=0, col="gray60")
  
  for(i in 1:length(levels(resid_yr))) {
    text(x=i,y=boxplot.stats(residuals_value[resid_yr==levels(resid_yr)[i]], coef=0)$stats[5],
         as.character(length(resid_yr[resid_yr==levels(resid_yr)[i]])), pos=3)
  }
  
}

#Say what season a date is intoda
season <- function(date) {
  season_atm <- function(dt) {
    mnth <- month(dt)
    if(mnth %in% c(12, 1, 2)) {
      ssn_atm <- "Winter"
    } else if(mnth %in% c(3, 4, 5)) {
      ssn_atm <- "Spring"
    } else if (mnth %in% c(6, 7, 8)) {
      ssn_atm <- "Summer"
    } else if (mnth %in% c(9, 10, 11)) {
      ssn_atm <- "Fall"
    }
    return(ssn_atm)
  }
  ssn <- sapply(date, season_atm)
  return(ssn)
}
  