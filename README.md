
This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

## Introduction

This application can be used to help develop and select a regression model relating measurements from a continuous water-quality monitor and streamgage to discretely sampled properties.

In general, as you use the app, you will move through the tabs from left to right, beginning with uploading data, and ending with the prediction time series. After the discussion of data requirements, this page will walk through each tab.

If you notice anything wrong with this application, let me know. 
 
Patrick Eslick  
peslick@usgs.gov  
785-832-3506

## Running modelDev Remotely

You will need to have the shiny, rmarkdown, scales, labeling, ggplot2, car, dataRetrieval, lubridate, smwrQW, smwrStats, XML, DAAG, and MASS packages installed. This could be done by running the following commands in an R Console (or RStudio):

``` r
install.packages("shiny") 
install.packages("shinydashboard")
install.packages("rmarkdown") 
install.packages("scales") 
install.packages("labeling")
install.packages("ggplot2") 
install.packages("car") 
install.packages("dataRetrieval", repos="https://owi.usgs.gov/R") 
install.packages("lubridate") 
install.packages("smwrQW", repos="https://owi.usgs.gov/R") 
install.packages("smwrStats", repos="https://owi.usgs.gov/R") 
install.packages("grid") 
install.packages("XML")
install.packages("DAAG")
install.packages("leaps")
install.packages("reshape2")
install.packages("MASS")
```
Once you have the packages installed, you can start the app with the following commands:

``` r
library(shiny)
runGitHub("PatrickEslick/modelDev", launch.browser=TRUE)
```
## Data requirements

Before you begin, you will need to prepare two data files, one containing merged discrete and continuous data, and one containing a continuous time series of measurements from the monitor. Both files should have a column called "datetime" with the complete date and time in one of the formats listed on the second tab of this application. Both files should share all of the continuous variables. For example, if you have specific conductance in your merged file, you must also have it in your continuous file. Any missing observations for any sample can be left blank, or filled in with "NA". Naming doesn't matter, as long as it's consistent accross the two files, and column names don't contain special characters. Do not include any other date variables, or transformations of variables. You will be able to add these later on. Your files might look like this:

**Merged file**

|datetime         | Cl | SC | Q  | Tby  |  
|:----------------|:--:|:--:|:--:|:----:|  
|1998-11-05 13:45 |7.74|56.0|862 |NA    |  
|1998-12-04 10:41 |79.0|234 |222 |NA    |  
|1998-02-01 11:06 |139 |300 |113 |NA    |   
|1998-03-15 12:05 |<5.0|12.1|50  |NA    |  
|...              |... |... |... |...   |   

**Continuous file**

|datetime         | Q  | SC | Tby |  
|:----------------|:--:|:--:|:---:|  
|1998-11-01 00:00 |100 |700 |NA   |  
|1998-11-01 01:00 |105 |701 |NA   |  
|1998-11-01 02:00 |107 |701 |NA   |  
|...              |... |... |...  |  

Notice that the example merged file has a censored data point "<5.0" in March 1998. Censored data points are allowed, and should be indicated with a "<" sign in front of the value. By default, these will be replaced by half the detection limit. You may want to consider using a different approach, especially if you have close to 5% or more censored data. 

## Upload/Examine Data

1. **Indicate the response variable** by typing the column heading of the response variable in the box. This should exactly match the column heading in your merged data file.
2. **Select your merged sample data file** by clicking the "Browse..." button and select the correct date format from the drop-down menu below.
3. **Select your continuous data file** as above.
4. **Click Upload Data** and your files will be uploaded and prepared for use in the rest of the app.

The table in the upper right will show the period of record (starting and ending dates) for each parameter. It will also indicate what portion of each variable is censored. A warning will be displayed if 5% or more of any parameter is censored.

Below the file-uploading controls, three tabs show various plots and statistics related to the data you uploaded.

#### Time series

You can view a time series of any of the parameters in your continuous data file by selecting it from the drop-down menu and clicking "Plot". Checking the "Log scale" checkbox will transform the axes of the plots into a base 10 log scale. You can zoom the lower plot by clicking and dragging over a region in the upper plot. The selected region can be cleared by single-clicking anywhere on the top plot.

#### Duration curves

You can view duration curves of any of the variables in your continuous file by selecting one from the drop-down menu and clicking "Plot". The solid line represents the percent excedence at each point along the x-axis and the logarithm of the measured value along the y-axis. The open circles represent the value and excedence of each sample. The same plot is repeated for each quarter.

#### Box plots

The top box plot shows the distribution of samples taken each month. Any values more than 1.5 times the inner-quartile above the median are shown as open circles.

Each of the lower boxplots compares the distribution of the continuous data with the distrubtion of points at the time of each sample. You can toggle between log and linear scales using the check box.

## Correlations/Transformations

This tab is meant to help narrow down which parameters you might want to include in your model, and how you might want to transform them. 

The scatter plot matrix illustrates the relationship between any two of your variables. If you have ruled out a variable, or simply want to declutter the plot, you can uncheck the box for that variable at the top of the tab. You can transform any of the variables using the transformations to the right of the scatter plot matrix. The transformations are arranged according to the ladder of powers. The bulging rule can be used to help determine which transformations you might want to use. The scatter matrix will update according to your choices.

You may also add seasonality variables here. They won't be shown in the scatter plot matrix, but will be shown in the table to the lower right, and can be used in the next tab.

At the top right, the correlation matrix shows the correlation coefficient between any two of your variables. 

On the bottom right, the "best" 3 models with a given number of variables are shown. You can use this table to help guide your decisions in the next tab. Keep in mind, if one of the seasonality variables is chosen for a model, its counterpart must also be included. For example, if sine is included, cosine must also be included.

Only the variables selected on this tab, with the transformations you have chosen, can be used in the next tab.

## Compare models

On this tab, you can compare up to three models at a time. The drop-down menu at the top of each column is used to select the variables you want to include in each model. Only variables selected on the previous tab will appear as options.

As you add variables to each model, a regression equation, statistics, and diagnostic plots will be given for each model in the "Model" tab of each column. In the plot showing computed vs observed values, the computed value has been re-transformed to its native units and multiplied by the bias correction factor (BCF) listed in the sumamry table.

The Data/Outliers tab gives a table of each point, its studentized residual, and statistics related to influence and leverage. You can sort by any column by clicking on the column heading. If a value of one of the statistics is considered "high" based on the test criteria, it will be indicated in the flag column. "C" indicates a high value of Cook's D - a measure of influence. "D" indicates a high value of DFFITS which is another measure of influence. "L" indicates a high leverage. "R" indicates a high studentized residual (an absolute value greater than 3). You can find points with any of these flags by typing it in the search box.

## Prediction Time Series

This tab uses each model to "predict" a time series of your response variable based on the continuous data file you provided. The table at the top is there to remind you which models you chose in the previous tab.

The sample points are overlayed as black triangles. You can zoom the plot by clicking and dragging on the top plot. The selected region is shown in the bottom plot.

