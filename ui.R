library(shiny)

library(grid)
library(shiny)
library(gtable)
library(ggplot2)
library(reshape2)
library(googleVis)

# Read data
Expend <- read.csv("./Household_Expenditure_Alcohol_1985_to_2015.csv")
Afford <- read.csv("./Alcohol_Affordability_1980_to_2015.csv")
Deaths <- read.csv("./Alcohol_Related_Deaths_2001_to_2014.csv")

# Treatment of Expenditure over the years
Expend <- Expend[Expend$Year >= min(Deaths$Year) & Expend$Year <= max(Deaths$Year),]
Expend2 <- dcast(Expend, Year ~ Metric, sum)
names(Expend2) <- c("Year", "Expenditure on alcohol as a percentage of expenditure", "Household expenditure on alcohol", "Household expenditure Total")

# Treatment of Afford over the years
Afford <- Afford[Afford$Year >= min(Deaths$Year) & Afford$Year <= max(Deaths$Year),]
Afford <- Afford[Afford$Metric == "Alcohol price index",]
Afford2 <- dcast(Afford, Year ~ Metric, sum)
names(Afford2) <- c("Year", "Alcohol Price Index")

# Treatment of Deaths over the years
Deaths$ICD10_Code <- NULL
Deaths2 <- dcast(Deaths, Year + Metric ~ ICD10_Description, sum)
DeathsTotal <- Deaths2[Deaths2$Metric == "All persons",]
DeathsTotal$Metric <- NULL

# Merge datasets
newData <- merge(Expend2, Afford2)
newData <- merge(newData, DeathsTotal)
names(newData)[dim(newData)[2]] <- "Total number of Deaths"




# ui defines inputs and outputs
shinyUI(pageWithSidebar(
  headerPanel("Alcohol expenditure vs. Alcohol related Deaths in the UK"),
  sidebarPanel(
    selectInput('variableDeath', 'Select Death variable to represent',
                choices = names(newData)[6:length(names(newData))],
                selectize = FALSE),
    selectInput('variableTotal', 'Select Expenditure variable to represent',
                choices = names(newData)[3:5],
                selectize = FALSE),
    sliderInput(inputId = "thres",
                label = "Select the Year to display",
                value = min(newData$Year), min = min(newData$Year), 
                max = max(newData$Year), step = 1)
    
  ),
  mainPanel(
    
    tabsetPanel(
      tabPanel("Totals Summary",
               h3("Total Deaths vs. Spent on Alcohol per Year."),
               htmlOutput("Graph2"),
               h4("There is a clear increase of Deaths caused by alcohol and 
                  expenditure in Alcohol. In 2008, the number of deaths reached
                  a peak as the consumption of alcohol increases suddenly.")),
      tabPanel("Death vs. Spend exploration",
               h3("Comparison between several spend indicators and deaths 
                  caused by alcohol across the years."),
               h4("Please select the variables to represent and the Year you 
                  want to retrieve its results."),
               plotOutput("Graph"),
               htmlOutput("Table2")),
      tabPanel("Death split per Year", 
               h3("Alcohol main illnesses causing the Death and number of deaths
                  for the year selected."),
               h4("Please select one Year to display its results."),
               htmlOutput("Table"),
               htmlOutput("Pie")),
      tabPanel("Historical spent on alcohol",
               h3("Historical expenditures in alcohol"),
               h4("Please select a Expenditure to display."),
               htmlOutput("Table3"),
               htmlOutput("barPlot"))
      
      )
    
      )
  )
)
