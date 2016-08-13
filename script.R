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

ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
  grid.newpage()
  # Increase right margin if which.axis == "y"
  if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  g2 = ggplot_gtable(ggplot_build(plot2))
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  ia = which(g2$layout$name == axis.lab)
  ga = g2$grobs[[ia]]
  ax = ga$children[[2]]
  # Switch position of ticks and labels
  if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
  ax$grobs = rev(ax$grobs)
  if(which.axis == "x") 
    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  # Add new row or column for axis label
  if(which.axis == "x") {
    g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    g = gtable_add_rows(g, g2$heights[1], 1)
    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
  } else {
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
  }
  # Draw it
  grid.draw(g)
}



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


#########################################################################################
server <- shinyServer(function(input, output) {
  datasetInput <- reactive({
    
  })
  
  output$Graph <- renderPlot({
    nameVar1 <- input$variableDeath
    variab1 <- names(newData)[names(newData) %in% nameVar1]
    visual1 <- as.data.frame(cbind(newData$Year, newData[, variab1]))
    nameVar2 <- input$variableTotal
    variab2 <- names(newData)[names(newData) %in% nameVar2]
    visual2 <- as.data.frame(cbind(newData$Year, newData[, variab2]))
    visual12 <- merge(visual1, visual2, by = "V1")
    names(visual12) <- c("Year", nameVar1, nameVar2)
    
    refLine <- input$thres
    
    g1 <- ggplot(visual12, aes(visual12$Year,visual12[,names(visual12)[2]])) +
      geom_line(size = 1.1, colour="#000099") + ggtitle("Spent vs. Deaths") + 
      ylab(names(visual12)[2]) +
      theme(plot.title = element_text(lineheight=.8, size = 18,  face="bold")) +
      xlab("Studied Year") + 
      scale_x_continuous(breaks = seq(min(newData$Year), max(newData$Year), by = 1)) +
      theme(panel.background = element_rect(fill = 'white', colour = 'black'),
            axis.text.y=element_text(size=11), axis.title.y=element_text(size=13,face="bold.italic",colour="#000099"),
            axis.text.x=element_text(size=12), axis.title.x=element_text(size=15,face="bold.italic"))
    
    breaks <- seq(min(visual12[,names(visual12)[3]]),max(visual12[,names(visual12)[3]]),
                  (max(visual12[,names(visual12)[3]]-min(visual12[,names(visual12)[3]])))/5)
    
    g2 <- ggplot(visual12, aes(visual12$Year,visual12[,names(visual12)[3]])) +
      geom_line(size = 1.1, colour="#ff6666") + geom_vline(xintercept = refLine, size = 1.1) +
      ylab(names(visual12)[3]) +
      theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
            axis.text=element_text(size=11), axis.title=element_text(size=13,face="bold.italic",colour="#ff6666")) +
      geom_hline(yintercept = breaks[2]) + geom_hline(yintercept = breaks[3]) +
      geom_hline(yintercept = breaks[4]) + geom_hline(yintercept = breaks[5])
    
    ggplot_dual_axis(g1, g2, "y")
    
  })
  
  output$Table2  <- renderGvis({ 
    nameVar1 <- input$variableDeath
    variab1 <- names(newData)[names(newData) %in% nameVar1]
    visual1 <- as.data.frame(cbind(newData$Year, newData[, variab1]))
    nameVar2 <- input$variableTotal
    variab2 <- names(newData)[names(newData) %in% nameVar2]
    visual2 <- as.data.frame(cbind(newData$Year, newData[, variab2]))
    visual12 <- merge(visual1, visual2, by = "V1")
    names(visual12) <- c("Year", nameVar1, nameVar2)
    visual12 <- visual12[visual12$Year == input$thres,]
    
    gvisTable(visual12, , options=list(width=1000, height="automatic")) 
    
  })
  
  output$Pie  <- renderGvis({
    threshold <- input$thres
    datos <- newData[newData$Year == threshold,6:(dim(newData)[2]-1)]
    newDatos <- cbind(names(datos), as.data.frame(t(datos), row.names = TRUE))
    names(newDatos) <- c("Cause of Death", "Number of Records")
    gvisPieChart(newDatos, options=list( width=1000, height=400,
                                         title=paste('Death percentage in year',input$thres)))
    
  })
  
  output$Table  <- renderGvis({ 
    threshold <- input$thres
    datos <- newData[newData$Year == threshold,6:(dim(newData)[2]-1)]
    newDatos <- cbind(names(datos), as.data.frame(t(datos), row.names = TRUE))
    names(newDatos) <- c("Cause of Death", "Number of Records")
    gvisTable(newDatos, options=list( width=1000, height=350,
                                      title=paste('Death records in year',input$thres))) #, formats=list(Value="#,###"))
    
  })
  
  output$Graph2  <- renderGvis({
    nameVar <- c(names(newData)[4],names(newData)[18])
    visual1 <- as.data.frame(cbind(newData$Year, newData[, nameVar]))
    names(visual1)[1] <- "Year"
    Line <- gvisLineChart(visual1, xvar="Year", yvar=nameVar,
                          options=list(title="Totals per Year", legend="bottom",
                                       titleTextStyle="{color:'red', fontSize:18}",
                                       vAxis="{gridlines:{color:'red', count:5}}",
                                       hAxis="{title:'Years', titleTextStyle:{color:'blue'}}",
                                       series="[{color:'green', targetAxisIndex: 0},
                                       {color: 'blue',targetAxisIndex:1}]",
                                       curveType="function", width="automatic", height=500))
  })
  
  output$Table3  <- renderGvis({ 
    nameVar2 <- input$variableTotal
    variab2 <- names(newData)[names(newData) %in% nameVar2]
    visual2 <- as.data.frame(cbind(newData$Year, newData[, variab2]))
    names(visual2) <- c("Year", variab2)
    visual2$`Units variance from previous year` <- c(0,diff(visual2[,2]))
    
    for (i in visual2$Year) {
      visual2$`Variation over 2014` <- round(visual2[,variab2]/visual2[dim(visual2)[1],variab2]*100,2)
    }
    visual2$`Percentage variance from previous year` <- round(c(0,diff(visual2[,4])),2)
    visual2$`Variation over 2014` <- paste0(visual2$`Variation over 2014`,"%")  
    visual2$`Percentage variance from previous year`<- paste0(visual2$`Percentage variance from previous year`,"%")
    
    gvisTable(visual2, options=list(width=1000, height=550,
                                    title='Death records in year'))
  })
  
  output$barPlot  <- renderGvis({ 
    nameVar2 <- input$variableTotal
    variab2 <- names(newData)[names(newData) %in% nameVar2]
    visual2 <- as.data.frame(cbind(newData$Year, newData[, variab2]))
    names(visual2) <- c("Year", variab2)
    
    gvisColumnChart(visual2, options=list(width=850, height="automatic",
                                          title=paste(variab2,"across the years"),
                                          colors= 'tomato')) 
  })
  
})

