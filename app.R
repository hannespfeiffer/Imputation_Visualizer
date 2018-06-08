library(shiny)
library(ggplot2)
library(imputeTS)

ui <- fluidPage(
  
  headerPanel("Imputation Visualizer"),
  sidebarPanel(
    textOutput("imp_method"),
    checkboxInput("method1","Linear Interpolation",FALSE),
    checkboxInput("method2","Spline Interpolation",FALSE),
    checkboxInput("method3","Stineman Interpolation",FALSE),
    checkboxInput("method4","Structural Model & Kalman Smoothing",FALSE),
    checkboxInput("method5","ARIMA State Space Representation & Kalman Sm.",FALSE),
    checkboxInput("method6","Last Observation Carried Forward",FALSE),
    checkboxInput("method7","Next Observation Carried Backward",FALSE),
    checkboxInput("method8","Simple Moving Average",FALSE),
    checkboxInput("method9","Linear Weighted Moving Average",FALSE),
    checkboxInput("method10","Exponential Weighted Moving Average",FALSE),
    checkboxInput("method11","Mean Value",FALSE),
    checkboxInput("method12","Median Value",FALSE),
    checkboxInput("method13","Mode Value",FALSE),
    selectInput('display_method', 'Missing value display method', 
                c("Error Boundaries","Boxplots")),
    actionButton("generateMissingValues","Generate Missing Values"),
    actionButton("toggle","Toggle vals$missing")
  ),
  mainPanel(
    plotOutput('plot1', click="plot1_click", brush="plot1_brush"),
    plotOutput('plot2', click="plot2_click", brush="plot2_brush"),
    verbatimTextOutput("info")
  )
  
)

server <- function(input, output) {
  
  sunspot = read.table(file="sunspot.month.csv", 
                       header=TRUE, sep=",", dec=".")
  wholeyear_mask = sunspot$time%%1==0
  year = sunspot$time[wholeyear_mask]
  sunvalue = sunspot$value[wholeyear_mask]
  cycle = c(seq(4,11), rep(seq(1,11),23), seq(1:4))
  factor_cycle = factor(cycle)
  sun_df=data.frame(year,sunvalue,factor_cycle)
  
  markedpoints = data.frame(year = numeric(),
                            sunvalue = numeric(),
                            factor_cycle = factor())
  missingpoints = data.frame(year = numeric(),
                             sunvalue = numeric(),
                             factor_cycle = factor())
  duplicate <- sun_df
  
  vals <- reactiveValues(data = sun_df,
                         imputedDataSet = duplicate,
                         markedPoints = markedpoints,
                         missingPoints = missingpoints,
                         missing=FALSE,
                         missingIndices = NULL,
                         group1 = NULL,
                         group2 = NULL,
                         group3 = NULL,
                         group4 = NULL,
                         group5 = NULL,
                         group6 = NULL)
  
  
  observeEvent(input$plot1_click,{
    nearpoint <- nearPoints(vals$data,input$plot1_click, 
                  xvar = "year", yvar="sunvalue",
                  threshold = 5, maxpoints = 1)
    if (nrow(nearpoint) != 0){
      vals$markedPoints <- vals$markedPoints[0,]
      vals$markedPoints <- rbind(vals$markedPoints, data.frame(
                                 year=nearpoint$year,
                                 sunvalue=nearpoint$sunvalue,
                                 factor_cycle=nearpoint$factor_cycle))
      #vals$data$year[vals$data$year %in% c(nearpoint$year)]=NA
    }
  })
  observeEvent(input$plot2_click,{
    nearpoint <- nearPoints(vals$data,input$plot2_click, 
                            xvar = "year", yvar="sunvalue",
                            threshold = 5, maxpoints = 1)
    if (nrow(nearpoint) != 0){
      vals$markedPoints <- vals$markedPoints[0,]
      vals$markedPoints <- rbind(vals$markedPoints, data.frame(
                                 year=nearpoint$year,
                                 sunvalue=nearpoint$sunvalue,
                                 factor_cycle=nearpoint$factor_cycle))
      #vals$data$year[vals$data$year %in% c(nearpoint$year)]=NA
    }
  })
  
  output$imp_method <- renderText({
    "Imputation Methods"
  })

  output$info <- renderPrint({
    print("Marked point:")
    print(nearPoints(vals$data,input$plot1_click, 
                                  xvar = "year", yvar="sunvalue",
                                  threshold = 5, maxpoints = 1))
    print("Imputed Dataset:")
    print(vals$imputedDataSet)

  })
  
  output$plot1 <- renderPlot({
    if(vals$missing){
      plot1 <- ggplot(vals$imputedDataSet, aes(x = year, y = sunvalue)) 
    } else {
      plot1 <- ggplot(vals$data, aes(x = year, y = sunvalue)) 
    }
    plot1 = plot1 +
      labs(x = "Year", y = "Sunvalue") +
      ggtitle("Sunspot cycles\n\nLinear graph") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(0, "lines"),
            plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
            #axis.text.x = element_blank(),
            #axis.ticks.x = element_blank(),
            strip.background = element_rect(fill = "white"))   
      
      
    if(vals$missing){
      plot1 = plot1 +
              geom_line(data=vals$group1, color="grey30", size=0.4) +
              geom_line(data=vals$group2, color="grey30", size=0.4) +
              geom_line(data=vals$group3, color="grey30", size=0.4) +
              geom_line(data=vals$group4, color="grey30", size=0.4) +
              geom_line(data=vals$group5, color="grey30", size=0.4) +
              geom_line(data=vals$group6, color="grey30", size=0.4) 
    } else {
      plot1 = plot1 + geom_line(color="grey30", size=0.4) 
    }
    
    plot1 = plot1 + 
            geom_point(fill="white", shape=21,color="cornflowerblue") +
            geom_point(data=vals$missingPoints, color="red",fill="red", size=2, shape=21) 
            #geom_point(data=vals$markedPoints, color="red", size=2) 
    
    plot1
  })
  
  output$plot2 <- renderPlot({
    if(vals$missing){
      plot2 <- ggplot(vals$imputedDataSet, aes(x = year, y = sunvalue)) 
    } else {
      plot2 <- ggplot(vals$data, aes(x = year, y = sunvalue)) 
    }
    plot2 = plot2 +  
      stat_smooth(method = "lm", formula = y ~ 1, se = FALSE, colour = "red", size=0.4) +
      facet_wrap(~ factor_cycle, nrow = 1) +
      labs(x = NULL, y = "Sunvalue") +
      ggtitle("Cyclic graph") +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(0, "lines"),
            #axis.line = element_line(color="green"),
            plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            strip.background = element_rect(fill = "white"))   
      
    #aes(color="darkseagreen", size=0.4)
    if(vals$missing){
      plot2 = plot2 + geom_line(color="darkseagreen", size=0.4) 
    } else {
      plot2 = plot2 + geom_line(color="darkseagreen", size=0.4) 
    }
      
    plot2 = plot2 + 
      geom_point(fill="white", shape=21, color="cornflowerblue") +
      geom_point(data=vals$missingPoints, color="red",fill="red", size=2, shape=21) 
      #geom_point(data=vals$markedPoints, color="red", size=2) 
    
    plot2
  }) 
  
  observeEvent(input$generateMissingValues, {
    vals$missingIndices <- sort(sample(1:nrow(sun_df),5))
    toImpute <- vals$imputedDataSet$sunvalue
    
    vals$missing = TRUE
    vals$imputedDataSet <- vals$data
    vals$imputedDataSet[vals$missingIndices,]$sunvalue = NA
    vals$imputedDataSet$sunvalue = na.mean(vals$imputedDataSet$sunvalue)
    if(vals$missing){
      vals$missingPoints = vals$imputedDataSet[vals$missingIndices,]
    } else {
      vals$missingPoints = vals$data[vals$missingIndices,]
    }
    
    vals$group1 <- vals$data[c(1:vals$missingIndices[1]-1),]
    vals$group2 <- vals$data[c(((vals$missingIndices[1])+1):((vals$missingIndices[2])-1)),]
    vals$group3 <- vals$data[c(((vals$missingIndices[2])+1):((vals$missingIndices[3])-1)),]
    vals$group4 <- vals$data[c(((vals$missingIndices[3])+1):((vals$missingIndices[4])-1)),]
    vals$group5 <- vals$data[c(((vals$missingIndices[4])+1):((vals$missingIndices[5])-1)),]
    vals$group6 <- vals$data[c(((vals$missingIndices[5])+1):length(vals$data$year)),]

  })
  
  observeEvent(input$toggle, {
    vals$missing = ifelse(vals$missing,FALSE,TRUE)
    if(vals$missing){
      vals$missingPoints = vals$imputedDataSet[vals$missingIndices,]
    } else {
      vals$missingPoints = vals$data[vals$missingIndices,]
    }
  })
}

shinyApp(ui = ui, server = server)
