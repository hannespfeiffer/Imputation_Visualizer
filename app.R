library(shiny)
library(ggplot2)
library(imputeTS)

ui <- fluidPage(
  
  headerPanel("Imputation Visualizer"),
  sidebarPanel(
    textOutput("imp_method"),
    tags$head(tags$style("#imp_method{color: black;
                                 font-size: 15px;
                                 font-weight: bold;
                         }"
              )
    ),
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
                c("Single Points (for Single Value Imp.)","Error Boundaries","Boxplots")),
    actionButton("generateMissingValues","Delete random Values"),
    actionButton("imputeAction","Impute Values"),
    actionButton("toggle","Toggle actual/imputed")
  ),
  mainPanel(
    plotOutput('plot1', click="plot1_click", brush="plot1_brush"),
    plotOutput('plot2', click="plot2_click", brush="plot2_brush"),
    verbatimTextOutput("info")
  )
  
)

server <- function(input, output, session) {
  
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
  missingpoints <- markedpoints
  duplicate <- sun_df
  missingPoints0 <- missingpoints
  missingPoints1 <- missingpoints
  missingPoints2 <- missingpoints
  missingPoints3 <- missingpoints
  missingPoints4 <- missingpoints
  missingPoints5 <- missingpoints
  missingPoints6 <- missingpoints
  missingPoints7 <- missingpoints
  missingPoints8 <- missingpoints
  missingPoints9 <- missingpoints
  missingPoints10 <- missingpoints
  missingPoints11 <- missingpoints
  missingPoints12 <- missingpoints
  missingPoints13 <- missingpoints
  emptyFrame <- missingpoints
  
  vals <- reactiveValues(data = sun_df,
                         dataSetWithNA = duplicate,
                         markedPoints = markedpoints,
                         missingPoints1 = missingpoints,
                         missing=FALSE,
                         missingIndices = NULL,
                         group1 = NULL,
                         group2 = NULL,
                         group3 = NULL,
                         group4 = NULL,
                         group5 = NULL,
                         group6 = NULL,
                         missingPoints0 = missingPoints0,
                         missingPoints1 = missingPoints1,
                         missingPoints2 = missingPoints2,
                         missingPoints3 = missingPoints3,
                         missingPoints4 = missingPoints4,
                         missingPoints5 = missingPoints5,
                         missingPoints6 = missingPoints6,
                         missingPoints7 = missingPoints7,
                         missingPoints8 = missingPoints8,
                         missingPoints9 = missingPoints9,
                         missingPoints10 = missingPoints10,
                         missingPoints11 = missingPoints11,
                         missingPoints12 = missingPoints12,
                         missingPoints13 = missingPoints13,
                         emptyFrame = emptyFrame)
  
  
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
    paste("Single Value Imputation Methods")
  })

  output$info <- renderPrint({
    print("Marked point:")
    print(vals$markedPoints)
    print("Imputed Dataset:")
    print(vals$dataSetWithNA)

  })
  
  output$plot1 <- renderPlot({
    if(vals$missing){
      plot1 <- ggplot(vals$dataSetWithNA, aes(x = year, y = sunvalue)) 
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
        geom_line(data=vals$group6, color="grey30", size=0.4) +
        geom_point(fill="white", shape=21,color="cornflowerblue") +
        geom_point(data=vals$missingPoints1, color="burlywood",fill="burlywood", size=1, shape=21) +
        geom_point(data=vals$missingPoints2, color="yellow3",fill="yellow3", size=1, shape=21) +
        geom_point(data=vals$missingPoints3, color="violetred",fill="violetred", size=1, shape=21) +
        geom_point(data=vals$missingPoints4, color="tomato",fill="tomato", size=1, shape=21) +
        geom_point(data=vals$missingPoints5, color="green3",fill="green3", size=1, shape=21) +
        geom_point(data=vals$missingPoints6, color="royalblue1",fill="royalblue1", size=1, shape=21) +
        geom_point(data=vals$missingPoints7, color="cadetblue",fill="cadetblue", size=1, shape=21) +
        geom_point(data=vals$missingPoints8, color="mediumpurple3",fill="mediumpurple3", size=1, shape=21) +
        geom_point(data=vals$missingPoints9, color="indianred2",fill="indianred2", size=1, shape=21) +
        geom_point(data=vals$missingPoints10, color="dodgerblue1",fill="dodgerblue1", size=1, shape=21) +
        geom_point(data=vals$missingPoints11, color="red",fill="red", size=1, shape=21) +
        geom_point(data=vals$missingPoints12, color="slategray4",fill="slategray4", size=1, shape=21) +
        geom_point(data=vals$missingPoints13, color="magenta",fill="magenta", size=1, shape=21) 
    } else {
      plot1 = plot1 + 
        geom_line(color="grey30", size=0.4)  +
        geom_point(fill="white", shape=21,color="cornflowerblue") +
        geom_point(data=vals$missingPoints0, color="red",fill="red", size=1.5, shape=21) 
    } 
    
    plot1 = plot1 + 
        geom_point(data=vals$markedPoints, color="red", size=2, fill="red",shape=21) 
    
    plot1
  })
  
  output$plot2 <- renderPlot({
    if(vals$missing){
      plot2 <- ggplot(vals$dataSetWithNA, aes(x = year, y = sunvalue)) 
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
            strip.background = element_rect(fill = "white")) + 
      geom_line(color="darkseagreen", size=0.4) +
      geom_point(fill="white", shape=21, color="cornflowerblue") 
      
   
     if(vals$missing){
      plot2 = plot2 +
        geom_point(fill="white", shape=21,color="cornflowerblue") +
        geom_point(data=vals$missingPoints1, color="burlywood",fill="burlywood", size=1, shape=21) +
        geom_point(data=vals$missingPoints2, color="yellow3",fill="yellow3", size=1, shape=21) +
        geom_point(data=vals$missingPoints3, color="violetred",fill="violetred", size=1, shape=21) +
        geom_point(data=vals$missingPoints4, color="tomato",fill="tomato", size=1, shape=21) +
        geom_point(data=vals$missingPoints5, color="green3",fill="green3", size=1, shape=21) +
        geom_point(data=vals$missingPoints6, color="royalblue1",fill="royalblue1", size=1, shape=21) +
        geom_point(data=vals$missingPoints7, color="cadetblue",fill="cadetblue", size=1, shape=21) +
        geom_point(data=vals$missingPoints8, color="mediumpurple3",fill="mediumpurple3", size=1, shape=21) +
        geom_point(data=vals$missingPoints9, color="indianred2",fill="indianred2", size=1, shape=21) +
        geom_point(data=vals$missingPoints10, color="dodgerblue1",fill="dodgerblue1", size=1, shape=21) +
        geom_point(data=vals$missingPoints11, color="red",fill="red", size=1, shape=21) +
        geom_point(data=vals$missingPoints12, color="slategray4",fill="slategray4", size=1, shape=21) +
        geom_point(data=vals$missingPoints13, color="magenta",fill="magenta", size=1, shape=21) 
    } else {
     # plot2 = plot2 + geom_line(color="darkseagreen", size=0.4) +
      plot2 = plot2 + 
        geom_point(data=vals$missingPoints0, color="red",fill="red", size=1.5, shape=21) 
      
    }
      
    plot2 = plot2 + 
      geom_point(data=vals$markedPoints, color="red", size=2, fill="red",shape=21) 
    
    plot2
  }) 
  
  observeEvent(input$generateMissingValues, {
    vals$missingIndices <- sort(sample(1:nrow(sun_df),5))
    vals$missing = TRUE
    vals$dataSetWithNA <- vals$data
    vals$missingPoints0 <- vals$emptyFrame
    vals$missingPoints0 <- vals$dataSetWithNA[vals$missingIndices,]
    vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
    
    
    vals$missingPoints1 <- vals$emptyFrame
    vals$missingPoints2 <- vals$emptyFrame
    vals$missingPoints3 <- vals$emptyFrame
    vals$missingPoints4 <- vals$emptyFrame
    vals$missingPoints5 <- vals$emptyFrame
    vals$missingPoints6 <- vals$emptyFrame
    vals$missingPoints7 <- vals$emptyFrame
    vals$missingPoints8 <- vals$emptyFrame
    vals$missingPoints9 <- vals$emptyFrame
    vals$missingPoints10 <- vals$emptyFrame
    vals$missingPoints11 <- vals$emptyFrame
    vals$missingPoints12 <- vals$emptyFrame
    vals$missingPoints13 <- vals$emptyFrame
    
    #Imputation
    vals$dataSetWithNA$sunvalue = na.mean(vals$dataSetWithNA$sunvalue)
    vals$missingPoints11 = vals$dataSetWithNA[vals$missingIndices,]
  
    vals$group1 <- vals$data[c(1:vals$missingIndices[1]-1),]
    vals$group2 <- vals$data[c(((vals$missingIndices[1])+1):((vals$missingIndices[2])-1)),]
    vals$group3 <- vals$data[c(((vals$missingIndices[2])+1):((vals$missingIndices[3])-1)),]
    vals$group4 <- vals$data[c(((vals$missingIndices[3])+1):((vals$missingIndices[4])-1)),]
    vals$group5 <- vals$data[c(((vals$missingIndices[4])+1):((vals$missingIndices[5])-1)),]
    vals$group6 <- vals$data[c(((vals$missingIndices[5])+1):length(vals$data$year)),]
    
    
    
    updateCheckboxInput(session, "method11", value = TRUE)

  })
  
  observeEvent(input$toggle, {
    vals$missing = ifelse(vals$missing,FALSE,TRUE)
    if(vals$missing){
      vals$missingPoints1 = vals$dataSetWithNA[vals$missingIndices,]
    } else {
      vals$missingPoints1 = vals$data[vals$missingIndices,]
    }
    
  })
  
  observeEvent(input$imputeAction, {
    
    vals$missingPoints1 <- vals$emptyFrame
    vals$missingPoints2 <- vals$emptyFrame
    vals$missingPoints3 <- vals$emptyFrame
    vals$missingPoints4 <- vals$emptyFrame
    vals$missingPoints5 <- vals$emptyFrame
    vals$missingPoints6 <- vals$emptyFrame
    vals$missingPoints7 <- vals$emptyFrame
    vals$missingPoints8 <- vals$emptyFrame
    vals$missingPoints9 <- vals$emptyFrame
    vals$missingPoints10 <- vals$emptyFrame
    vals$missingPoints11 <- vals$emptyFrame
    vals$missingPoints12 <- vals$emptyFrame
    vals$missingPoints13 <- vals$emptyFrame
    
    if(vals$missing){
      vals$dataSetWithNA <- vals$data
      vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
      
      if(input$method1==FALSE && input$method2==FALSE &&
          input$method3==FALSE && input$method4==FALSE &&
          input$method5==FALSE && input$method6==FALSE &&
          input$method7==FALSE && input$method8==FALSE &&
          input$method9==FALSE && input$method10==FALSE &&
          input$method11==FALSE && input$method12==FALSE &&
          input$method3==FALSE){
        updateCheckboxInput(session, "method11", value = TRUE)
        showNotification(session=session,"Please choose at least one imputation method
                         ('Mean' chosen as default)",
                         type="warning")
      } else {
        #Imputation
        if(input$method1){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.interpolation(vals$dataSetWithNA$sunvalue,
                                                         option="linear")
          vals$missingPoints1 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method2){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.interpolation(vals$dataSetWithNA$sunvalue,
                                                         option="spline")
          vals$missingPoints2 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method3){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.interpolation(vals$dataSetWithNA$sunvalue,
                                                         option="stine")
          vals$missingPoints3 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method4){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.kalman(vals$dataSetWithNA$sunvalue,
                                                         model="StructTS")
          vals$missingPoints4 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method5){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.kalman(vals$dataSetWithNA$sunvalue,
                                                         model="auto.arima")
          vals$missingPoints5 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method6){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.locf(vals$dataSetWithNA$sunvalue,
                                                         option="locf")
          vals$missingPoints6 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method7){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.locf(vals$dataSetWithNA$sunvalue,
                                                         option="nocb")
          vals$missingPoints7 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method8){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.ma(vals$dataSetWithNA$sunvalue,
                                              weighting="simple")
          vals$missingPoints8 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method9){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.ma(vals$dataSetWithNA$sunvalue,
                                              weighting="linear")
          vals$missingPoints9 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method10){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.ma(vals$dataSetWithNA$sunvalue,
                                              weighting="exponential")
          vals$missingPoints10 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method11){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.mean(vals$dataSetWithNA$sunvalue,
                                                         option="mean")
          vals$missingPoints11 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method12){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.mean(vals$dataSetWithNA$sunvalue,
                                                         option="median")
          vals$missingPoints12 = vals$dataSetWithNA[vals$missingIndices,]
        }
        if(input$method13){
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          vals$dataSetWithNA$sunvalue = na.mean(vals$dataSetWithNA$sunvalue,
                                                         option="mode")
          vals$missingPoints13 = vals$dataSetWithNA[vals$missingIndices,]
        }
        
      }
      
    } else {
      showNotification(session=session,type="message","Please delete values first (or switch to imputed)")
    }
  })
}

shinyApp(ui = ui, server = server)
