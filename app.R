library(shiny)
library(ggplot2)
library(imputeTS)
library(mice)
library(randomForest)
library(sinew)


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
    tags$head(tags$style("#imp2_method{color: black;
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
    textOutput("imp2_method"),
    checkboxInput("method14","Predictive Mean Matching",FALSE),
    checkboxInput("method15","Random Forest",FALSE),
    checkboxInput("method16","Imputation of quadratic terms",FALSE),
    selectInput('display_method', 'Missing value display method', 
                c("Single Points and Boxplots","Error Boundaries (for Multiple Imp.)")),
    actionButton("generateMissingValues","Delete random Values"),
    actionButton("imputeAction","Impute Values"),
    actionButton("toggle","Toggle actual/imputed")
  ),
  mainPanel(
    plotOutput('plot1', click="plot1_click", dblclick = "plot1_dblclick", 
               brush=brushOpts(id = "plot1_brush", resetOnNew = TRUE)),
    plotOutput('plot2', click="plot2_click", dblclick = "plot2_dblclick",
               brush=brushOpts(id = "plot1_brush", resetOnNew = TRUE)),
    verbatimTextOutput("info")
  )
)

#' Server function 
#'
#' @param input 
#' @param output 
#' @param session 
server <- function(input, output, session) {
  
  sunspot = read.table(file="sunspot.month.csv", 
                       header=TRUE, sep=",", dec=".")
  wholeyear_mask = sunspot$time%%1==0
  year = sunspot$time[wholeyear_mask]
  sunvalue = sunspot$value[wholeyear_mask]
  cycle = c(seq(4,11), rep(seq(1,11),23), seq(1:4))
  factor_cycle = factor(cycle)
  sun_df=data.frame(year,sunvalue,factor_cycle)
  duplicate <- sun_df
  emptyFrame = data.frame(year = numeric(), sunvalue = numeric(), factor_cycle = factor())
  markedpoints <- emptyFrame
  otherMarkedpoints <- emptyFrame
  missingpoints <- emptyFrame
  missingPoints0 <- emptyFrame
  missingPoints1 <- emptyFrame
  missingPoints2 <- emptyFrame
  missingPoints3 <- emptyFrame
  missingPoints4 <- emptyFrame
  missingPoints5 <- emptyFrame
  missingPoints6 <- emptyFrame
  missingPoints7 <- emptyFrame
  missingPoints8 <- emptyFrame
  missingPoints9 <- emptyFrame
  missingPoints10 <- emptyFrame
  missingPoints11 <- emptyFrame
  missingPoints12 <- emptyFrame
  missingPoints13 <- emptyFrame
  missingPoints14 <- emptyFrame
  missingPoints15 <- emptyFrame
  method14 <- list(one<-emptyFrame, 
                   two<-emptyFrame,
                   three<-emptyFrame,
                   four<-emptyFrame,
                   five<-emptyFrame)
  method15 <- list(one<-emptyFrame, 
                   two<-emptyFrame,
                   three<-emptyFrame,
                   four<-emptyFrame,
                   five<-emptyFrame)
  method16 <- list(one<-emptyFrame, 
                   two<-emptyFrame,
                   three<-emptyFrame,
                   four<-emptyFrame,
                   five<-emptyFrame)
  
  vals <- reactiveValues(data = sun_df,
                         dataSetWithNA = duplicate,
                         markedPoints = markedpoints,
                         missingPoints1 = missingpoints,
                         missing=FALSE,
                         valuesDeleted=FALSE,
                         singleImpMethods=FALSE,
                         multipleImpMethods=FALSE,
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
                         method14 = method14,
                         method15 = method15,
                         method16 = method16,
                         method14chosen = FALSE,
                         method15chosen = FALSE,
                         method16chosen = FALSE,
                         emptyFrame = emptyFrame,
                         xGraph1 = NULL,
                         yGraph1 = NULL,
                         xGraph2 = NULL,
                         yGraph2 = NULL)
  
  
  observeEvent(input$plot1_click,{
    nearpoint <- nearPoints(vals$data,input$plot1_click, 
                  xvar = "year", yvar="sunvalue",
                  threshold = 5, maxpoints = 1)
    if (nrow(nearpoint) != 0){
      vals$markedPoints <- vals$markedPoints[0,]
      vals$otherMarkedPoints <- vals$data[factor_cycle==nearpoint$factor_cycle,]
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
      vals$otherMarkedPoints <- vals$data[factor_cycle==nearpoint$factor_cycle,]
      vals$markedPoints <- rbind(vals$markedPoints, data.frame(
                                 year=nearpoint$year,
                                 sunvalue=nearpoint$sunvalue,
                                 factor_cycle=nearpoint$factor_cycle))
    }
  })
  
  output$imp_method <- renderText({
    paste("Single Value Imputation Methods")
  })
  output$imp2_method <- renderText({
    paste("Multiple Imputation Methods")
  })

  #output$info <- renderPrint({

  #})
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      vals$xGraph1 <- c(brush$xmin, brush$xmax)
      vals$yGraph1 <- c(brush$ymin, brush$ymax)
      
    } else {
      vals$xGraph1 <- NULL
      vals$yGraph1 <- NULL
    }
  })
  
  observeEvent(input$plot2_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      vals$xGraph2 <- c(brush$xmin, brush$xmax)
      vals$yGraph2 <- c(brush$ymin, brush$ymax)
      
    } else {
      vals$xGraph2 <- NULL
      vals$yGraph2 <- NULL
    }
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
      coord_cartesian(xlim = vals$xGraph1, ylim = vals$yGraph1, expand = FALSE) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(0, "lines"),
            plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5),
            #axis.text.x = element_blank(),
            #axis.ticks.x = element_blank(),
            strip.background = element_rect(fill = "white"))  
    
    
    if(vals$missing){
      if(input$display_method == "Error Boundaries (for Multiple Imp.)"){
        temp <- vals$data[-vals$missingIndices,]
        median

        plot1 = plot1 +
          geom_line(data=vals$group1, color="grey30", size=0.4) +
          geom_line(data=vals$group2, color="grey30", size=0.4) +
          geom_line(data=vals$group3, color="grey30", size=0.4) +
          geom_line(data=vals$group4, color="grey30", size=0.4) +
          geom_line(data=vals$group5, color="grey30", size=0.4) +
          geom_line(data=vals$group6, color="grey30", size=0.4) +
          geom_point(data=temp,fill="white", shape=21,color="cornflowerblue", size=1) +
          geom_segment(aes(x = vals$data[vals$missingIndices[1],]$year,
                           y = min(vals$method14points$one$sunvalue, vals$method15points$one$sunvalue,
                                   vals$method16points$one$sunvalue),
                           xend = vals$data[vals$missingIndices[1],]$year,
                           yend = max(vals$method14points$one$sunvalue, vals$method15points$one$sunvalue, 
                                      vals$method16points$one$sunvalue)), color="red") +
          geom_segment(aes(x = vals$data[vals$missingIndices[2],]$year,
                           y = min(vals$method14points$two$sunvalue, vals$method15points$two$sunvalue,
                                   vals$method16points$two$sunvalue),
                           xend = vals$data[vals$missingIndices[2],]$year,
                           yend = max(vals$method14points$two$sunvalue, vals$method15points$two$sunvalue, 
                                      vals$method16points$two$sunvalue)), color="red") +
          geom_segment(aes(x = vals$data[vals$missingIndices[3],]$year,
                           y = min(vals$method14points$three$sunvalue, vals$method15points$three$sunvalue,
                                   vals$method16points$three$sunvalue),
                           xend = vals$data[vals$missingIndices[3],]$year,
                           yend = max(vals$method14points$three$sunvalue, vals$method15points$three$sunvalue, 
                                      vals$method16points$three$sunvalue)), color="red") +
          geom_segment(aes(x = vals$data[vals$missingIndices[4],]$year,
                           y = min(vals$method14points$four$sunvalue, vals$method15points$four$sunvalue,
                                   vals$method16points$four$sunvalue),
                           xend = vals$data[vals$missingIndices[4],]$year,
                           yend = max(vals$method14points$four$sunvalue, vals$method15points$four$sunvalue, 
                                      vals$method16points$four$sunvalue)), color="red") +
          geom_segment(aes(x = vals$data[vals$missingIndices[5],]$year,
                           y = min(vals$method14points$five$sunvalue, vals$method15points$five$sunvalue,
                                   vals$method16points$five$sunvalue),
                           xend = vals$data[vals$missingIndices[5],]$year,
                           yend = max(vals$method14points$five$sunvalue, vals$method15points$five$sunvalue, 
                                      vals$method16points$five$sunvalue)), color="red") 
      } else {
     
      if(vals$method14chosen){
        plot1 = plot1 + 
          geom_boxplot(data=vals$method14points$one, color="blue", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method14points$two, color="blue", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method14points$three, color="blue", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method14points$four, color="blue", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method14points$five, color="blue", width=0.3, size=0.3, outlier.size = 0.7) 
      }
      if(vals$method15chosen){
        plot1 = plot1 + 
          geom_boxplot(data=vals$method15points$one, color="red3", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method15points$two, color="red3", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method15points$three, color="red3", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method15points$four, color="red3", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method15points$five, color="red3", width=0.3, size=0.3, outlier.size = 0.7) 
      }
      if(vals$method16chosen){
        plot1 = plot1 + 
          geom_boxplot(data=vals$method16points$one, color="springgreen4", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method16points$two, color="springgreen4", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method16points$three, color="springgreen4", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method16points$four, color="springgreen4", width=0.3, size=0.3, outlier.size = 0.7) +
          geom_boxplot(data=vals$method16points$five, color="springgreen4", width=0.3, size=0.3, outlier.size = 0.7) 
      }
      
      plot1 = plot1 +
        geom_line(data=vals$group1, color="grey30", size=0.4) +
        geom_line(data=vals$group2, color="grey30", size=0.4) +
        geom_line(data=vals$group3, color="grey30", size=0.4) +
        geom_line(data=vals$group4, color="grey30", size=0.4) +
        geom_line(data=vals$group5, color="grey30", size=0.4) +
        geom_line(data=vals$group6, color="grey30", size=0.4) +
        geom_point(fill="white", shape=21,color="cornflowerblue", size=1) +
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
  
      }
    
      
    } else {
      plot1 = plot1 + 
        geom_line(color="grey30", size=0.4)  +
        geom_point(fill="white", shape=21,color="cornflowerblue", size=1) +
        geom_point(data=vals$missingPoints0, color="red",fill="red", size=1, shape=21) 
    } 
    
    
    plot1 = plot1 + 
        geom_point(data=vals$otherMarkedPoints, color="violetred", size=1, fill="violetred",shape=21) +
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
      coord_cartesian(xlim = vals$xGraph2, ylim = vals$yGraph2, expand = FALSE) +
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
      geom_point(fill="white", shape=21, color="cornflowerblue", size=1) 
      
   
     if(vals$missing){
      plot2 = plot2 +
        geom_point(fill="white", shape=21,color="cornflowerblue", size=1) +
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
      #geom_point(data=vals$otherMarkedPoints, color="violetred", size=1, fill="violetred",shape=21) +
      geom_point(data=vals$markedPoints, color="red", size=2, fill="red",shape=21) 
    
    plot2
  }) 
  
  observeEvent(input$generateMissingValues, {
    vals$missingIndices <- sort(sample(1:nrow(sun_df),5))
    vals$missing = TRUE
    vals$valuesDeleted = TRUE
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
    vals$method14points$one <- vals$emptyFrame
    vals$method14points$two <- vals$emptyFrame
    vals$method14points$three <- vals$emptyFrame
    vals$method14points$four <- vals$emptyFrame
    vals$method14points$five <- vals$emptyFrame
    
    #Default Imputation (mean)
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
  })
  
  observeEvent(input$imputeAction, {
    
    if(vals$missing && vals$valuesDeleted){
      
      if(input$method1==FALSE && input$method2==FALSE &&
          input$method3==FALSE && input$method4==FALSE &&
          input$method5==FALSE && input$method6==FALSE &&
          input$method7==FALSE && input$method8==FALSE &&
          input$method9==FALSE && input$method10==FALSE &&
          input$method11==FALSE && input$method12==FALSE &&
          input$method13==FALSE && input$method14==FALSE &&
          input$method15==FALSE){
        updateCheckboxInput(session, "method11", value = TRUE)
        showNotification(session=session,"No imputation method chosen 
                         ('Mean' chosen as default)",
                         type="warning")
      } else {

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
        method14chosen = input$method14
        method15chosen = input$method15
        method16chosen = input$method16
        vals$method14points$one <- vals$emptyFrame
        vals$method14points$two <- vals$emptyFrame
        vals$method14points$three <- vals$emptyFrame
        vals$method14points$four <- vals$emptyFrame
        vals$method14points$five <- vals$emptyFrame
        vals$method14 <- list(one<-vals$emptyFrame, 
                         two<-vals$emptyFrame,
                         three<-vals$emptyFrame,
                         four<-vals$emptyFrame,
                         five<-vals$emptyFrame)
        vals$method15points$one <- vals$emptyFrame
        vals$method15points$two <- vals$emptyFrame
        vals$method15points$three <- vals$emptyFrame
        vals$method15points$four <- vals$emptyFrame
        vals$method15points$five <- vals$emptyFrame
        vals$method15 <- list(one<-vals$emptyFrame, 
                              two<-vals$emptyFrame,
                              three<-vals$emptyFrame,
                              four<-vals$emptyFrame,
                              five<-vals$emptyFrame)
        vals$method16points$one <- vals$emptyFrame
        vals$method16points$two <- vals$emptyFrame
        vals$method16points$three <- vals$emptyFrame
        vals$method16points$four <- vals$emptyFrame
        vals$method16points$five <- vals$emptyFrame
        vals$method16 <- list(one<-vals$emptyFrame, 
                              two<-vals$emptyFrame,
                              three<-vals$emptyFrame,
                              four<-vals$emptyFrame,
                              five<-vals$emptyFrame)
        
        vals$dataSetWithNA <- vals$data
        vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
        
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
        
        if(input$method14){
          vals$method14chosen = TRUE
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          combined <- invisible(mice(vals$dataSetWithNA, m=10, method="pmm", maxit=10, printFlag = FALSE))
#' Title
#'
#' @param df 
#' @param index 
#'
#' @return
#' @export
#'
#' @examples
          vals$method14 <- mapply(function(df,index) {
            for(i in 1:10){
              df <- rbind(df,complete(combined,i)[vals$missingIndices[index],])
            }
            df$year=df$year - 0.6
            if(index==1){vals$method14points$one <- df}
            if(index==2){vals$method14points$two <- df}
            if(index==3){vals$method14points$three <- df}
            if(index==4){vals$method14points$four <- df}
            if(index==5){vals$method14points$five <- df}
          },vals$method14, seq_along(vals$method14))

        }
        
        if(input$method15){
          vals$method15chosen = TRUE
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          combined <- invisible(mice(vals$dataSetWithNA, m=10, method="rf", maxit=10, printFlag = FALSE))
          vals$method15 <- mapply(function(df,index) {
            for(i in 1:10){
              df <- rbind(df,complete(combined,i)[vals$missingIndices[index],])
            }
            if(index==1){vals$method15points$one <- df}
            if(index==2){vals$method15points$two <- df}
            if(index==3){vals$method15points$three <- df}
            if(index==4){vals$method15points$four <- df}
            if(index==5){vals$method15points$five <- df}
          },vals$method15, seq_along(vals$method15))

        }
        
        if(input$method16){
          vals$method16chosen = TRUE
          vals$dataSetWithNA[vals$missingIndices,]$sunvalue = NA
          combined <- invisible(mice(vals$dataSetWithNA, m=10, method="quadratic", maxit=10, printFlag = FALSE))
          vals$method16 <- mapply(function(df,index) {
            for(i in 1:10){
              df <- rbind(df,complete(combined,i)[vals$missingIndices[index],])
            }
            df$year=df$year + 0.6
            if(index==1){vals$method16points$one <- df}
            if(index==2){vals$method16points$two <- df}
            if(index==3){vals$method16points$three <- df}
            if(index==4){vals$method16points$four <- df}
            if(index==5){vals$method16points$five <- df}
          },vals$method16, seq_along(vals$method16))
          
        }
        
      }
      
    } else {
      showNotification(session=session,type="message","Please delete values first (or toggle to imputed)")
    }
  })
}

shinyApp(ui = ui, server = server)
