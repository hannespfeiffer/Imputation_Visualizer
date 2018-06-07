library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  headerPanel("Imputation Visualizer"),
  sidebarPanel(
    textOutput("packages"),
    selectInput('xcol', 'X Variable', {.packages()}),
    selectInput('ycol', 'Y Variable', names(iris),
      selected = names(iris)[[2]]),
    numericInput('clusters', 'Imputation method', 3,
      min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1', click="plot1_click", brush="plot1_brush"),
    plotOutput('plot2', click="plot2_click", brush="plot2_brush"),
    verbatimTextOutput("info")
  )
)

server <- function(input, output) {
  
  output$packages <- renderText({.packages()})
  
  sunspot = read.table(file="sunspot.month.csv", 
                       header=TRUE, sep=",", dec=".")
  wholeyear_mask = sunspot$time%%1==0
  year = sunspot$time[wholeyear_mask]
  sunvalue = sunspot$value[wholeyear_mask]
  cycle = c(seq(4,11), rep(seq(1,11),23), seq(1:4))
  factor_cycle = factor(cycle)
  vals <- reactiveValues(data=data.frame(year,sunvalue,factor_cycle))
  
  #selectedData <- reactive({
  #  iris[, c(input$xcol, input$ycol)]
  #})

  #clusters <- reactive({
  #  kmeans(selectedData(), input$clusters)
  #})
  
  observeEvent(input$plot1_click,{
    nearpoint <- nearPoints(vals$data,input$plot1_click, 
                            xvar = "year", yvar="sunvalue",
                            threshold = 5, maxpoints = 1)
    #if (nrow(nearpoint) != 0){
    #  vals$data <- rbind(vals$data,
    #    data.frame(year=nearpoint$year,value=nearpoint$value,fcycle=13))
    #}
    if (nrow(nearpoint) != 0){
    vals$data <- rbind(vals$data, data.frame(
      year=nearpoint$year,
      sunvalue=nearpoint$sunvalue,
      factor_cycle=as.factor(13)))
    }
    
  })

  output$info <- renderPrint({
    row <- nearPoints(vals$data,input$plot1_click, 
                      xvar = "year", yvar="sunvalue",
                      threshold = 5, maxpoints = 1)
    row2 <- nearPoints(vals$data,input$plot2_click,
                       xvar = "year", yvar = "sunvalue",
                       threshold = 5, maxpoints = 1)
    print(row)
    print(row2)
    print(vals$data)
  })
  
  output$plot1 <- renderPlot({
    ggplot(vals$data,aes(x=year,y=sunvalue)) +
      geom_line(color="blue", size=0.4) +
      geom_point(fill="white", shape=21) +
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
  })
  
  output$plot2 <- renderPlot({
    ggplot(vals$data, aes(x = year, y = sunvalue)) +
      geom_line(color="darkseagreen", size=0.4) +
      geom_point(fill="white", shape=21) +
      stat_smooth(method = "lm", formula = y ~ 1, se = FALSE, colour = "red", size=0.2) +
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
  }) 
}

shinyApp(ui = ui, server = server)
