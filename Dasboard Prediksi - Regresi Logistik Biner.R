library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(DT)
library(caret)
library(pROC)
library(dplyr)
library(tidyr)
library(corrplot)
library(RColorBrewer)
library(rsample)
library(GGally)
library(readxl)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Logistic Regression Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "inputData", icon = icon("upload")),
      menuItem("Pre-processing Data", tabName = "preprocessData", icon = icon("wrench")),
      menuItem("Explore Data", tabName = "exploreData", icon = icon("chart-bar")),
      menuItem("Model", tabName = "model", icon = icon("sliders-h")),
      menuItem("Evaluation Matrix", tabName = "evalMatrix", icon = icon("table")),
      menuItem("Predict", tabName = "predict", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tabItems(
      tabItem(tabName = "inputData",
              fluidRow(
                box(
                  title = "Upload File", status = "primary", solidHeader = TRUE, width = 12,
                  radioButtons("fileType", "Choose file type",
                               choices = list("CSV" = "csv", "Excel" = "xlsx"),
                               selected = "csv"),
                  fileInput("file1", "Choose File",
                            accept = c(".csv", ".xlsx")),
                  tags$hr(),
                  checkboxInput("header", "Header", TRUE),
                  conditionalPanel(
                    condition = "input.fileType == 'csv'",
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ",")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Data Preview", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("contents")
                )
              ),
              downloadButton("downloadData", "Download Data")
      ),
      tabItem(tabName = "preprocessData",
              fluidPage(
                titlePanel("Data Preprocessing"),
                selectInput("variableSelect", "Select Variables", choices = NULL, multiple = TRUE),
                actionButton("detectMissing", "Detect Missing Values"),
                actionButton("detectOutliers", "Detect Outliers"),
                verbatimTextOutput("missingSummary"),
                plotOutput("outlierPlot"),
                downloadButton("downloadOutlierPlot", "Download Outlier Plot")
              )
      ),
      tabItem(tabName = "exploreData",
              fluidPage(
                titlePanel("Explore Data"),
                fluidRow(
                  box(title = "Bar Chart", status = "success", solidHeader = TRUE, 
                      selectInput("barVars", "Select Variable", choices = NULL),
                      plotlyOutput("barChart")),
                  box(title = "Correlation Plot", status = "success", solidHeader = TRUE, 
                      selectInput("corrVars", "Select Variables", choices = NULL, multiple = TRUE),
                      plotOutput("correlationPlot")),
                  box(title = "Pair Plot", status = "success", solidHeader = TRUE, 
                      selectInput("pairVars", "Select Variables", choices = NULL, multiple = TRUE),
                      plotOutput("pairPlot")),
                  box(title = "Heatmap", status = "success", solidHeader = TRUE, 
                      selectInput("heatVars", "Select Variables", choices = NULL, multiple = TRUE),
                      plotOutput("heatmap"))
                ),
                fluidRow(
                  box(title = "Summary Statistics", status = "success", solidHeader = TRUE, verbatimTextOutput("summaryStats"), width = 12)
                )
              )
      ),
      tabItem(tabName = "model",
              fluidPage(
                titlePanel("Model Summary"),
                uiOutput("varSelect"),
                fluidRow(
                  box(sliderInput("trainSize", "Training Set Size (%)", min = 50, max = 90, value = 70)),
                  box(title = "Regression Logistic Analysis", status = "primary", solidHeader = TRUE, verbatimTextOutput("modelSummary"), width = 12)
                ),
                fluidRow(
                  box(title = "Goodness-of-Fit Tests", status = "primary", solidHeader = TRUE, verbatimTextOutput("gofTests")),
                  box(title = "Likelihood Ratio Test", status = "primary", solidHeader = TRUE, verbatimTextOutput("lrTest"))
                )
              )
      ),
      tabItem(tabName = "evalMatrix",
              fluidPage(
                titlePanel("Evaluation Matrix"),
                verbatimTextOutput("confMatrix"),
                tableOutput("evalTable"),
                plotOutput("rocCurve"),
                verbatimTextOutput("aucValue")
              )
      ),
      tabItem(tabName = "predict",
              fluidPage(
                titlePanel("Predict"),
                uiOutput("predictorInput"),
                actionButton("predictBtn", "Predict"),
                verbatimTextOutput("predictionResult"),
                verbatimTextOutput("predictedProbability")
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    
    if (input$fileType == "csv") {
      df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    } else if (input$fileType == "xlsx") {
      df <- read_excel(inFile$datapath, col_names = input$header)
    }
    df
  })
  
  output$contents <- DT::renderDataTable({
    DT::datatable(data(),
                  options = list(scrollX = TRUE,
                                 pageLength = 10)
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  observe({
    req(data())
    updateSelectInput(session, "barVars", choices = names(data()))
    updateSelectInput(session, "corrVars", choices = names(data()))
    updateSelectInput(session, "pairVars", choices = names(data()))
    updateSelectInput(session, "heatVars", choices = names(data()))
    updateSelectInput(session, "variableSelect", choices = names(data()))
  })
  
  observeEvent(input$detectMissing, {
    output$missingSummary <- renderPrint({
      data.frame(
        Missing_Values = sapply(data(), function(x) sum(is.na(x)))
      )
    })
  })
  
  observeEvent(input$detectOutliers, {
    output$outlierPlot <- renderPlot({
      req(input$variableSelect)
      
      plot_list <- lapply(input$variableSelect, function(var) {
        plotData <- data()[[var]]
        
        ggplot(data(), aes_string(x = "1", y = var)) + 
          geom_boxplot(fill = "#28859d") +
          stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red") +  # mean point
          stat_summary(fun = median, geom = "point", shape = 20, size = 4, color = "blue") +  # median point
          annotate("text", x = 1, y = mean(plotData, na.rm = TRUE), label = paste("Mean:", round(mean(plotData, na.rm = TRUE), 2)), color = "white", vjust = -1, size = 5) +
          annotate("text", x = 1, y = median(plotData, na.rm = TRUE), label = paste("Median:", round(median(plotData, na.rm = TRUE), 2)), color = "white", vjust = 1.5, size = 5) +
          labs(x = NULL, y = var, title = paste("Boxplot of", var)) + 
          theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center, bold, large title
                panel.background = element_rect(fill = "white"))  # Set background to white
      })
      
      # Combine all plots into a grid
      gridExtra::grid.arrange(grobs = plot_list, ncol = length(plot_list))
    })
  })
  
  output$downloadOutlierPlot <- downloadHandler(
    filename = function() {
      paste("outlier_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      plot_list <- lapply(input$variableSelect, function(var) {
        plotData <- data()[[var]]
        
        ggplot(data(), aes_string(x = "1", y = var)) + 
          geom_boxplot(fill = "#28859d") +
          stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "red") +  # mean point
          stat_summary(fun = median, geom = "point", shape = 20, size = 4, color = "blue") +  # median point
          annotate("text", x = 1, y = mean(plotData, na.rm = TRUE), label = paste("Mean:", round(mean(plotData, na.rm = TRUE), 2)), color = "white", vjust = -1, size = 5) +
          annotate("text", x = 1, y = median(plotData, na.rm = TRUE), label = paste("Median:", round(median(plotData, na.rm = TRUE), 2)), color = "white", vjust = 1.5, size = 5) +
          labs(x = NULL, y = var, title = paste("Boxplot of", var)) + 
          theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), 
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 14),  # Center, bold, large title
                panel.background = element_rect(fill = "white"))  # Set background to white
      })
      
      # Save all plots into a single file with vertical layout
      ggsave(file, plot = gridExtra::marrangeGrob(plot_list, ncol = 1, nrow = length(plot_list)), 
             width = 7, height = length(plot_list) * 5, device = "png")
    }
  )
  
  output$barChart <- renderPlotly({
    req(input$barVars)
    plotData <- data()
    p <- ggplot(plotData, aes_string(x = input$barVars)) + 
      geom_histogram(fill = "#28859d", binwidth = 0.5, position = "stack") +
      theme_minimal(base_size = 15) +  # Set base size for text
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            legend.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "grey80"),
            panel.grid.minor = element_line(color = "grey90"))
    
    ggplotly(p)
  })
  
  output$correlationPlot <- renderPlot({
    req(input$corrVars)
    plotData <- data() %>% select(input$corrVars)
    corr <- cor(plotData, use = "complete.obs")
    corrplot(corr, method = "circle", col = brewer.pal(n = 8, name = "RdYlBu"))
  })
  
  output$pairPlot <- renderPlot({
    req(input$pairVars)
    plotData <- data() %>% select(input$pairVars)
    ggpairs(plotData)
  })
  
  output$heatmap <- renderPlot({
    req(input$heatVars)
    plotData <- data() %>% select(input$heatVars)
    corrMatrix <- cor(plotData, use = "complete.obs")
    heatmap(corrMatrix, scale = "none", col = brewer.pal(9, "Blues"), margins = c(7,14), cexRow = 1, cexCol = 1)
    legend("topright", legend = seq(0, 1, length.out = 9), fill = brewer.pal(9, "Blues"), title = "Correlation")
  })
  
  output$summaryStats <- renderPrint({
    summary(data())
  })
  
  trainData <- reactive({
    req(input$responseVar, input$predictorVars)
    set.seed(123)
    dataSplit <- initial_split(data(), prop = input$trainSize / 100)
    training(dataSplit)
  })
  
  testData <- reactive({
    req(input$responseVar, input$predictorVars)
    set.seed(123)
    dataSplit <- initial_split(data(), prop = input$trainSize / 100)
    testing(dataSplit)
  })
  
  model <- reactive({
    req(input$responseVar, input$predictorVars)
    glm(as.formula(paste(input$responseVar, "~", paste(input$predictorVars, collapse = " + "))),
        data = trainData(), family = "binomial")
  })
  
  output$modelSummary <- renderPrint({
    req(input$responseVar, input$predictorVars)
    summary(model())
  })
  
  output$varSelect <- renderUI({
    tagList(
      selectInput("responseVar", "Select Response Variable", choices = names(data()), selected = NULL),
      selectInput("predictorVars", "Select Predictor Variables", choices = names(data()), multiple = TRUE, selected = NULL),
      actionButton("updateModel", "Update Model")
    )
  })
  
  observeEvent(input$updateModel, {
    updateSelectInput(session, "responseVar", selected = input$responseVar)
    updateSelectInput(session, "predictorVars", selected = input$predictorVars)
  })
  
  output$confMatrix <- renderPrint({
    req(input$responseVar, input$predictorVars)
    pred <- predict(model(), newdata = testData(), type = "response")
    predClass <- ifelse(pred > 0.5, 1, 0)
    actual <- testData()[[input$responseVar]]
    table(predClass, actual)
  })
  
  output$evalTable <- renderTable({
    req(input$responseVar, input$predictorVars)
    pred <- predict(model(), newdata = testData(), type = "response")
    predClass <- ifelse(pred > 0.5, 1, 0)
    actual <- testData()[[input$responseVar]]
    confusionMatrix <- caret::confusionMatrix(as.factor(predClass), as.factor(actual))
    metrics <- c(
      Accuracy = confusionMatrix$overall['Accuracy'],
      Sensitivity = confusionMatrix$byClass['Sensitivity'],
      Specificity = confusionMatrix$byClass['Specificity']
    )
    as.data.frame(t(metrics))
  })
  
  output$rocCurve <- renderPlot({
    req(input$responseVar, input$predictorVars)
    pred <- predict(model(), newdata = testData(), type = "response")
    actual <- testData()[[input$responseVar]]
    rocObj <- roc(actual, pred)
    plot(rocObj, col = "blue", main = "ROC Curve")
    abline(a = 0, b = 1, lty = 2)
  })
  
  output$aucValue <- renderPrint({
    req(input$responseVar, input$predictorVars)
    pred <- predict(model(), newdata = testData(), type = "response")
    actual <- testData()[[input$responseVar]]
    rocObj <- roc(actual, pred)
    auc(rocObj)
  })
  
  output$predictorInput <- renderUI({
    req(input$responseVar, input$predictorVars)
    predictors <- input$predictorVars
    lapply(predictors, function(predictor) {
      numericInput(predictor, label = paste("Enter", predictor), value = 0)
    })
  })
  
  output$predictionResult <- renderPrint({
    req(input$predictBtn)
    predictorValues <- sapply(input$predictorVars, function(predictor) {
      input[[predictor]]
    })
    newdata <- as.data.frame(t(predictorValues))
    colnames(newdata) <- input$predictorVars
    pred <- predict(model(), newdata = newdata, type = "response")
    ifelse(pred > 0.5, "Response: At Risk of Heart Attack", "Response: Not At Risk of Heart Attack")
  })
  
  output$predictedProbability <- renderPrint({
    req(input$predictBtn)
    predictorValues <- sapply(input$predictorVars, function(predictor) {
      input[[predictor]]
    })
    newdata <- as.data.frame(t(predictorValues))
    colnames(newdata) <- input$predictorVars
    pred <- predict(model(), newdata = newdata, type = "response")
    paste("Predicted Probability: ", round(pred, 4))
  })
  
  output$downloadProcessedData <- downloadHandler(
    filename = function() { paste("processed-data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      processedData <- data() %>% select(input$responseVar, input$predictorVars)
      write.csv(processedData, file)
    }
  )
  
  output$gofTests <- renderPrint({
    req(input$responseVar, input$predictorVars)
    modelObj <- model()
    pchisq(deviance(modelObj), df.residual(modelObj), lower.tail = FALSE)
  })
  
  output$lrTest <- renderPrint({
    req(input$responseVar, input$predictorVars)
    nullModel <- glm(as.formula(paste(input$responseVar, "~ 1")), data = trainData(), family = "binomial")
    fullModel <- model()
    lrStat <- nullModel$deviance - fullModel$deviance
    lrDf <- nullModel$df.residual - fullModel$df.residual
    pValue <- pchisq(lrStat, lrDf, lower.tail = FALSE)
    list(
      `Likelihood Ratio Statistic` = lrStat,
      `Degrees of Freedom` = lrDf,
      `p-value` = pValue
    )
  })
}

shinyApp(ui, server)
