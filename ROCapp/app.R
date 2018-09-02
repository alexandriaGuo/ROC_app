library(shiny)
library(pROC)
library(ROCR)
library(ggplot2)
setwd("/Users/alexandriaguo/Desktop/thesis/test_app")

source("helper.R")

ui <- fluidPage (
  
  titlePanel("ROC Curve"), 
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create ROC curves for various datasets using
               binary classification"),
      
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Adult Salaries")
      ),
      selectInput(inputId = "model",
                  label = "Choose a model/link function:",
                  choices = c("logistic",
                              "probit")
      ),
      hr(),
      actionButton(inputId = "update",
                   label = "Update curve")
    ),
    mainPanel(
      plotOutput(outputId = "ROCplot"),
      textOutput(outputId= "AUC")
    )
  )
)

server <- function(input, output) {
  
  roc <- reactiveValues(data = NULL)
  auc <- reactiveValues(data = NULL) 
  
  observeEvent(input$update, {
    
    ## PARTITION DATASET
    data <- switch(input$dataset,
                   "Adult Salaries" = adult)
    model <-switch(input$model,
                   "logistic" = "logit",
                   "probit" = "probit")
    smp_size <- floor(0.75 * nrow(data)) # 75% of the sample size
    set.seed(125) # set the seed to make partition reproducible
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind,]
    test <- data[-train_ind,]
    
    ## TEST FIT
    full.fit <- glm(salary~., family=binomial(link=model), data=train)
    prob <- predict.glm(full.fit, newdata=test, type="response")
    pred <- prediction(prob, test$salary)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    dd <- data.frame(FP = perf@x.values[[1]], TP = perf@y.values[[1]])
    
    roc$data <- dd
    auc$data <- auc(test$salary, prob)
  })
  
  output$ROCplot <- renderPlot({
    if (is.null(roc$data)) return(ggplot() + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)))
    ## PLOT ROC CURVE
    ggplot() + 
      geom_line(data = roc$data, aes(x = FP, y = TP, color = "red")) +
      geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
      guides(color=FALSE) +
      labs(x = '1 - Specificity', y = 'Sensitivity') 
  })
  
  output$AUC <- renderText({
    if (is.null(auc$data)) paste("The area under the curve is ", 0)
    else paste("The area under the curve is ", auc$data)
  })
}

shinyApp(ui, server)