library(shiny)
library(mlbench)
library(pROC)
library(ROCR)
library(ggplot2)
setwd("/Users/alexandriaguo/Documents/2018-2019/thesis/ROC_app")

source("helper.R")

ui <- fluidPage (
  
  titlePanel("ROC Curve"), 
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create ROC curves for various datasets using
               binary classification"),
      
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Adult Salaries",
                              "Pima Indians Diabetes")
      ),
      selectInput(inputId = "model",
                  label = "Choose a model/link function:",
                  choices = c("logistic",
                              "probit")
      ),
      actionButton(inputId = "update",
                   label = "Update curve"),
      actionButton(inputId = "clear",
                   label = "Clear plot"),
      hr(),
      helpText("Choose a discrimination threshold"),
      sliderInput("thresh", NULL,
                  min = 0, max = 1, value = 0.5),
      actionButton(inputId = "add_pt",
                   label = "Highlight point")
    ),
    mainPanel(
      plotOutput(outputId = "ROCplot"),
      textOutput(outputId= "AUC")
    )
  )
)

server <- function(input, output) {
  
  roc <- reactiveValues(main = NULL, layer1 = NULL)
  roc$main <- ggplot() +
    geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
    labs(x = '1 - Specificity', y = 'Sensitivity')
  auc <- reactiveValues(data = NULL) 
  test_sub <- reactiveValues(data = NULL)
  
  observeEvent(input$update, {
    
    ## PARTITION DATASET
    data <- switch(input$dataset,
                   "Adult Salaries" = adult,
                   "Pima Indians Diabetes" = pima)
    model <-switch(input$model,
                   "logistic" = "logit",
                   "probit" = "probit")
    data$label <- relevel(factor(data$label), ref = "0")
    smp_size <- floor(0.75 * nrow(data)) # 75% of the sample size
    set.seed(125) # set the seed to make partition reproducible
    train_ind <- sample(seq_len(nrow(data)), size = smp_size)
    train <- data[train_ind,]
    test <- data[-train_ind,]
    
    ## TEST FIT
    full.fit <- glm(label~., family=binomial(link=model), data=train)
    prob <- predict.glm(full.fit, newdata=test, type="response")
    test_sub$data <- cbind(test, prob)
    pred <- prediction(prob, test$label)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    dd <- data.frame(FP = perf@x.values[[1]], TP = perf@y.values[[1]])
    
    roc$main <- roc$main + geom_line(data = dd, aes(x = FP, y = TP, colour = input$dataset))
    auc$data <- auc(test$label, prob)
  })
  
  observeEvent(input$clear, {
    roc <- reactiveValues(main = NULL, layer1 = NULL)
    roc$main <- ggplot() +
      geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
      labs(x = '1 - Specificity', y = 'Sensitivity') +
      guides(color=FALSE)
  })
  
  observeEvent(input$add_pt, {
    pt <- find_thresh(test_sub$data, input$thresh)
    roc$layer1 <- geom_point(data = pt, aes(x = FP, y = TP), colour = "blue", size = 4)
  })
  
  output$ROCplot <- renderPlot({
    roc$main + roc$layer1
  })
  
  output$AUC <- renderText({
    if (is.null(auc$data)) paste("The area under the curve is ", 0)
    else paste("The area under the curve is ", auc$data)
  })
}

shinyApp(ui, server)