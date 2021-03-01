# Title: MC Hybrid Model for Time Series
# Author: Gabriel Teotonio
# Date: 2021/02/22


# Packages -----
library(forecast)
library(tidyverse)
library(e1071)
library(rpart)
library(MLmetrics)

# Data -----
data("lynx")
data <- lynx


# Aux. functions -----

# Lags -----
lags <- c(2:24)

lag_names <- glue('lag_{str_pad(lags, nchar(max(lags)), pad = "0")}')

lag_functions <-
  map(lags, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
  set_names(lag_names)

# lag_names_ml <- glue('lag_{str_pad(lags, nchar(max(lags)), pad = "0")}_ml')
# lag_names_mnl <- glue('lag_{str_pad(lags, nchar(max(lags)), pad = "0")}_mnl')
# 
# lag_functions_ml <-
#   map(lags, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
#   set_names(lag_names_ml)
# 
# lag_functions_mnl <-
#   map(lags, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
#   set_names(lag_names_mnl)

# SVR model -----
svm_model <- function(data, lags = 2:24) {
  
  best_performance <- 10^10
  result_n_lags <- 0
  for (i in lags) {
    
    model <- tune(svm, 
                  as.formula(paste(colnames(data)[1], "~",
                                   paste(colnames(data)[c(2:i)], collapse = "+"),
                                   sep = ""
                  )),  
                  data = data,
                  type = "eps-regression",
                  kernel = "radial",
                  ranges = list(gamma = c(1, 0.1, 0.01, 0.001),
                                cost = c(0.1, 1, 100, 1000, 10000),
                                epsilon = c(0.1, 0.01, 0.001)
                                )
                  )
    model$n_lags <- i
    
    if (model$best.performance < best_performance) {
      
      best_performance <- model$best.performance
      result_model <- model$best.model
      result_n_lags <- model$n_lags
    }
  }
  
  return(list("best_model" = result_model,
              "best_n_lags" = result_n_lags))
}

# TBR model -----
tbr_model <- function(data, lags = 2:24) {
  
  best_performance <- 10^10
  result_n_lags <- 0
  for (i in lags) {
    
    tree <- rpart(as.formula(paste(colnames(data)[1], "~",
                                    paste(colnames(data)[c(2:i)], collapse = "+"),
                                    sep = ""
                   )), 
                   data = data,
                   method = "anova")
    
    model <- prune(tree,
                   cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
    
    model$best.performance <- MSE(model$y, predict(model))
    model$n_lags <- i
    
    if (model$best.performance < best_performance) {
      
      best_performance <- model$best.performance
      result_model <- model
      result_n_lags <- model$n_lags
    }
  }
  
  return(list("best_model" = result_model,
              "best_n_lags" = result_n_lags))
}

# MC algorithm -----
MC <- function(data, mc_model = "tbr") {
  
  arima_order <- c()
  
  # ARIMA ----
  arima_model <- auto.arima(data, 
                            stepwise = TRUE)
  
  arima_order <- arima_model$coef
  
  residuals <- data.frame(y = as.matrix(arima_model$residuals), 
                          date = time(arima_model$residuals)) %>% 
    as_tibble() %>% 
    mutate_at(vars(y), .funs=lag_functions) %>% 
    select(-date)
  
  if (mc_model == "tbr") {
    
    # MNL -----
    mnl_model <- svm_model(residuals, lags = 2:24)
    
    # MC -----
    for (j in 1:20) {
      
      
      data <- data.frame(z = arima_model$x)
      mc_model <- tbr_model(data, lags = 1:20)
      
    }
    
    
    
    
  } else {
    
  }
  
  
  
  
}