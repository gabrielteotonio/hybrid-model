# Title: MC Hybrid Model for Time Series
# Author: Gabriel Teotonio
# Date: 2021/02/22


# Packages -----
library(forecast)
library(tidyverse)
library(e1071)

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

svm_model <- function(data, lags = 2:24) {
  
  best_performance <- 10^10
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
    
    if (model$best.performance < best_performance) {
      
      best_performance <- model$best.performance
      result_model <- model$best.model
    }
  }
  
  return(result_model)
}

# MC algorithm -----
MC <- function(data, mc_model = "tbr") {
  
  # ARIMA ----
  arima_model <- auto.arima(data, 
                            stepwise = TRUE)
  
  residuals <- data.frame(y = as.matrix(arima_model$residuals), 
                          date = time(arima_model$residuals)) %>% 
    as_tibble() %>% 
    mutate_at(vars(Y), .funs=lag_functions) %>% 
    select(-date)
  
  if (mc_model == "tbr") {
    
    # MNL -----
    mnl_model <- svm_model(residuals, lags = 2:24)
    
    # MC -----
    
    
    
  } else {
    
  }
  
  
  
  
}