# Title: MC Hybrid Model for Time Series
# Author: Gabriel Teotonio
# Date: 2021/02/22


# Packages -----
library(forecast)
library(tidyverse)
library(e1071)
library(rpart)
library(MLmetrics)
library(glue)

# Aux. functions -----

# Lags -----
lags <- c(2:24)

lag_names <- glue('lag_{str_pad(lags, nchar(max(lags)), pad = "0")}')

lag_functions <-
  map(lags, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
  set_names(lag_names)
# ------
lags_mc <- c(1:20)

lag_names_ml <- glue('lag_{str_pad(lags_mc, nchar(max(lags)), pad = "0")}_ml')
lag_names_mnl <- glue('lag_{str_pad(lags_mc, nchar(max(lags)), pad = "0")}_mnl')

lag_functions_ml <-
  map(lags_mc, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
  set_names(lag_names_ml)

lag_functions_mnl <-
  map(lags_mc, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
  set_names(lag_names_mnl)

# SVR model -----
svm_model <- function(data, lags = 2:24, method = "mnl") {
  
  if (method == "mnl") {
    best_performance <- 10^10
    result_n_lags <- 0
    
    for (i in lags) {
      print(i)
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
  } else {
    best_performance <- 10^10
    result_n_lags <- 0
    part_ml <- data[,2:22]
    part_mnl <- data[,23:43]
   
    for (i in lags) {
      print(i)
      model <- tune(svm, 
                    as.formula(paste(colnames(data)[1], "~",
                                     paste0(paste(colnames(part_ml)[c(1:i)], collapse = "+"), 
                                            "+", 
                                            paste(colnames(part_mnl)[c(1:i)], collapse = "+")),
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
}

# TBR model -----
tbr_model <- function(data, lags = 2:24, method = "mnl") {
  
  if (method == "mnl") {
    best_performance <- 10^10
    result_n_lags <- 0
    
    for (i in lags) {
      print(i)
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
  } else {
    best_performance <- 10^10
    result_n_lags <- 0
    part_ml <- data[,2:22]
    part_mnl <- data[,23:43]
    
    for (i in lags) {
      print(i)
      tree <- rpart(as.formula(paste(colnames(data)[1], "~",
                                     paste0(paste(colnames(part_ml)[c(1:i)], collapse = "+"), 
                                            "+", 
                                            paste(colnames(part_mnl)[c(1:i)], collapse = "+")),
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
}

# MC algorithm -----
MC_trainning <- function(data, mc_model = "tbr") {
  
  arima_order <- c()
  
  # ARIMA ----
  print("Fitting ARIMA")
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
    print("MNL")
    mnl_model <- svm_model(residuals, lags = 2:24, method = "mnl")
    
    # MC -----
    print("MC")
    comb_ml <- data.frame(ml = as.vector(arima_model$fitted)) %>% 
      as_tibble() %>% 
      mutate_at(vars(ml), .funs=lag_functions_ml)
    
    comb_mnl <- data.frame(mnl = c(rep(0, mnl_model$best_n_lags), predict(mnl_model$best_model))) %>% 
      as_tibble() %>% 
      mutate_at(vars(mnl), .funs=lag_functions_mnl)
    
    data_comb <- bind_cols(arima_model$x, comb_ml, comb_mnl) %>% 
      rename(y = ...1)

    mc_model <- tbr_model(data_comb, lags = 1:21, method = "mc")
      
    
  } else {
    # MNL -----
    print("MNL")
    mnl_model <- tbr_model(residuals, lags = 2:24, method = "mnl")
    
    # MC -----
    print("MC")
    comb_ml <- data.frame(ml = as.vector(arima_model$fitted)) %>% 
      as_tibble() %>% 
      mutate_at(vars(ml), .funs=lag_functions_ml)
    
    lag_dif <- length(arima_model$fitted) - length(predict(mnl_model$best_model))
    comb_mnl <- data.frame(mnl = c(rep(0, lag_dif), predict(mnl_model$best_model))) %>% 
      as_tibble() %>% 
      mutate_at(vars(mnl), .funs=lag_functions_mnl)
    
    data_comb <- bind_cols(arima_model$x, comb_ml, comb_mnl) %>% 
      rename(y = ...1)

    mc_model <- svm_model(data_comb, lags = 1:21, method = "mc")
      
    
  }
  
  return(list("arima_model" = arima_model,
              "mnl_model" = mnl_model,
              "mc_model" = mc_model))
  
}

# MC testing -----
MC_testing <- function(data, mc_trainning_object) {
  
  arima_model <- mc_trainning_object$arima_model
  mnl_model <- mc_trainning_object$mnl_model
  mc_model <- mc_trainning_object$mc_model
  mnl_forecast <- as_tibble()
  
  # Arima forecast -----
  new_fit <- Arima(c(arima_model$x, data), model = arima_model)
  arima_forecast_one_step <- tail(fitted(new_fit), n = length(data))
  arima_forecast <- fitted(new_fit)
  
  # MNL forecast -----
  residuals <- data.frame(y = as.matrix(arima_model$residuals), 
                          date = time(arima_model$residuals)) %>% 
    as_tibble() %>% 
    mutate_at(vars(y), .funs=lag_functions) %>% 
    select(-date)
  
  best_n_lags_mnl <- mnl_model$best_n_lags
  new_data <- tail(residuals, n = 1)[1, 1:best_n_lags_mnl]
  for (i in 1:length(data)) {
    
    mnl_forecast_one_step <- predict(mnl_model$best_model, new_data)
    append_data <- (c(mnl_forecast_one_step, 
                      new_data) %>% 
                      as_tibble(.name_repair = ~names(c("", new_data))))
    append_data <- append_data[, 1:(length(append_data)-1)]
    names(append_data) <- names(new_data)
    mnl_forecast <- bind_rows(mnl_forecast,
                              append_data)
    new_data <- tail(mnl_forecast, n = 1)[1, 1:best_n_lags_mnl]
  
  }
  
  # MC forecast -----
  # lags -----
  best_n_lags_mc <- mc_model$best_n_lags
  lags_test <- c(1:best_n_lags_mc)
  
  lag_names_ml_test <- glue('lag_{str_pad(lags_test, nchar(max(lags)), pad = "0")}_ml')
  lag_names_mnl_test <- glue('lag_{str_pad(lags_test, nchar(max(lags)), pad = "0")}_mnl')
  
  lag_functions_ml_test <-
    map(lags_test, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
    set_names(lag_names_ml_test)
  
  lag_functions_mnl_test <-
    map(lags_test, ~ eval(parse(text=glue("~ dplyr::lag(.x, {.x})")))) %>%
    set_names(lag_names_mnl_test)
  
  # combined forecasts -----
  comb_ml_test <- data.frame(ml = as.vector(arima_forecast)) %>% 
    as_tibble() %>% 
    mutate_at(vars(ml), .funs=lag_functions_ml_test)
  
  lag_dif_test <- length(arima_model$fitted) - length(predict(mnl_model$best_model))
  comb_mnl_test <- data.frame(mnl = c(rep(0, lag_dif_test), predict(mnl_model$best_model), mnl_forecast$y)) %>% 
    as_tibble() %>% 
    mutate_at(vars(mnl), .funs=lag_functions_mnl_test)
  
  data_comb_test <- bind_cols(comb_ml_test, comb_mnl_test)
  data_comb_test <- tail(data_comb_test, n = length(data))
  mc_forecast <- predict(mc_model$best_model, data_comb_test)
  
  return(list("arima_forecast" = arima_forecast_one_step,
              "mnl_forecast" = mnl_forecast$y,
              "mc_forecast" = mc_forecast))
}
