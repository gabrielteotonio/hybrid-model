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
MC <- function(data, mc_model = "tbr") {
  
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
    
    # error here
    data_comb <- bind_cols(arima_model$x, comb_ml, comb_mnl) %>% 
      rename(y = ...1)

    mc_model <- svm_model(data_comb, lags = 1:21, method = "mc")
      
    
  }
  
  return(list("arima_model" = arima_model,
              "mnl_model" = mnl_model,
              "mc_model" = mc_model))
  
}
test_mc <- MC(data)
pred <- ts(predict(test_mc$mc_model$best_model), 
           start = 1821, 
           end = 1934,
           frequency = 1)
plot(data, type="l", pch=35, col="black", xlab="Value", ylab="Time", main= "Lynx data (tbr)")
lines(pred, pch=35, col="red", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


test_mc_svr <- MC(data, mc_model = "svr")
pred_svr <- ts(predict(test_mc_svr$mc_model$best_model), 
               start = 1821, 
               end = 1934,
               frequency = 1)
plot(data, type="l", pch=35, col="black", xlab="Value", ylab="Time", main= "Lynx data (svr)")
lines(pred_svr, pch=35, col="red", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)


par(mfrow=c(1,2))
