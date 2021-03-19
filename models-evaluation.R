# Title: Evaluation for MC Hybrid Models
# Author: Gabriel Teotonio
# Date: 2021/03/18

# Packages -----
library(forecast)
library(tidyverse)
library(e1071)
library(rpart)
library(MLmetrics)
library(glue)
library(boot)
library(tsdl)


# Data sets -----
# Lynx -----
data("lynx")
data_lynx <- lynx
lynx_train <- subset(data_lynx, end = 100)
lynx_test <- subset(data_lynx, start = 101)

# Sunspot -----
data_sunspot <- sunspot.year
sunspot_train <- subset(data_sunspot, end = 221)
sunspot_test <- subset(data_sunspot, start = 222)

# Exchange rate -----
data_exchange <- ts(read_csv("data/DEXUSUK.csv") %>% 
  select(DEXUSUK) %>%
  as.vector())
exchange_train <- subset(data_exchange, end = 679)
exchange_test <- subset(data_exchange, star = 678)

# Colorado river -----
tsdl_data <- subset(tsdl, description = "Monthly Flows, Colorado River Lees Ferry. 1911 – 1972")
data_colorado <- tsdl_data[[1]]
colorado_train <- subset(data_colorado, end = 595)
colorado_test <- subset(data_colorado, start = 596)

# Airline -----
tsdl_data <- subset(tsdl, description = "International airline passengers: monthly totals in thousands. Jan 49 – Dec 60")
airline_data <- tsdl_data[[1]]
airline_train <- subset(airline_data, end = 115)
airline_test <- subset(airline_data, start = 116)

# Star ----
data_star <- ts(as.vector(t(read.table("data/star.dat"))), start = 1, end = 600, frequency = 1)
star_train <- subset(data_star, end = 480)
star_test <- subset(data_star, start = 481)

