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
## Lynx -----
data("lynx")
data_lynx <- lynx
lynx_train <- subset(data_lynx, end = 100)
lynx_test <- subset(data_lynx, start = 101)

## Sunspot -----
data_sunspot <- sunspot.year
sunspot_train <- subset(data_sunspot, end = 221)
sunspot_test <- subset(data_sunspot, start = 222)

## Exchange rate -----
data_exchange <- ts(read_csv("data/DEXUSUK.csv") %>% 
  select(DEXUSUK) %>%
  as.vector())
exchange_train <- subset(data_exchange, end = 679)
exchange_test <- subset(data_exchange, star = 678)

## Colorado river -----
tsdl_data <- subset(tsdl, description = "Monthly Flows, Colorado River Lees Ferry. 1911 – 1972")
data_colorado <- tsdl_data[[1]]
colorado_train <- subset(data_colorado, end = 595)
colorado_test <- subset(data_colorado, start = 596)

## Airline -----
tsdl_data <- subset(tsdl, description = "International airline passengers: monthly totals in thousands. Jan 49 – Dec 60")
airline_data <- tsdl_data[[1]]
airline_train <- subset(airline_data, end = 115)
airline_test <- subset(airline_data, start = 116)

## Star ----
data_star <- ts(as.vector(t(read.table("data/star.dat"))), start = 1, end = 600, frequency = 1)
star_train <- subset(data_star, end = 480)
star_test <- subset(data_star, start = 481)

# Trainning ----
## Lynx -----
lynx_trainning_svr <- MC_trainning(lynx_train, mc_model = "svr")
lynx_trainning_tbr <- MC_trainning(lynx_train, mc_model = "tbr")

## Sunspot -----
sunspot_trainning_svr <- MC_trainning(sunspot_train, mc_model = "svr")
sunspot_trainning_tbr <- MC_trainning(sunspot_train, mc_model = "tbr")

## Exchange rate  ----
exchange_trainning_svr <- MC_trainning(exchange_train, mc_model = "svr")
exchange_trainning_tbr <- MC_trainning(exchange_train, mc_model = "tbr")

## Colorado river -----
colorado_trainning_svr <- MC_trainning(colorado_train, mc_model = "svr")
colorado_trainning_tbr <- MC_trainning(colorado_train, mc_model = "tbr")

## Airline -----
airline_trainning_svr <- MC_trainning(airline_train, mc_model = "svr")
airline_trainning_tbr <- MC_trainning(airline_train, mc_model = "tbr")

## Star -----
star_trainning_svr <- MC_trainning(star_train, mc_model = "svr")
star_trainning_tbr <- MC_trainning(star_train, mc_model = "tbr")

# Testing -----
## Lynx -----
lynx_testing_svr <- MC_testing(lynx_test, lynx_trainning_svr)
lynx_testing_tbr <- MC_testing(lynx_test, lynx_trainning_tbr)

## Sunspot -----
sunspot_testing_svr <- MC_testing(sunspot_test, sunspot_trainning_svr)
sunspot_testing_tbr <- MC_testing(sunspot_test, sunspot_trainning_tbr)

## Exchange rate -----
exchange_testing_svr <- MC_testing(exchange_test, exchange_trainning_svr)
exchange_testing_tbr <- MC_testing(exchange_test, exchange_trainning_tbr)

## Colorado river -----
colorado_testing_svr <- MC_testing(colorado_test, colorado_trainning_svr)
colorado_testing_tbr <- MC_testing(colorado_test, colorado_trainning_tbr)

## Airline -----
airline_testing_svr <- MC_testing(airline_test, airline_trainning_svr)
airline_testing_tbr <- MC_testing(airline_test, airline_trainning_tbr)

## Star -----
star_testing_svr <- MC_testing(star_test, star_trainning_svr)
star_testing_tbr <- MC_testing(star_test, star_trainning_tbr)

# Plots of forecasting -----
## Lynx -----
### svr -----
data <- ts(lynx_test, 
           start = 1, 
           end = length(lynx_test), 
           frequency = 1)
pred_mc <- ts(lynx_testing_svr$mc_forecast,
              start = 1,
              end = length(lynx_test),
              frequency = 1)
pred_arima <- ts(lynx_testing_svr$arima_forecast,
                 start = 1,
                 end = length(lynx_test),
                 frequency = 1)
pred_sum <- ts(lynx_testing_svr$arima_forecast + lynx_testing_svr$mnl_forecast,
               start = 1,
               end = length(lynx_test),
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(0, 4000), 
     xlab="Conjunto de teste (Anualmente)", ylab="Qtd. de linces capturadas")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(8, 3500,#"top", 
       legend = c("Lynx", "MC(SVR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
       )
### tbr -----
data <- ts(lynx_test, 
           start = 1, 
           end = length(lynx_test), 
           frequency = 1)
pred_mc <- ts(lynx_testing_tbr$mc_forecast,
              start = 1,
              end = length(lynx_test),
              frequency = 1)
pred_arima <- ts(lynx_testing_tbr$arima_forecast,
                 start = 1,
                 end = length(lynx_test),
                 frequency = 1)
pred_sum <- ts(lynx_testing_tbr$arima_forecast + lynx_testing_tbr$mnl_forecast,
               start = 1,
               end = length(lynx_test),
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(0, 4000), 
     xlab="Conjunto de teste (Anualmente)", ylab="Qtd. de linces capturadas")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(8, 3500,#"top", 
       legend = c("Lynx", "MC(TBR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)

## Sunspot -----
### svr -----
data <- ts(sunspot_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(sunspot_testing_svr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(sunspot_testing_svr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(sunspot_testing_svr$arima_forecast + sunspot_testing_svr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(0, 200), 
     xlab="Conjunto de teste (Anualmente)", ylab="Qtd. de sunspot")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(8, 200,#"top", 
       legend = c("Sunspot", "MC(SVR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)
### tbr -----
data <- ts(sunspot_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(sunspot_testing_tbr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(sunspot_testing_tbr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(sunspot_testing_tbr$arima_forecast + sunspot_testing_tbr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(0, 200), 
     xlab="Conjunto de teste (Anualmente)", ylab="Qtd. de sunspot")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(8, 200,#"top", 
       legend = c("Sunspot", "MC(TBR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)

## Exchange rate -----
### svr -----
data <- ts(exchange_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(exchange_testing_svr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(exchange_testing_svr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(exchange_testing_svr$arima_forecast + exchange_testing_svr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(1.42, 1.62), 
     xlab="Conjunto de teste (Semanalmente)", ylab="Valor da taxa cambial")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(9, 1.62,#"top", 
       legend = c("Exchange", "MC(SVR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)
### tbr -----
data <- ts(exchange_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(exchange_testing_tbr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(exchange_testing_tbr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(exchange_testing_tbr$arima_forecast + exchange_testing_tbr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(1.42, 1.62), 
     xlab="Conjunto de teste (Semanalmente)", ylab="Valor da taxa cambial")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(9, 1.62,#"top", 
       legend = c("Exchange", "MC(TBR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)

## Colorado river -----
### svr -----
data <- ts(colorado_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(colorado_testing_svr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(colorado_testing_svr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(colorado_testing_svr$arima_forecast + colorado_testing_svr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(0, 5), 
     xlab="Conjunto de teste (Mensalmente)", ylab="Fluxo do rio Colorado")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(14, 4,#"top", 
       legend = c("Colorado", "MC(SVR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)
### tbr -----
data <- ts(colorado_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(colorado_testing_tbr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(colorado_testing_tbr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(colorado_testing_tbr$arima_forecast + colorado_testing_tbr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(0, 5), 
     xlab="Conjunto de teste (Mensalmente)", ylab="Fluxo do rio Colorado")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(14, 4,#"top", 
       legend = c("Colorado", "MC(TBR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)

## Airline -----
### svr -----
data <- ts(airline_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(airline_testing_svr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(airline_testing_svr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(airline_testing_svr$arima_forecast + airline_testing_svr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(250, 620), 
     xlab="Conjunto de teste (Mensalmente)", ylab="Qtd. de passageiros")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(14, 600,#"top", 
       legend = c("Airline", "MC(SVR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)
### tbr -----
data <- ts(airline_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(airline_testing_tbr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(airline_testing_tbr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(airline_testing_tbr$arima_forecast + airline_testing_tbr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(250, 620), 
     xlab="Conjunto de teste (Mensalmente)", ylab="Qtd. de passageiros")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(14, 600,#"top", 
       legend = c("Airline", "MC(TBR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)

## Star -----
### svr -----
data <- ts(star_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(star_testing_svr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(star_testing_svr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(star_testing_svr$arima_forecast + star_testing_svr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(5, 26), 
     xlab="Conjunto de teste (Diariamente)", ylab="Brilho da estrela")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(14, 15,#"top", 
       legend = c("Star", "MC(SVR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)
### tbr -----
data <- ts(star_test, 
           start = 1, 
           end = 30, 
           frequency = 1)
pred_mc <- ts(star_testing_tbr$mc_forecast,
              start = 1,
              end = 30,
              frequency = 1)
pred_arima <- ts(star_testing_tbr$arima_forecast,
                 start = 1,
                 end = 30,
                 frequency = 1)
pred_sum <- ts(star_testing_tbr$arima_forecast + star_testing_tbr$mnl_forecast,
               start = 1,
               end = 30,
               frequency = 1)

plot(data, type="l", pch=35, col="black", ylim = c(5, 26), 
     xlab="Conjunto de teste (Diariamente)", ylab="Brilho da estrela")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)

legend(14, 17,#"top", 
       legend = c("Star", "MC(TBR)", "ARIMA", "HÍBRIDO"), 
       col = c("black", "red", "blue", "green"), 
       pch = c(10,9), 
       bty = "n", 
       pt.cex = 1, 
       cex = .8, 
       text.col = "black", 
       horiz = F #, 
       #inset = c(0.1, 0.1),
)

# Quality measures
## Lynx
lynx_measures <- rbind(c(MSE(y_pred = lynx_testing_svr$arima_forecast, y_true = as.vector(lynx_test)),
                         MSE(y_pred = lynx_testing_svr$arima_forecast + lynx_testing_svr$mnl_forecast, 
                             y_true = as.vector(lynx_test)),
                         MSE(y_pred = lynx_testing_svr$arima_forecast + lynx_testing_tbr$mnl_forecast, 
                             y_true = as.vector(lynx_test)),
                         MSE(y_pred = lynx_testing_svr$mc_forecast, y_true = as.vector(lynx_test)),
                         MSE(y_pred = lynx_testing_tbr$mc_forecast, y_true = as.vector(lynx_test))),
                       c(MAPE(y_pred = lynx_testing_svr$arima_forecast, y_true = as.vector(lynx_test)),
                         MAPE(y_pred = lynx_testing_svr$arima_forecast + lynx_testing_svr$mnl_forecast, 
                             y_true = as.vector(lynx_test)),
                         MAPE(y_pred = lynx_testing_svr$arima_forecast + lynx_testing_tbr$mnl_forecast, 
                              y_true = as.vector(lynx_test)),
                         MAPE(y_pred = lynx_testing_svr$mc_forecast, y_true = as.vector(lynx_test)),
                         MAPE(y_pred = lynx_testing_tbr$mc_forecast, y_true = as.vector(lynx_test))),
                       c(MAE(y_pred = lynx_testing_svr$arima_forecast, y_true = as.vector(lynx_test)),
                         MAE(y_pred = lynx_testing_svr$arima_forecast + lynx_testing_svr$mnl_forecast, 
                             y_true = as.vector(lynx_test)),
                         MAE(y_pred = lynx_testing_svr$arima_forecast + lynx_testing_tbr$mnl_forecast, 
                             y_true = as.vector(lynx_test)),
                         MAE(y_pred = lynx_testing_svr$mc_forecast, y_true = as.vector(lynx_test)),
                         MAE(y_pred = lynx_testing_tbr$mc_forecast, y_true = as.vector(lynx_test))
                      )) %>% as.data.frame() %>% 
  rename(arima = V1, hibrido_svr = V2, hibrido_tbr = V3, mc_svr = V4, mc_tbr = V5) %>% 
  mutate(data = "lynx", metric = c("MSE", "MAPE", "MAE"))

## Sunspot
sunspot_measures <- rbind(c(MSE(y_pred = sunspot_testing_svr$arima_forecast, y_true = as.vector(sunspot_test)),
                         MSE(y_pred = sunspot_testing_svr$arima_forecast + sunspot_testing_svr$mnl_forecast, 
                             y_true = as.vector(sunspot_test)),
                         MSE(y_pred = sunspot_testing_svr$arima_forecast + sunspot_testing_tbr$mnl_forecast, 
                             y_true = as.vector(sunspot_test)),
                         MSE(y_pred = sunspot_testing_svr$mc_forecast, y_true = as.vector(sunspot_test)),
                         MSE(y_pred = sunspot_testing_tbr$mc_forecast, y_true = as.vector(sunspot_test))),
                       c(MAPE(y_pred = sunspot_testing_svr$arima_forecast, y_true = as.vector(sunspot_test)),
                         MAPE(y_pred = sunspot_testing_svr$arima_forecast + sunspot_testing_svr$mnl_forecast, 
                              y_true = as.vector(sunspot_test)),
                         MAPE(y_pred = sunspot_testing_svr$arima_forecast + sunspot_testing_tbr$mnl_forecast, 
                              y_true = as.vector(sunspot_test)),
                         MAPE(y_pred = sunspot_testing_svr$mc_forecast, y_true = as.vector(sunspot_test)),
                         MAPE(y_pred = sunspot_testing_tbr$mc_forecast, y_true = as.vector(sunspot_test))),
                       c(MAE(y_pred = sunspot_testing_svr$arima_forecast, y_true = as.vector(sunspot_test)),
                         MAE(y_pred = sunspot_testing_svr$arima_forecast + sunspot_testing_svr$mnl_forecast, 
                             y_true = as.vector(sunspot_test)),
                         MAE(y_pred = sunspot_testing_svr$arima_forecast + sunspot_testing_tbr$mnl_forecast, 
                             y_true = as.vector(sunspot_test)),
                         MAE(y_pred = sunspot_testing_svr$mc_forecast, y_true = as.vector(sunspot_test)),
                         MAE(y_pred = sunspot_testing_tbr$mc_forecast, y_true = as.vector(sunspot_test))
                       )) %>% as.data.frame() %>% 
  rename(arima = V1, hibrido_svr = V2, hibrido_tbr = V3, mc_svr = V4, mc_tbr = V5) %>% 
  mutate(data = "sunspot", metric = c("MSE", "MAPE", "MAE"))

## Exchange
exchange_measures <- rbind(c(MSE(y_pred = exchange_testing_svr$arima_forecast, y_true = as.vector(exchange_test)),
                            MSE(y_pred = exchange_testing_svr$arima_forecast + exchange_testing_svr$mnl_forecast, 
                                y_true = as.vector(exchange_test)),
                            MSE(y_pred = exchange_testing_svr$arima_forecast + exchange_testing_tbr$mnl_forecast, 
                                y_true = as.vector(exchange_test)),
                            MSE(y_pred = exchange_testing_svr$mc_forecast, y_true = as.vector(exchange_test)),
                            MSE(y_pred = exchange_testing_tbr$mc_forecast, y_true = as.vector(exchange_test))),
                          c(MAPE(y_pred = exchange_testing_svr$arima_forecast, y_true = as.vector(exchange_test)),
                            MAPE(y_pred = exchange_testing_svr$arima_forecast + exchange_testing_svr$mnl_forecast, 
                                 y_true = as.vector(exchange_test)),
                            MAPE(y_pred = exchange_testing_svr$arima_forecast + exchange_testing_tbr$mnl_forecast, 
                                 y_true = as.vector(exchange_test)),
                            MAPE(y_pred = exchange_testing_svr$mc_forecast, y_true = as.vector(exchange_test)),
                            MAPE(y_pred = exchange_testing_tbr$mc_forecast, y_true = as.vector(exchange_test))),
                          c(MAE(y_pred = exchange_testing_svr$arima_forecast, y_true = as.vector(exchange_test)),
                            MAE(y_pred = exchange_testing_svr$arima_forecast + exchange_testing_svr$mnl_forecast, 
                                y_true = as.vector(exchange_test)),
                            MAE(y_pred = exchange_testing_svr$arima_forecast + exchange_testing_tbr$mnl_forecast, 
                                y_true = as.vector(exchange_test)),
                            MAE(y_pred = exchange_testing_svr$mc_forecast, y_true = as.vector(exchange_test)),
                            MAE(y_pred = exchange_testing_tbr$mc_forecast, y_true = as.vector(exchange_test))
                          )) %>% as.data.frame() %>% 
  rename(arima = V1, hibrido_svr = V2, hibrido_tbr = V3, mc_svr = V4, mc_tbr = V5) %>% 
  mutate(data = "exchange", metric = c("MSE", "MAPE", "MAE"))

## Colorado
colorado_measures <- rbind(c(MSE(y_pred = colorado_testing_svr$arima_forecast, y_true = as.vector(colorado_test)),
                             MSE(y_pred = colorado_testing_svr$arima_forecast + colorado_testing_svr$mnl_forecast, 
                                 y_true = as.vector(colorado_test)),
                             MSE(y_pred = colorado_testing_svr$arima_forecast + colorado_testing_tbr$mnl_forecast, 
                                 y_true = as.vector(colorado_test)),
                             MSE(y_pred = colorado_testing_svr$mc_forecast, y_true = as.vector(colorado_test)),
                             MSE(y_pred = colorado_testing_tbr$mc_forecast, y_true = as.vector(colorado_test))),
                           c(MAPE(y_pred = colorado_testing_svr$arima_forecast, y_true = as.vector(colorado_test)),
                             MAPE(y_pred = colorado_testing_svr$arima_forecast + colorado_testing_svr$mnl_forecast, 
                                  y_true = as.vector(colorado_test)),
                             MAPE(y_pred = colorado_testing_svr$arima_forecast + colorado_testing_tbr$mnl_forecast, 
                                  y_true = as.vector(colorado_test)),
                             MAPE(y_pred = colorado_testing_svr$mc_forecast, y_true = as.vector(colorado_test)),
                             MAPE(y_pred = colorado_testing_tbr$mc_forecast, y_true = as.vector(colorado_test))),
                           c(MAE(y_pred = colorado_testing_svr$arima_forecast, y_true = as.vector(colorado_test)),
                             MAE(y_pred = colorado_testing_svr$arima_forecast + colorado_testing_svr$mnl_forecast, 
                                 y_true = as.vector(colorado_test)),
                             MAE(y_pred = colorado_testing_svr$arima_forecast + colorado_testing_tbr$mnl_forecast, 
                                 y_true = as.vector(colorado_test)),
                             MAE(y_pred = colorado_testing_svr$mc_forecast, y_true = as.vector(colorado_test)),
                             MAE(y_pred = colorado_testing_tbr$mc_forecast, y_true = as.vector(colorado_test))
                           )) %>% as.data.frame() %>% 
  rename(arima = V1, hibrido_svr = V2, hibrido_tbr = V3, mc_svr = V4, mc_tbr = V5) %>% 
  mutate(data = "colorado", metric = c("MSE", "MAPE", "MAE"))

## Airline
airline_measures <- rbind(c(MSE(y_pred = airline_testing_svr$arima_forecast, y_true = as.vector(airline_test)),
                             MSE(y_pred = airline_testing_svr$arima_forecast + airline_testing_svr$mnl_forecast, 
                                 y_true = as.vector(airline_test)),
                             MSE(y_pred = airline_testing_svr$arima_forecast + airline_testing_tbr$mnl_forecast, 
                                 y_true = as.vector(airline_test)),
                             MSE(y_pred = airline_testing_svr$mc_forecast, y_true = as.vector(airline_test)),
                             MSE(y_pred = airline_testing_tbr$mc_forecast, y_true = as.vector(airline_test))),
                           c(MAPE(y_pred = airline_testing_svr$arima_forecast, y_true = as.vector(airline_test)),
                             MAPE(y_pred = airline_testing_svr$arima_forecast + airline_testing_svr$mnl_forecast, 
                                  y_true = as.vector(airline_test)),
                             MAPE(y_pred = airline_testing_svr$arima_forecast + airline_testing_tbr$mnl_forecast, 
                                  y_true = as.vector(airline_test)),
                             MAPE(y_pred = airline_testing_svr$mc_forecast, y_true = as.vector(airline_test)),
                             MAPE(y_pred = airline_testing_tbr$mc_forecast, y_true = as.vector(airline_test))),
                           c(MAE(y_pred = airline_testing_svr$arima_forecast, y_true = as.vector(airline_test)),
                             MAE(y_pred = airline_testing_svr$arima_forecast + airline_testing_svr$mnl_forecast, 
                                 y_true = as.vector(airline_test)),
                             MAE(y_pred = airline_testing_svr$arima_forecast + airline_testing_tbr$mnl_forecast, 
                                 y_true = as.vector(airline_test)),
                             MAE(y_pred = airline_testing_svr$mc_forecast, y_true = as.vector(airline_test)),
                             MAE(y_pred = airline_testing_tbr$mc_forecast, y_true = as.vector(airline_test))
                           )) %>% as.data.frame() %>% 
  rename(arima = V1, hibrido_svr = V2, hibrido_tbr = V3, mc_svr = V4, mc_tbr = V5) %>% 
  mutate(data = "airline", metric = c("MSE", "MAPE", "MAE"))

## Star
star_test[star_test == 0] <- c(0.001, 0.001)
star_measures <- rbind(c(MSE(y_pred = star_testing_svr$arima_forecast, y_true = as.vector(star_test)),
                            MSE(y_pred = star_testing_svr$arima_forecast + star_testing_svr$mnl_forecast, 
                                y_true = as.vector(star_test)),
                            MSE(y_pred = star_testing_svr$arima_forecast + star_testing_tbr$mnl_forecast, 
                                y_true = as.vector(star_test)),
                            MSE(y_pred = star_testing_svr$mc_forecast, y_true = as.vector(star_test)),
                            MSE(y_pred = star_testing_tbr$mc_forecast, y_true = as.vector(star_test))),
                          c(MAPE(y_pred = star_testing_svr$arima_forecast, y_true = as.vector(star_test)),
                            MAPE(y_pred = star_testing_svr$arima_forecast + star_testing_svr$mnl_forecast, 
                                 y_true = as.vector(star_test)),
                            MAPE(y_pred = star_testing_svr$arima_forecast + star_testing_tbr$mnl_forecast, 
                                 y_true = as.vector(star_test)),
                            MAPE(y_pred = star_testing_svr$mc_forecast, y_true = as.vector(star_test)),
                            MAPE(y_pred = star_testing_tbr$mc_forecast, y_true = as.vector(star_test))),
                          c(MAE(y_pred = star_testing_svr$arima_forecast, y_true = as.vector(star_test)),
                            MAE(y_pred = star_testing_svr$arima_forecast + star_testing_svr$mnl_forecast, 
                                y_true = as.vector(star_test)),
                            MAE(y_pred = star_testing_svr$arima_forecast + star_testing_tbr$mnl_forecast, 
                                y_true = as.vector(star_test)),
                            MAE(y_pred = star_testing_svr$mc_forecast, y_true = as.vector(star_test)),
                            MAE(y_pred = star_testing_tbr$mc_forecast, y_true = as.vector(star_test))
                          )) %>% as.data.frame() %>% 
  rename(arima = V1, hibrido_svr = V2, hibrido_tbr = V3, mc_svr = V4, mc_tbr = V5) %>% 
  mutate(data = "star", metric = c("MSE", "MAPE", "MAE"))

## Consolidate ----
measures <- rbind(lynx_measures, sunspot_measures, exchange_measures, colorado_measures, airline_measures, star_measures)
write_csv(measures, "results/measures_quality.csv")
