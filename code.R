model <- tune(randomForest, 
              as.formula(paste(colnames(data)[1], "~",
                               paste(colnames(data)[c(2:i)], collapse = "+"),
                               sep = ""
              )),
              data = data,
              na.action = na.omit#,
              #ranges = list(#ntree = c(60, 70, 80, 90),
              #              nodesize = c(3, 5, 8, 10)
              #)
)
######
data <- subset(data, end = 100)
mc_trainning <- MC_trainning(data, mc_model = "svr")

data <- lynx
data <- subset(data, start = 101)
mc_testing <- MC_testing(data, mc_trainning)

pred_mc <- ts(mc_testing$mc_forecast,
              start = 1921,
              end = 1934,
              frequency = 1)
pred_arima <- ts(mc_testing$arima_forecast,
                 start = 1921,
                 end = 1934,
                 frequency = 1)
pred_sum <- ts(mc_testing$arima_forecast + mc_testing$mnl_forecast$y,
               start = 1921,
               end = 1934,
               frequency = 1)
plot(data, type="l", pch=35, col="black", xlab="Value", ylab="Time", main= "Lynx data (tbr)")
lines(pred_mc, pch=35, col="red", type="l", lty=2)
lines(pred_arima, pch=35, col="blue", type="l", lty=2)
lines(pred_sum, pch=35, col="green", type="l", lty=2)
legend(1835, 6000, legend=c("Observed", "Fitted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
######

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