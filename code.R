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