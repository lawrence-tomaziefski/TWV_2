require(dplyr)
require(caret)
require(mlbench)
require(parallel)
require(doParallel)


phase_iii = read.csv("phase_iii.csv", header = TRUE, stringsAsFactors = FALSE)

data = phase_iii %>% 
        select(7:8,-9, -10, 11:26, -(27:28), 29:33) %>%
        mutate(grouped_unit_type = as.factor(grouped_unit_type),
               vignette = as.factor(vignette),
               role = as.factor(role))



data_cs = scale(data[4:23], center = TRUE, scale = TRUE)
data = cbind(data[,1:3],data_cs)

set.seed(5001)
in_train = createDataPartition(y = data$grouped_unit_type, p = .75, list = FALSE)
training = data[in_train, ]
testing = data[-in_train, ]

levels(training[,1:3]) = levels(testing[,1:3])

x = training[,-1]
y = as.factor(training[,1])

cluster = makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fit_control3 = trainControl(method = "boot", number = 30, allowParallel = TRUE)
fit3 = train(x, y, method = "rf", trControl = fit_control3, importance = TRUE, na.action = na.omit) 

stopCluster(cluster)
registerDoSEQ()

fit3
fit3$times
fit3$resample
confusionMatrix.train(fit3)

var_importance = unlist(varImp(fit3))

predict_fit3= predict(fit3, testing[,-1])

confusionMatrix(predict_fit3, testing$grouped_unit)





data1 = data %>% select(-(2:3), -(19:23))


set.seed(5001)
in_train = createDataPartition(y = data1$grouped_unit_type, p = .75, list = FALSE)
training = data1[in_train, ]
testing = data1[-in_train, ]

levels(training[,1:3]) = levels(testing[,1:3])

x = training[,-1]
y = as.factor(training[,1])

cluster = makeCluster(detectCores() - 1)
registerDoParallel(cluster)

fit_control3 = trainControl(method = "boot", number = 30, allowParallel = TRUE)
fit3 = train(x, y, method = "rf", trControl = fit_control3, importance = TRUE, na.action = na.omit) 

stopCluster(cluster)
registerDoSEQ()

fit3
fit3$times
fit3$resample
confusionMatrix.train(fit3)

var_importance = as.data.frame(unlist(varImp(fit3)))

predict_fit3= predict(fit3, testing[,-1])
confusionMatrix(predict_fit3, testing$grouped_unit)
