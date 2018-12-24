##Analytics Vidhya: subham76

##approach reference: https://medium.com/data-science-analytics/black-friday-data-science-hackathon-4172a0554944

install.packages('rpart')
library(rpart)

##importing the training dataset
train_read<-read.csv(file.choose())
train<-train_read
head(train)

##importing the testing dataset
test_read<-read.csv(file.choose())
test<-test_read
head(test)

#working on the training set
class(train)
summary(train)
str(train)

train$data <- 1       #default value
test$Purchase <- 0    #default value
test$data <- 0        #default value

total <- rbind(train,test)

for (i in 1:11)
{
  total[,i] <- as.factor(total[,i])
}

#prediction
train.pre <- total[total$Marital_Status == 0 ,] 
train.pre <- train.pre[train.pre$data == 1,]
test.pre <- total[total$data == 0 ,]

train.pre$data <- NULL
test.pre$Purchase <- NULL
test.pre$data <- NULL

#Decision Tree
sample_model <- rpart(Purchase ~ .,data = train.pre)
predicted_tree <- predict(model, test.pre)

submit <- data.frame(User_ID = test$User_ID,
                     Product_ID = test$Product_ID,
                     Purchase = predicted_tree)
View(submit)
write.csv(submit, "C:/Users/Sam Bhattacharya/Desktop/ML/Samplesubmit.csv")

# XGboostAlgorithm (reference: https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/)
install.packages('xgboost')
library(xgboost)

for (i in 1:12)
{
  train.pre[,i] <-  as.numeric(train.pre[,i])
}

for (i in 1:11)
{
  test.pre[,i] <-  as.numeric(test.pre[,i])
}

##tuning the model
X_columns <- c( "User_ID" , "Product_ID" , "Gender" ,                   
                 "Age" , "Occupation" ,  "City_Category" ,           
                 "Stay_In_Current_City_Years" , "Product_Category_1" ,       
                 "Product_Category_2" , "Product_Category_3")
X_output <- train.pre$Purchase

xgtrain <- xgb.DMatrix(data <- as.matrix(train.pre[, X_columns]), label = X_output, missing = NA)
xgtest <- xgb.DMatrix(data <- as.matrix(test.pre[, X_columns]), missing = NA)


params <- list()
params$objective <- "reg:linear"
params$eta <- 0.1
params$max_depth <- 15
params$subsample <- 0.5
params$colsample_bytree <- 0.5
params$min_child_weight <- 2
params$seed=1
params$eval_metric <- "rmse"

##XGB Model
model_xgb <- xgb.train(params <- params, xgtrain, nrounds <- 100)
View(model_xgb)

y_pred <- predict(model_xgb, xgtest)

View(y_pred)
