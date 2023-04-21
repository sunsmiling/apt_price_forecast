Sys.setlocale("LC_ALL","Korean") 
data = read.csv("../data/preprocessed_df.csv")
head(data) 
#data = df_total_merge
set.seed(1227)
data %>% head()
colnames(data)[12:15]
data = data %>% select(-c(시군구,단지명,도로명,kaptCode,kaptName.x,kaptName.y,kaptAddr,
                          doroJuso.x, doroJuso.y,hoCnt,bjdCode,subwayLine))
data %>% head()
#Use 70% of dataset as training set and remaining 30% as testing set
set.seed(1227)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, ]
test   <- data[!sample, ]
train %>% dim() #[1] 31843    55
head(train)
## Linear model MSE
model.lm <- lm(단위면적당전세가~., data = train)
lm_summary <-summary(model.lm)
mean(lm_summary$residuals^2) #train mse : 50544.24
mean((test$단위면적당전세가- predict(model.lm, test))^2, na.rm = T) #test mse : 51728.57
 
## xgboost mse
library(xgboost)
library(caret)  
train_x = data.matrix(train[, -1])
train_y = train[,1]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -1])
test_y = test[, 1]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)
#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteration
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)
#define final model
model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

summary(model_xgboost)
pred_y = predict(model_xgboost, xgb_test)
sqrt(mean((test_y - pred_y)^2)) #mse - Mean Squared Error: 172.0508
