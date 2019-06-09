# Random Forest
train = Training[, 2:7]
validation = Validation[, 2:7]
test = Test[, 2:7]

train$Occupancy <- as.factor(train$Occupancy)
validation$Occupancy <- as.factor(validation$Occupancy)
test$Occupancy <- as.factor(test$Occupancy)

library(tree)
library(randomForest)
m = c(1,2,3,4,5)
acc_list = c()
# Tuning parameter mtry with validation set
for(i in m){
  RF.train =randomForest(Occupancy~.,data=train,
                        mtry=i, importance =TRUE, ntree=500)
#plot(RF.train)
#getTree(RF.train, 1, labelVar=TRUE)
  # Prediction on test set
  yhat.RF = predict(RF.train, newdata=validation[,1:5])
  # Calculate accuracy
  acc = sum((as.integer(validation$Occupancy)==as.integer(yhat.RF))/length(yhat.RF))
  acc_list = append(acc_list, acc)
}
best_mtry = m[which.max(acc_list)]

# Test on the testing set
RF.train =randomForest(Occupancy~.,data=train,
                       mtry=best_mtry, importance =TRUE, ntree=500)
yhat.RF = predict(RF.train, newdata=test[,1:5])
accuracy = sum((as.integer(test$Occupancy)==as.integer(yhat.RF))/length(yhat.RF))
print(accuracy)