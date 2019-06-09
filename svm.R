# SVM
train = Training[3000:4000, 2:7]
validation = Validation[, 2:7]
test = Test[, 2:7]
# Tuing parameter
C = c(0.01, 0.1, 0.5, 1, 5, 10, 100)
valid_accuracy = c()
for(i in C){
  svm.model = svm(Occupancy~., data = train, kernel = "linear",
                  cost = i, tolerance = 0.0001, scale = TRUE)
  svm.pred <- predict(svm.model, validation[,-6])
  # Calculate the validation accuracy
  for(j in 1:length(svm.pred)){
    if(svm.pred[j] >= 1){
      svm.pred[j] = 1
    }
    else{
      svm.pred[j] = 0
    }
  }
  accuracy = sum(validation$Occupancy == svm.pred)/length(svm.pred)
  valid_accuracy = append(valid_accuracy, accuracy)
}
best_C = C[which.max(valid_accuracy)]

# Using best_C to train the modeland predict the testing set
svm.model = svm(Occupancy~., data = train, kernel = "linear", cost = 100)
svm.pred <- predict(svm.model, test[,-6])
# Calculate the testing accuracy
for(i in 1:length(svm.pred)){
  if(svm.pred[i] >= 1){
    svm.pred[i] = 1
  }
  else{
    svm.pred[i] = 0
  }
}
svm.accuracy = sum(test$Occupancy == svm.pred)/length(svm.pred)
