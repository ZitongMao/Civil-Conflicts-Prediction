library(plyr)
library(randomForest)
library(ROCR)

# loading data
load("~/CS112 Final/WGB Replication Files/Data/fl.three.RData")
data_test <- fl.three

set.seed(1)

# k-fold cross validation method

k = 5 # Folds

# sample from 1 to k, nrow times (the number of observations in the data_test)
data_test$id <- sample(1:k, nrow(data_test), replace = TRUE)
list <- 1:k

# prediction and testset data_test frames that we add to with each iteration over
# the folds
prediction.f <- data.frame()
prediction <- data.frame()
testsetCopy <- data.frame()

# creating a progress bar to visualize the CV process
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

# loop for k times
for (i in 1:k) {
  # remove rows with id i from data_testframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data_test, id %in% list[-i]) #[-i] means except i
  testset <- subset(data_test, id %in% list[i])
  
  # run the random forest model
  flmodel.forest <- randomForest(onset ~ warl + gdpenl + 
                                   lpopl1 + lmtnest + ncontig + 
                                   Oil + nwstate + instab + polity2l
                                 + ethfrac + relfrac, data = trainingset, importance = TRUE)
  
  # getting predicted results
  temp <- as.data.frame(predict(flmodel.forest, testset))
  temp.f <- ifelse(temp > 0.3, 1, 0) # threshold
  
  # append this iteration's predictions to the end of the prediction data_test frame
  prediction <- rbind(prediction, temp)
  prediction.f <- rbind(prediction.f, temp.f)
  
  # append this iteration's test set to the test set copy data frame
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset))
  
  progress.bar$step()
}

# add predictions and actual dependent variables values
result <- cbind(prediction.f, testsetCopy$onset)
names(result) <- c("Predicted", "Actual")
result$Difference <- result$Actual == result$Predicted

# use % correct classification 
table(result$Difference)
# % Accuracy
sum(result$Difference)/length(result$Difference)

# create ROC Plot
pred<- prediction(prediction, testsetCopy$onset)
perf<- performance(pred,"tpr","fpr")
plot(perf, main="ROC Plot: Random Forest Model (Cross-validated)", lwd=3)

# calculate the Area Under Curve (AUC)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

text(0.3,0.4,"Total Area Under Curve: 0.673",adj=c(0,1))

#show importance of variables in the random forest
varImpPlot (flmodel.forest)

