library(plyr)
library(randomForest)

# data contains parameters related to how physicians react to different types of marketing techniques (ie digital, email, phone call, etc). 
load("~/CS112 Final/WGB Replication Files/Data/fl.three.RData")
data_test <- fl.three
set.seed(1)
# predict the dependent variable values from the other variables in the data_testset

k = 5 # Folds

# sample from 1 to k, nrow times (the number of observations in the data_test)
data_test$id <- sample(1:k, nrow(data_test), replace = TRUE)
list <- 1:k

# prediction and testset data_test frames that we add to with each iteration over
# the folds
prediction.f <- data.frame()
prediction <- data.frame()
testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)


for (i in 1:k) {
  # remove rows with id i from data_testframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data_test, id %in% list[-i]) #[-i] means except i
  testset <- subset(data_test, id %in% list[i])
  
  # run a random forest model
  
  
  #  flmodel.forest2 <- randomForest(as.factor(onset) ~ warl + gdpenl + 
  #                                     lpopl1 + lmtnest + ncontig + 
  #                                    Oil + nwstate + instab + polity2l
  #                                  + ethfrac + relfrac, data = trainingset, mtry = 11, importance = TRUE)
  
#  flmodel.forest <- randomForest(onset ~ warl + gdpenl + 
#                                   lpopl1 + lmtnest + ncontig + 
#                                   Oil + nwstate + instab + polity2l
#                                 + ethfrac + relfrac, data = trainingset, importance = TRUE)
  
  flmodel <- glm(as.factor(onset) ~ 
                    warl + gdpenl + lpopl1 + lmtnest + ncontig + 
                    Oil + nwstate + instab + polity2l
                   + ethfrac + relfrac, family = binomial(link = logit),
                   data = trainingset)
  
#  temp <- as.data.frame(predict(flmodel.forest, testset))
#  temp.f <- ifelse(temp > 0.3, 1, 0)
  
  #  temp <- as.data.frame(predict(flmodel.forest2, testset))
  
    temp <- as.data.frame(predict.glm(flmodel, testset, type="response"))
    temp.f <- ifelse(temp > 0.3, 1, 0)
  
  
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

# As an example use % correct classification 
table(result$Difference)
# % Accuracy
sum(result$Difference)/length(result$Difference)

pred<- prediction(prediction, testsetCopy$onset)
perf<- performance(pred,"tpr","fpr")

plot(perf, main="ROC Plot: Fearon & Laitin Model (Cross-validated)", lwd=3)

#varImpPlot (flmodel.forest)

