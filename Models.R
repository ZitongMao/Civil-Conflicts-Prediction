#FL Original Model
flmodel <- glm(as.factor(onset) ~ 
                 warl + gdpenl + lpopl1 + lmtnest + ncontig + 
                 Oil + nwstate + instab + polity2l
               + ethfrac + relfrac, family = binomial(link = logit),
               data = fl.three) #no out-of-sample prediction


#Logistic Regression
flmodel <- glm(as.factor(onset) ~ 
                 warl + gdpenl + lpopl1 + lmtnest + ncontig + 
                 Oil + nwstate + instab + polity2l
               + ethfrac + relfrac, family = binomial(link = logit),
               data = trainingset) 

#Random Forest
flmodel.forest <- randomForest(onset ~ warl + gdpenl + 
                                 lpopl1 + lmtnest + ncontig + 
                                 Oil + nwstate + instab + polity2l
                               + ethfrac + relfrac, data = trainingset, importance = TRUE)

#rpart - Recursive Partitioning and Regression Trees
flmodel.rpart <- rpart(as.factor(onset) ~ 
                         warl + gdpenl + lpopl1 + lmtnest + ncontig + 
                         Oil + nwstate + instab + polity2l
                       + ethfrac + relfrac, data = trainingset, 
                       control=rpart.control(minsplit=6, minbucket=2, cp=0.003)) #loosing the control for better predictive power
