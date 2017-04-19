
#Logistic Regression
flmodel <- glm(as.factor(onset) ~ 
                 warl + gdpenl + lpopl1 + lmtnest + ncontig + 
                 Oil + nwstate + instab + polity2l
               + ethfrac + relfrac, family = binomial(link = logit),
               data = trainingset)
summary(flmodel)   

#Random Forest 1
flmodel.forest <- randomForest(onset ~ warl + gdpenl + 
                                 lpopl1 + lmtnest + ncontig + 
                                 Oil + nwstate + instab + polity2l
                               + ethfrac + relfrac, data = trainingset, importance = TRUE)

#Random Forest 2
flmodel.forest2 <- randomForest(as.factor(onset) ~ warl + gdpenl + 
                                 lpopl1 + lmtnest + ncontig + 
                                 Oil + nwstate + instab + polity2l
                               + ethfrac + relfrac, data = trainingset, importance = TRUE)

