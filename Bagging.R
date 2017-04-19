set.seed(1)

flmodel.forest2 <- randomForest(as.factor(onset) ~ warl + gdpenl + 
                                  lpopl1 + lmtnest + ncontig + 
                                  Oil + nwstate + instab + polity2l
                                + ethfrac + relfrac, data = trainingset, mtry = 6, importance = TRUE)
flmodel.forest2
