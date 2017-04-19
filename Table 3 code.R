# Table 1 code:

load("fl.three.RData")


# Run both models:
flmodel <- glm(as.factor(onset) ~ 
        warl + gdpenl + lpopl1 + lmtnest + ncontig + 
        Oil + nwstate + instab + polity2l
        + ethfrac + relfrac, family = binomial(link = logit),
        data = trainingset)
summary(flmodel)   



# Now define the function for the contingency table:
contingencytable<-function(phat,y,threshold=0.67) {
	tp<-sum(phat>=threshold & y==1)
	fp<-sum(phat>=threshold & y==0)
	tn<-sum(phat<threshold & y==0)
	fn<-sum(phat<threshold & y==1)
	results<-matrix(c(tn,fp,fn,tp),2,2)
	rownames(results)<-c("Predicted Non-Events","Predicted Events")
	colnames(results)<-c("Actual Non-Events","Actual Events")
	results	
	}

# Now create the contingency tables:
contingencytable(flmodel$fitted,flmodel$y,threshold=0.5)
contingencytable(flmodel$fitted,flmodel$y,threshold=0.3)
contingencytable(flmodel$fitted,flmodel$y,threshold=0.1)



contingencytable(flmodel.forest$fitted,flmodel.forest$y,threshold=0.5)
contingencytable(flmodel.forest$fitted,flmodel.forest$y,threshold=0.3)
contingencytable(flmodel.forest$fitted,flmodel.forest$y,threshold=0.1)

contingencytable(flmodel.rpart$fitted,flmodel.rpart$y,threshold=0.5)
contingencytable(flmodel.rpart$fitted,flmodel.rpart$y,threshold=0.3)
contingencytable(flmodel.rpart$fitted,flmodel.rpart$y,threshold=0.1)
### Extra stuff:

# generate the fpr and tpr at different thresholds:
# first for FL:
threshold<-c(0.1,0.01)
fpr<-rep(NA,2)
tpr<-rep(NA,2)
for (i in 1:length(threshold)) {
	results<-contingencytable(flmodel$fitted,flmodel$y,threshold=threshold[i])
	fpr[i]<-results[2,1]/sum(results[,1])
	tpr[i]<-results[2,2]/sum(results[,2])	
	} # close i loop


	
