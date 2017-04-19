# Code for Figure 1 in the final draft (ROC plots for both models using full data):

setwd("~/CS112 Final/WGB Replication Files/Data/")
load("fl.three.RData")

library(Hmisc)
library(ROCR)

# Run the model:
flmodel <- glm(as.factor(onset) ~ 
        warl + gdpenl + lpopl1 + lmtnest + ncontig + 
        Oil + nwstate + instab + polity2l
        + ethfrac + relfrac, family = binomial(link = logit),
        data = fl.three)
summary(flmodel)   

# Now do the annotated FL ROC plot:
pdf(file="~/CS112 Final/latex/Fig1FLx.pdf", width=7, height=7)
par(las=1, cex=1.4)
phat<-predict.glm(flmodel,type="response")
pred<- prediction(phat, flmodel$y)
perf<- performance(pred,"tpr","fpr")
plot(perf,main="ROC Plot: Fearon & Laitin Model", lwd=3)
segments(x0=0.01048451, x1=0.01048451, y0=-0.1, y1=0.1401869, lwd=3, col=2, lty=3)
segments(x0=-0.1, x1=0.01048451, y0=0.1401869, y1=0.1401869, lwd=3, col=2, lty=3)
text(0.01,0.14,"  A", adj=c(0,1), col=2, cex=1.3)
segments(x0=0.52835584, x1=0.52835584, y0=-0.1, y1=0.8411215, lwd=3, col=2, lty=3)
segments(x0=-0.1, x1=0.52835584, y0=0.8411215, y1=0.8411215, lwd=3, col=2, lty=3)
text(0.53,0.82,"  B", adj=c(0,1), col=2, cex=1.3)
fl.AUC.main<-somers2(plogis(predict(flmodel)),flmodel$y)[1]
text(0.2,0.4,"Total Area Under Curve: 0.761",adj=c(0,1))
dev.off()
