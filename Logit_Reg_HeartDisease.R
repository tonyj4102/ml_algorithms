heartdata=read.csv("Heart_Disease.csv")
str(heartdata)     #integer, factor and numerical variables
summary(heartdata)
names(heartdata)

pairs(heartdata,col=heartdata$chd)
# Logistic regression
glm.fit=glm(chd~.,data=heartdata,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response") 
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5,"Yes","No")

table(glm.pred)  # summary of predictions

##Split train and test

install.packages("caTools")
library(caTools)

set.seed(88)      				 ## So that we get the same set
split = sample.split(heartdata$chd, SplitRatio = 0.75)  
heartdataTrain = subset(heartdata, split == TRUE)
heartdataTest = subset(heartdata, split == FALSE)


#Build model from the training set

help(glm)

glm.fits=glm(chd~.,data=heartdataTrain,family=binomial)
summary(glm.fits)

#Evaluate Co-efficients and AIC

#AIC=quality of the model Preferred=lowest AIC 

#Predict using Model

glm.probsTr=predict(glm.fits,newdata=heartdataTrain,type="response")
glm.probsTs=predict(glm.fits,newdata=heartdataTest,type="response")

str(heartdataTest$chd)
str(glm.probsTs)
head(glm.probsTs)


#Predict classes
TestPredictionBinary1=ifelse(glm.probsTs >0.5,1,0)
TestPredictionBinary2 = as.numeric(glm.probsTs > 0.5)
summary(TestPredictionBinary1)
table(TestPredictionBinary1)
summary(TestPredictionBinary2)
table(TestPredictionBinary2)


##Confustion Matrix (False Positives, True Negatives, True Positives, False Positives)

table(heartdataTest$chd, glm.probsTs > 0.5)
#TN FP
#FN TP

#Sensitivity=TP/TP+FN (True positive rate)  =26/(14+26)
26/40
#High Confidence =low False Positive rate

#Specificity= TN/TN+FP (True negative rate)= 67/(67+9)
67/76

#Accuracy = 0.802
=TN+TP/(TN+TP+FN+FP)
(67+26)/(67+26+14+9)


#Evaluation using ROC Curve

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function  			
ROCRpred = prediction(glm.probsTs, heartdataTest$chd)

# Performance function			 #DEFINES WHAT WE’D LIKE TO PLOT
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
