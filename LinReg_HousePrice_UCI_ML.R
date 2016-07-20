
housedata=read.csv("HousingPricingData.csv")
str(housedata)
summary(housedata)
fix(housedata)           #edit values in variable

pairs(housedata)

plot(housedata$MEDV,housedata$CRIM)
plot(housedata$MEDV,housedata$ZN)
plot(housedata$MEDV,housedata$INDUS)
plot(housedata$MEDV,housedata$AGE)

cor(housedata)

LinReg = lm(MEDV ~ ., data = housedata)
summary(LinReg)

LinReg1 = lm(MEDV ~ CRIM , data = housedata)
summary(LinReg1)
LinReg2 = lm(MEDV ~ ZN, data = housedata)
summary(LinReg2)
LinReg3 = lm(MEDV ~ INDUS , data = housedata)
summary(LinReg3)
LinReg4 = lm(MEDV ~ CHAS, data = housedata)
summary(LinReg4)

predictTrain = predict(LinReg, newdata=housedata)
summary(predictTrain)

#Baseline Error on the predicted model based on the dataset

SSE = sum((housedata$MEDV-predictTrain)^2)
SSE
SST = sum((housedata$MEDV-mean(housedata$MEDV))^2)
SST
1-SSE/SST
#R^2

##Split train and test

install.packages("caTools")
library(caTools)

set.seed(88)    					 ## So that we get the same set
split = sample.split(housedata$MEDV, SplitRatio = 0.75)  
housedataTrain = subset(housedata, split == TRUE)
housedataTest = subset(housedata, split == FALSE)

str(housedataTrain)
str(housedataTest)

# build 3 different models based on split data

LinRegTrain1 = lm(MEDV ~ ., data = housedataTrain)  #"." all the data 
summary(LinRegTrain1)

LinRegTrain2 = lm(MEDV ~ CRIM+ZN+CHAS+NOX+RM+DIS+PTRATIO+B+LSTAT+TAX, data = housedataTrain)  #"." all the data 
summary(LinRegTrain2)

LinRegTrain3 = lm(MEDV ~ CRIM+ZN+CHAS+NOX+RM+DIS+PTRATIO+B+LSTAT, data = housedataTrain)  #"." all the data 
summary(LinRegTrain3)

#Test Error-model 1

predictTest1 = predict(LinRegTrain1, newdata=housedataTest)
summary(predictTest1)
          
SSE1 = sum((housedataTest$MEDV-predictTest1)^2)
SSE1
SST1 = sum((housedataTest$MEDV-mean(housedataTest$MEDV))^2)
SST1
1-SSE1/SST1
#R^2

#Test Error-model 2

predictTest2 = predict(LinRegTrain2, newdata=housedataTest)
summary(predictTest2)

SSE2 = sum((housedataTest$MEDV-predictTest2)^2)
SSE2
SST2 = sum((housedataTest$MEDV-mean(housedataTest$MEDV))^2)
SST2
1-SSE2/SST2
#R^2

#Test Error-model 3

predictTest3 = predict(LinRegTrain3, newdata=housedataTest)
summary(predictTest3)

SSE2 = sum((housedataTest$MEDV-predictTest3)^2)
SSE2
SST2 = sum((housedataTest$MEDV-mean(housedataTest$MEDV))^2)
SST2
1-SSE2/SST2
#R^2


plot(residuals(LinRegTrain3)) # residuals

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(LinRegTrain3)

####

shapiro.test(residuals(LinRegTrain3))

