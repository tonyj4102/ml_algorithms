#Trend equation after deasonalizing time-series data

train=read.csv("Sales_DeseasonalizedbyMonth12.csv")
str(train)
plot (train$Month, train$Sales)
LinReg = lm(train$Sales ~ ., data = train)
summary(LinReg)


abline(LinReg,col='red')

# Read R-squared - how well data fit a statistical model (larger the better)
# R-squared=0.4499 , adjusted R-squared =0.3803

# b1=co-efficient of t=3653
# b0=intercept        =345584



