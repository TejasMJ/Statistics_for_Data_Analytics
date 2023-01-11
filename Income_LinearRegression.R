income_data <- read.csv("F:\\LEARNING OBJECTS\\National College of Ireland\\Semester 1\\2. Staticstics for Data Analytics\\IncomeData.csv")
attach(income_data)

names(income_data)[1] <- 'age'

head(income_data)

str(income_data)

#Converting the datatype from integer to categorical
income_data$edcat = as.factor(income_data$edcat)
income_data$default = as.factor(income_data$default)
income_data$jobsat = as.factor(income_data$jobsat)
income_data$homeown = as.factor(income_data$homeown)
str(income_data)

#Summary of the dataset
summary(income_data)

cor(income_data)


plot(income_data)

#Normality Check
print("Test of Normality")
print(" Variable - 'Age' ")
print(shapiro.test(income_data$age))
print(" Variable - 'Yrsed")
print(shapiro.test(income_data$yrsed))
print(" Variable - 'Yrsempl")
print(shapiro.test(income_data$yrsempl))
print(" Variable - 'Income")
print(shapiro.test(income_data$income))
print(" Variable - 'Creddebt")
print(shapiro.test(income_data$creddebt))
print(" Variable - 'Othdebt")
print(shapiro.test(income_data$othdebt))
print(" Variable - 'Address")
print(shapiro.test(income_data$address))
print(" Variable - 'car")
print(shapiro.test(income_data$cars))
print(" Variable - 'Car Value")
print(shapiro.test(income_data$carvalue))

#Checking Correlations
fin_num <- income_data[,-c(3,8:10)]
head(fin_num)
cor(fin_num, method = "spearman")
plot(fin_num)

data_2 = income_data


#Checking for outliers

model1 <-lm(income~., data = data_2)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
durbinWatsonTest(model1)

vif(model1)
max(cooks.distance(model1))

library(leaps)
library(car)

influencePlot(model1)
data_2 = data_2[-c(848,1109,3647,4261),]
cooksd=cooks.distance(model1)
min(cooks.distance(model1))
max(cooks.distance(model1))

influence_var <- as.numeric(names(cooksd)[(cooksd >4/4508)])
data_2 <- data_2[-influence_var,]



#Model building
model_1 = lm(income~.,data=data_2)
summary(model_1)
par(mfrow=c(2,2))
plot(model_1)
durbinWatsonTest(model_1)

vif(model_1)

max(cooks.distance(model_1))

model_2 = lm(income~age + yrsed + yrsempl + creddebt + othdebt + default + jobsat + carvalue, data=data_2)
summary(model_2)
par(mfrow=c(2,2))
plot(model_2)
durbinWatsonTest(model_2)

vif(model_2)
max(cooks.distance(model_2))


model_3 = lm(log(income)~  + yrsempl + creddebt + othdebt + default + jobsat + carvalue, data=data_2)
summary(model_3)
par(mfrow=c(2,2))
plot(model_3)
durbinWatsonTest(model_3)

vif(model_3)

model_4 = lm(log(income)~ age + yrsed + yrsempl + creddebt + othdebt + default + log(carvalue), data=data_2)
summary(model_3)
par(mfrow=c(2,2))
plot(model_3)
durbinWatsonTest(model_3)

vif(model_4)


model3 = lm(log(income)~ age + yrsempl + yrsed + othdebt + default  + sqrt(creddebt) + log(carvalue), data=data_2)
summary(model_3)
par(mfrow=c(2,2))
plot(model3)
durbinWatsonTest(model3)

vif(model3)

max(cooks.distance(model3))

model_7 = lm(log(income)~default + sqrt(othdebt) + sqrt(creddebt) + log(carvalue), data=data_2)
summary(model_7)
par(mfrow=c(2,2))
plot(model_7)
durbinWatsonTest(model_7)

vif(model_7)

data_2 = data_2[-c(2374, 2470, 462),]
data_2 = data_2[-c(3258,2030,1691,),]
data_2 = data_2[-c(3258),]

model4 = lm(log(income)~ age + yrsempl:jobsat + yrsed + othdebt:creddebt + default + log(carvalue), data=data_2)
summary(model4)
par(mfrow=c(2,2))
plot(model4)
durbinWatsonTest(model4)

vif(model4)

max(cooks.distance(model4))

vif(model4)

influencePlot(model4)
data_2 = data_2[-c(3258,2060,1691),]
cooksd=cooks.distance(model4)

influence_var <- as.numeric(names(cooksd)[(cooksd >4/4508)])
data_2 <- data_2[-influence_var,]

model3 = lm(log(income)~  age + yrsempl:jobsat + yrsed + othdebt:creddebt + default + log(carvalue), data=data_2)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
durbinWatsonTest(model3)
max(cooks.distance(model3))
vif(model3)

















#------------------------------------------------------------------------------------------------------------------------
#Graphs

df <- income_data["age"]
library(ggplot2)
# Basic histogram
ggplot(income_data, aes(x=age)) + geom_histogram()

