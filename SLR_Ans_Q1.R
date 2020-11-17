#----------   simple linear regression   ----------

#Q1) Calories_consumed-> predict weight gained using calories consumed ?
#Y = we need to predict = weight gained
#X = Calories Consumed

cal_consum <- read.csv(file.choose())

View(cal_consum)                
attach(cal_consum)              

#-----------EDA----------
summary(cal_consum)


#install.packages("moments")
library(moments)
skewness(cal_consum)
kurtosis(cal_consum)

#install.packages("lattice")#this package is for plots
library(lattice)

dotplot(Weight.gained..grams., main = "Dot plot of Weight gained grams.")
dotplot(Calories.Consumed, main="Dot Plot of calories consumed")

boxplot(Weight.gained..grams.,col="dodgerblue4", horizontal = T)
boxplot(Calories.Consumed, col="red", horizontal = T)
boxplot(cal_consum ,col="dodgerblue4", horizontal = T)

hist(Weight.gained..grams.)
hist(Calories.Consumed)

qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)

qqnorm(Calories.Consumed)
qqline(Calories.Consumed)

#plot(x,y)
plot(Calories.Consumed, Weight.gained..grams.)
plot(Calories.Consumed, Weight.gained..grams. ,main="Scatter Plot",
     col="red", col.main="Dodgerblue4", col.lab="Dodgerblue4", 
     xlab="calories consumed", 
     ylab="Weight.gained..grams.", pch=20)  # plot(x,y)


cor(Calories.Consumed, Weight.gained..grams.)


#---------- Model Building ----------
#SLR =(y~x, data = dataset)
Model1 <- lm(Weight.gained..grams.~ Calories.Consumed, 
            data=cal_consum)
summary(Model1)
confint(Model1,level=0.95)


#To see value predicted by Model i.e(Y Hat) or our fitted.values
#1 Model1$fitted.values
#or
#2 as.data.frame(Model1$fitted.values)
#or
pred <- predict(Model1)
View(pred)
predict(Model1,interval="predict")
#fit = our fitted value    [lwr   upr = (range)]

sum(Model1$residuals)
mean(Model1$residuals)
sqrt(sum(Model1$residuals^2)/nrow(cal_consum))  #RMSE which should be minimum

# ggplot for adding regresion line for data

#installed.packages("ggplot2")
library(ggplot2)

ggplot(data = cal_consum, aes(x = cal_consum$Calories.Consumed,
                              y = cal_consum$Weight.gained..grams.)) + 
  geom_point(color='blue') +
        geom_line(color='red',data = cal_consum, aes(x=Calories.Consumed, y=pred))



#---- transform the variables to check whether the predicted values are better ----

#----------- Transform 1 -----------
Model_sqrt1 <- lm(Weight.gained..grams. ~ sqrt(Calories.Consumed), 
                 data = cal_consum)
summary(Model_sqrt1)
as.data.frame(Model_sqrt1$fitted.values)
as.data.frame(Model_sqrt1$residuals)
sqrt(sum(Model_sqrt1$residuals^2)/nrow(cal_consum))

#----------- Transform 2 -----------
Model_log1 <- lm(Weight.gained..grams. ~ log(Calories.Consumed),
                data = cal_consum)
summary(Model_log1)
as.data.frame(Model_log1$fitted.values)
as.data.frame(Model_log1$residuals)
sqrt(sum(Model_log1$residuals^2)/nrow(cal_consum))

#conclusion : according to parameters of all transformation "Model" 
#gives us the best
#output with following values 


#MODEL NAME	  R-Square	P-value   	RMSE	  Accepted Model
#Model1	       0.8968 	2.86E-07	103.3025	yes
#Model_sqrt1	 0.8567 	2.08E-06	121.7122	no
#Model_log1	   0.8077 	1.25E-05	141.0054	no




