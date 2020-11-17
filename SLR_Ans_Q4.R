#---------- Q4 -----------

#Q4) Salary_Hike -> Build a prediction model for Salary_hike ?

#Y = we need to predict = Salary
#X = Years_Experience


x <- read.csv(file.choose())

View(x)                
attach(x)                

#-----------EDA----------
summary(x)

#install.packages("lattice")
library(lattice)
dotplot(x$Years_Experience, main = "Dot plot of Years_Experience")
dotplot(x$Salary, main="Dot Plot of Salary")

#install.packages("moments")
library(moments)


skewness(x)
kurtosis(x)

boxplot(x$Years_Experience ,col="dodgerblue4", horizontal = T)
boxplot(x$Salary, col="red", horizontal = T)


hist(x$Years_Experience)
hist(x$Salary)

qqnorm(x$Years_Experience)
qqline(x$Years_Experience)

qqnorm(x$Salary)
qqline(x$Salary)

plot(x$Years_Experience, x$Salary)
plot(x$Years_Experience, x$Salary,main="Scatter Plot", col="green", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Sorting Time", 
     ylab="Calories Consumed", pch=20)  # plot(x,y)

cor(x$Years_Experience, x$Salary)


#---------- Model Building ----------

Model4 <- lm(x$Salary ~ x$Years_Experience, data = x)
summary(Model4)
confint(Model4,level=0.95)
as.data.frame(Model4$fitted.values)
as.data.frame(Model4$residuals)
sqrt(sum(Model4$residuals^2)/nrow(x))

#---- transform the variables to check whether the predicted values are better ----

#----------- Transform 1 -----------
Model_sqrt4 <- lm(Salary ~sqrt(Years_Experience) , data= x)
summary(Model_sqrt4)
as.data.frame(Model_sqrt4$fitted.values)
as.data.frame(Model_sqrt4$residuals)
sqrt(sum(Model_sqrt4$residuals^2)/nrow(x))



#----------- Transform 2 -----------
Model_sqrt1_4 <- lm(sqrt(Salary) ~ Years_Experience , data= x)
summary(Model_sqrt1_4)
as.data.frame(Model_sqrt1_4$fitted.values)
as.data.frame(Model_sqrt1_4$residuals)
sqrt(sum(Model_sqrt1_4$residuals^2)/nrow(x))


#----------- Transform 3 -----------
Model_log4 <- lm(Salary ~ log(Years_Experience), data = x)
summary(Model_log4)
as.data.frame(Model_log4$fitted.values)
as.data.frame(Model_log4$residuals)
sqrt(sum(Model_log4$residuals^2)/nrow(x))


#----------- Transform 4 -----------
Model_log1_4<-lm(log(Salary)~ Years_Experience, data= x)
summary(Model_log1_4)
as.data.frame(exp(Model_log1_4$fitted.values))
as.data.frame(exp(Model_log1_4$residuals))
sqrt(sum(Model_log1_4$residuals^2)/nrow(x))

#conclusion : according to parameters of all transformation "Model_log1"
#gives us the best output with following values


#MODEL NAME      R-Square 	P-value	   RMSE	    Accepted Model
#Model4          	0.957	    2.20E-16	5592.044 	 NO
#Model_sqrt4	    0.931	    2.20E-16	7080.096	 NO
#Model_sqrt1_4	  0.9498	  2.20E-16	10.93702	 NO
#Model_log4	      0.8539	  3.25E-13	10302.89	 NO
#Model_log1_4 	  0.932	    2.20E-16	0.09457437 Yes
