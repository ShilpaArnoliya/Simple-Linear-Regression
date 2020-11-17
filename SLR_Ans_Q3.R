
#----------- Q3 ----------
#Q3) Emp_data -> Build a prediction model for Churn_out_rate 
#Y = we need to predict = Churn_out_rate
#X = Salary_hike


Emp_data <- read.csv(file.choose())
View(Emp_data)
attach(Emp_data)

#-----------EDA----------
summary(Emp_data)

#install.packages("lattice")
library(lattice)
dotplot(Emp_data$Salary_hike, main = "Dot plot of Salary Hike")
dotplot(Emp_data$Churn_out_rate, main="Dot Plot of Churn_out_rate")

#install.packages("moments")
library(moments)

skewness(Emp_data)
kurtosis(Emp_data)

boxplot(Salary_hike ,col="dodgerblue4", horizontal = T)
boxplot(Churn_out_rate , col="red", horizontal = T)
boxplot(Emp_data ,col="dodgerblue4", horizontal = T)

hist(Salary_hike)
hist(Churn_out_rate)

qqnorm(Salary_hike)
qqline(Salary_hike)

qqnorm(Churn_out_rate)
qqline(Churn_out_rate)

plot(Salary_hike,  Churn_out_rate)
plot(Salary_hike, Churn_out_rate ,main="Scatter Plot", col="red", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab= "Salary_hike", 
     ylab="Churn_out_rate", pch=20)  # plot(x,y)

cor(Salary_hike , Churn_out_rate)


#---------- Model Building ----------

Model3 <- lm(Churn_out_rate ~ Salary_hike, data=Emp_data)
summary(Model3)
confint(Model3,level=0.95)
as.data.frame(Model3$fitted.values)
as.data.frame(Model3$residuals)
sqrt(sum(Model3$residuals^2)/nrow(Emp_data))


#---- transform the variables to check whether the predicted values are better ----

#----------- Transform 1 -----------
Model_sqrt3 <- lm(Churn_out_rate ~ sqrt(Salary_hike), data=Emp_data)
summary(Model_sqrt3)
as.data.frame(Model_sqrt3$fitted.values)
as.data.frame(Model_sqrt3$residuals)
sqrt(sum(Model_sqrt3$residuals^2)/nrow(Emp_data))


#----------- Transform 2 -----------
Model_sqrt1_3 <- lm(sqrt(Churn_out_rate) ~ Salary_hike , data=Emp_data)
summary(Model_sqrt1_3)
as.data.frame(Model_sqrt1_3$fitted.values)
as.data.frame(Model_sqrt1_3$residuals)
sqrt(sum(Model_sqrt1_3$residuals^2)/nrow(Emp_data))


#----------- Transform 3 -----------
Model_log3 <- lm(Churn_out_rate ~ log(Salary_hike), data = Emp_data)
summary(Model_log3)
as.data.frame(Model_log3$fitted.values)
as.data.frame(Model_log3$residuals)
sqrt(sum(Model_log3$residuals^2)/nrow(Emp_data))


#----------- Transform 4 -----------
Model_log1_3<-lm(log(Churn_out_rate)~ Salary_hike, data= Emp_data)
summary(Model_log1_3)
as.data.frame(exp(Model_log1_3$fitted.values))
as.data.frame(exp(Model_log1_3$residuals))
sqrt(sum(Model_log1_3$residuals^2)/nrow(Emp_data))


#conclusion : according to parameters of all transformation "Model_log1"
#gives us the best output with following values
              
#MODEL NAME	   R-Square 	P-value	   RMSE	       Accepted Model
#Model3	        0.8312  	2.39E-04	 3.997528	     NO
#Model_sqrt3	  0.84	    1.92E-04	 3.891995	     NO
#Model_sqrt1_3 	0.853	    1.36E-04	 0.2156411	   NO
#Model_log3	    0.8486	  1.53E-04	 3.786004	     No
#Model_log1_3	  0.8735	  7.38E-05	 0.04641748	   Yes

             
              
              
