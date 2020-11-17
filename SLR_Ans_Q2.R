#---------- Q2 -----------

#Q2) Delivery_time -> Predict delivery time using sorting time ?
#y= Delivery.Time
#x= Sorting.Time
Dlv_tym <- read.csv(file.choose())

View(Dlv_tym)                
attach(Dlv_tym)                 

#-----------EDA----------
summary(Dlv_tym)

#install.packages("moments")
library(moments)
skewness(Dlv_tym)
kurtosis(Dlv_tym)

#install.packages("lattice")#this package is for plots
library(lattice)

dotplot(Delivery.Time, main = "Dot plot of Delivery.Time")
dotplot(Sorting.Time, main="Dot Plot of Sorting.Time")


boxplot(Delivery.Time ,col="dodgerblue4", horizontal = T)
boxplot(Sorting.Time, col="red", horizontal = T)
boxplot(Dlv_tym ,col="dodgerblue4", horizontal = T)

hist(Delivery.Time)
hist(Sorting.Time)

qqnorm(Delivery.Time)
qqline(Delivery.Time)

qqnorm(Sorting.Time)
qqline(Sorting.Time)

plot(Sorting.Time, Delivery.Time)
plot(Sorting.Time, Delivery.Time ,main="Scatter Plot", col="green", 
     col.main="Dodgerblue4", col.lab="Dodgerblue4", xlab="Sorting Time", 
     ylab="Delivery.Time", pch=20)  # plot(x,y)

cor(Sorting.Time, Delivery.Time)


#---------- Model Building ----------

Model2 <- lm(Delivery.Time ~ Sorting.Time, data=Dlv_tym)
summary(Model2)
confint(Model2,level=0.95)
as.data.frame(Model2$fitted.values)
as.data.frame(Model2$residuals)
sqrt(sum(Model2$residuals^2)/nrow(Dlv_tym))

#---- transform the variables to check whether the predicted values are better ----

#----------- Transform 1 -----------
Model_sqrt2 <- lm(Delivery.Time ~sqrt(Sorting.Time) , data=Dlv_tym)
summary(Model_sqrt2)
as.data.frame(Model_sqrt2$fitted.values)
as.data.frame(Model_sqrt2$residuals)
sqrt(sum(Model_sqrt2$residuals^2)/nrow(Dlv_tym))


#----------- Transform 2 -----------
Model_sqrt1_2 <- lm(sqrt(Delivery.Time) ~Sorting.Time , data=Dlv_tym)
summary(Model_sqrt1_2)
as.data.frame(Model_sqrt1_2$fitted.values)
as.data.frame(Model_sqrt1_2$residuals)
sqrt(sum(Model_sqrt1_2$residuals^2)/nrow(Dlv_tym))


#----------- Transform 3 -----------
Model_log2 <- lm(Delivery.Time ~ log(Sorting.Time), data = Dlv_tym)
summary(Model_log2)
as.data.frame(Model_log2$fitted.values)
as.data.frame(Model_log2$residuals)
sqrt(sum(Model_log2$residuals^2)/nrow(Dlv_tym))

#----------- Transform 4 -----------
Model_log1_2<-lm(log(Delivery.Time)~ Sorting.Time, data= Dlv_tym)
summary(Model_log1_2)
as.data.frame(exp(Model_log1_2$fitted.values))
as.data.frame(exp(Model_log1_2$residuals))
sqrt(sum(Model_log1_2$residuals^2)/nrow(Dlv_tym))


#conclusion : according to parameters of all transformation "Model_log1"
#gives us the best output with following values


#MODEL NAME      	R-Square	P-value	   RMSE	   Accepted Model
#Model2      	    0.6823  	3.98E-06	2.79165	    No
#Model_sqrt2 	    0.6958  	2.61E-06	2.731543	  No
#Model_sqrt1_2 	  0.704	    2.00E-06	0.3323459 	No
#Model_log2	      0.6954	  2.64E-06	2.733171    No
#Model_log1_2	    0.7109	  1.59E-06	0.1669628	  Yes
