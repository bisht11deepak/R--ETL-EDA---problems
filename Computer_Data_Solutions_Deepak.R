### Multiple Linear regression
library(readr)
Computer_Data<-read.csv("E:/torrent downloaded movies/Assingments/Multi Linear Regression/Computer_Data.csv")
Computer_Data<-Computer_Data[,-1]
View(Computer_Data)
str(Computer_Data)

#####install.packages("psych")
#####library(psych)
######cd_dummy<-dummy.code(Computer_Data$cd, group = NULL)
#######cd_dummy

str(Computer_Data)
View(Computer_Data)
attach(Computer_Data)

### Exploratory data analysis
attach(Computer_Data)
cor(speed,price)
cor(price,hd)
cor(price,ram)
cor(price,screen)
cor(price,cd)
cor(price,multi)
cor(price,premium)
cor(price,ads)
cor(Profit,trend)
pairs(Computer_Data)
summary(Computer_Data)

## we can also see coorelation coefficeceint and scatter plot together
library(GGally)
windows()
ggpairs(Computer_Data)
plot(Computer_Data)

##Designing the multiple regression model
Computer_Data_model<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend, data=Computer_Data)
summary(Computer_Data_model)
## The adjusted R squared value iof this model is 77.56%, which not excellentbut good enough for the model
## In this model all the independent values are significant having P value less than 0.05%. 
## Therefore, the  price of the computers are dependent on the all of the independent variable
Computer_Data_fiitedl0<-Computer_Data_model$fitted.values
Computer_Data_fiitedl0
Computer_Data_model$residuals
plot(Computer_Data_fiitedl0)
qqnorm(Computer_Data_fiitedl0)
qqline(Computer_Data_fiitedl0)

###Model1
Computer_Data_model_sqr<-lm(sqrt(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend, data=Computer_Data)
summary(Computer_Data_model_sqr)
Computer_Data_model_sqr1<-(Computer_Data_model_sqr$fitted.values)^2
Computer_Data_model_sqr1
qqnorm(Computer_Data_model_sqr1)
qqline(Computer_Data_model_sqr1)
Computer_Data_model_sqr_error<-(Computer_Data_model_sqr$residuals)^2
Computer_Data_model_sqr_error

### Model2



Computer_Data_model
#checking autocorrelation
library(car)
vif(Computer_Data)
avPlots(Computer_Data)

?vcov


