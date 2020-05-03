library(readr)
top50startups<-read.csv("E:/torrent downloaded movies/Assingments/Multi Linear Regression/50_Startups.csv")
top50startups
View(top50startups)
attach(top50startups)
top50startups$State<-as.numeric(top50startups$State)
str(top50startups)
View(top50startups)

### Exploratory data analysis
cor(Profit,R.D.Spend)
cor(Profit,Administration)
cor(Profit,Marketing.Spend)
cor(Profit,State)
pairs(top50startups)
summary(top50startups)
 
## we can also see coorelation coefficeceint and scatter plot together
install.packages("GGally")
install.packages("stringi")
library(GGally)
windows()
ggpairs(top50startups)
plot(top50startups)

##Designing the multiple regression model
model50startups<-lm(Profit~R.D.Spend+Administration+Marketing.Spend+State, data=top50startups)
summary(model50startups)
model50startups$residuals

# Model1 after elimintaing the state and administrtartion
model50startups1<-lm(Profit~R.D.Spend, data=top50startups)
summary(model50startups1)


# Model2 after elimintaing the state and administrtartion
model50startups2<-lm(Profit~R.D.Spend+Marketing.Spend)
summary(model50startups2) ## standard error has rediced but r^2 value has also reduce by 50

str(top50startups)


# model3 after transformation
model50startups3<-lm(Profit~sqrt(R.D.Spend)+Marketing.Spend)
summary(model50startups3)

## checking the fiited values for different models
modelfitted0<-c(model50startups$fitted.values)
modelfitted1<-c(model50startups1$fitted.values)
modelfitted2<-c(model50startups2$fitted.values)
modelfitted3<-c(model50startups3$fitted.values)
fiitedvaluecomparison<-data.frame(modelfitted0,modelfitted1,modelfitted2,modelfitted3)
View(fiitedvaluecomparison)

model50startups3
#checking autocorrelation
library(car)
vif(model50startups)
avPlots(model50startups)


## Compairing Models R^2 value
Rsquared_comparison <- data.frame(model_no = c("model0","model1","model2","model3"), 
                           rsquared_value=c(0.9508, 0.9465, 0.9505, 0.9207), 
                           residual_standarderror=c(9439, 9416, 9161, 11590))
attach(Rsquared_comparison)
view(Rsquared_comparison)                           
                           
## model 3 is best according to me beacause it includes two significant variables and r^2 value is also good
## Model0 and model1 is also a good model, due to its r^2 value. However, in modelo two varibales  are not significant
  ## Model1 has has two dependent varibales but Marketing spend variable is not significant, because its pvalue>0.05
## I have also checkd the VIF value, which shows that these varibales dont possess auto correlation charactertics
