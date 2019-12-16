library(fBasics)
library(forecast) 

# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

datos<-read.csv("Homework2DATA.csv",header=TRUE,sep=";",dec=",")

week<-datos[,1]
ibex<-datos[,2]
exchange<-datos[,3]
shortterm<-datos[,4]
longterm<-datos[,5]

par(mfrow=c(3,1))
ts.plot(ibex)
acf(ibex,110)  #110 in order to see at least two years
pacf(ibex,110)
#IBEX data non stationary (ACF values decreasing to zero)
#Lag 1 in PACF (autoregressive)

#IBEX is cyclical? With this information, it does not appear as cyclical so we cannot say that. At the end of the assignment, when we do the
#prediction, we cannot say that IBEX is cyclical neither because applying our models our prediction is a line that varies a little
#but not much (and it should descend enough to be able to intuit a cyclicity)

par(mfrow=c(3,1))
ts.plot(exchange)
acf(exchange,110)
pacf(exchange,110)  
#Exchange rate data non stationary (ACF values decreasing to zero)
#Lag 1 (and 14) in PACF (autoregressive)

par(mfrow=c(3,1))
ts.plot(shortterm)
acf(shortterm,110)
pacf(shortterm,110)  
#Short term data non stationary (ACF values decreasing to zero)
#Lag 1 in PACF (autoregressive)

par(mfrow=c(3,1))
ts.plot(longterm)
acf(longterm,110)
pacf(longterm,110) 
#Long term data non stationary (ACF values decreasing to zero)
#Lag 1 in PACF (autoregressive)

#Autocorrelation problem: we have autocorrelation AR(1). Later on, with the arima model, we will see how we solve this

#Playing with lags (110, 220...) We do not see any seasonality in data

s=52
#Because its weekly and we have 52 weeks in a year, so after 52 weeks, we will be "in the same" week again

#Lets check the number of differences to apply to s

ndiffs(ibex, alpha=0.05, test=c("adf")) # regular differences?
#1

nsdiffs(ibex,m=s,test=c("ocsb"))  # seasonal differences?
#0

#Lets do ARIMA with one difference in non seasonal data:
fit<-arima(ibex,order=c(0,1,0)) 

#We do 0,1,0 instead of 1,1,0, taking into account the autoregressive of order 1 that we have seen in PACF because
#its simpler. If this model is valid, we will check 1,1,0 to see (if that is valid too) which one is better

fit
 
#Lets review if residuals are stationary in mean

ts.plot(fit$residuals)

#Residuals are stationary in mean
nlags=50

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)    

#There are no lags out of limits in ACF nor PACF, so residuals are WN. We will confirm this with Box.text:

Box.test(fit$residuals,lag=110) #white noise residuals? Yes

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
#There are no differences 

shapiro.test(fit$residuals)  #normality test

#Lets check 1,1,0, taking into account the lag 1 out of limit of PACF

fit_1<-arima(ibex,order=c(1,1,0)) 

fit_1

#If we do 0.1101 +- 0.0956 * 1.96, it contains 0, so this is not a valid model

#Our first model is going to be fit: arima (0,1,0)

#Once we have our ARIMA model, lets check models with dependent variables

#Models for dependent variables

ts.plot(ibex,col="blue",ylab="value",
        main = "ibex")
par(new=TRUE)
ts.plot(shortterm,col="red")

ts.plot(ibex,col="blue",ylab="value",
        main = "ibex")
par(new=TRUE)
ts.plot(longterm,col="red")

#We can see that the relation of IBEX with both terms is: when one is going up, the others are going down in a simular way, and viceversa
#We will use this information for the linear models

ts.plot(longterm,col="blue",ylab="value",
        main = "ibex")
par(new=TRUE)
ts.plot(shortterm,col="red")

#With this last plot we compare long term and short term, and we can see that both are very similar. 
#We will check the correlation of the variables to see if we can use not all columns

x <- data.frame("ibex" = ibex, "week" = week, "exchange" = exchange, "shorterm" = shortterm, "longterm" = longterm)
cor(x, method = "pearson", use = "complete.obs")

#All columns are highly correlation within each other. 
#1) The lower correlation is week with long term (-0.78)
#2) Week with short term has correlation of -0.92
#3) long term is higher correlated with ibex than shorterm, but both are very similar (-0.94 and -0.93)

#We will take the three assumptions of above while analyzin the linear models that we are going to create to see if we delete short term or not

colnames(datos)

#We are going to create a first model with all variables/columns
model0 <- lm(IBEX~.,datos)
summary(model0)

#With model0 We see that short term rate is not significant.
#Based on this last information and the said assumptions of the correlation part, we are not gonna have short term rate in the next models

model1 <- lm(IBEX~+Exchange.rate....+Week+Long.term.rate,datos)
summary(model1)
#Adjusted R-squared:  0.9688

#If we use Short term instead of Long term

model1b <- lm(IBEX~+Exchange.rate....+Week+Short.term.rate,datos)
summary(model1b)
#Adjusted R-squared:  0.9028 is worse

#We are going to multiply week with long term rate

model2 <- lm(IBEX~+Exchange.rate....+Week*Long.term.rate,datos)
summary(model2)

#We get better r squared (0.9714 > 0.9688)
#With this model Week is not being understood as a time series, that is why we tried with this multiplication

#We are going to try to multiply the three columns

model3 <- lm(IBEX~Exchange.rate....*Week*Long.term.rate,datos)
summary(model3)

#Adjusted R-squared:  0.9814

model3b <- lm(IBEX~log(Exchange.rate....)*Week*log(Long.term.rate),datos)
summary(model3b)
#Same R-quared, but more difficult model, so until now we take model3 as our best linear model

par(mfrow=c(2,2))
plot(model3)

#If we check the Residuals vs Fitted graph, we can see a flat line. This means that residuals 
#are randomnly spread/distributed, which means that there is no heteroscedastity.

#We have seen that all variables are quite correlated, but lets check multicollinearity with VIF:
install.packages("car")
library(car) 
vif(model3)

#Based on the values of VIF, we are going to delete Week because it is the column that appears in all top 4 highest values 

model4 <- lm(IBEX~Exchange.rate....+Long.term.rate,datos)
summary(model4)

#The Adjusted R-squared is 0.9094, which is worse; but with this model4 we avoid multicollinearity.
#We can check between model3 and model4 based on in which we want to focus more

#As at the end we need to predict based on Short term rate, Long term rate and Exchange rate, lets check 
#the model with those 3 variables

model5 <- lm(IBEX~Exchange.rate....+Short.term.rate+Long.term.rate,datos)
summary(model5)

#The Adjusted R-squared is 0.9455, which is better, and Short term rate appears as significant, so 
#We keep this model as the best one yet. 

#With anova(model), if we go to "Mean Sq-Residuals", we can check the variance of the residuals:
anova(model5)

#Another way of doing this is by taking the SD of the model (from summary(model)) to the square
summary(model5)

#The result is very similar: 16724 and 16718.49 (129.3^2)

#If we check the variance of the residuals of model4 (our 2nd best model if we take into account multicollinearity),
#the variance of the residuals is quite worse (around 27816)

#If we check again the VIF:
vif(model5)
#Now the values are lower and similar

#Now, lets check the residuals of model5:
#We can use checkresiduals(model5) or, as we are gonna use again later on:

plot(model5$residuals, type='l')
acf(model5$residuals,lag=111)
pacf(model5$residuals,lag=111)

#We can see ACF decreasing to zero and with 17-18 different lags out of limits. This means that the data 
#is non stationary, which means that residuals are not White Noise
#In the PACF we can see lag 1 and lag 2 (and maybe 40 a bit) out of limit. We will take this into account in the arima model

ndiffs(model5$residuals, alpha=0.05, test=c("adf"))
#0

#Also, with checkresiduals(model5) we can see the test with p-value < 0.05
checkresiduals(model5)

#We are gonna run a model taking into account the lag 2 of the autoregressive part (PACF)
fit_model5<-arima(model5$residuals,order=c(2,0,0))    
fit_model5

#-0.2448 +- 0.0922*1.96 does not contain zero, so this model is valid

par(mfrow=c(3,1))
ts.plot(fit_model5$residuals)
acf(fit_model5$residuals)
pacf(fit_model5$residuals)  

#Now, ACF does not appear decreasing to zero and with lags out of limit, so data is stationary.
#Also, in PACF we do not see the lags that we saw before arima (2,0,0)
#These reflects that residuals are White Noise, but lets confirm this with Box-Pierce test

Box.test(fit_model5$residuals,lag=5)

#P-value > 0.05, so residuals are white noise

#Lets check the variance of the residuals of this fit_model
summary(fit_model5)

#Sigma^2 is the same that saying standard deviation^2, and in this case it appears sigma^2 = 3946


#Now, lets do the joint estimation of the model

#We create a matrix with the values of the three columns that we use to predict ibex
xreg = matrix(c(exchange,longterm,shortterm), nrow=109, ncol=3) 

#Taking into account the commented lag 2 of PACF and the three columns that we want to use to predict,
#lets run two models, one of them with the method of maximum likelihood

fit_model5_final = arima(ibex,order=c(2,0,0),xreg=xreg)
fit_model5_final

fit_model5_finalb = arima(ibex,order=c(2,0,0),xreg=xreg,include.mean=F, method = "ML")
fit_model5_finalb

#Without maximum likelihood, the model has a better result in terms of AIC and 
#sigma^2 (variance of residuals = 3191), so we take fit_model5_final. This is, until now, our best sigma^2

#Lets check if residuals are stationary in the mean and if there is no lag out of limits in ACF nor PACF
par(mfrow=c(3,1))
plot(fit_model5_final$residuals,type='l')
acf(fit_model5_final$residuals)
pacf(fit_model5_final$residuals)

#Data is stationariy in the mean and there is no lags out of limits in ACP nor PAC.
#Residuals are White Noise. We can also confirm that the residuals are White Noise with the Box test (p-value > 0.05)
Box.test(fit_model5_final$residuals)

#However, if we increase the lag in the aboved ACF and PACF (to, for example, 110), we can see
#1)lag 39 a bit out of limits in ACF
#2)lag 39 clearly out of limits in PACF

#We are not going to run arima(39,0,0) because with that we are saying "39 weeks ago,
#that influence", and we dont believe that.

#If we come back to fit_model5_final, if we review xreg1, xreg2 and xreg3 with their standard deviations, we
#can see that xreg3 (-10.7181 +- 15.4530*1.96) contains zero in between its range, so its not significant and we can delete that variable from our model
#(like we did in model4).
#xreg1 and xreg3 (exchange rate and long term rate) are significant because they do not contain zero in their rates

#Having only exchange and longterm was what we did for getting "model4". Now, we are doing the same for achieving
#the time series error, giving to arima the two independent variables (in xreg)

#We create a matrix with the values of the two columns that we use to predict ibex
xreg2 = matrix(c(exchange,longterm), nrow=109, ncol=2) 

summary(model4)
#We got the said R-squared of 0.9094 

par(mfrow=c(3,1))
ts.plot(model4$residuals)
acf(model4$residuals)
#ACF out of limits and decreasing to zero. Data is not stationary
pacf(model4$residuals)  
#Lag 1 out of limit

ndiffs(model4$residuals, alpha=0.05, test=c("adf"))
#1

#We are going to try a model just taking into account the autoregressive 1:

fit_model4<-arima(model4$residuals,order=c(1,0,0)) 
fit_model4
#For AR(1), 0.9390 +- 0.0327*1.96 does not contains zero, so this is a valid model
#sigma^2 (residual of the variance) = 3619

ndiffs(fit_model4$residuals, alpha=0.05, test=c("adf"))
#Number of differences was 1 after AR(1) and now its 0. We can continue

fit_model4_final=Arima(ibex,order=c(1,0,0),xreg=xreg2)
#VERY IMPORTANT TO HAVE "A" of Arima in uppercase, if not the prediction of below does not work properly.
fit_model4_final
#As commented, both variables are relevant, and AR(1) does not contain zero in its range.

par(mfrow=c(3,1))
ts.plot(fit_model4_final$residuals)
acf(fit_model4_final$residuals)
pacf(fit_model4_final$residuals)    
#A bit of lag in number 4, but it is not important

Box.test(fit_model4_final$residuals,lag=50)
Box.test(fit_model4_final$residuals^2,lag=30)
#p-value > 0.05 : residuals are white noise

shapiro.test(fit_model4_final$residuals)
#p-value > 0.05 : Normalized data

#In fit_model4_final, sigma^2 (residual of the variance) =  3234. 
#This is our second best sigma^2; best one was fit_model5_final = 3191, but we did not need Short term rate,
#so we are taking fit_model4_final as our best model and the one to use for the forescasting step


# Forecasting step

#We had model 5 with the three variables, but as at the end our best model does not take into consideration 
#short term rate, we are not going to use the forecasting value of short term of 7.6  

pred_x=t(cbind(c(0.781,10.76))) 

prediction=forecast(fit_model4_final,xreg=pred_x) 

#Our prediction is 3336.309 with an upperlimit of 3449.873 and a lowerlimit of 3222.745, with 95% of confidence interval

prediction2=predict(fit_model4_final,newxreg = pred_x) 

prediction2$pred   # point predictions
prediction2$se    # standard errors

#Plotting real data with point predictions

new <- c(ibex,prediction2$pred) # real data + predicted values

plot.ts(new,main="Predictions",
        ylab="Ibex",col=3,lwd=2) # time series plot
lines(ibex,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

