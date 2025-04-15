##### 時間數列分析 期末報告
##### 組員：陳葳芃 楊岳錩
##### 選用資料：2000-2023 台灣出生月人口數
##### 資料來源：https://reurl.cc/XLzEGg
##### 更新時間：0617
par(mfrow=c(2,1))
fit <- arima(data, order=c(3,0,2)) 
fit <- arima(fit_res, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=s))


data<-df_birth$amount
df<-data[1:267]
# 讀資料檔 ####
library(readxl)
df_birth <- read_excel("1112-NCCU/時間序列期末報告/tw_birth(2000-2023)(1).xlsx")
str(df_birth)
d2<-log(data)
# 期中差分部分 ####
plot(df_birth$time, df_birth$amount, main="Monthly number of births in Taiwan",type='l',xlab = "Year", ylab = "Number of births")
plot(d2,type="l")
library(tidyverse)
df_birth$month <- as.factor(format(df_birth$time, "%m"))
month_colors <- c("black","black","black","black","green", "black","black","black","black","purple","orange","red")
ggplot(df_birth, aes(x = time, y = amount)) +
  geom_line() +
  # 加入點的顯示
  geom_point(aes(color = month), size = 1, shape = 21) +
  # 在點上加上月份
  geom_text(aes(label = month), hjust = -0.1, color = month_colors[df_birth$month],size = 5)+
  scale_color_manual(values = month_colors) +
  labs(x = "Year", y = "Number of births", title = "Monthly number of births in Taiwan")

par(mfrow=c(1,2))
acf(df_birth$amount, main="ACF",lag.max = 200)
pacf(df_birth$amount, main="PACF",lag.max = 100)

library(tseries)
data<-df_birth$amount
adf.test(data) ##0.03096


###1期 difference
data_diff1<-diff(data)
ts.plot(data_diff1, main="Time series after lag=1 difference",xlab = "Time", ylab = "Diff")  
acf(data_diff1, main="ACF of lag=1 difference", lag.max = 100)
pacf(data_diff1, main="PACF of lag=1 difference",lag.max = 100)


###12期 difference
data_diff12<-diff(data, lag = 12)
ts.plot(data_diff12, main="Time series after lag=12 difference",xlab = "Time", ylab = "Diff") 
par(mfrow=c(2,1)) 
acf(data_diff12, main="ACF of lag=12 difference", lag.max = 100)
pacf(data_diff12, main="PACF of lag=12 difference", lag.max = 100)

library(tseries)
adf.test(data)
adf.test(data_diff1)
adf.test(data_diff12)


## 模型建立####
pdq<-c(4,0,0)        
PDQ<-c(0,1,1)        
s <-12
  
fit <- arima(df, order=pdq, seasonal=list(order=PDQ, period=s))
fit

## ####
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="lag3 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)


## 舊模型 ####
fit011_500 <- arima(data, order=c(0,1,1), seasonal=list(order=c(5,0,0), period=12)) #log likelihood = -2418.62,  aic = 4851.24
fit110_500 <- arima(data, order=c(1,1,0), seasonal=list(order=c(5,0,0), period=12)) #log likelihood = -2437.07,  aic = 4888.15
fit111_301 <- arima(data, order=c(1,1,1), seasonal=list(order=c(3,0,1), period=12)) #log likelihood = -2435.37,  aic = 4880.75
fit110_300 <- arima(data, order=c(1,1,0), seasonal=list(order=c(3,0,0), period=12)) #log likelihood = -2455.28,  aic = 4920.57
fit210_302 <- arima(data, order=c(0,0,1), seasonal=list(order=c(5,0,3), period=12))
fit002_301 <- arima(data, order=c(0,0,2), seasonal=list(order=c(3,0,1), period=12)) #log likelihood = -2507.04,  aic = 5028.09
fit102_301 <- arima(data, order=c(1,0,2), seasonal=list(order=c(3,0,1), period=12)) #log likelihood = -2420.06,  aic = 4856.12

fit211_411 <- arima(data[1:(length(data)-12)], order=c(2,1,1), seasonal=list(order=c(4,1,1), period=12)) #log likelihood = -2300.54,  aic = 4617.08
fit311_411 <- arima(data, order=c(3,1,1), seasonal=list(order=c(4,1,1), period=12)) #log likelihood = -2299.71,  aic = 4617.41
fit012_411 <- arima(data, order=c(0,1,2), seasonal=list(order=c(4,1,1), period=12)) #log likelihood = -2302.27,  aic = 4618.55
fit013_411 <- arima(data, order=c(0,1,3), seasonal=list(order=c(4,1,1), period=12)) #log likelihood = -2301.17,  aic = 4618.34



## 修正 ####
fit004_101 <- arima(df, order=c(0,0,4), seasonal=list(order=c(1,0,1), period=12)) #log likelihood = -2463.28,  aic = 4942.55
fit005_101 <- arima(df, order=c(0,0,5), seasonal=list(order=c(1,0,1), period=12)) #log likelihood = -2457.58,  aic = 4933.16
fit011_100 <- arima(df, order=c(0,1,1), seasonal=list(order=c(1,0,0), period=12)) #log likelihood = -2455.63,  aic = 4915.25
fit011_200 <- arima(df, order=c(0,1,1), seasonal=list(order=c(2,0,0), period=12)) #log likelihood = -2447.81,  aic = 4901.62
fit103_211 <- arima(df, order=c(1,0,3), seasonal=list(order=c(2,1,1), period=12)) #log likelihood = -2311.99,  aic = 4637.98
fit102_210 <- arima(df, order=c(1,0,2), seasonal=list(order=c(2,1,0), period=12)) #log likelihood = -2335.95,  aic = 4681.9
fit103_210 <- arima(df, order=c(1,0,3), seasonal=list(order=c(2,1,0), period=12)) #log likelihood = -2334.47,  aic = 4680.93
fit400_011 <- arima(df, order=c(4,0,0), seasonal=list(order=c(0,1,1), period=12)) #log likelihood = -2211.27,  aic = 4432.53



## 診斷fit004_101$residuals ####
#method1
par(mfrow=c(2,2))
res004<-fit004_101$residuals
qqnorm(res004)             
qqline(res004,col="red")
hist(res004)
plot(res004)
plot(res004,type="p")

shapiro.test(res004)

#method2
par(mfrow=c(1,2))
acf(res004)
pacf(res004)

#method3
library(TSA)

Box.test(fit004_101$residuals, lag=7, type="Ljung-Box", fitdf=6) 
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit004_101$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="004_101 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)








## 診斷fit005_101$residuals ####
#method1
par(mfrow=c(2,2))
res005<-fit005_101$residuals
qqnorm(res005)             
qqline(res005,col="red")
hist(res005)
plot(res005)
plot(res005,type="p")

shapiro.test(res005)

#method2
par(mfrow=c(1,2))
acf(res005)
pacf(res005)

#method3
library(TSA)

Box.test(fit005_101$residuals, lag=8, type="Ljung-Box", fitdf=7) 
B_text_p_value = c()
for(hh in 8:20){
  B_text_p_value[hh] = Box.test(fit005_101$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="005_101 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)









## 診斷fit011_100$residuals ####
#method1
par(mfrow=c(2,2))
res011<-fit011_100$residuals
qqnorm(res011)             
qqline(res011,col="red")
hist(res011)
plot(res011)
plot(res011,type="p")

shapiro.test(res011)

#method2
par(mfrow=c(1,2))
acf(res011)
pacf(res011)

#method3
library(TSA)

Box.test(fit011_100$residuals, lag=3, type="Ljung-Box", fitdf=2) 
B_text_p_value = c()
for(hh in 3:20){
  B_text_p_value[hh] = Box.test(fit011_100$residuals, lag=hh, type="Ljung-Box", fitdf=2)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="011_100 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)










## 診斷fit011_200$residuals ####
#method1
par(mfrow=c(2,2))
res011200<-fit011_200$residuals
qqnorm(res011200)             
qqline(res011200,col="red")
hist(res011200)
plot(res011200)
plot(res011200,type="p")

shapiro.test(res011200)

#method2
par(mfrow=c(1,2))
acf(res011200)
pacf(res011200)

#method3
library(TSA)

Box.test(fit011_200$residuals, lag=4, type="Ljung-Box", fitdf=3) 
B_text_p_value = c()
for(hh in 4:20){
  B_text_p_value[hh] = Box.test(fit011_200$residuals, lag=hh, type="Ljung-Box", fitdf=3)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="011_200 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)











## 診斷fit103_211$residuals ####
#method1
par(mfrow=c(2,2))
res103211<-fit103_211$residuals
qqnorm(res103211)             
qqline(res103211,col="red")
hist(res103211)
plot(res103211)
plot(res103211,type="p")

shapiro.test(res103211)

#method2
par(mfrow=c(1,2))
acf(res103211)
pacf(res103211)

#method3
library(TSA)

Box.test(fit103_211$residuals, lag=7, type="Ljung-Box", fitdf=6) 
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit103_211$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="103_211 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)





## 診斷fit102_210$residuals ####
#method1
par(mfrow=c(2,2))
res102210<-fit102_210$residuals
qqnorm(res102210)             
qqline(res102210,col="red")
hist(res102210)
plot(res102210)
plot(res102210,type="p")

shapiro.test(res102210)

#method2
par(mfrow=c(1,2))
acf(res102210)
pacf(res102210)

#method3
library(TSA)

Box.test(fit102_210$residuals, lag=6, type="Ljung-Box", fitdf=5) 
B_text_p_value = c()
for(hh in 6:20){
  B_text_p_value[hh] = Box.test(fit102_210$residuals, lag=hh, type="Ljung-Box", fitdf=5)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="102_210 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)

## 診斷fit103_210$residuals ####
#method1
par(mfrow=c(2,2))
res103210<-fit103_210$residuals
qqnorm(res103210)             
qqline(res103210,col="red")
hist(res103210)
plot(res103210)
plot(res103210,type="p")

shapiro.test(res103210)

#method2
par(mfrow=c(1,2))
acf(res103210)
pacf(res103210)

#method3
library(TSA)

Box.test(fit103_210$residuals, lag=7, type="Ljung-Box", fitdf=6) 
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit103_210$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="103_210 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)


## 診斷fit103_210$residuals ####
#method1
par(mfrow=c(2,2))
res400011<-fit400_011$residuals
qqnorm(res400011)             
qqline(res400011,col="red")
hist(res400011)
plot(res400011)
plot(res400011,type="p")

shapiro.test(res400011)

#method2
par(mfrow=c(1,2))
acf(res400011)
pacf(res400011)

#method3
library(TSA)
B_text_p_value = c()
for(hh in 6:20){
  B_text_p_value[hh] = Box.test(fit400_011$residuals, lag=hh, type="Ljung-Box", fitdf=5)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="400_011 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)




## 12步預測code ####


x.pred = predict(fit, n.ahead=12)
x.pred
pred.U = x.pred$pred + 1.96*x.pred$se
pred.L = x.pred$pred - 1.96*x.pred$se

ts.plot(c(data[219:267], rep(NA,20)),xlab = "Time", ylab = "Number of birth",ylim=c(7500,20000))
lines(length(data[219:267])+(1:12), x.pred$pred, col="blue")
lines(length(data[219:267])+(1:12), pred.U, col="red", lty=2)
lines(length(data[219:267])+(1:12), pred.L, col="red", lty=2)
points(length(data[219:267])+(1:12), data[268:279], pch=16,cex=0.8)

b<-x.pred$pred-data[268:279]
mean(b^2)
sum(b^2)

a<-x.pred$pred[1:12]
dataout<-data.frame(real=data[268:279],est=a,uu=pred.U[1:12],ll=pred.L[1:12],error=b[1:12])



## 1步預測 12次 code ####

df2<-df
se<-vector()
fit400_011 <- arima(df2, order=c(4,0,0), seasonal=list(order=c(0,1,1), period=12)) #log likelihood = -2311.99,  aic = 4637.98
for (i in 1:12){
  y.pred = predict(fit400_011, n.ahead=1)
  df2[267+i]<-y.pred$pred
  se[i]<-y.pred$se
  fit400_011 <- arima(df2, order=c(4,0,0), seasonal=list(order=c(0,1,1), period=12))
  
}


pred.U2 = df2[268:279] + 1.96*se
pred.L2 = df2[268:279] - 1.96*se

ts.plot(c(df2[219:267], rep(NA,20)),xlab = "Time", ylab = "Number of birth",ylim=c(7500,20000))
lines(length(df2[219:267])+(1:12), df2[268:279], col="green")
lines(length(df2[219:267])+(1:12), pred.U2, col="purple", lty=2)
lines(length(df2[219:267])+(1:12), pred.L2, col="purple", lty=2)
points(length(data[219:267])+(1:12), data[268:279], pch=16,cex=0.8)

c<-df2[268:279]-data[268:279]
c^2
sum(c^2)



write.csv(dataout,"dataout.csv")





# 期末3次方迴歸配適 ####
data<-df_birth$amount

df<-data[1:267]
t<-1:length(df)
plot(df,type="l")

adf.test(df)
acf(df, main="ACF ", lag.max = 100)
abline(h=0.5, lty=2, col="red")
abline(v=20, lty=2, col="green")
pacf(df, main="PACF ",lag.max = 100)


lm.fit<-lm(df~poly(t,3))
summary(lm.fit)

lm.res<-lm.fit$residuals

fit <- lm(df ~I(t^3)+I(t^2)+I(t) )
summary(fit)

fit.res<-fit$residuals

#觀察regression配飾
plot(df,type="l",xlab = "時間",family = "BiauKai", ylab = "出生人數", main="出生人數時間序列")
lines(predict(lm.fit),col="red")
legend("topright", legend = "3 degree regression", col = "red", lwd = 1,text.font = 1)

par(mfrow=c(1,2))
acf(lm.res, lag=100, main="")
title(main="3次多項式detrend acf", family = "DFKaiShu-SB-Estd-BF")
pacf(lm.res, lag=100, main="")
title(main="3次多項式detrend pacf", family = "DFKaiShu-SB-Estd-BF")
adf.test(lm.res)

## 3次多項式配模型 ####
rpdq<-c(1,0,3)
rPDQ<-c(2,1,1)        #(2,1,0) (3,0,2)
s <-12

lr.fit <- arima(lm.res[1:267], order=rpdq, seasonal=list(order=rPDQ, period=s))
lr.fit

lm.res
lrx.pred = predict(lr.fit, n.ahead=12)


lrpred.U = lrx.pred$pred + 1.96*lrx.pred$se
lrpred.L = lrx.pred$pred - 1.96*lrx.pred$se

ts.plot(c(lm.res[219:267], rep(NA,20)),xlab = "Time", ylab = "Number of birth")
lines(length(lm.res[219:267])+(1:12), lrx.pred$pred, col="blue")
lines(length(lm.res[219:267])+(1:12), lrpred.U, col="red", lty=2)
lines(length(lm.res[219:267])+(1:12), lrpred.L, col="red", lty=2)
points(length(lm.res[219:267])+(1:12), lm.res[268:279], pch=16,cex=0.8)

a=x.pred$pred+pp[268:279]
sum(a^2)
pre<-predict(lm.fit)[268:279]+pp[268:279]


## poly 1步預測
library(forecast)
para<-auto.arima(data,seasonal=TRUE,test="adf",ic="aic")
names(para)


fit$residuals

x.pred = predict(fit4, n.ahead=20)
x.pred
pred.U = x.pred$pred + 1.96*x.pred$se
pred.L = x.pred$pred - 1.96*x.pred$se

predict(lm.fit,data=c(268:279))

## 1步預測 12次 code ####

df2<-df
se<-vector()
fit103_211 <- arima(df2, order=c(1,0,3), seasonal=list(order=c(2,1,1), period=12)) #log likelihood = -2311.99,  aic = 4637.98
for (i in 1:12){
  y.pred = predict(fit103_211, n.ahead=1)
  df2[268+i-1]<-y.pred$pred
  se[i]<-y.pred$se
  fit103_211 <- arima(df2, order=c(1,0,3), seasonal=list(order=c(2,1,1), period=12))
}


pred.U2 = df2[268:279] + 1.96*se
pred.L2 = df2[268:279] - 1.96*se

ts.plot(c(df2[219:267], rep(NA,20)),xlab = "Time", ylab = "Number of birth",ylim=c(7500,20000))
lines(length(df2[219:267])+(1:12), df2[268:279], col="green")
lines(length(df2[219:267])+(1:12), pred.U2, col="purple", lty=2)
lines(length(df2[219:267])+(1:12), pred.L2, col="purple", lty=2)
points(length(data[219:267])+(1:12), data[268:279], pch=16,cex=0.8)

a<-df2[268:279]-data[268:279]
a^2
sum(a^2)









###### 不需要的code diff=1 change lag part####
par(mfrow=c(3,3)) 
lags <- paste0("lag", 1:12)
acf <- paste0("acf", 1:12)
pacf <- paste0("pacf", 1:12)

for (i in 1:12){
  print(i)
  data_diff<-diff(data,lag = i)
  ts.plot(data_diff ,xlab = "Time", ylab = "Diff",main = lags[i])
  acf(data_diff ,xlab = "Time", ylab = "Diff", main = acf[i])
  pacf(data_diff ,xlab = "Time", ylab = "Diff", main = pacf[i])
}

par(mfrow=c(3,4)) 
for (i in 1:12){
  print(i)
  data_diff<-diff(data,lag = i)
  ts.plot(data_diff ,xlab = "Time", ylab = "Diff",main = lags[i])
}

###### lag=1 change diff part
difference <- paste0("difference", 1:12)
acf <- paste0("acf", 1:12)
pacf <- paste0("pacf", 1:12)

par(mfrow=c(3,3)) 
for (i in 1:12){
  print(i)
  data_diff<-diff(data,difference = i)
  ts.plot(data_diff ,xlab = "Time", ylab = "Diff",main = difference[i])
  acf(data_diff ,xlab = "Time", ylab = "Diff", main = acf[i])
  pacf(data_diff ,xlab = "Time", ylab = "Diff", main = pacf[i])
}

par(mfrow=c(3,4)) 
for (i in 1:12){
  print(i)
  data_diff<-diff(data,difference = i)
  ts.plot(data_diff ,xlab = "Time", ylab = "Diff",main = difference[i])
}






## 診斷fit002_301$residuals ####
#method1
par(mfrow=c(2,2))
res002<-fit002_301$residuals
qqnorm(res002)             #X
qqline(res002,col="red")
hist(res002)
plot(res002)
plot(res002,type="p")

shapiro.test(res002)

#method2
par(mfrow=c(1,2))
acf(res002)
pacf(res002)

#method3
library(TSA)

Box.test(fit002_301$residuals, lag=7, type="Ljung-Box", fitdf=6) 
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit002_301$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)



## 診斷fit102_301$residuals ####
#method1
par(mfrow=c(2,2))
res102<-fit102_301$residuals
qqnorm(res102)             #X
qqline(res102,col="red")
hist(res102)
plot(res102)
plot(res102,type="p")

shapiro.test(res102)

#method2
par(mfrow=c(1,2))
acf(res102)
pacf(res102)

#method3
library(TSA)

Box.test(fit102_301$residuals, lag=8, type="Ljung-Box", fitdf=7) 
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit002_301$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="102_301 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)



## 診斷fit011_500$residuals ####
#method1
par(mfrow=c(2,2))
res011<-fit011_500$residuals
qqnorm(res011)             #X
qqline(res011,col="red")
hist(res011)
plot(res011)
plot(res011,type="p")

shapiro.test(res011)

#method2
par(mfrow=c(1,2))
acf(res011)
pacf(res011)

#method3
library(TSA)

Box.test(fit011_500$residuals, lag=7, type="Ljung-Box", fitdf=6) 
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit011_500$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="lag3 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)


## 診斷fit110_500$residuals ####
#method1
par(mfrow=c(2,2))
res110<-fit110_500$residuals
qqnorm(res110)               #X
qqline(res110,col="red")
hist(res110)
plot(res110)
plot(res110, type="p")

shapiro.test(res110)
#method2
par(mfrow=c(1,2))
acf(res110)
pacf(res110)

#method3
library(TSA)

Box.test(fit110_500$residuals, lag=7, type="Ljung-Box", fitdf=6) 
B_text_p_value = c()
for(hh in 7:20){
  B_text_p_value[hh] = Box.test(fit110_500$residuals, lag=hh, type="Ljung-Box", fitdf=6)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="lag3 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)








## 診斷fit211_411$residuals ####
#method1
par(mfrow=c(2,2))
res211<-fit211_411$residuals
qqnorm(res211)               
qqline(res211,col="red")
hist(res211)
plot(res211)
plot(res211, type="p")

shapiro.test(res211)
#method2
par(mfrow=c(1,2))
acf(res211)
pacf(res211)

#method3
library(TSA)

Box.test(fit110_500$residuals, lag=9, type="Ljung-Box", fitdf=8) 
B_text_p_value = c()
for(hh in 9:20){
  B_text_p_value[hh] = Box.test(fit211_411$residuals, lag=hh, type="Ljung-Box", fitdf=8)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="211_411 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)









## 診斷fit311_411$residuals ####
#method1
par(mfrow=c(2,2))
res311<-fit311_411$residuals
qqnorm(res311)               
qqline(res311,col="red")
hist(res311)
plot(res311)
plot(res311, type="p")

shapiro.test(res311)
#method2
par(mfrow=c(1,2))
acf(res311)
pacf(res311)

#method3
library(TSA)

Box.test(fit311_411$residuals, lag=10, type="Ljung-Box", fitdf=9) 
B_text_p_value = c()
for(hh in 10:20){
  B_text_p_value[hh] = Box.test(fit311_411$residuals, lag=hh, type="Ljung-Box", fitdf=9)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="311_411 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)



















## 診斷fit012_411$residuals ####
#method1
par(mfrow=c(2,2))
res012<-fit012_411$residuals
qqnorm(res012)               
qqline(res012,col="red")
hist(res012)
plot(res012)
plot(res012, type="p")

shapiro.test(res012)
#method2
par(mfrow=c(1,2))
acf(res012)
pacf(res012)

#method3
library(TSA)

Box.test(fit012_411$residuals, lag=8, type="Ljung-Box", fitdf=7) 
B_text_p_value = c()
for(hh in 8:20){
  B_text_p_value[hh] = Box.test(fit012_411$residuals, lag=hh, type="Ljung-Box", fitdf=7)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="012_411 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)









## 診斷fit013_411$residuals ####
#method1
par(mfrow=c(2,2))
res013<-fit013_411$residuals
qqnorm(res013)               
qqline(res013,col="red")
hist(res013)
plot(res013)
plot(res013, type="p")

shapiro.test(res013)
#method2
par(mfrow=c(1,2))
acf(res013)
pacf(res013)

#method3
library(TSA)

Box.test(fit013_411$residuals, lag=9, type="Ljung-Box", fitdf=8) 
B_text_p_value = c()
for(hh in 9:20){
  B_text_p_value[hh] = Box.test(fit013_411$residuals, lag=hh, type="Ljung-Box", fitdf=8)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="013_411 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)


























