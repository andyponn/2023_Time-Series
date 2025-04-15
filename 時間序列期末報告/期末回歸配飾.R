df<-df_birth$amount
t<-1:length(df)
df<-as.vector(df)
plot(df_birth$amount,type="l")

adf.test(df_birth$amount)
acf(df, main="ACF ", lag.max = 100)
abline(h=0.5, lty=2, col="red")
abline(v=20, lty=2, col="green")

pacf(df, main="PACF ",lag.max = 100)

fit<-lm(df_birth$amount~poly(t,3))
summary(fit)

res<-fit$residuals

plot(df_birth$amount,type="l")
lines(predict(fit),col="red")


acf(fit$residuals, lag=100)
pacf(fit$residuals, lag=100)
adf.test(fit$residuals)



lmdiffs<-diff(fit$residuals,lag=1)

par(mfrow=c(1,2)) 
acf(lmdiffs)
pacf(lmdiffs)

#202201
#202103
tt<-length(fit$residuals)
fit_res<-fit$residuals[1:(tt-6)]
fit4 <- arima(fit_res, order=c(2,0,2), seasonal=list(order=c(2,0,1), period=6) )

library(TSA)
fit4_res = as.vector(rstandard(fit4))


Box.test(fit4_res, lag=5, type="Ljung-Box", fitdf=4) 

fit_22$aic
B_text_p_value = c()
for(hh in 8:20){
  B_text_p_value[hh] = Box.test(fit4_res, lag=hh, type="Ljung-Box", fitdf=7)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="lag3 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)

fit_22 <- arima(fit_res, order=c(2,0,2)) # ARMA(3,3)
fit2 <- arima(fit_res, order=c(0,1,1), seasonal=list(order=c(2,0,0), period=6) )
res<-scale(fit_22$residuals)

Box.test(res, lag=3, type="Ljung-Box", fitdf=2) 

fit_22$aic
B_text_p_value = c()
for(hh in 3:20){
  B_text_p_value[hh] = Box.test(res, lag=hh, type="Ljung-Box", fitdf=2)$p.value
}
plot(1:20, B_text_p_value[1:20], type="p", 
     main="lag3 Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)




library(forecast)
para<-auto.arima(fit$residuals,seasonal=TRUE,test="adf",ic="aic")
names(para)


fit$residuals

x.pred = predict(fit4, n.ahead=20)
x.pred
pred.U = x.pred$pred + 1.96*x.pred$se
pred.L = x.pred$pred - 1.96*x.pred$se



###ARMA(2,0,2) 