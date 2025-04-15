##### 時間數列分析 期中報告 SARIMA Ljoung Box
##### 組員：陳葳芃 楊岳錩
##### 選用資料：2000-2023 台灣出生月人口數
##### 資料來源：

data_diff1<-diff(data)



fit1 <- arima(data, order=c(0,1,1), seasonal=list(order=c(3,0,0), period=12) )
fit1

fit2 <- arima(data, order=c(1,1,0), seasonal=list(order=c(3,0,0), period=12) )
fit2


# model selection
names(fit1)
fit1$aic #preferred
fit2$aic  




#residual analysis
library(TSA)
rstandard(fit2)  #package TSA
ts.plot(rstandard(fit2), ylab="", main="standardized residuals")
fit2_res = as.vector(rstandard(fit2))

acf(fit2_res)
pacf(fit2_res)
#no further dependence

Box.test(fit2_res, lag=3, type="Ljung-Box")
names(Box.test(fit2_res, lag=3, type="Ljung-Box"))

B_text_p_value = c(0,0)
for(hh in 3:20){
  B_text_p_value[hh] = Box.test(fit2_res, lag=hh, type="Ljung-Box")$p.value
}
plot(3:20, B_text_p_value[3:20], type="p", 
     main="p values for Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)
#acceptable => there is no inadequacy of the model
