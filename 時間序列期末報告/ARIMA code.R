##### 時間數列分析 期中報告 ARIMA Ljoung Box
##### 組員：陳葳芃 楊岳錩
##### 選用資料：2000-2023 台灣出生月人口數
##### 資料來源：


par(mfrow=c(1,1)) 
fit_413 <- arima(data, order=c(4,1,3)) # ARMA(3,3)
fit_model <- fit_413



#residual analysis
#(1)
fit_model$residuals 
ts.plot(fit_model$residuals)
#(2)
library(TSA)
rstandard(fit_model)  #package TSA
ts.plot(rstandard(fit_model), ylab="", main="standardized residuals")
#change in variability => ARCH-type model. no other systematic pattern
fit_model_res = rstandard(fit_model)
#(3)
acf(fit_model_res)
pacf(fit_model_res)
#not satisfactory, further check the Ljung�VBox�VPierce Q-statistic
#(4) construct Fig 3.16
Box.test(fit_model_res, lag=13, type="Ljung-Box", fitdf=7)   #input "fitdf" according to p+q in the fitted model
names(Box.test(fit_model_res, lag=10, type="Ljung-Box", fitdf=7))
B_text_p_value = c(0,0)
for(hh in 7:22){
  B_text_p_value[hh] = Box.test(fit_model_res, lag=hh, type="Ljung-Box", fitdf=7)$p.value
}
plot(7:22, B_text_p_value[4:22], type="p", 
     main="p values for Ljung-Box statistic", 
     xlab="lag", ylab="p value", ylim=c(0,1))
abline(h=0.05, lty=2, col=4)


#results and comments:
# fit_33: residuals not good enough

# fit_50: residuals better, but estimates not good enough
# fit_51: not necessary in term of estimate

# fit_05: residuals perfect, and estimates ok
# fit_15: not necessary in term of estimate

# fit_35: residuals ok, but unstable estimates / not significant estimates 


#tsdiag() function can replace the code in (1) ~ (4)
# however, this is a comment in the textbook: The script tsdiag is available in R to run diagnostics for an ARIMA object, however, the script has errors and we do not recommend using it.

