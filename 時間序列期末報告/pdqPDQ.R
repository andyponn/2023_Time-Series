library(dplyr)

n <- 5
res <- rep(Inf, n)
parameters <- matrix(0, nrow = n, ncol = 4)
results <- data.frame(AIC = res, P = parameters[, 1], Q = parameters[, 2], p = parameters[, 3], q = parameters[, 4])

for (p in 0:5) {
  for (q in 0:5) {
    for (P in 0:5) {
      for (Q in 0:5) {
        fit4 <- arima(fit$residuals, order = c(p, 0, q), seasonal = list(order = c(P, 0, Q), period = 6))
        rescaled <- scale(fit4$residuals)
        aic <- fit4$aic
        
        min_AIC_index <- which.min(results$AIC)
        if (aic < results$AIC[min_AIC_index]) {
          results$AIC[min_AIC_index] <- aic
          results$P[min_AIC_index] <- P
          results$Q[min_AIC_index] <- Q
          results$p[min_AIC_index] <- p
          results$q[min_AIC_index] <- q
        }
      }
    }
  }
}

top_5_results <- arrange(results, AIC) %>% head(5)
top_5_results