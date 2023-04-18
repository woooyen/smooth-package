library(Mcomp)
library(smooth)
library(forecast)

m3_m <- subset(M3,"monthly")
m3_o <- subset(M3,"Other")
m3_q <- subset(M3,"Quarterly")
m3_y <- subset(M3,"Yearly")

# Original M3 forecasts
nseries <- length(m3_m)
theta <- as.matrix(M3Forecast$THETA)
fpro <- as.matrix(M3Forecast$ForecastPro)
fcx <- as.matrix(M3Forecast$ForcX)
bjauto <- as.matrix(M3Forecast$`B-J auto`)
ab1 <- as.matrix(M3Forecast$AutoBox1)
ab2 <- as.matrix(M3Forecast$AutoBox2)
ab3 <- as.matrix(M3Forecast$AutoBox3)
ets1 <- sarima <- hybrid <- matrix(NA, nrow=nseries, ncol=18)
es1 <- aarima <- hybrid <- matrix(NA, nrow=nseries, ncol=18)
# ETS and ARIMA forecasts, and combination
for (i in seq(nseries)) {
  #ets1[i, ] <- forecast(ets(m3_y[[i]]$x), h=6, PI=FALSE)$mean
  #ets1[i, ] <- forecast(ets(m3_o[[i]]$x), h=8, PI=FALSE)$mean
  #ets1[i, ] <- forecast(ets(m3_q[[i]]$x), h=4, PI=FALSE)$mean
  #ets1[i, ] <- forecast(ets(m3_y[[i]]$x), h=6, PI=FALSE)$mean
  #es1[i, ] <- forecast(es(m3_y[[i]]$x,model="ZZZ"), h=6, PI=FALSE)$mean
  #ets1[i, ] <- forecast(ets(m3_o[[i]]$x), h=8, PI=FALSE)$mean
  #ets1[i, ] <- forecast(ets(m3_q[[i]]$x), h=4, PI=FALSE)$mean
  #ets1[i, ] <- forecast(ets(m3_y[[i]]$x), h=6, PI=FALSE)$mean
  aarima[i, ] <- forecast::forecast(auto.arima(m3_m[[i]]$x), h=18)$mean
  sarima[i, ] <- forecast(auto.ssarima(m3_m[[i]]$x), h=18)$mean
  #hybrid[i, ] <- 0.5 * (aarima[i, ] + ets1[i, ])
}


# Compute accuracy of all methods
mase <- mape <- smape <- matrix(NA, nrow=2, ncol=nseries)
f <- matrix(NA, nrow=2, ncol=18)
for (i in seq(nseries)) {
  x <- m3_m[[i]]$xx
  n <- length(x)
  f[1, seq(n)] <- aarima[i, seq(n)]
  f[2, seq(n)] <- sarima[i, seq(n)]
  scale <- mean(abs(diff(m3_m[[i]]$x, lag = frequency(x))))
  for (j in seq(2)) {
    e <- abs((x - f[j, seq(n)]))
    mape[j, i] <- mean(e / x) * 100
    smape[j, i] <- mean(e / (abs(x) + abs(f[j, seq(n)]))) * 200
    mase[j, i] <- mean(e / scale)
  }
}


m3table <- matrix(NA, nrow = 2, ncol = 3)
m3table[, 1] <- rowMeans(mape, na.rm = TRUE)
m3table[, 2] <- rowMeans(smape)
m3table[, 3] <- rowMeans(mase)
rownames(m3table) <- c("arima", "sarima")
colnames(m3table) <- c("MAPE", "Average_sMAPE_recalculated", "MASE")
j <- order(m3table[, 3])
m3table <- cbind(m3table,
                 Average_sMAPE = c(13.01, 13.19, 13.49, 14.01, 15.23, 14.41, 15.33, NA, NA, NA))
kable(round(m3table[
  c("Theta", "ForecastPro", "ForecastX", "BJauto", "Autobox2", "Autobox1", "Autobox3"),
  c("Average_sMAPE", "Average_sMAPE_recalculated", "MAPE", "MASE")], 2))
