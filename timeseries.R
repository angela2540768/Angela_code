install.packages("tseries")
install.packages('forecast', dependencies = TRUE)
devtools::install_github("robjhyndman/forecast")
install.packages("ggfortify")
library(tseries)
library(forecast)
library(plyr)
library(devtools)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(scales)
# 唇彩預測
 # data prepare 
trend_data <- rbind(month_PV2015, month_PV, month_PV2017, month_PV2018)
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- trend_data[trend_data$first==5,] #第一層屬性
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second),]
trend_data <- trend_data[with(trend_data, order(time)),]
 # 計算比例
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
 # 挑出目標分析屬性
 # 變成時間序列資料
ts <- ts(trend_data[trend_data$second==37,"pv_per"], frequency = 12, start = c(2015,9))
ts
plot(ts)
autoplot(ts)
# 1. 看 acf & pacf 圖
tsdisplay(ts)
# 2. 單根檢定
adf.test(ts) # failed
# 3. 一階差分
ts_diff <- diff(ts, differences = 1)
tsdisplay(ts_diff)
adf.test(ts_diff)
# 4. 二階差分
ts_diff2 <- diff(ts, differences = 2)
tsdisplay(ts_diff2)
adf.test(ts_diff2)
# 5. 取對數
ts_log <- log(ts)
tsdisplay(ts_log)
adf.test(ts_log) # failed!
# 6. 季節性因素
tsdisplay(diff(ts,12))
# 7. use auto.arima to look for the order
auto.arima(ts) 
# 8. use Arima with order and seasonal
ts_arima <- Arima(ts, order = c(3,1,0),seasonal=list(order=c(0,1,0),period=12))
# 9. forecast
ts_forecast <- forecast(ts_arima,8)
ts_forecast$mean
ts_forecast$lower
# 10. plot
plot(ts_forecast)
autoplot(ts)
autoplot(ts_forecast, predict.linetype = 'dashed')+
  scale_x_date(breaks= date_breaks("1 month"))+
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5)) # use ggplot2 that gap is removed
# 11. test the accuracy
accuracy(ts_forecast)

  