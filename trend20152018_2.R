######################################################################

# 第三層屬性
# 進階護膚拆開來看:唇部保養
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==24,] # 唇部保養：24
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,third_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$third_name),]
# 底妝裡面的子屬性factor
trend_data$third_name <- factor(trend_data$third_name,levels = c("護唇膏","護唇精華","唇膜","其它唇部保養"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(底妝全部)
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=third_name,color=third_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[1:4],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804粉底趨勢.png", height=12, width=16)


# 看兩者相關性
# 唇彩and唇部保養:38 & 24
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data38 <- trend_data[trend_data$second==38,]
trend_data24 <- trend_data[trend_data$second==24,] 
trend_data24 <- trend_data24[!is.na(trend_data24$product_id),] 
trend_data38 <- trend_data38[!is.na(trend_data38$product_id),] 
trend_data38 <- ddply(trend_data38, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data24 <- ddply(trend_data24, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data38 <- trend_data38[!is.na(trend_data38$second_name),]

# 唇彩裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("唇彩","唇部保養"))

# 排序
trend_data24 <- trend_data24[with(trend_data24, order(third_name)),]
trend_per24 <- ddply(trend_data24, .(third_name), summarize, per=(pv_sum-mean(pv_sum))/sd(pv_sum))
trend_per38 <- ddply(trend_data38, .(second_name), summarize, per=(pv_sum-mean(pv_sum))/sd(pv_sum))

# 去季節性
trend_data24$pv_per <- trend_per24$per
trend_data38$pv_per <- trend_per38$per
rm(trend_per24, trend_per38)
names(trend_data38)[2] <- "name"
trend_data3824 <- rbind(trend_data38, trend_data24)
# 繪圖(唇彩,唇部保養)
ggplot(data = trend_data3824,aes(x=as.factor(time), y=pv_per,group=name,color=name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[1:5],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804唇彩及唇部保養相關性圖.png", height=12, width=16)

# 計算相關性
dtrend_data3824 <- dcast(trend_data3824, time~name, value.var = ("pv_per"))
cor(dtrend_data3824[,2:6]) 
#               其它唇部保養      唇彩       唇膜   護唇精華    護唇膏
# 唇彩            0.8844233 1.0000000 0.73010921 0.52542393 0.7461845
rm(dtrend_data3824, trend_data24, trend_data38, trend_data3824)


# 從成長率來看唇彩和純部保養
diff_38 <- data.frame(diff=diff(trend_data38$desea), time=trend_data38[2:32,"time"], org=trend_data38[1:31,"desea"])
diff_24 <- data.frame(diff=diff(trend_data24$desea), time=trend_data24[2:32,"time"], org=trend_data24[1:31,"desea"])
diff_38$per <- diff_38$diff / diff_38$org
diff_24$per <- diff_24$diff / diff_24$org
diff_24$name <- "純部保養"
diff_38$name <- "唇彩"
diff_3824 <- rbind(diff_24, diff_38)
cor(diff_24$per, diff_38$per)
ggplot(data = diff_3824,aes(x=as.factor(time), y=per,group=name,linetype=name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_linetype_manual(values = c(1,4),
                        guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 4, linetype="dashed")+
  geom_vline(xintercept = 16, linetype="dashed")+
  geom_vline(xintercept = 28, linetype="dashed")

b_24 <- diff_24[diff_24$time<201608,]
b_38 <- diff_38[diff_38$time<201608,]
a_24 <- diff_24[diff_24$time>=201608&diff_24$time<=201712,]
a_38 <- diff_38[diff_38$time>=201608&diff_38$time<=201712,]
cor(b_24$per, b_38$per) #0.6768212
cor(a_24$per, a_38$per) #0.8160716
ggsave("201509-201804唇彩及唇部保養成長相關性圖_去季節性.png", height=12, width=16)
########################################################################

# 舒緩,敏感,修護字眼的產品是否有成長?
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data[grep("舒緩", trend_data$p_name_tw),"toohot2"] <- 1
trend_data[grep("敏感", trend_data$p_name_tw),"toohot2"] <- 1
trend_data[grep("修護", trend_data$p_name_tw),"toohot3"] <- 1
trend_data[is.na(trend_data$toohot),"toohot"] <- 0
trend_data[grep("修護", trend_data$p_name_tw),"toohot3"] <- 1
trend_data$sensitive <- ifelse(trend_data$toohot==1|trend_data$toohot2==1|trend_data$toohot3==1,1,0)
trend_data <- trend_data[trend_data$sensitive==1,]
trend_data <- trend_data[with(trend_data, order(time)),]
trend_data <- ddply(trend_data, .(time),summarize,pv_sum=sum(PV_month))

trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
ts_sensitive <- ts(trend_data$pv_sum, frequency = 12, c(2015,9))
ts_sensitive
autoplot(ts_sensitive)
trend_diff <- data.frame(diff(trend_data$pv_sum))
trend_diff$time <- trend_data[2:32,"time"]
trend_diff$org <- trend_data[1:31,2]
trend_diff$per <- round(trend_diff$diff.trend_data.pv_sum./trend_diff$org, 4)
ts_sensitive <- ts(trend_diff$per, frequency = 12, c(2015,10))
autoplot(ts_sensitive)

###################################################################
# 假說:2016~2018年 有關潤唇的唇部護理產品關注增加
# 唇部保養中 2018年最受關注的產品
trend_data <- month_PV2018[month_PV2018$second==24,]
trend_data <- trend_data[!is.na(trend_data$product_id),] 
#product_92 <- trend_data[trend_data$third==92,]
product_92 <- ddply(trend_data, .(p_name_tw), summarize, pv_sum=sum(PV_month, na.rm = T))
product_92$per <- round(product_92$pv_sum/sum(product_92$pv_sum, na.rm = T),4)
product_92 <- product_92[with(product_92, order(per, decreasing = T)),]
product_92 <- product_92[1:20,]
product_92$p_name_tw <- factor(product_92$p_name_tw, levels = product_92$p_name_tw[order(product_92$per)])
ggplot(product_92[1:10,])+
  geom_histogram(aes(x=p_name_tw, y=per), stat = "identity", width=0.3, fill=colorfixed[1])+
  coord_flip()+
  labs(x="產品名稱", y="比例")+
  theme(axis.text.y = element_text(size = 72),
        axis.text.x = element_text(size = 72))
ggsave("2018唇部保養最受關注商品.png", height=12, width=16)
# 唇部保養中 2017年最受關注的產品
trend_data <- month_PV2017[month_PV2017$second==24,]
trend_data <- trend_data[!is.na(trend_data$product_id),] 
#product2017_92 <- trend_data[trend_data$third==92,]
product2017_92 <- ddply(trend_data, .(p_name_tw), summarize, pv_sum=sum(PV_month, na.rm = T))
product2017_92$per <- round(product2017_92$pv_sum/sum(product2017_92$pv_sum, na.rm = T),4)
product2017_92 <- product2017_92[with(product2017_92, order(per, decreasing = T)),]
product2017_92 <- product2017_92[1:20,]
product2017_92$p_name_tw <- factor(product2017_92$p_name_tw, levels = product2017_92$p_name_tw[order(product2017_92$per)])
ggplot(product2017_92[1:10,])+
  geom_histogram(aes(x=p_name_tw, y=per), stat = "identity",width=0.3, fill=colorfixed[2])+
  coord_flip()+
  labs(x="產品名稱", y="比例")+
  theme(axis.text.y = element_text(size = 72),
        axis.text.x = element_text(size = 72))
ggsave("2017唇部保養最受關注商品.png", height=12, width=16)
# 唇部保養中 2016年最受關注的產品
trend_data <- month_PV[month_PV$second==24,]
trend_data <- trend_data[!is.na(trend_data$product_id),] 
#product2016_92 <- trend_data[trend_data$third==92,]
product2016_92 <- ddply(trend_data, .(p_name_tw), summarize, pv_sum=sum(PV_month, na.rm = T))
product2016_92$per <- round(product2016_92$pv_sum/sum(product2016_92$pv_sum, na.rm = T),4)
product2016_92 <- product2016_92[with(product2016_92, order(per, decreasing = T)),]
product2016_92 <- product2016_92[1:20,]
product2016_92$p_name_tw <- factor(product2016_92$p_name_tw, levels = product2016_92$p_name_tw[order(product2016_92$per)])
ggplot(product2016_92[1:10,])+
  geom_histogram(aes(x=p_name_tw, y=per), stat = "identity", width=0.3, fill=colorfixed[3])+
  coord_flip()+
  labs(x="產品名稱", y="比例")+
  theme(axis.text.y = element_text(size = 72),
        axis.text.x = element_text(size = 72))
ggsave("2016唇部保養最受關注商品.png", height=12, width=16)
