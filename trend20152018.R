library(ggplot2)
# 時間序列分析
# 201601~201804每月各屬性趨勢圖
trend_data <- rbind(month_PV, month_PV2017, month_PV2018)
trend_data <- ddply(trend_data, .(time,first_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$first_name),]
trend_data$first_name <- factor(trend_data$first_name,levels = c("基礎保養","底妝","彩妝","進階護膚","美髮","防曬","身體保養","香水香氛","美容工具","營養補給"))
# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖
ggplot(data = trend_data)+
  geom_line(aes(x=as.factor(time), y=pv_per,group=first_name,color=first_name),size=1)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=first_name,color=first_name))
ggsave("201601-201804各屬性趨勢.png", height=12, width=16)

# 彩妝拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==5,]
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
trend_data$second_name <- factor(trend_data$second_name,levels = c("唇彩","眼影","頰彩","修容","眼線","眉彩","睫毛","美甲","多功能彩妝"))
  
# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(含唇彩)
ggplot(data = trend_data)+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  labs(x="時間",y="比例")+
  scale_color_manual(values = c("#67001f","#b2182b","#66c2a5","#f4a582","#053061","#5aae61","#4393c3","#92c5de","#d6604d"),
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804彩妝趨勢.png", height=12, width=16)

# 繪圖(不含唇彩)
ggplot(data = trend_data[trend_data$second_name!="唇彩",])+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  scale_color_manual(values = c("#b2182b","#66c2a5","#f4a582","#053061","#5aae61","#4393c3","#92c5de","#d6604d"),
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804彩妝趨勢(不含唇彩).png", height=12, width=16)

