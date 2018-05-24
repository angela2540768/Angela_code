library(ggplot2)
library(RColorBrewer)
brewer.pal.info
display.brewer.all()
# 時間序列分析
# 201509~201804每月各屬性趨勢圖
trend_data <- rbind(month_PV2015,month_PV, month_PV2017, month_PV2018)
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
ggsave("201509-201804各屬性趨勢.png", height=12, width=16)

# 201509~201804每月各屬性趨勢圖(不含基礎保養、底妝、彩妝)
ggplot(data = trend_data[trend_data$first_name!="基礎彩妝"&trend_data$first_name!="底妝"&trend_data$first_name!="彩妝",])+
  geom_line(aes(x=as.factor(time), y=pv_per,group=first_name,color=first_name),size=1)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=first_name,color=first_name))+
  labs(x="時間",y="比例")+
  scale_color_manual(values = c("#f4a582","#053061","#2166ac","#4393c3","#92c5de","#d1e5f0","#fddbc7"),
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804各屬性趨勢＿不含3大.png", height=12, width=16)

#######################################################################
# 彩妝拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==5,] # 彩妝：5
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

#################################################################
# 基礎保養拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==1,] # 基礎保養：1
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
 # 基礎保養裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("精華","化妝水","洗臉","卸妝","乳液","乳霜","凝霜","凝膠","前導","面膜","多功能保養"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(基礎保養全部)
ggplot(data = trend_data)+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804基礎保養趨勢.png", height=12, width=16)

#################################################################
# 進階保養拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==2,] # 進階保養：2
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
# 進階保養裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("去角質","眼睫保養","唇部保養","進階保養"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(進階保養全部)
ggplot(data = trend_data)+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804進階保養趨勢.png", height=12, width=16)

#################################################################
# 美髮拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==6,] # 美髮：6
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
# 美髮裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("洗髮","潤髮","護髮","頭皮護理","染髮","頭髮造型"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(美髮全部)
ggplot(data = trend_data)+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804美髮趨勢.png", height=12, width=16)

#################################################################
# 身體保養拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==7,] # 身體保養：7
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
# 身體保養裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("美體保養","手部保養","腿足保養","其它部位保養","私密護理","沐浴清潔","爽身制汗","牙齒保養"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(身體保養全部)
ggplot(data = trend_data)+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804身體保養趨勢.png", height=12, width=16)


#################################################################
# 美容工具拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==9,] # 美容工具：9
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
# 身體保養裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("臉部保養工具","彩妝工具","身體保養工具","美髮工具","美容家電"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(美容工具全部)
ggplot(data = trend_data)+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804美容工具趨勢.png", height=12, width=16)





