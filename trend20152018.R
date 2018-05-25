install.packages("directlabels")
install.packages("gghighlight")
library(gghighlight)
library(ggplot2)
library(directlabels)
library(plyr)
library(reshape2)
##############################################################
# color fixed
# red, blue, green, purple, brown, gray
colorfixed <- c("#871F16","#CC645A","#153B54","#6C899D","#166D66","#4AB9B0","#5B20A2","#A275DA","#97521C","#CB8E5D","#ECD77A")
# 時間序列分析
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
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=first_name,color=first_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = c("#871F16","#CC645A","#153B54","#6C899D","#166D66","#4AB9B0","#5B20A2","#A275DA","#97521C","#CB8E5D"),
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804各屬性趨勢.png", height=12, width=16)

# 201509~201804每月各屬性趨勢圖(不含基礎保養、底妝、彩妝)
ggplot(data = trend_data[trend_data$first_name!="基礎保養"&trend_data$first_name!="底妝"&trend_data$first_name!="彩妝",],
       aes(x=as.factor(time), y=pv_per,group=first_name,color=first_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = c("#6C899D","#166D66","#4AB9B0","#5B20A2","#A275DA","#97521C","#CB8E5D"),
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804各屬性趨勢＿不含3大.png", height=12, width=16)
####################################################################
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
  scale_color_manual(values = colorfixed[1:9],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804彩妝趨勢.png", height=12, width=16)

# 繪圖(不含唇彩)
ggplot(data = trend_data[trend_data$second_name!="唇彩",])+
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name),size=1.3)+
  geom_point(aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  scale_color_manual(values = colorfixed[2:9],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804彩妝趨勢(不含唇彩).png", height=12, width=16)
######################################################################
# highlight growth
trend_data$growth <- ifelse(trend_data$second_name=="頰彩"|trend_data$second_name=="修容"|trend_data$second_name=="多功能彩妝", 1, 0)
trend_highlight <- trend_data[trend_data$growth==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name), data = trend_data[trend_data$second_name!="唇彩",], colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_line(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight)+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
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
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed,
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

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
# 繪圖(進階護膚全部)
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
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

ggsave("201509-201804進階護膚趨勢.png", height=12, width=16)

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
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[1:6],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804美髮趨勢.png", height=12, width=16)

# 繪圖(美髮不含洗髮護髮)
ggplot(data = trend_data[trend_data$second_name!="洗髮"&trend_data$second_name!="護髮",],aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(2,4:6)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804美髮趨勢_不含洗護.png", height=12, width=16)

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
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[1:8],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804身體保養趨勢.png", height=12, width=16)

# 繪圖(身體保養不含美體保養、沐浴清潔)
ggplot(data = trend_data[trend_data$second_name!="美體保養"&trend_data$second_name!="沐浴清潔",],aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(2,3,4,5,7,8)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804身體保養趨勢_不含美沐.png", height=12, width=16)



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
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
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
  
ggsave("201509-201804美容工具趨勢.png", height=12, width=16)

# 繪圖(美容工具不含彩裝工具)
ggplot(data = trend_data[trend_data$second_name!="彩妝工具",],aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(1,3,4,5)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804美容工具趨勢_不含彩妝工具.png", height=12, width=16)

#################################################################
# 底妝拆開來看
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==4,] # 底妝：4
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
# 底妝裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("妝前","遮瑕","粉底","定妝"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(底妝全部)
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
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

ggsave("201509-201804底妝趨勢.png", height=12, width=16)

# 繪圖(底妝不含粉底)
ggplot(data = trend_data[trend_data$second_name!="粉底",],aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(1,2,4)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804底妝趨勢不含粉底.png", height=12, width=16)
######################################################################

# 第三層屬性
# 底妝拆開來看:粉底
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==30,] # 粉底：30
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,third_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$third_name),]
# 底妝裡面的子屬性factor
trend_data$third_name <- factor(trend_data$third_name,levels = c("粉底液","粉餅","粉霜","氣墊粉餅","BB霜","CC霜","其它粉底"))

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
  scale_color_manual(values = colorfixed[1:7],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804粉底趨勢.png", height=12, width=16)

# 第三層屬性
# 彩妝拆開來看:唇彩
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==38,] # 唇彩：38
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,third_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$third_name),]
# 彩妝裡面的子屬性factor
trend_data$third_name <- factor(trend_data$third_name,levels = c("唇膏","唇筆","唇線筆","唇蜜","唇釉","唇露","其它唇彩"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(唇彩全部)
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=third_name,color=third_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[1:7],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804唇彩趨勢.png", height=12, width=16)

# 繪圖(唇彩不含唇膏)
ggplot(data = trend_data[trend_data$third_name!="唇膏",],aes(x=as.factor(time), y=pv_per,group=third_name,color=third_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[2:7],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804唇彩趨勢_不含唇膏.png", height=12, width=16)

#######################################################################

# 看兩者相關性
# 唇彩and唇部保養:38 & 24
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==38|trend_data$second==24,] # 
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
# 唇彩裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("唇彩","唇部保養"))

# 排序
trend_data <- trend_data[with(trend_data, order(second_name)),]
trend_per <- ddply(trend_data, .(second_name), summarize, per=(pv_sum-mean(pv_sum))/sd(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(唇彩,唇部保養)
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804唇彩及唇部保養相關性圖.png", height=12, width=16)
# 計算相關性
trend_data <- dcast(trend_data, time~second_name, value.var = ("pv_per"))
cor(trend_data$唇彩,trend_data$唇部保養) # 0.8387856

#######################################################################

# 看兩者相關性
# 乳霜and面膜保養
trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==15|trend_data$second==20,] # 底妝：4
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
# 底妝裡面的子屬性factor
trend_data$second_name <- factor(trend_data$second_name,levels = c("乳霜","面膜"))

# 排序
trend_data <- trend_data[with(trend_data, order(second_name)),]
trend_per <- ddply(trend_data, .(second_name), summarize, per=(pv_sum-mean(pv_sum))/sd(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# 繪圖(乳霜面膜全部)
ggplot(data = trend_data,aes(x=as.factor(time), y=pv_per,group=second_name,color=second_name))+
  geom_line(size=1.3)+
  geom_point()+
  labs(x="時間",y="比例")+
  scale_color_brewer(palette = "Paired",
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804乳霜及面膜保養相關性圖.png", height=12, width=16)
# 計算相關性
trend_data <- dcast(trend_data, time~second_name, value.var = ("pv_per"))
cor(trend_data$乳霜,trend_data$面膜) # 0.6224824

####################################################################
# 特定有趣的圖形
# 彩妝:頰彩, 修容, 多功能彩妝上升趨勢

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
# highlight growth
trend_data$type <- ifelse(trend_data$second_name=="頰彩"|trend_data$second_name=="修容"|trend_data$second_name=="多功能彩妝", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name), data = trend_data[trend_data$second_name!="唇彩",], colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_line(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(3,4,9)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804彩妝類上升圖.png", height=12, width=16)

####################################################################
# 特定有趣的圖形
# 彩妝:眼影, 眼線, 睫毛下降趨勢

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
# highlight growth
trend_data$type <- ifelse(trend_data$second_name=="眼影"|trend_data$second_name=="眼線"|trend_data$second_name=="睫毛", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name), data = trend_data[trend_data$second_name!="唇彩",], colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_line(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(2,5,7)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804彩妝類下降圖.png", height=12, width=16)

######################################################################
# 特定有趣的圖形
# # 基礎保養裡面的替代關係
# 面膜, 乳霜

trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==1,]
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
trend_data$second_name <- factor(trend_data$second_name,levels = c("精華","化妝水","洗臉","卸妝","乳液","乳霜","凝霜","凝膠","前導","面膜","多功能保養"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# highlight growth
trend_data$type <- ifelse(trend_data$second_name=="乳霜"|trend_data$second_name=="面膜", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name), data = trend_data[trend_data$second_name!="唇彩",], colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_line(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(6,10)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804面膜乳霜互補圖.png", height=12, width=16)


######################################################################
# 特定有趣的圖形
# # 基礎保養裡面的關係脫鉤
# 洗臉、卸妝

trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==1,]
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
trend_data$second_name <- factor(trend_data$second_name,levels = c("精華","化妝水","洗臉","卸妝","乳液","乳霜","凝霜","凝膠","前導","面膜","多功能保養"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# highlight growth
trend_data$type <- ifelse(trend_data$second_name=="洗臉"|trend_data$second_name=="卸妝", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name), data = trend_data[trend_data$second_name!="唇彩",], colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_line(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(3,4)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804洗臉卸妝分家圖.png", height=12, width=16)

######################################################################
# 特定有趣的圖形
#  粉底裡面的繁榮興衰
# 粉底液近來攀升

trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==30,]
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,third_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$third_name),]
trend_data$third_name <- factor(trend_data$third_name,levels = c("粉底液","粉餅","粉霜","氣墊粉餅","BB霜","CC霜","其它粉底"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# highlight growth
trend_data$type <- ifelse(trend_data$third_name=="粉底液", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=third_name), data = trend_data, colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_rect(data=trend_data, mapping=aes(xmin=29, xmax=32, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  geom_rect(data=trend_data, mapping=aes(xmin=5, xmax=8, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  geom_rect(data=trend_data, mapping=aes(xmin=17, xmax=20, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  geom_line(aes(x=as.factor(time), y=pv_per,colour =third_name, group=third_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =third_name, group=third_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[1],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804粉底液2018年初成長圖.png", height=12, width=16)

######################################################################
# 特定有趣的圖形
#  粉底裡面的繁榮興衰
# 氣墊粉餅近來疲弱

trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==30,]
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,third_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$third_name),]
trend_data$third_name <- factor(trend_data$third_name,levels = c("粉底液","粉餅","粉霜","氣墊粉餅","BB霜","CC霜","其它粉底"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# highlight growth
trend_data$type <- ifelse(trend_data$third_name=="氣墊粉餅", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=third_name), data = trend_data, colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_rect(data=trend_data, mapping=aes(xmin=29, xmax=32, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  geom_rect(data=trend_data, mapping=aes(xmin=5, xmax=8, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  geom_rect(data=trend_data, mapping=aes(xmin=17, xmax=20, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  geom_line(aes(x=as.factor(time), y=pv_per,colour =third_name, group=third_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =third_name, group=third_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[4],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804氣墊粉餅2018年初衰退圖.png", height=12, width=16)

######################################################################
# 特定有趣的圖形
# 粉底裡面的繁榮興衰
# BB霜 CC霜越來越收斂

trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$second==30,]
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,third_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$third_name),]
trend_data$third_name <- factor(trend_data$third_name,levels = c("粉底液","粉餅","粉霜","氣墊粉餅","BB霜","CC霜","其它粉底"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# highlight growth
trend_data$type <- ifelse(trend_data$third_name=="BB霜"|trend_data$third_name=="CC霜", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=third_name), data = trend_data, colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  #geom_rect(data=trend_data, mapping=aes(xmin=29, xmax=32, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  #geom_rect(data=trend_data, mapping=aes(xmin=5, xmax=8, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  #geom_rect(data=trend_data, mapping=aes(xmin=17, xmax=20, ymin=0, ymax=max(pv_per)),fill="yellow",alpha=0.005)+
  geom_line(aes(x=as.factor(time), y=pv_per,colour =third_name, group=third_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =third_name, group=third_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[5:6],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")

ggsave("201509-201804_BBCC收斂圖.png", height=12, width=16)

######################################################################
# 特定有趣的圖形
# # 底妝裡面的關係
# 遮瑕 修容

trend_data <- rbind(month_PV, month_PV2015,month_PV2017, month_PV2018)
trend_data <- trend_data[trend_data$first==4,]
trend_data <- trend_data[!is.na(trend_data$product_id),]
trend_data <- ddply(trend_data, .(time,second_name),summarize,pv_sum=sum(PV_month))
trend_data <- trend_data[!is.na(trend_data$second_name),]
trend_data$second_name <- factor(trend_data$second_name,levels = c("妝前","遮瑕","粉底","定妝"))

# 排序
trend_data <- trend_data[with(trend_data, order(time)),]
trend_per <- ddply(trend_data, .(time), summarize, per=pv_sum/sum(pv_sum))
trend_data$pv_per <- trend_per$per
rm(trend_per)
# highlight growth
trend_data$type <- ifelse(trend_data$second_name=="遮瑕"|trend_data$second_name=="妝前", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name), data = trend_data[trend_data$second_name!="粉底",], colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_line(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(1,2)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804妝前遮瑕分家圖.png", height=12, width=16)

####################################################################
# 特定有趣的圖形
# 彩妝:眉彩,睫毛下降趨勢

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
# highlight growth
trend_data$type <- ifelse(trend_data$second_name=="眉彩"|trend_data$second_name=="睫毛", 1, 0)
trend_highlight <- trend_data[trend_data$type==1,]
ggplot() +
  # draw the original data series with grey
  geom_line(aes(x=as.factor(time), y=pv_per,group=second_name), data = trend_data[trend_data$second_name!="唇彩",], colour = alpha("grey", 0.7)) +
  # colourise only the filtered data
  geom_line(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, size=1.5)+
  geom_smooth(aes(x=as.factor(time), y=pv_per,colour =second_name, group=second_name), data = trend_highlight, linetype="dashed", method = "loess", size=0.5, alpha=0.3)+
  labs(x="時間",y="比例")+
  scale_color_manual(values = colorfixed[c(6,7)],
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5),
        legend.title = element_text(size = 20, vjust = 0.5),
        legend.text = element_text(size = 20))+
  geom_vline(xintercept = 5, linetype="dashed")+
  geom_vline(xintercept = 17, linetype="dashed")+
  geom_vline(xintercept = 29, linetype="dashed")
ggsave("201509-201804眉彩睫毛下降圖.png", height=12, width=16)
