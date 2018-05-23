library(reshape2)
# 合併PC版和M版的PV
# 檢查有沒有na or 重複
PV2015main[duplicated(PV2015main),]
PV2015main[is.na(PV2015main$product_id),]
PV2018main <- PV2018main[!is.na(PV2018main$product_id),]
PV2018main <- PV2018main[!duplicated(PV2018main),]
PV2015mobile[duplicated(PV2015mobile),]
PV2015mobile[is.na(PV2015mobile$product_id),]
PV2015mobile <- PV2015mobile[!is.na(PV2015mobile$product_id),]
PV2015mobile <- PV2015mobile[!duplicated(PV2015mobile),]
PV_2015_Main[duplicated(PV_2015_Main),]
PV_2015_Main[is.na(PV_2015_Main$product_id),]
PV_2015_Mobile[duplicated(PV_2015_Mobile),]
PV_2015_Mobile[is.na(PV_2015_Mobile$product_id),]

# 合併
PV2015_all <- rbind(PV2015main, PV2015mobile)

# 計算各product_id的每月PV
library(plyr)
month_PV2015 <- ddply(PV2015_all, .(product_id, time), summarize, PV_month=sum(pv))
write.csv(month_PV, "month_PV2015.csv", row.names = F)
# 計算各product_id的每年PV
year_PV2018 <- ddply(month_PV2018, .(product_id), summarize, PV_year=sum(PV_month))
write.csv(year_PV, "year_PV2016.csv", row.names = F)


# 目前資料缺少欄位:
# 產品名稱
# 產品品牌
# 產品屬性

# 屬性合併
month_PV <- merge(month_PV, product_all_cate, by="product_id", all.x = T)
month_PV2015 <- merge(month_PV2015, product_all_cate, by="product_id", all.x = T)
month_PV2017 <- merge(month_PV2017, product_all_cate, by="product_id", all.x = T)
month_PV2018 <- merge(month_PV2018, product_all_cate, by="product_id", all.x = T)

# 產品品牌和名稱先合併
names(p_brand)[1] <- "brand_id"
p_information <- merge(p_name, p_brand, by="brand_id", all.x =T) 
rm(p_brand, p_name)
names(p_information) <- c("brand_id","product_id", "p_name_en", "p_name_tw","channel","b_name_en","b_name_tw")

# PV與產品資訊合併
month_PV <- merge(month_PV,p_information,by="product_id", all.x = T)
month_PV2015 <- merge(month_PV2015,p_information,by="product_id", all.x = T)
month_PV2017 <- merge(month_PV2017,p_information,by="product_id", all.x = T)
month_PV2018 <- merge(month_PV2018,p_information,by="product_id", all.x = T)

# 屬性填寫
month_PV2018[is.na(month_PV2018$third),"third"] <- month_PV2018[is.na(month_PV2018$third),"second"]
month_PV2018[month_PV2018$third_name=="" & !is.na(month_PV2018$third_name),"third_name"] <- month_PV2018[month_PV2018$third_name==""& !is.na(month_PV2018$third_name),"second_name"]
month_PV2017[is.na(month_PV2017$third),"third"] <- month_PV2017[is.na(month_PV2017$third),"second"]
month_PV2017[month_PV2017$third_name=="" & !is.na(month_PV2017$third_name),"third_name"] <- month_PV2017[month_PV2017$third_name==""& !is.na(month_PV2017$third_name),"second_name"]
month_PV[is.na(month_PV$third),"third"] <- month_PV[is.na(month_PV$third),"second"]
month_PV[month_PV$third_name=="" & !is.na(month_PV$third_name),"third_name"] <- month_PV[month_PV$third_name==""& !is.na(month_PV$third_name),"second_name"]
month_PV2015[is.na(month_PV2015$third),"third"] <- month_PV2015[is.na(month_PV2015$third),"second"]
month_PV2015[month_PV2015$third_name=="" & !is.na(month_PV2015$third_name),"third_name"] <- month_PV2015[month_PV2015$third_name==""& !is.na(month_PV2015$third_name),"second_name"]

# 檔案儲存
write.csv(month_PV,"month_PV2016.csv", na="", row.names = F)
write.csv(month_PV2017,"month_PV2017.csv", na="", row.names = F)
write.csv(month_PV2018,"month_PV2018.csv", na="", row.names = F)
write.csv(month_PV2015,"month_PV2015.csv", na="", row.names = F)

# 資料清洗完畢 #
######################################################################
library(ggplot2)
# 分析開始 #
 # 201601~201804每月各屬性趨勢圖
trend_data <- rbind(month_PV,month_PV2017)
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
  geom_point(aes(x=as.factor(time), y=pv_per,group=first_name,color=first_name))+
  labs(x="時間",y="比例")+
  scale_color_manual(values = c("#67001f","#b2182b","#d6604d","#f4a582","#053061","#2166ac","#4393c3","#92c5de","#d1e5f0","#fddbc7"),
                     guide = guide_legend(title = "屬性"))+
  theme(axis.text.x = element_text(size = 15, angle = 75, vjust = 0.5))
ggsave("201509-201804各屬性趨勢.png", height=12, width=16)

 # 試試看季節性分解
#################################################################################
 # 基礎保養
trend_data <- trend_data[with(trend_data,order(time)),]
ts_1 <- ts(trend_data[trend_data$first_name=="基礎保養","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)
 # 底妝
ts_1 <- ts(trend_data[trend_data$first_name=="底妝","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)
 # 彩妝
ts_1 <- ts(trend_data[trend_data$first_name=="彩妝","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)
 # 進階護膚
ts_1 <- ts(trend_data[trend_data$first_name=="進階護膚","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)

 # 美髮
ts_1 <- ts(trend_data[trend_data$first_name=="美髮","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)

 # 防曬
ts_1 <- ts(trend_data[trend_data$first_name=="防曬","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)

 # 身體保養
ts_1 <- ts(trend_data[trend_data$first_name=="身體保養","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)

 # 香水香氛
ts_1 <- ts(trend_data[trend_data$first_name=="香水香氛","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)

 # 美容工具
ts_1 <- ts(trend_data[trend_data$first_name=="美容工具","pv_per"], frequency = 12, start = c(2016,1))
ts_1
plot(ts_1)
ts_de <- decompose(ts_1)
plot(ts_de$figure, type = "b", xaxt="n", xlab = "")
monthNames <- months(ISOdate(2016,1:12,1))
axis(1,at=1:12, labels = monthNames, las=2)
plot(ts_de)

rm(ts_1,ts_de)

#################################################################################
 # 整體臉部保養品牌表現
 # 基礎保養、進階護理、防曬
month_PV2018 <- month_PV2018[!is.na(month_PV2018$product_id),]
skincare <- month_PV2018[month_PV2018$first==1|month_PV2018$first==2|month_PV2018$first==3,]
skincare <- skincare[!is.na(skincare$product_id),]
skin_brand <- ddply(skincare, .(brand_id), summarize, pv_sum=sum(PV_month))
 # 臉部保養10大品牌
skin_brand <- skin_brand[with(skin_brand, order(pv_sum, decreasing = T)),]
skin_brand$per <- round(skin_brand$pv_sum/sum(skin_brand$pv_sum),4) # 計算比例
write.csv(skin_brand,"skin_brand.csv", na="", row.names = F) #整體臉部保養品牌排名
 # 臉部保養每月成長退步品牌
###########################################################################
skin_brand <- ddply(skincare, .(brand_id, time), summarize, pv_sum=sum(PV_month))
skin_brand <- skin_brand[with(skin_brand,order(time)),]
 # 計算每月品牌所佔比例
skin_per <- ddply(skin_brand, .(time), summarize, per=round(pv_sum/sum(pv_sum),4))
skin_brand$per <- skin_per$per
rm(skin_per)
 # 變成寬資料來比較不同月份的比例
skin_dcast <- dcast(skin_brand, formula = brand_id ~ time, value.var = c("per"))
 # 計算每個月的成長差異
skin_dcast$diff0403 <- skin_dcast$`201804`-skin_dcast$`201803`
 # 連續三個月正成長
skin_dcast$growth <- ifelse(skin_dcast$diff0201>0&skin_dcast$diff0302>0&skin_dcast$diff0403>0,1,0)
 # 連續三個月負成長
skin_dcast$down <- ifelse(skin_dcast$diff0201<0&skin_dcast$diff0302<0&skin_dcast$diff0403<0,1,0)
write.csv(skin_dcast,"skin_dcast.csv", na="", row.names = F) #臉部保養_品牌成長變化
################################################################################
 # 臉部保養屬性分布表現
 # 有些產品只到第二層屬性，必須用第二層屬性填第三層
skincare[is.na(skincare$third),"third"] <- skincare[is.na(skincare$third),"second"]
skincare[skincare$third_name=="","third_name"] <- skincare[skincare$third_name=="","second_name"]
 # 臉部保養屬性總排名
skin_cate_all <- ddply(skincare, .(third_name), summarize, pv_sum=sum(PV_month))
skin_cate_all$per <- round(skin_cate_all$pv_sum/sum(skin_cate_all$pv_sum),4) #比例化
skin_cate_all <- skin_cate_all[with(skin_cate_all, order(pv_sum, decreasing = T)),]
write.csv(skin_cate_all,"skin_cate_all.csv", na="", row.names = F) # 臉部保養整體屬性排名
 # 計算每個月屬性成長差異
#################################################################################
skin_cate_each <- ddply(skincare, .(third_name, time), summarize, pv_sum=sum(PV_month))
skin_cate_each <- skin_cate_each[with(skin_cate_each,order(time)),]
skin_cate_per <- ddply(skin_cate_each, .(time), summarize, per=round(pv_sum/sum(pv_sum),4))
skin_cate_each$per <- skin_cate_per$per
rm(skin_cate_per)
 # 變成寬資料
skin_cast_dcast <- dcast(skin_cate_each, formula = third_name ~ time, value.var = c("per"))
 # 每個月成長差異_屬性
skin_cast_dcast$diff0201 <- skin_cast_dcast$`201802`-skin_cast_dcast$`201801`
 # 連續三個月正成長
skin_cast_dcast$growth <- ifelse(skin_cast_dcast$diff0201>0&skin_cast_dcast$diff0302>0&skin_cast_dcast$diff0403>0,1,0)
 # 連續三個月負成長
skin_cast_dcast$down <- ifelse(skin_cast_dcast$diff0201<0&skin_cast_dcast$diff0302<0&skin_cast_dcast$diff0403<0,1,0)
write.csv(skin_cast_dcast,"skin_cast_dcast.csv",na="",row.names = F) # 臉部保養屬性成長變化
################################################################################
 # 臉部保養五大屬性: 精華液>化妝水>乳霜>臉部防曬>乳液
 # 各別屬性分析
 # 精華液
skincare78 <- skincare[skincare$third==78,]
skincare78[is.na(skincare78$product_id),] # check for NA: no NAs
 # 計算產品排行
rank78 <- ddply(skincare78, .(product_id), summarize, pv_sum=sum(PV_month))
rank78$per <- round(rank78$pv_sum/sum(rank78$pv_sum),4) #比例化
 # 合併產品資訊
rank78 <- merge(rank78, p_information, by="product_id", all.x = T)
 # 重新排列
rank78 <- rank78[with(rank78, order(per, decreasing = T)),]
head(rank78[,"product_id"], 10)
write.csv(rank78, "rank78.csv",na="",row.names = F) # 臉部保養_精華液產品排名
###############################################################################
 # 計算每個月屬性成長差異
rank78 <- ddply(skincare78, .(product_id, time), summarize, pv_sum=sum(PV_month))
rank78 <- rank78[with(rank78,order(time)),]
rank78_per <- ddply(rank78, .(time), summarize, per=round(pv_sum/sum(pv_sum),4))
rank78$per <- rank78_per$per
rm(rank78_per)
 # 變成寬資料
rank78 <- dcast(rank78, formula = product_id ~ time, value.var = c("per"))
 # 每個月成長差異_屬性
rank78$diff0201 <- rank78$`201802`-rank78$`201801`
rank78$diff0302 <- rank78$`201803`-rank78$`201802`
rank78$diff0403 <- rank78$`201804`-rank78$`201803`
 # 連續三個月正成長
rank78$growth <- ifelse(rank78$diff0201>0&rank78$diff0302>0&rank78$diff0403>0,1,0)
 # 連續三個月負成長
rank78$down <- ifelse(rank78$diff0201<0&rank78$diff0302<0&rank78$diff0403<0,1,0)
 # 前五名:82615 53239  4821 79207 76798 32567 77085 51160 83585 46312
first78 <- data.frame(product_id=c(82615 ,53239  ,4821 ,79207 ,76798 , 77085, 51160, 83585 ,46312,82283), rank=1:10)
rank78 <- merge(rank78, first78, by="product_id", all.x = T) # 看前五名變化
write.csv(rank78, "rank78.csv",na="",row.names = F) # 臉部保養_精華液產品變化
################################################################################