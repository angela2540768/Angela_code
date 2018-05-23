library(plyr)
## 程式化 ##
# 品牌表現
brand_cate <- function(condition,dataname){
  month_PV2018 <- month_PV2018[!is.na(month_PV2018$product_id),]
  cate <- month_PV2018[condition,]
  cate <- cate[!is.na(cate$product_id),]
  cate <- ddply(cate, .(brand_id), summarize, pv_sum=sum(PV_month))
  # 臉部保養10大品牌
  cate <- cate[with(cate, order(pv_sum, decreasing = T)),]
  cate$per <- round(cate$pv_sum/sum(cate$pv_sum, na.rm = T),4) # 計算比例
  brand <- p_information[,c(1,5,6,7)]
  brand <- brand[!duplicated(brand),]
  cate <- merge(cate,brand,by="brand_id", all.x = T)
  write.csv(cate,dataname, na="", row.names = F) #整體臉部保養品牌排名
}
# example
condition <- month_PV2018$first==4|month_PV2018$first==5
dataname <- "cosmetic_brand.csv"
brand_cate(condition,dataname) # 成功!!

# 屬性表現
cate_rank <- function(condition,dataname){
  cate <- month_PV2018[condition,]
  cate <- ddply(cate, .(third_name), summarize, pv_sum=sum(PV_month))
  cate$per <- round(cate$pv_sum/sum(cate$pv_sum, na.rm = T),4) #比例化
  cate <- cate[with(cate, order(per, decreasing = T)),]
  write.csv(cate,dataname, na="", row.names = F) # 臉部保養整體屬性排名
}
# example
condition <- month_PV2018$first==4|month_PV2018$first==5
dataname <- "cosmetic_caterank.csv"
cate_rank(condition,dataname) # 成功!!

# 各別屬性分析
cate_product <- function(condition,dataname){
  cate <- month_PV2018[condition,]
  cate <- cate[!is.na(cate$product_id),] # check for NA: no NAs
  # 計算產品排行
  cate <- ddply(cate, .(product_id), summarize, pv_sum=sum(PV_month))
  cate$per <- round(cate$pv_sum/sum(cate$pv_sum, na.rm = T),4) #比例化
  # 合併產品資訊
  cate <- merge(cate, p_information, by="product_id", all.x = T)
  # 重新排列
  cate <- cate[with(cate, order(per, decreasing = T)),]
  write.csv(cate,dataname, na="", row.names = F)
}
# example
condition <- month_PV2018$third==138
dataname <- "rank138.csv"
cate_product(condition,dataname) # 成功!!
