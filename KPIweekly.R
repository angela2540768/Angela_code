KPI_weekly <- function(KPI){
  names(KPI) <- c("status", "month","document_id", "start", "stop", "sales", "agency", "brand", "BID", "case", "money", "money_media", "money_making")
  KPI$month_m <- as.integer(substr(KPI$month, start = 4, stop = 5))
  KPI$start <- as.Date(KPI$start, format="%Y-%m-%d")
  KPI$stop <- as.Date(KPI$stop, format="%Y-%m-%d")
  KPI$total_day <- as.numeric((KPI$stop - KPI$start) + 1)
  KPI$month_1 <- as.numeric(ifelse(KPI$stop<=as.Date("2018-01-31", format="%Y-%m-%d"), (KPI$stop-KPI$start)+1,
                                   ifelse(KPI$start<=as.Date("2018-01-31", format="%Y-%m-%d") & KPI$stop>=as.Date("2018-01-01", format="%Y-%m-%d"),(as.Date("2018-01-31", format="%Y-%m-%d")-KPI$start)+1, 0))) 
  KPI$month_2 <- as.numeric(ifelse(KPI$start<=as.Date("2018-02-28", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-02-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-02-28", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1), 
                                          as.Date("2018-02-28", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1)),0))
  KPI$month_3 <- as.numeric(ifelse(KPI$start<=as.Date("2018-03-31", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-03-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-03-31", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1+KPI$month_2), 
                                          as.Date("2018-03-31", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2)),0))
  KPI$month_4 <- as.numeric(ifelse(KPI$start<=as.Date("2018-04-30", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-04-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-04-30", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3), 
                                          as.Date("2018-04-30", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3)),0))
  KPI$month_5 <- as.numeric(ifelse(KPI$start<=as.Date("2018-05-31", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-05-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-05-31", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4), 
                                          as.Date("2018-05-31", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4)),0))
  KPI$month_6 <- as.numeric(ifelse(KPI$start<=as.Date("2018-06-30", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-06-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-06-30", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5), 
                                          as.Date("2018-06-30", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5)),0))
  KPI$month_7 <- as.numeric(ifelse(KPI$start<=as.Date("2018-07-31", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-07-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-07-31", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6), 
                                          as.Date("2018-07-31", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6)),0))
  KPI$month_8 <- as.numeric(ifelse(KPI$start<=as.Date("2018-08-31", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-08-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-08-31", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7), 
                                          as.Date("2018-08-31", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7)),0))
  KPI$month_9 <- as.numeric(ifelse(KPI$start<=as.Date("2018-09-30", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-09-01", format="%Y-%m-%d"),
                                   ifelse(KPI$stop<=as.Date("2018-09-30", format="%Y-%m-%d"), 
                                          KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8), 
                                          as.Date("2018-09-30", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8)),0))
  KPI$month_10 <- as.numeric(ifelse(KPI$start<=as.Date("2018-10-31", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-10-01", format="%Y-%m-%d"),
                                    ifelse(KPI$stop<=as.Date("2018-10-31", format="%Y-%m-%d"), 
                                           KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8+KPI$month_9), 
                                           as.Date("2018-10-31", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8+KPI$month_9)),0))
  KPI$month_11 <- as.numeric(ifelse(KPI$start<=as.Date("2018-11-30", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-11-01", format="%Y-%m-%d"),
                                    ifelse(KPI$stop<=as.Date("2018-11-30", format="%Y-%m-%d"), 
                                           KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8+KPI$month_9+KPI$month_10), 
                                           as.Date("2018-11-30", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8+KPI$month_9+KPI$month_10)),0))
  KPI$month_12 <- as.numeric(ifelse(KPI$start<=as.Date("2018-12-31", format="%Y-%m-%d")&KPI$stop>=as.Date("2018-12-01", format="%Y-%m-%d"),
                                    ifelse(KPI$stop<=as.Date("2018-12-31", format="%Y-%m-%d"), 
                                           KPI$total_day-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8+KPI$month_9+KPI$month_10+KPI$month_11), 
                                           as.Date("2018-12-31", format="%Y-%m-%d")-KPI$start+1-(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8+KPI$month_9+KPI$month_10+KPI$month_11)),0))
  KPI$year <- as.integer(substr(KPI$month, start = 1, stop = 3))
  #return(KPI)
  KPI <- KPI[KPI$year==107,]
  
  #KPI <- ifelse(KPI$month_1+KPI$month_2+KPI$month_3+KPI$month_4+KPI$month_5+KPI$month_6+KPI$month_7+KPI$month_8+KPI$month_9+KPI$month_10+KPI$month_11+KPI$month_12==KPI$total_day, KPI, stop("date calculation is wrong!"))
  each_month <- KPI[,16:27]
  
  for(j in 1:nrow(KPI)){
    for(i in 1:12){
      each_month[j,i] <- as.integer(round(KPI[j,"money_media"]/KPI[j,"total_day"]*each_month[j,i]))
      each_month[j,i] <- ifelse(KPI[j,"month_m"] == i, each_month[j,i]+KPI[j,"money_making"], each_month[j,i])
    }
  }
  
  each_month$status <- KPI$status
  
  each_month_money <- matrix(data = 0, nrow = 6, ncol = 12)
  each_month_money <- as.data.frame(each_month_money)
  
  for(i in 1:12){
    each_month_money[1,i] <- ifelse(any(each_month$status==100),sum(each_month[each_month$status==100,i]),0)
    each_month_money[2,i] <- ifelse(any(each_month$status==95),sum(each_month[each_month$status==95,i]),0)
    each_month_money[3,i] <- ifelse(any(each_month$status==75),sum(each_month[each_month$status==75,i]),0)
    each_month_money[4,i] <- ifelse(any(each_month$status==65),sum(each_month[each_month$status==65,i]),0)
    each_month_money[5,i] <- ifelse(any(each_month$status==50),sum(each_month[each_month$status==50,i]),0)
  }
  each_month_money[6,] <- colSums(each_month_money)
  rownames(each_month_money) <- c("Deal", "A_rank", "B_rank", "C_rank", "D_rank", "Total")
  colnames(each_month_money) <- 1:12
  return(each_month_money)
  
}

##########################################################################

## import multiple files ##

getwd()
setwd("D:/Job/R/KPI_weekly/KPI_weekly")
file.list <- list.files(path = "D:/Job/R/KPI_weekly/KPI_weekly", pattern = "*.xls")
library(readxl)
df.list <- lapply(file.list, read_excel, col_types = c("numeric", 
                                                       "numeric", "text", "date", "date", "text", 
                                                       "text", "text", "numeric", "text", "numeric", 
                                                       "numeric", "numeric"))
all_result <- lapply(df.list, KPI_weekly)
APS <- all_result[[1]]
ATS <- all_result[[2]]
BFC <- all_result[[3]]
BSA <- all_result[[4]]
CI <- all_result[[5]]
VIP <- all_result[[6]]