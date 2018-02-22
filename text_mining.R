### text mining ###

## 先安裝好全部需要的套件 ## 
install.packages("jiebaR") ##此為這次文字探勘最主要的套件
install.packages("plyr")
install.packages("stringr")
install.packages("reshape2")
install.packages("readxl")
install.packages("data.table")

## 每次打開Ｒ時，需要重新library相關套件 ##
library(jiebaR)
library(jiebaRD)
library(plyr)
library(stringr)
library(reshape2)
library(readxl)
library(data.table)

## 由於我們是中文文字探勘，因此需要把Ｒ的預設語系改為中文 ##
### 更改語系 change to chinese system ### for MAC iOS
system("defaults write org.R-project.R force.LANG zh_TW.UTF-8")

### 更改語系 change to chinese system ### for windows
Sys.getlocale(category = "LC_ALL")
Sys.setlocale(category = "LC_ALL", locale = "cht")


## 以下是jiebaR套件的斷詞測試，測試套件本身斷詞的效果 ##
### example ###
test <- worker() # 先設置一個worker
test #可以看到套件默認的一些設置和各個詞典的預設值，其中，重要的為stop_word和user
## 以下為三種斷詞方法，三種方法的結果是相同的：
test["我們今天要開會"]
test <= "我們今天要開會"
segment("我們今天要開會",test)
### 測試結束 ###
###########################################################


### 以下可以看到辭典以及其路徑 ###
show_dictpath()
dir(show_dictpath())
### 看辭典內的詞 ###
scan(file="C:/Users/Angela/Documents/R/win-library/3.4/jiebaRD/dict/user.dict.utf8", what=character(),nlines=50,sep='\n',encoding='utf-8',fileEncoding='utf-8')
#############################################################



##### 以下開始為董監事學經歷資料的整理 #####


### import the data ###
pondata <- read_excel("C:/patent/TEJ/TSE_origin.csv")
pondata <- fread("C:/patent/TEJ/TSE_origin.csv")
head(pondata)
print(object.size(pondata), units = "Mb")
names(pondata)
director <- pondata[,c(1,2,3,133,134,136,137,139,140,146)] 
rm(pondata)
gc()
write.csv(director,"C:/patent/TEJ/director_10vars.csv", na="")
pondata <- director
b_melt_c <- fread("C:/patent/TEJ/b_melt_c.csv")
## 此資料一共有637,669筆，時間由2006年到2017年
## 以人為單位，記錄其學經歷資料 

## 首先，檢查重複值，並且刪除重複值
board <- pondata[!duplicated(pondata),] 
## 果然有重複值，剩下630,777筆

## 我們主要的目標是ge欄，觀察到同一格子中會有兩個以上學經歷，因此要先使用str_split剖開
## 再跟原資料檔合併
board2 <- as.data.frame(str_split(board$ge, pattern = "、", simplify = T))
board2$n <- 1:630777
board$n <- 1:630777
board <- merge(board, board2, by="n")
rm(board2)
board <- board[,-1]

### 只留下要分析的欄位
names(board)
b_analysis <- board[,c(-1,-2,-3,-5,-7,-8,-9,-10)]

## 重新命名欄位 ##
names(b_analysis)
names(b_analysis)[1] <- "date"
names(b_analysis)[2] <- "name"

## 設置新的欄位 : year
b_analysis$year <- substr(b_analysis$date, start = 1, stop = 4)
b_analysis <- b_analysis[,-1]



## 用 melt 將多欄轉成一欄 ##
b_melt <- melt(b_analysis, id.vars=c('name', 'year'),var='experience')
b_melt <- b_melt[which(b_melt$value!=""),] ## 把空白刪除
b_melt <- b_melt[with(b_melt, order(year,name)),] ## 重新排序
b_melt <- b_melt[,-3]
names(b_melt)[3] <- "exp"
b_melt <- b_melt[!duplicated(b_melt),]
write.csv(b_melt, "b_melt.csv", na="", fileEncoding = "big5") ## 先存檔，要注意編碼格式

## 用不一樣的符號拆: "/", "#" ##
board2 <- as.data.frame(str_split(b_melt$exp, pattern = "/", simplify = T))
board2$n <- 1:683405
b_melt$n <- 1:683405
b_melt_2 <- merge(b_melt, board2, by="n")
b_melt_2 <- b_melt_2[,c(-1,-4)]
b_melt_2 <- melt(b_melt_2, id.vars=c('name', 'year'),var='experience')
b_melt_2 <- b_melt_2[which(b_melt_2$value!=""),]
b_melt_2 <- b_melt_2[with(b_melt_2, order(year,name)),] ## 重新排序
b_melt_2 <- b_melt_2[,-3]
names(b_melt_2)[3] <- "exp"
b_melt_2 <- b_melt_2[!duplicated(b_melt_2),]
b_melt <- b_melt_2
rm(b_melt_2,b_english)
write.csv(b_melt_c, "C:/patent/TEJ/b_melt_c.csv", na="") ## 先存檔，要注意編碼格式




### 看看各個名字每年出現次數 ###
namefreq <- ddply(b_melt, .(name, year), summarize, Freq=table(name))

## 把有英文字的部分挑出來 ##
b_english <- b_melt[grep("[A-Za-z]",b_melt$exp),]
## 有68,761筆資料有




###### 資料整理結束 #########
#############################################################



####### 以下開始為斷詞 ########

## 建立文字探勘環境 ##
b_parse <- worker() ## 目前尚未加入user自己的辭典
user_parse <- worker(user="C:/patent/TEJ/user_dict.utf8") ## add user dict.

## 以下為斷詞程序 ##
a <- list()
for(i in 1:616117){
  a[[i]] <- user_parse[b_melt_c[[i,"exp"]]]
}
## 斷詞完 a為一個list格式
## 可以讓斷詞結果變成data.frame
b_mining <- ldply(a, rbind)

## 亦可以計算每個詞出現的頻率多寡
a_2 <- unlist(a)
freq_list <- freq(a_2)

## 加入user的辭典 ##
c <- list()
for(i in 1:616117){
  c[[i]] <- user_parse[b_melt_c[[i,"exp"]]]
}
c_2 <- unlist(c)
freq_list_2 <- freq(c_2)


###############################################################
write.csv(b_melt_c, "C:/patent/TEJ/b_melt_c_am.csv", na="") ## 拆字後存檔

### 開始做後續處理
#  由政府部門判別開始
#  概念: 如果有符合gov_name裡的字詞，dummy: "gov" <- 1
gov_name <- read.csv("D:/Angela/TEJ/gov_name.csv",stringsAsFactors = F)
b_melt_2 <- fread("D:/Angela/TEJ/b_melt_c_am.csv", header = T, stringsAsFactors = F)

## 不一樣的方法需要的時間不同

# 方法1:
# 雙迴圈直接判斷
for(i in 1:616117){
  for(j in 1:209){
    if(b_melt_2[i,4:31]==gov_name[j]){
      b_melt_2[i,"gov"] <- 1
      break
    } else {
      b_melt_2[i,"gov"] <- 0
    }
  }
}
# 效率最慢: 估計3天

# 方法2:
# 方法1裡面空格也會下去跑，浪費時間
# 可以把字拆開分組，寫判斷式分群
library(reshape2)
b_melt_2$index <- 1:616117
test2 <- b_melt_2[,4:31]
test3 <- melt(test2, id.vars = "index")
test3 <- test3[which(test3$value!=""),]
write.csv(test3,"D:/Angela/text_mining/director_melt.csv",na="")
test4 <- split(test3, 1:200)

table(nchar(gov_name$gov_name))
gov_name1 <- gov_name[which(nchar(gov_name$gov_name)<=3),]
gov_name2 <- gov_name[which(nchar(gov_name$gov_name)>=4 & nchar(gov_name$gov_name)<=6),]
gov_name3 <- gov_name[which(nchar(gov_name$gov_name)>=7),]
write.csv(gov_name1, "D:/Angela/text_mining/gov_name1.csv",na="")
write.csv(gov_name2, "D:/Angela/text_mining/gov_name2.csv",na="")
write.csv(gov_name3, "D:/Angela/text_mining/gov_name3.csv",na="")

matching <- function(data){
  dataframe <- as.data.frame(data)
  for(i in 1:nrow(dataframe)){
    if(nchar(dataframe[i,2])<=3){
      for(j in seq_along(gov_name1)){
        if(dataframe[i,2]==gov_name1[j]){
          dataframe[i,"gov"] <- 1
          break
        } else {
          dataframe[i,"gov"] <- 0
        }
      }
    } else if(nchar(dataframe[i, 2]>=4 & nchar(dataframe[i,2])<=6)){
      for(k in seq_along(gov_name2)){
        if(dataframe[i,2]==gov_name2[k]){
          dataframe[i,"gov"] <- 1
          break
        } else {
          dataframe[i,"gov"] <- 0
        }
      }
    } else if(nchar(dataframe[i,2])>=7){
      for(p in seq_along(gov_name3)){
        if(dataframe[i,2]==gov_name3[p]){
          dataframe[i,"gov"] <- 1
          break
        } else {
          dataframe[i,"gov"] <- 0
        }
      }
    }
  }
  path <- "D:/Angela/TEJ/gov_name/"
  csv <- ".csv"
  write.csv(dataframe, paste0(path,names(data),csv))
}


for(e in 1:200){
  matching(data = test4[e])
}

## 效率中等: 6小時

# 方法3：
# 用melt()融化後merge()
gov_name$gov <- 1
names(test3)[3] <- "gov_name"
test5 <- merge(test3, gov_name, by="gov_name", all = F)
test5 <- test5[,-1]
test5 <- test5[!duplicated(test5),]
b_melt_2 <- merge(b_melt_2, test5, by="index", all.x = T)
write.csv(b_melt_2, "D:/Angela/TEJ/b_melt_2.csv", na="")
b_melt_2[is.na(b_melt_2$gov),"gov"] <- 0
## 效率最快: 幾秒鐘
