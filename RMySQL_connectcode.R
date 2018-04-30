## 把My_SQL和R做串連

# 安裝package
install.packages("RMySQL")
library(RMySQL)

# 資料庫串接：記得改dbname
mysqlconnection <- dbConnect(MySQL(), user = "root", password = "", dbname = 'testdb',host = 'localhost')
dbListTables(mysqlconnection) # 看有哪些資料集


# 由R寫入資料入資料庫
dbWriteTable(
  conn = mysqlconnection,
  name = "mydata",
  value = iris,
  row.names = FALSE
)
dbListTables(mysqlconnection)

# 由資料庫叫表進入R
mydata1 <- dbReadTable(
  conn = mysqlconnection,
  name = "mydata",
)

# 由R刪除資料庫的表
dbRemoveTable(mysqlconnection,"mydata")

# 斷開連結
dbDisconnect(mysqlconnection)
