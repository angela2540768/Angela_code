# 清單List
# 清單類似於向量，但是與向量不同之處在於每個元素的資料型態可以不同
# 向量：[數字, 數字, 數字, …]
# 清單：[數字, 字串, 矩陣, 函數, …]

x <- list() # 空清單函數
x
x <- list(1,2,3:5,list(6,7,8))
y <- c(1,2,3:5,c(6,7,8)) # 比較向量扁平化
x
y
is.recursive(x) #看是否有遞迴關係
is.recursive(y) #由於向量扁平化特性，所以不會有一層一層的概念

# ch2 List 建立
score <- list(number=104258009, economics=TRUE, type="Final", grade=92)
score
str(score)
# 注意：清單會有$符號，在後面取值時很重要

# name, economics, type, grade為元素的名稱，叫做tag，對於建立list來說非必要
score_2 <- list(104258009, T, "Final", 92)
score_2

# list 裡面也可以是矩陣或函數->非常彈性
q <- list(rep(1:10, 2), LETTERS, matrix(1:25, nrow = 5), sqrt)
q

# ch2 List tag
# 給名字的方式類似於向量
names(score_2)
names(score_2) <- c("number", "economics", "type", "grade")
score_2

# ch2 List index
score$number
score$eco # 支援縮寫

# 也可以用[]
score[1]
score[[1]]
class(score[1])
class(score[[1]])
class(score$number) # 取值時如果只用[]，則只是取出清單的sublist，[[]]才會是進入元素裡把value取出來

# 比較：vector 也有[[]]
p <- 1:6
p[3]
p[[3]] # 在向量中兩個方法取值相同
p[7] # NA
p[[7]] # 但[[]]比較嚴格，可以用來看看有沒有超出邊界

# 用tag名字取值
score["grade"]
score[["grade"]]
class(score["grade"])
class(score[["grade"]])

# 清單裡面的matrix取值
i <- list(1,2,3,matrix(1:25, nrow = 5))
i
i[[4]]
i[[4]][3,2]

# ch2 List的運算
a <- list(1:5)
b <- list(7:11)
a+b # error 清單無法直接做運算，必須把argument裡面的值取出來才能運算
a[[1]]+b[[1]]

# ch2 增減list的元素
weight <- list(name="Ponpon", weight=66)
weight
# 新增一個身高
weight$height <- 173
weight
str(weight)
weight["height"]
# 新增彭的體脂肪率: body_fat 
weight$body_fat <- 23
weight
weight$body_fat <- NULL # 刪掉其中一個argument
weight
weight[["body_fat"]] <- 23 #不一樣的新增元素方法
weight[5] <- "male" # 也可以用數字給值
weight[7] <- "Master" # 第6個就會變成NULL
weight[6] <- NULL # 把第6個設NULL就會往前遞補

# 遞迴索引
l <- list(A=1:10, B=2, C="hello", D=month.abb)
l
l[1:2] # 表示把l裡的第一和第二argument取出，依然是list
l[[1:2]] # 表第1個元素裡的第2個值：兩層
l[[4:3]] # 表第4個元素裡的第3個值：兩層
l[[2:3]] # error 表第2個元素裡的第3個值：兩層
l[[1:3]] # error 表第1個元素裡的第2個元素的第3個值：三層

l <- list(A=1:10, B=2, C="hello", D=month.abb, E=matrix(1:25, nrow = 5))
l
l[[5]][25]
l[[c(5,25)]]
# 用遞迴關係找出l裡第5個元素裡的第25個值


# ch2 List與向量之間的轉換
a <- c(1:10)
b <- as.list(a)
b
str(b)

c <- as.numeric(list(1,2,5,3))
c

d <- as.numeric(list(1,"A",2)) #當list出現非numeric，會發生什麼情形?
d
# 非數值型態會變成NA

d <- as.character(list(1,"A",2))
d

# 假如list裡每個元素長度皆不為1時，上述方法不可用
f <- list(A=c(1,3), B=2, C=4, D=1:10)
as.numeric(f) # error
unlist(f)

# lapply(), sapply()
# lapply()用法相當於矩陣中的apply()
# 回傳的結果一定是list，且比apply有效率
# sapply()把list結果簡化成vector（前提是每個結果的長度要相同）
?lapply
lapply(list(1:3, 25:29), median)
class(lapply(list(1:3, 25:29), median))
sapply(list(1:3, 25:29), median)
class(sapply(list(1:3, 25:29), median))
# example
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
x
lapply(x, quantile)#, probs = 1:3/4)
sapply(x, quantile)
class(sapply(x, quantile))

d <- list(a=rnorm(50), b=rnorm(30), c=rnorm(50))
lapply(d, function(x) which(x>=0))
sapply(d, function(x) sum(x>=0))
sapply(d, function(x) which(x>=0)) #長度不同 不能簡化

# list lapply結合
myfun_list <- list(mean, median, sum, sd)
# 要取出其中一個使用[[]]
myfun_list[[1]]
data <- rnorm(100)
myfun_list[[1]](data) # 相當於mean(data)
MY_FUN <- function(f, data) f(data)
lapply(myfun_list, FUN = MY_FUN, data=sample(1:100, 30, replace = T))
# 函數解釋：myfun_list為一個list，裡面所含的四個argument均為函數
# 而我們的目的是要能夠對一筆資料同時做平均、中位數、總和和標準差
# MY_FUN函數則為一個函數，argument分別是f和data，從函數內容可以看出
# f為一個函數，並且對data作用
# 因此整個想法應該會變成mean(data), median(data), sum(data), sd(data)
# 用lapply()即可以讓R按照myfun_list裡面的函數，依序對data執行
# 亦即：第一次：f為mean
# 第二次：f為median
# 第三次：f為sum
# 第四次：f為sd
# 最後結果輸出為list格式

### end ###



