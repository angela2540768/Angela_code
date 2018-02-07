## 271232抽卡配對機

draw271232 <- function(name=c("Bibi", "JB", "Zoe", "Angela"), number=4){
  m <- TRUE
  while(m==TRUE){
    result271232 <- matrix(nrow = number, ncol = 2)
    name <- name
    result271232[,1] <- sample(name, size = number, replace = F)
    result271232[,2] <- sample(name, size = number, replace = F)
    m <- any(result271232[,1]==result271232[,2])
    if(m==FALSE) {
      break
      }
  }
  colnames(result271232) <- c("drawer", "drawed")
  return(result271232)
}

draw271232()

