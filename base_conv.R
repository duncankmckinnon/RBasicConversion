
tens_place <- function(num){
  n <- 1
  m <- 0
  while(n < num){
    n <- n * 10
    m <- m + 1
  }
  return(c(n / 10, m - 1))
}

fromX_toY <- function(repr, X, Y, mapping = c(0:9, 'A', 'B', 'C', 'D', 'E', 'F')){
  n1 <- repr
  if(X != 10){
    n1 <- to_base10(repr, X)
  }
  n2 <- from_base10(n1, Y, mapping)
  return(n2)
}

to_base10 <- function(repr, base){
  vals <- tens_place(repr)
  base_10 <- vals[1]
  decim <- vals[2]
  num <- 0
  while(base_10 > 1){
    val <- floor(repr / base_10)
    num <- num + val * base ^ decim
    repr <- repr - val * base_10
    base_10 <- base_10 / 10
    decim <- decim - 1
  }
  num <- num + repr
  return(num)
}

from_base10 <- function(repr, base, mapping = c(0:9,'A','B','C','D','E','F')){
  val <- c()
  while(repr >= base){
    nextn <- mapping[repr %% base + 1]
    val <- c(nextn, val)
    repr <- floor(repr / base)
  }
  val <- c(mapping[repr + 1], val)
  str_rep <- paste(val, collapse='')
  return(ifelse(all(val %in% c(0:9)), as.numeric(str_rep), str_rep))
}

base10to2<-function(num){
  val <- c()
  while(num > 1){
    nextn <- num %% 2
    val <- c(nextn, val)
    num <- num - ceiling(num/2)
  }
  val <- c(num, val)
  n <- 10 ^ seq((length(val)-1),0,-1)
  #return(as.numeric(paste0(val, collapse = '')))
  return(sum(val*n))
}

base2to10<-function(num){
  val <- 0
  n <- tens_place(num)
  tens <- n[1]
  base <- n[2]
  while(tens > 1){
    nextn <- floor(num / tens)
    val <- val + nextn * 2 ^ base
    num <- num - tens * nextn
    base <- base - 1
    tens <- tens / 10
  }
  val <- val + num
  return(val)
}