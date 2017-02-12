library(reshape)

readData <- function() {
  df <- readLines('C:\\Users\\Miko\\Desktop\\Dropbox\\ormno-proj\\knapsack\\data2.txt')
  v1 <- unlist(strsplit(df[1], split=" "))
  lim <- as.numeric(v1[1])
  N <- as.numeric(v1[2])
  conNum <- ceiling(lim / 10)
  objNum <- ceiling(N / 10)
  
  s <- c()
  for(i in 2:(objNum +1))
    s <- c(s,df[i])
  
  s <- as.numeric(unlist(strsplit(s, split=" ")))
  s <- s[!is.na(s)]
  
  profits <- s
  
  s <- c()
  for(i in (objNum+2):(objNum+conNum+1))
    s <- c(s,df[i])
  
  s <- as.numeric(unlist(strsplit(s, split=" ")))
  s <- s[!is.na(s)]
  constraints <- s
  
  
  costs <- data.frame(c(1:N))
  for(i in 1:lim) {
    row <- objNum+conNum+2+(i-1)*objNum
    s <- c()
    for(j in row:(row+objNum-1))
      s <- c(s,df[j])
    
    s <- as.numeric(unlist(strsplit(s, split=" ")))
    s <- s[!is.na(s)]
    costs[,i] <- s
  }
  costs <- t(costs)
  
  data <- list()
  data$profits <- profits
  data$constraints <- constraints
  data$costs <- costs
  data$N <- N
  return(data)
}

initialize <- function(N, costs, constraints) {
  sol <- c()
  while(TRUE) {
    item <- getNewItem(sol, N)
    newSol <- c(sol, item)
    if(isFeasible(newSol, costs, constraints))
      sol <- newSol
    else
      return(sol)
  }
} 

getNewItem <- function(sol, N) {
  while(length(sol) < N) {
    item <- sample(1:N, 1)
    if(!item %in% sol)
      return(item)
  }
}

isFeasible <- function(sol, costs, constraints) {
  for(i in 1:length(constraints)) {
    costSum <- 0
    for(j in 1:length(sol)) {
      costSum <- costSum + costs[i, sol[j]]
    }
    
    if(costSum > constraints[i])
      return(FALSE)
  }
  return(TRUE)
}

countProfit <- function (sol, profits) {
  if(length(sol) == 0)
    return(0)
  
  profit <- 0
  for(i in 1:length(sol))
    profit <- profit + profits[sol[i]]
  
  return(profit)
}

sa <- function() {
  temp <- 1000
  tempLimit <- 0.01
  alfa <- 0.95
  
  data <- readData()
  N <- data$N
  profits <- data$profits
  constraints <- data$constraints
  costs <- data$costs
  currSol <- initialize(N, costs, constraints)
  
  while(temp > tempLimit) {
    item <- getNewItem(currSol, N)
    newSol <- c(currSol, item)
    
    while(!isFeasible(newSol, costs, constraints)) {
      len <- length(newSol) - 1
      i <- sample(1:len, 1)
      newSol <- newSol[-i]
    }
    
    diff <- countProfit(newSol, profits) - countProfit(currSol, profits)
    
    if (diff > 0 || runif(1, 0, 1) < exp(diff / temp))
      currSol <- newSol
    
    temp <- temp * alfa
  }
  print(currSol)
  print(countProfit(currSol, profits))
}



sa()
