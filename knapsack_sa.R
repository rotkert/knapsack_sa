library(reshape)

readData <- function(dataFile) {
  df <- readLines(dataFile)
  v1 <- unlist(strsplit(df[1], split=" "))
  N <- as.numeric(v1[2])
  lim <- as.numeric(v1[3])
  optSol <- as.numeric(v1[4])
  conNum <- ceiling(lim / 7)
  objNum <- ceiling(N / 7)
  
  s <- c()
  for(i in 2:(objNum +1))
    s <- c(s,df[i])
  
  s <- as.numeric(unlist(strsplit(s, split=" ")))
  s <- s[!is.na(s)]
  
  profits <- s
  
  costs <- data.frame(c(1:N))
  for(i in 1:lim) {
    row <- objNum+2+(i-1)*objNum
    s <- c()
    for(j in row:(row+objNum-1))
      s <- c(s,df[j])

    s <- as.numeric(unlist(strsplit(s, split=" ")))
    s <- s[!is.na(s)]
    costs[,i] <- s
  }
  
  s <- c()
  for(i in (objNum+(objNum * lim)+2):(objNum+(objNum * lim)+ conNum + 1))
    s <- c(s,df[i])

  s <- as.numeric(unlist(strsplit(s, split=" ")))
  s <- s[!is.na(s)]
  constraints <- s

  costs <- t(costs)

  data <- list()
  data$profits <- profits
  data$constraints <- constraints
  data$costs <- costs
  data$N <- N
  data$optSol <- optSol
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

sa <- function(dataFile) {
  temp <- 1000
  tempLimit <- 0.0001
  alfa <- 0.9
  M <- 14
  
  data <- readData(dataFile)
  
  startTime <- Sys.time()
  
  N <- data$N
  profits <- data$profits
  constraints <- data$constraints
  costs <- data$costs
  currSol <- c() #initialize(N, costs, constraints)
  
  while(temp > tempLimit) {
    m <- 0
    while(m < M) {
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
      
      m <- m + 1
    }  
    temp <- temp * alfa
  }
  
  endTime <- Sys.time()
  
  output <- c()
  output <- c(output, basename(dataFile))
  output <- c(output, endTime - startTime)
  output <- c(output, countProfit(currSol, profits))
  output <- c(output, data$optSol)
  write(output, file="C:\\Users\\Miko\\Desktop\\Dropbox\\ormno-proj\\hard_in\\sa_results.csv", append = TRUE, sep = "\t", ncolumns = 4)
}

analyze <- function() {
  dataFiles <- list.files("C:\\Users\\Miko\\Desktop\\Dropbox\\ormno-proj\\hard_in\\formath1\\", pattern="*.dat", full.names=TRUE)
  lapply(dataFiles, sa)
}

analyze()

