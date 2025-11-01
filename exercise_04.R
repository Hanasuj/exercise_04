rm(list=ls())
setwd("E:/asdghjk/skola/VUT Brno/inzinier/4rocnik/PRG")

## Task 1 - coin change
ReturnCoins <- function(M){
  available_coins <- c(50, 20, 10, 5, 2, 1)
  change <- c()
  for (i in available_coins){
    change <- c(change, as.integer(M/i))
    M <- M %% i
  }
  return(change)
}

print(ReturnCoins(677))


## Task 2
UniversalReturnCoins <- function(M, coins){
  available_coins <- coins
  change <- c()
  for (i in available_coins){
    change <- c(change, as.integer(M/i))
    M <- M %% i
  }
  return(change)
}

print(UniversalReturnCoins(17.8, c(2,1,0.5,0.2,0.1)))

## Task 3
Chocolate <- function(M, row, col, bars){
  if (row == nrow(my_matrix)){
    return(M[row, col])
  }else{
    bars <- M[row, col]
    down <- Chocolate(M, row + 1, col)
    diagonal <- Chocolate(M, row + 1, col + 1)
    return(max(down, diagonal) + M[row, col])
  }
}


my_matrix <- matrix(c(3,1,5,1,0,4,3,2,0,0,0,6,0,0,0,7), nrow = 4, ncol = 4)
print(Chocolate(my_matrix,1,1,3))

## Task 4
HanoiTowers <- function(n, fromPeg, toPeg){
  if (n == 1){
    print(paste("Move disc from", fromPeg, "to", toPeg))
    return()
  }
  emptyPeg <- 6 - fromPeg - toPeg
  HanoiTowers(n-1, fromPeg, emptyPeg)
  print(paste("Move disc from", fromPeg, "to", toPeg))
  HanoiTowers(n-1, emptyPeg, toPeg)
  return()
}

HanoiTowers(5, 1, 3)


