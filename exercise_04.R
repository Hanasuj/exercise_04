rm(list=ls())
setwd("V:/MPA/MPAPRG/exercise_04")

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
  
  if(M[(row+1),col] > M[(row+1),(col+1)]){
    bars <- bars + (M[(row+1),col])
    row <- row + 1
  }else{
    bars <- bars + (M[(row+1),(col+1)])
    row <- row + 1
    col <- col + 1
  }
  print(bars)
  
  if (row < dim(M)[1]){
    Chocolate(M, row, col, bars)
  }
  return(bars)
}

my_matrix <- matrix(c(3,1,5,1,0,4,3,2,0,0,0,6,0,0,0,7), nrow = 4, ncol = 4)

print(Chocolate(my_matrix,1,1,3))






