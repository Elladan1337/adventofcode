library(tidyverse)

input <- readLines("input10.txt")
test_input <- readLines("test10.txt")


solve <- function(input){
  cycle <- 0
  X <- 1
  values_twenty <- 0
  for(i in 1:length(input)){
  if(input[i] == "noop"){
    cycle <- cycle + 1
    if((cycle - 20) %% 40 == 0){values_twenty <- values_twenty + X * cycle}
  }
  else{cycle <- cycle + 1
    if((cycle - 20) %% 40 == 0){values_twenty <- values_twenty + X * cycle}
    cycle <- cycle + 1
    if((cycle - 20) %% 40 == 0){values_twenty <- values_twenty + X * cycle}
    X <- X + as.numeric(str_extract(input[i], "-?\\d+"))}  
  }
  return(values_twenty)}


test_solve <- function(input){
  cycle <- 0
  X <- 1
  values_twenty <- vector()
  for(i in 1:length(input)){
    if(input[i] == "noop"){
      cycle <- cycle + 1
      if((cycle - 20) %% 40 == 0){values_twenty <- append(values_twenty, (X * cycle))}
    }
    else{cycle <- cycle + 1
    if((cycle - 20) %% 40 == 0){values_twenty <- append(values_twenty, (X * cycle))}
    cycle <- cycle + 1
    if((cycle - 20) %% 40 == 0){values_twenty <- append(values_twenty, (X * cycle))}
    X <- X + as.numeric(str_extract(input[i], "-?\\d+"))
    }  
  print(paste("Cycle: ", cycle, " X_Value: ", X))}
  return(values_twenty)}

## Part 1
solve(input)
