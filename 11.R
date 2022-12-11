library(tidyverse)

monkey0 <- function(integer){
  integer <- integer * 11
  integer <- floor(integer / 3)
  if(integer %% 5 == 0){
  inventory_7 <<- append(inventory_7, integer)}
  else{inventory_4 <<- append(inventory_4, integer)}
  counter_0 <<- counter_0 + 1
  inventory_0 <<- vector()
}

monkey1 <- function(integer){
  integer <- integer + 4
  integer <- floor(integer / 3)
  if(integer %% 2 == 0){
    inventory_2 <<- append(inventory_2, integer)}
  else{inventory_6 <<- append(inventory_6, integer)}
  counter_1 <<- counter_1 + 1
  inventory_1 <<- vector()
}

monkey2 <- function(integer){
  integer <- integer * 19
  integer <- floor(integer / 3)
  if(integer %% 13 == 0){
    inventory_5 <<- append(inventory_5, integer)}
  else{inventory_0 <<- append(inventory_0, integer)}
  counter_2 <<- counter_2 + 1
  inventory_2 <<- vector()
}

monkey3 <- function(integer){
  integer <- integer * integer
  integer <- floor(integer / 3)
  if(integer %% 7 == 0){
    inventory_6 <<- append(inventory_6, integer)}
  else{inventory_1 <<- append(inventory_1, integer)}
  counter_3 <<- counter_3 + 1
  inventory_3 <<- vector()
}

monkey4 <- function(integer){
  integer <- integer + 1
  integer <- floor(integer / 3)
  if(integer %% 19 == 0){
    inventory_3 <<- append(inventory_3, integer)}
  else{inventory_7 <<- append(inventory_7, integer)}
  counter_4 <<- counter_4 + 1
  inventory_4 <<- vector()
}

monkey5 <- function(integer){
  integer <- integer + 3
  integer <- floor(integer / 3)
  if(integer %% 11 == 0){
    inventory_0 <<- append(inventory_0, integer)}
  else{inventory_4 <<- append(inventory_4, integer)}
  counter_5 <<- counter_5 + 1
  inventory_5 <<- vector()
}

monkey6 <- function(integer){
  integer <- integer + 8
  integer <- floor(integer / 3)
  if(integer %% 3 == 0){
    inventory_5 <<- append(inventory_5, integer)}
  else{inventory_2 <<- append(inventory_2, integer)}
  counter_6 <<- counter_6 + 1
  inventory_6 <<- vector()
}

monkey7 <- function(integer){
  integer <- integer + 7
  integer <- floor(integer / 3)
  if(integer %% 17 == 0){
    inventory_3 <<- append(inventory_3, integer)}
  else{inventory_1 <<- append(inventory_1, integer)}
  counter_7 <<- counter_7 + 1
  inventory_7 <<- vector()
}

inventory_0 <- c(61)
inventory_1 <- c(76, 92, 53, 93, 79, 86, 81)
inventory_2 <- c(91, 99)
inventory_3 <- c(58, 67, 66)
inventory_4 <- c(94, 54, 62, 73)
inventory_5 <- c(59, 95, 51, 58, 58)
inventory_6 <- c(87, 69, 92, 56, 91, 93, 88, 73)
inventory_7 <- c(71, 57, 86, 67, 96, 95)

counter_0 <- 0
counter_1 <- 0 
counter_2 <- 0 
counter_3 <- 0 
counter_4 <- 0 
counter_5 <- 0 
counter_6 <- 0 
counter_7 <- 0 

solve <- function(rounds){
  for(n in 1:rounds){
    sapply(inventory_0, monkey0)
    sapply(inventory_1, monkey1)
    sapply(inventory_2, monkey2)
    sapply(inventory_3, monkey3)
    sapply(inventory_4, monkey4)
    sapply(inventory_5, monkey5)
    sapply(inventory_6, monkey6)
    sapply(inventory_7, monkey7)
  }
}

solve(20)

counters <- c(counter_0, counter_1, counter_2, counter_3, counter_4,
              counter_5, counter_6, counter_7)

## Part 2

monkey0 <- function(integer){
  integer <- integer * 11
  integer <- integer %% 9699690
  if(integer %% 5 == 0){
    inventory_7 <<- append(inventory_7, integer)}
  else{inventory_4 <<- append(inventory_4, integer)}
  counter_0 <<- counter_0 + 1
  inventory_0 <<- vector()
}

monkey1 <- function(integer){
  integer <- integer + 4
  integer <- integer %% 9699690
  if(integer %% 2 == 0){
    inventory_2 <<- append(inventory_2, integer)}
  else{inventory_6 <<- append(inventory_6, integer)}
  counter_1 <<- counter_1 + 1
  inventory_1 <<- vector()
}

monkey2 <- function(integer){
  integer <- integer * 19
  integer <- integer %% 9699690
  if(integer %% 13 == 0){
    inventory_5 <<- append(inventory_5, integer)}
  else{inventory_0 <<- append(inventory_0, integer)}
  counter_2 <<- counter_2 + 1
  inventory_2 <<- vector()
}

monkey3 <- function(integer){
  integer <- integer * integer
  integer <- integer %% 9699690
  if(integer %% 7 == 0){
    inventory_6 <<- append(inventory_6, integer)}
  else{inventory_1 <<- append(inventory_1, integer)}
  counter_3 <<- counter_3 + 1
  inventory_3 <<- vector()
}

monkey4 <- function(integer){
  integer <- integer + 1
  integer <- integer %% 9699690
  if(integer %% 19 == 0){
    inventory_3 <<- append(inventory_3, integer)}
  else{inventory_7 <<- append(inventory_7, integer)}
  counter_4 <<- counter_4 + 1
  inventory_4 <<- vector()
}

monkey5 <- function(integer){
  integer <- integer + 3
  integer <- integer %% 9699690
  if(integer %% 11 == 0){
    inventory_0 <<- append(inventory_0, integer)}
  else{inventory_4 <<- append(inventory_4, integer)}
  counter_5 <<- counter_5 + 1
  inventory_5 <<- vector()
}

monkey6 <- function(integer){
  integer <- integer + 8
  integer <- integer %% 9699690
  if(integer %% 3 == 0){
    inventory_5 <<- append(inventory_5, integer)}
  else{inventory_2 <<- append(inventory_2, integer)}
  counter_6 <<- counter_6 + 1
  inventory_6 <<- vector()
}

monkey7 <- function(integer){
  integer <- integer + 7
  integer <- integer %% 9699690
  if(integer %% 17 == 0){
    inventory_3 <<- append(inventory_3, integer)}
  else{inventory_1 <<- append(inventory_1, integer)}
  counter_7 <<- counter_7 + 1
  inventory_7 <<- vector()
}

inventory_0 <- c(61)
inventory_1 <- c(76, 92, 53, 93, 79, 86, 81)
inventory_2 <- c(91, 99)
inventory_3 <- c(58, 67, 66)
inventory_4 <- c(94, 54, 62, 73)
inventory_5 <- c(59, 95, 51, 58, 58)
inventory_6 <- c(87, 69, 92, 56, 91, 93, 88, 73)
inventory_7 <- c(71, 57, 86, 67, 96, 95)

counter_0 <- 0
counter_1 <- 0 
counter_2 <- 0 
counter_3 <- 0 
counter_4 <- 0 
counter_5 <- 0 
counter_6 <- 0 
counter_7 <- 0 

solve <- function(rounds){
  for(n in 1:rounds){
    sapply(inventory_0, monkey0)
    sapply(inventory_1, monkey1)
    sapply(inventory_2, monkey2)
    sapply(inventory_3, monkey3)
    sapply(inventory_4, monkey4)
    sapply(inventory_5, monkey5)
    sapply(inventory_6, monkey6)
    sapply(inventory_7, monkey7)
  }
}

# 5, 2, 13, 7, 19, 11, 3, 17

solve(10000)