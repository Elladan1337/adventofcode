monkey0 <- function(integer){
  integer <- integer * 19
  integer <- floor(integer / 3)
  if(integer %% 23 == 0){
    inventory_2 <<- append(inventory_2, integer)}
  else{inventory_3 <<- append(inventory_3, integer)}
  counter_0 <<- counter_0 + 1
  inventory_0 <<- vector()
}

monkey1 <- function(integer){
  integer <- integer + 6
  integer <- floor(integer / 3)
  if(integer %% 19 == 0){
    inventory_2 <<- append(inventory_2, integer)}
  else{inventory_0 <<- append(inventory_0, integer)}
  counter_1 <<- counter_1 + 1
  inventory_1 <<- vector()
}

monkey2 <- function(integer){
  integer <- integer * integer
  integer <- floor(integer / 3)
  if(integer %% 13 == 0){
    inventory_1 <<- append(inventory_1, integer)}
  else{inventory_3 <<- append(inventory_3, integer)}
  counter_2 <<- counter_2 + 1
  inventory_2 <<- vector()
}

monkey3 <- function(integer){
  integer <- integer + 3
  integer <- floor(integer / 3)
  if(integer %% 17 == 0){
    inventory_0 <<- append(inventory_0, integer)}
  else{inventory_1 <<- append(inventory_1, integer)}
  counter_3 <<- counter_3 + 1
  inventory_3 <<- vector()
}

counter_0 <- 0
counter_1 <- 0 
counter_2 <- 0 
counter_3 <- 0 

inventory_0 <- c(79, 98)
inventory_1 <- c(54, 65, 75, 74)
inventory_2 <- c(79, 60, 97)
inventory_3 <- c(74)

solve <- function(rounds){
  for(n in 1:rounds){
    sapply(inventory_0, monkey0)
    sapply(inventory_1, monkey1)
    sapply(inventory_2, monkey2)
    sapply(inventory_3, monkey3)
  }
}