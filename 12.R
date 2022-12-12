library(tidyverse)
library(rlist)



to_numbers <- function(x){
  if(is.na(as.numeric(x)) == FALSE) {return (as.numeric(x))}
  return(which(letters == x))
}


get_kids <- function(map, parent){
  kids <- list()
  possible_coordinates <- lapply(list(c(0, 1), c(1,0), c(-1, 0), c(0, -1)), function(x){
    return(parent + x)
  })
  for(i in possible_coordinates){
    if(i[1] < 1 | i[2] < 1 | i[1] > nrow(map) | i[2] > ncol(map)){next}
    if(map[i[1], i[2]] <= (map[parent[1], parent[2]] + 1)){
      kids <- list.append(kids, i)}
  }
  return(kids)
}

main <- function(map, start_position){
  matice_vzdalenosti <- matrix(1000000, nrow = nrow(map), ncol = ncol(map))
  queue <- list(start_position)
  matice_vzdalenosti[start_position[1], start_position[2]] <- 0
  reached <- list(start_position)
  while(length(queue) != 0){
    parent <- queue[[1]]
    if(map[parent[1], parent[2]] == 27)
    {final_matrix <<- matice_vzdalenosti
      return(matice_vzdalenosti[parent[1], parent[2]])}
    queue <- queue[-1]
    kids <- get_kids(map, parent)
    for(kid in kids){
      if(list(kid) %in% reached){next}
      reached <- list.append(reached, kid)
      matice_vzdalenosti[kid[1], kid[2]] <- 
        matice_vzdalenosti[parent[1], parent[2]] + 1
      queue <- list.append(queue, kid)
    }
  print(queue)}
return(matice_vzdalenosti)}

parsed <-read_lines("input12.txt") %>%
  sapply(str_split, "")

parsed <- lapply(parsed, str_replace_all, "S", "0") %>%
  lapply(str_replace_all, "E", "27")

map <- matrix(sapply(unlist(parsed), to_numbers), ncol = length(parsed[[1]]), nrow = length(parsed), byrow = TRUE)
starting_position <- which(map == 0, arr.ind = TRUE)

final_matrix <- matrix()

main(map, starting_position)


## Part 2
reverse_main <- function(map, start_position){
  matice_vzdalenosti <- matrix(1000000, nrow = nrow(map), ncol = ncol(map))
  queue <- list(start_position)
  matice_vzdalenosti[start_position[1], start_position[2]] <- 0
  reached <- list(start_position)
  while(length(queue) != 0){
    parent <- queue[[1]]
    if(map[parent[1], parent[2]] == 1)
    {final_matrix <<- matice_vzdalenosti
    return(matice_vzdalenosti[parent[1], parent[2]])}
    queue <- queue[-1]
    kids <- get_kids2(map, parent)
    for(kid in kids){
      if(list(kid) %in% reached){next}
      reached <- list.append(reached, kid)
      matice_vzdalenosti[kid[1], kid[2]] <- 
        matice_vzdalenosti[parent[1], parent[2]] + 1
      queue <- list.append(queue, kid)
    }
    print(queue)}
  return(matice_vzdalenosti)}

get_kids2 <- function(map, parent){
  kids <- list()
  possible_coordinates <- lapply(list(c(0, 1), c(1,0), c(-1, 0), c(0, -1)), function(x){
    return(parent + x)
  })
  for(i in possible_coordinates){
    if(i[1] < 1 | i[2] < 1 | i[1] > nrow(map) | i[2] > ncol(map)){next}
    if(map[i[1], i[2]] >= (map[parent[1], parent[2]] - 1)){
      kids <- list.append(kids, i)}
  }
  return(kids)
}

reverse_main(map, c(21, 121))
