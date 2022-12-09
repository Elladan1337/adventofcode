library(tidyverse)
library(rlist)

input <- readLines("input9.txt")

solve <- function(input){
head <- c(0,0)
tail1 <- c(0,0)
tail2 <- c(0,0)
tail3 <- c(0,0)
tail4 <- c(0,0)
tail5 <- c(0,0)
tail6 <- c(0,0)
tail7 <- c(0,0)
tail8 <- c(0,0)
tail9 <- c(0,0)

visited1 <- list(c(0,0))
visited9 <- list(c(0,0))

  for(line in 1:length(input)){
  direction <- str_extract(input[line], "[:alpha:]")
  steps <- str_extract(input[line], "\\d+")
  for(n in 1:steps){
    if(direction == "U"){
      head <- moveUp(head)
      tail1 <- moveTail(tail1, head)
      tail2 <- moveTail(tail2, tail1)
      tail3 <- moveTail(tail3, tail2)
      tail4 <- moveTail(tail4, tail3)
      tail5 <- moveTail(tail5, tail4)
      tail6 <- moveTail(tail6, tail5)
      tail7 <- moveTail(tail7, tail6)
      tail8 <- moveTail(tail8, tail7)
      tail9 <- moveTail(tail9, tail8)
      visited1 <- list.append(visited1, tail1)
      visited9 <- list.append(visited9, tail9)
    }
    if(direction == "R"){
      head <- moveRight(head)
      tail1 <- moveTail(tail1, head)
      tail2 <- moveTail(tail2, tail1)
      tail3 <- moveTail(tail3, tail2)
      tail4 <- moveTail(tail4, tail3)
      tail5 <- moveTail(tail5, tail4)
      tail6 <- moveTail(tail6, tail5)
      tail7 <- moveTail(tail7, tail6)
      tail8 <- moveTail(tail8, tail7)
      tail9 <- moveTail(tail9, tail8)
      visited1 <- list.append(visited1, tail1)
      visited9 <- list.append(visited9, tail9)
    }
    if(direction == "L"){
      head <- moveLeft(head)
      tail1 <- moveTail(tail1, head)
      tail2 <- moveTail(tail2, tail1)
      tail3 <- moveTail(tail3, tail2)
      tail4 <- moveTail(tail4, tail3)
      tail5 <- moveTail(tail5, tail4)
      tail6 <- moveTail(tail6, tail5)
      tail7 <- moveTail(tail7, tail6)
      tail8 <- moveTail(tail8, tail7)
      tail9 <- moveTail(tail9, tail8)
      visited1 <- list.append(visited1, tail1)
      visited9 <- list.append(visited9, tail9)
    }
    if(direction == "D"){
      head <- moveDown(head)
      tail1 <- moveTail(tail1, head)
      tail2 <- moveTail(tail2, tail1)
      tail3 <- moveTail(tail3, tail2)
      tail4 <- moveTail(tail4, tail3)
      tail5 <- moveTail(tail5, tail4)
      tail6 <- moveTail(tail6, tail5)
      tail7 <- moveTail(tail7, tail6)
      tail8 <- moveTail(tail8, tail7)
      tail9 <- moveTail(tail9, tail8)
      visited1 <- list.append(visited1, tail1)
      visited9 <- list.append(visited9, tail9)
    }
    #print(head)
    }
  }
#return(head)
return(c(length(unique(visited1)), length(unique(visited9))))
}


moveRight <- function(position){
  position[1] <- position[1] + 1
  return(position)
}

moveLeft <- function(position){
  position[1] <- position[1] - 1
  return(position)
}

moveUp <- function(position){
  position[2] <- position[2] + 1
  return(position)
}

moveDown <- function(position){
  position[2] <- position[2] - 1
  return(position)
}

moveTail <- function(tail, head){
  if(head[1] == tail[1] + 2 & head[2] == tail[2]) {tail <- moveRight(tail)}
  else if(head[1] == tail[1] - 2 & head[2] == tail[2]) {tail <- moveLeft(tail)}
  else if(head[2] == tail[2] + 2 & head[1] == tail[1]) {tail <- moveUp(tail)}
  else if(head[2] == tail[2] - 2 & head[1] == tail[1]) {tail <- moveDown(tail)}
  else if(head[1] != tail[1] & head[2] != tail[2] & (abs(head[1] - tail[1]) == 2 |
  abs(head[2] - tail [2]) == 2)){tail <- moveDiagonal(tail, head)}
  return(tail)
}

moveDiagonal <- function(tail, head){
  if(abs(tail[1] - head[1]) == 1){tail[1] <- head[1]}
  else if(abs(tail[2] - head[2]) == 1){tail[2] <- head[2]}
  else if(identical(abs(tail - head), c(2, 2))){
    tail[1] <- tail[1] + (head[1] - tail[1]) / 2
    tail[2] <- tail[2] + (head[2] - tail[2]) / 2
    return(tail)
  }
  if(head[1] == tail[1] + 2 & head[2] == tail[2]) {tail <- moveRight(tail)}
  else if(head[1] == tail[1] - 2 & head[2] == tail[2]) {tail <- moveLeft(tail)}
  else if(head[2] == tail[2] + 2 & head[1] == tail[1]) {tail <- moveUp(tail)}
  else if(head[2] == tail[2] - 2 & head[1] == tail[1]) {tail <- moveDown(tail)}
  return(tail)
}

## Part 1 
solve(input)[1]

## Part 2
solve(input)[2]
