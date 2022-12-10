library(tidyverse)

input <- readLines("input10.txt")

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

solve2 <- function(input){
  
  crt_screen <- matrix(".", nrow = 6, ncol = 40)
  sprite <- 2
  level <- 1
  cycle <- 0
  
  for(i in 1:length(input)){
    if(input[i] == "noop"){
      cycle <- cycle + 1
      if(cycle == 41){cycle <- 1
      level <- level + 1}
      if(abs(sprite - cycle) <= 1){crt_screen[level, cycle] <- "#"}
    }
    else{cycle <- cycle + 1
    if(cycle == 41){cycle <- 1
    level <- level + 1}
    if(abs(sprite - cycle) <= 1){crt_screen[level, cycle] <- "#"}
    cycle <- cycle + 1
    if(cycle == 41){cycle <- 1
    level <- level + 1}
    if(abs(sprite - cycle) <= 1){crt_screen[level, cycle] <- "#"}
    sprite <- sprite + as.numeric(str_extract(input[i], "-?\\d+"))
    }
  }
  return(crt_screen)}

## Part 1
solve(input)

## Part 2
write_delim(as.data.frame(solve2(input)), "output10.txt", delim = "", col_name = FALSE)

###..#....####.####.####.#.....##..####.
#..#.#....#.......#.#....#....#..#.#....
#..#.#....###....#..###..#....#....###..
###..#....#.....#...#....#....#.##.#....
#.#..#....#....#....#....#....#..#.#....
#..#.####.####.####.#....####..###.####.


