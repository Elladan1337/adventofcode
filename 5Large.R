library(tidyverse)
library(rlist)

find_line <- function(input){readLines(input) %>%
str_which("^.{0}$")
}

map <- function(raw){
  result <- vector()
  for(n in seq.int(from = 1, to = 35, by = 4)){
    fragment <- str_sub(raw, start = n, end = n + 3)
    result <- append(result, fragment)
  }
  return(result)
}

rotate <- function(list){
  final <- list()
  for(n in 1:length(list[[1]])){
    crate_row <- sapply(list, "[[", n) %>%
    na.omit()
    final <- list.append(final, crate_row)
  }
  return(final)
}

move_crates <- function(crate_map, instructions){
  for(n in 1:nrow(instructions)){
    crate_map[[instructions[n, 3]]] <- append(crate_map[[instructions[n, 3]]],
                                              rev(crate_map[[instructions[n, 2]]][1:instructions[n, 1]]), 
                                              after = 0)
    crate_map[[instructions[n, 2]]] <- crate_map[[instructions[n, 2]]][-1:-instructions[n, 1]]
  }
  return(crate_map)}

move_crates2 <- function(crate_map, instructions){
  for(n in 1:nrow(instructions)){
    crate_map[[instructions[n, 3]]] <- append(crate_map[[instructions[n, 3]]],
                                              crate_map[[instructions[n, 2]]][1:instructions[n, 1]], 
                                              after = 0)
    crate_map[[instructions[n, 2]]] <- crate_map[[instructions[n, 2]]][-1:-instructions[n, 1]]
  }
  return(crate_map)}

strip <- function(x){
  init_mat <- str_extract_all(x, "\\d+", simplify = TRUE)
  final_mat <- matrix(as.numeric(init_mat), ncol = ncol(init_mat))
}

highlight <- function(x){
  sapply(x, "[[", 1) %>%
    str_c(collapse = "")
}

### Loading
read_map <- readLines("aoc_2022_day05_large_input.txt", 
                      n = find_line("aoc_2022_day05_large_input.txt")-2)
instructions <- read_lines("aoc_2022_day05_large_input.txt", 
                           skip = find_line("aoc_2022_day05_large_input.txt"))


### Prepare Map
starting_map <- lapply(read_map, map) %>%
  lapply(str_extract, pattern = "[A-Z]") %>%
  rotate()

### Prepare Instructions
starting_instructions <- strip(instructions)

###Work
final_state <- move_crates(starting_map, starting_instructions)
highlight(final_state)

###Part2
final_state2 <- move_crates2(starting_map, starting_instructions)
highlight(final_state2)

