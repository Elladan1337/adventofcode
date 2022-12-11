library(tidyverse)
library(rlist)

createMonke <- setRefClass("Monke", fields = list(
  operation = "character",
  divisor = "integer",
  throws_to_true = "integer",
  throws_to_false = "integer",
  inventory = "numeric",
  inspected = "numeric"),
  methods = list(
    inspect = function(item){
      if(str_detect(operation, "\\*") & str_detect(operation, "\\d+")){
        item <- item * as.numeric(str_extract(operation, "\\d+"))}
      else if(str_detect(operation, "\\+") & str_detect(operation, "\\d+")){
        item <- item + as.numeric(str_extract(operation, "\\d+"))}
      else if(str_detect(operation, "\\*")){item <- item * item}
      else if(str_detect(operation, "\\+")){item <- item + item}
      item <- floor(item / 3)
      if(item %% divisor == 0){
        monkeList[[throws_to_true + 1]]$inventory <<- 
          append(monkeList[[throws_to_true + 1]]$inventory, item)}
      else{monkeList[[throws_to_false + 1]]$inventory <<- 
        append(monkeList[[throws_to_false + 1]]$inventory, item)}
      inventory <<- inventory[-1]
      inspected <<- inspected + 1
        },
    inspect_all = function(){
      if(length(inventory) == 0){}
      else{for(i in 1:length(inventory)){
        inspect(inventory[1])}
    }},
    inspect_simplify = function(item){
      if(str_detect(operation, "\\*") & str_detect(operation, "\\d+")){
        item <- item * as.numeric(str_extract(operation, "\\d+"))}
      else if(str_detect(operation, "\\+") & str_detect(operation, "\\d+")){
        item <- item + as.numeric(str_extract(operation, "\\d+"))}
      else if(str_detect(operation, "\\*")){item <- item * item}
      else if(str_detect(operation, "\\+")){item <- item + item}
      item <- item %% simplifier
      if(item %% divisor == 0){
        monkeList[[throws_to_true + 1]]$inventory <<- 
          append(monkeList[[throws_to_true + 1]]$inventory, item)}
      else{monkeList[[throws_to_false + 1]]$inventory <<- 
        append(monkeList[[throws_to_false + 1]]$inventory, item)}
      inventory <<- inventory[-1]
      inspected <<- inspected + 1
    },
    inspect_simplify_all = function(){
      if(length(inventory) == 0){}
      else{for(i in 1:length(inventory)){
        inspect_simplify(inventory[1])}
      }}
  ))

parse_monkeys <- function(input){
  monkeList <- list()
  for(n in seq(1, length(input), by = 7)){
    monkey_info <- input[n :(n + 5)]
    monkeList <- list.append(monkeList,
      createMonke(
        inventory = as.numeric(str_extract_all(monkey_info[2], "\\d+", simplify = TRUE)),
        operation = str_extract(monkey_info[3], "(?<=\\=\\s).+"),
        divisor = as.integer(str_extract(monkey_info[4], "\\d+")),
        throws_to_true = as.integer(str_extract(monkey_info[5], "\\d+")),
        throws_to_false = as.integer(str_extract(monkey_info[6], "\\d+")),
        inspected = 0
        )
      )
  }
return(monkeList)}

solve <- function(input, rounds){
  monkeList <<- parse_monkeys(input)
  for(n in 1:rounds){
    for(x in 1:length(monkeList)){
      monkeList[[x]]$inspect_all()
    }
  }
  business <- sapply(monkeList, function(x){return(x$inspected)}) %>%
    sort(decreasing = TRUE)
  return(business[1] * business[2])
  }

solve2 <- function(input, rounds){
  monkeList <<- parse_monkeys(input)
  simplifier <<- sapply(monkeList, function(x){return(x$divisor)}) %>%
    prod()
  for(n in 1:rounds){
    for(x in 1:length(monkeList)){
      monkeList[[x]]$inspect_simplify_all()
    }
  }
  business <- sapply(monkeList, function(x){return(x$inspected)}) %>%
    sort(decreasing = TRUE)
  return(business[1] * business[2])
}

input <- readLines("test11.txt")


## Part 1
monkeList <- list()
solve(input, 20)

## Part 2
monkeList <- list()
simplifier <- 0
solve2(input, 10000)
