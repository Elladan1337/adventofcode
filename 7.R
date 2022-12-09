library(tidyverse)
library(data.tree)

input <- readLines("input7.txt")

move_out <- function(position){
  if(str_detect(position, "/")){position <- str_remove(position, "/[:alpha:]+$")}
  else{position <- ""}
}

move_in <- function(position, string){
  if(position == ""){position <- str_extract(string, "\\w+$")}
  else{position <- str_c(position, "/", str_extract(string, "\\w+$"))}
}

add_node <- function(string, position){
  if(str_detect(string, "dir")){string <- str_extract(string, "\\w+$")
  Navigate(tree, position)$AddChild(string, type = "dir")}
  else{bytes <- as.numeric(str_extract(string, "\\d+"))
  name <- str_extract(string, "[:graph:]+$")
  Navigate(tree, position)$AddChild(name = name, type = "file", size = bytes)}
}

tree_builder <- function(input){
  tree <- Node$new("start")
  position <- ""
  for(n in 1:length(input)){
    if(input[n] == "$ cd /"){position <- ""}
    else if(input[n] == "$ ls"){next}
    else if(input[n] == "$ cd .."){position <- move_out(position)}
    else if(str_detect(input[n], "\\$ cd [:alpha:]+")){position <- move_in(position, input[n])}
    else{add_node(input[n], position)}
    }
}

tree <- Node$new("start")
tree_builder(input)

tree$Do(function(node) node$size <- Aggregate(node, attribute = "size", aggFun = sum), traversal = "post-order")
print(tree, "size")

tree$type <- "dir"

result <- tree$Get("size", pruneFun = function(x) x$type == "dir")

sum(result[result < 100000])

## Part 2

min(result[result["start"] - result <= 40000000])
