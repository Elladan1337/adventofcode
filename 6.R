library(stringr)

find_agnostic_start <- function(string, signal_length){
  for(n in 1:str_length(string)){
    fragment <- str_sub(string, start = n, end = n + signal_length - 1)
    for(i in 1:signal_length){
      if(nrow(str_locate_all(fragment, str_sub(fragment, start = i, end = i))[[1]])
         > 1){break}
      if(i == signal_length){return(n + signal_length - 1)}
    }
  }
}

## Read input
input <- readLines("input6.txt")

## Part 1
find_agnostic_start(input, 4)
## Part 2
find_agnostic_start(input, 14)



find_packet_start <- function(string){
  for(n in 1:str_length(string)){
  fragment <- str_sub(string, start = n, end = n + 3)
  if(str_sub(fragment, 1, 1) != str_sub(fragment, 2, 2) &
     str_sub(fragment, 1, 1) != str_sub(fragment, 3, 3) &
     str_sub(fragment, 1, 1) != str_sub(fragment, 4, 4) &
     str_sub(fragment, 2, 2) != str_sub(fragment, 3, 3) &
     str_sub(fragment, 2, 2) != str_sub(fragment, 4, 4) &
     str_sub(fragment, 3, 3) != str_sub(fragment, 4, 4)){
    break 
  }
}
return(n+3)}

find_message_start <- function(string){
  for(n in 1:str_length(string)){
    fragment <- str_sub(string, start = n, end = n + 13)
    for(i in 1:14){
      if(nrow(str_locate_all(fragment, str_sub(fragment, start = i, end = i))[[1]])
        > 1){break}
      if(i == 14){return(n + 13)}
    }
  }
}



## Read input
input <- readLines("input6.txt")

## Part 1
find_packet_start(input)
find_agnostic_start(input, 4)
## Part 2
find_message_start(input)
find_agnostic_start(input, 14)
