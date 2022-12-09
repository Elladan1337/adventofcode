library(tidyverse)
input <- as.numeric(read_lines("input1.1.txt"))
i = 1
value = 0
result = vector()

while(TRUE){
  if(is.na(input[i]) == FALSE){
  value <- value + input[i]
  i <- i + 1
  }
  if(is.na(input[i]) == TRUE){
  result <- append(result, value)
  value <- 0
  i <- i + 1
  }
  if(i == length(input)){break}
}
max(result)

result2 <- vector()
for(i in 1:3){
result2 <- append(result2, max(result))
result <- result[!(result == max(result))]}
sum(result2)