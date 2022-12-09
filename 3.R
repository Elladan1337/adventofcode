library(tidyverse)
input <- read_lines("input3.txt")
str_length(input)/2

compartment1 <- str_sub(input, start = 1, end = str_length(input)/2)
compartment2 <- str_sub(input, start = str_length(input)/2+1)

all_letters <- append(letters, LETTERS)

result <- vector()
for(n in 1:length(compartment1)){
str1 <- compartment1[n]
str2 <- compartment2[n]
for(i in 1:length(all_letters)){
  if(str_detect(str1, all_letters[i]) == TRUE & str_detect(str2, all_letters[i]) == TRUE)
  {break}
}
result <- append(result, i)
}

sum(result)

##### Part 2
result <- vector()
for(n in seq.int(1, 300, by = 3)){
  str1 <- input[n]
  str2 <- input[n+1]
  str3 <- input[n+2]
for(i in 1:length(all_letters)){
  if(str_detect(str1, all_letters[i]) == TRUE & 
     str_detect(str2, all_letters[i]) == TRUE &
     str_detect(str3, all_letters[i]) == TRUE){break}
}
result <- append(result, i)
}
sum(result)
