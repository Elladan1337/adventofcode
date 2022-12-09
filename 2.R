library(tidyverse)
input <- read_lines("input2.1.txt")

sum(
case_when(
  input == "A X" ~ 3,
  input == "A Y" ~ 4,
  input == "A Z" ~ 8,
  input == "B X" ~ 1,
  input == "B Y" ~ 5,
  input == "B Z" ~ 9,
  input == "C X" ~ 2,
  input == "C Y" ~ 6,
  input == "C Z" ~ 7,
)
)
