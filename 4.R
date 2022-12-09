input <- read_csv("input4.txt", col_names= FALSE)

processed <- input %>%
  separate(X1, c('Low1', 'Top1'), sep = "-") %>%
  separate(X2, c('Low2', 'Top2'), sep = "-") %>%
  mutate(Low1 = as.numeric(Low1),
         Low2 = as.numeric(Low2),
         Top1 = as.numeric(Top1),
         Top2 = as.numeric(Top2))

final <- processed %>%
  mutate(included = ifelse((Low1 >= Low2 & Top1 <= Top2) |
                          (Low2 >= Low1 & Top2 <= Top1), 
                           1, 0))

sum(final[, 5])

## Part 2

final2 <- processed %>%
  mutate(included = ifelse((Low1 <= Top2 & Top1 >= Top2) |
                             (Low2 <= Top1 & Top2 >= Top1), 
                           1, 0))

sum(final2[, 5])
