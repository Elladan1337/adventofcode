library(tidyverse)

isVisible <- function(row_n, col_n, matrix){
  if(isVisibleLeft(row_n, col_n, matrix) == TRUE){return(TRUE)}
  if(isVisibleRight(row_n, col_n, matrix) == TRUE){return(TRUE)}
  if(isVisibleTop(row_n, col_n, matrix) == TRUE){return(TRUE)}
  if(isVisibleBottom(row_n, col_n, matrix) == TRUE){return(TRUE)}
  else{return(FALSE)}
}


isVisibleLeft <- function(row_n, col_n, matrix){
  if(col_n == 1){return(TRUE)}
  for(x in 1:(col_n-1)){
    if(matrix[row_n, col_n] <= matrix[row_n, x]){return(FALSE)}
    if(x == col_n-1){return(TRUE)}
  }
}

isVisibleRight <- function(row_n, col_n, matrix){
  if(col_n == ncol(matrix)){return(TRUE)}
  for(x in col_n+1:ncol(matrix)){
    if(matrix[row_n, col_n] <= matrix[row_n, x]){return(FALSE)}
    if(x == ncol(matrix)){return(TRUE)}
  }
}

isVisibleTop <- function(row_n, col_n, matrix){
  if(row_n == 1){return(TRUE)}
  for(x in 1:(row_n-1)){
    if(matrix[row_n, col_n] <= matrix[x, col_n]){return(FALSE)}
    if(x == row_n-1){return(TRUE)}
  }
}

isVisibleBottom <- function(row_n, col_n, matrix){
  if(row_n == nrow(matrix)){return(TRUE)}
  for(x in row_n+1:nrow(matrix)){
    if(matrix[row_n, col_n] <= matrix[x, col_n]){return(FALSE)}
    if(x == nrow(matrix)){return(TRUE)}
  }
}

sumVisible <- function(matrix){
  result <- 0
  for(y in 1:nrow(matrix)){
    for(x in 1:ncol(matrix)){
      if(isVisible(y, x, matrix) == TRUE){result <- result + 1}
    }
  }
  return(result)
}

input <- str_extract_all(read_lines("input8.txt"), ".", simplify = TRUE) %>%
  as.numeric() %>%
  matrix(nrow = 99, ncol = 99)

sumVisible(input)

## Part 2

sceneLeft <- function(row_n, col_n, matrix){
  if(col_n == 1){return(0)}
  for(step in 1:col_n){
    if(col_n-step == 1){return(step)}
    else if(matrix[row_n, col_n] > matrix[row_n, (col_n - step)]){next}
    else if(matrix[row_n, col_n] <= matrix[row_n, (col_n - step)]){return(step)}
  }
}

sceneRight <- function(row_n, col_n, matrix){
  if(col_n == ncol(matrix)){return(0)}
  for(step in 1:(ncol(matrix)-col_n)){
    if((ncol(matrix)-col_n)-step == 0){return(step)}
    else if(matrix[row_n, col_n] > matrix[row_n, (col_n + step)]){next}
    else if(matrix[row_n, col_n] <= matrix[row_n, (col_n + step)]){return(step)}
  }
}

sceneTop <- function(row_n, col_n, matrix){
  if(row_n == 1){return(0)}
  for(step in 1:row_n){
    if(row_n-step == 1){return(step)}
    else if(matrix[row_n, col_n] > matrix[row_n - step, col_n]){next}
    else if(matrix[row_n, col_n] <= matrix[row_n - step, col_n]){return(step)}
  }
}

sceneBottom <- function(row_n, col_n, matrix){
  if(row_n == nrow(matrix)){return(0)}
  for(step in 1:(nrow(matrix)-row_n)){
    if((nrow(matrix)-row_n)-step == 0){return(step)}
    else if(matrix[row_n, col_n] > matrix[row_n + step, col_n]){next}
    else if(matrix[row_n, col_n] <= matrix[row_n + step, col_n]){return(step)}
  }
}

sceneScore <- function(row_n, col_n, matrix){
  return(sceneTop(row_n, col_n, matrix) *
          sceneRight(row_n, col_n, matrix) *
         sceneLeft(row_n, col_n, matrix) *
         sceneBottom(row_n, col_n, matrix)
  )
}

highestSceneScore <- function(matrix){
  result <- vector()
  for(y in 1:nrow(matrix)){
    for(x in 1:ncol(matrix)){
      result <- append(result, sceneScore(y, x, matrix))
    }
  }
  return(max(result))
  }

highestSceneScore(input)
