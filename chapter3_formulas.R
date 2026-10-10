# Get the moving average of an array with a specific order
moving_average <- function(arr, order){
  
  tmparr <- c(1)
    
  for (i in 1:order){
    tmparr[i] = 1
  }
  
  if (order %% 2 == 0){
    tmparr[1] = 0.5
    tmparr[order+1] = 0.5
  }
  
  stats::filter(arr, tmparr / order)
  
}