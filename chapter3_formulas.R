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

time_series <- function(arr, initialyear, freq){
  
  library(tidyverse)

  uk <- ts ( arr , start = c (initialyear , 1) , frequency = freq)
  # print(qrt_car_sales)
  
  tend <- moving_average(uk, freq)
  
  # plot (qrt_car_sales)
  # lines ( tend , col = "purple")
  
  est_aleM <- uk / tend
  # print("est_aleM:  )
  # print(est_aleM)
  
  est1M <- colMeans ( matrix ( est_aleM , ncol =freq , byrow = T ) ,
                      na.rm = T )
  
  estM <- est1M / mean ( est1M )
  # print("estM:")
  # print(estM)
  
  aleM <- est_aleM / estM
  # print("aleM:")
  # print(aleM)
  
  desestM <- uk / estM
  # print("Desest M: ")
  # print(desestM)
  
  xx <- seq_along(uk)
  
  model <- lm ( desestM ~ xx )
  
  regL <- ts ( predict.lm ( model ) , start = c (initialyear , 1) ,
               frequency =freq)
  
  plot ( desestM )
  lines ( regL , col =6)
}