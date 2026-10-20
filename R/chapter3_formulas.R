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
  
  uk <- ts ( arr , start = c (initialyear , 1) , frequency = freq)
  # print(qrt_car_sales)
  
  tend <- moving_average(uk, freq)
  
  plot (uk)
  #lines ( tend , col = "purple")
  
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
  
  print("Intercept line:")
  intercept <- round(model$coefficients[1],5)
  xslope <- round(model$coefficients[2],5)
  print(paste(intercept, " ", xslope, "x"))
  
  plot ( desestM )
  lines ( regL , col =6)
}

# Function that calculates autocorrelation for a frequency n
autoCorrel <- function ( data , n ) {
  # Remove the last n data points
  xx1 <- head ( data , -n )
  # Remove the first n data points
  xx2 <- tail ( data , -n )
  # correlation
  sqrt ( summary ( lm ( xx1 ~ xx2 )) $ r.squared )
}

# The frequency searched will be the one that has the highest correlation.
# For that, we check which one the highest is between 1 and 12 and that will be the answer.
calculateCorrel <- function(data) {
  
  biggest <- 0
  bigfreq <- 1
  top <- 12
  
  if (length(data) < 12){
    top <- length(data)-1
  }
  
  for (i in 1:top){
    current <- autoCorrel(data, i)
    
    if (current > biggest){
      biggest = current
      bigfreq = i
    }
    
  }
  
  print(paste("The frequency will be: ", bigfreq, ", with correlation: ", round(biggest, 5)))
  
}