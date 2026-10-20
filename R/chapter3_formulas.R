#' Moving Average of a Time Series
#'
#' Computes the centered moving average of an array with a specific order.
#'
#' @param arr Numeric vector or time series.
#' @param order Integer order of the moving average.
#' @return A filtered time series object.
#' @examples
#' moving_average(c(10, 12, 15, 14, 18, 20, 22), order = 3)
#' @export
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

#' Classical Time Series Decomposition & Trend Fitting
#'
#' Performs seasonal decomposition using moving averages and fits a linear trend to deseasonalized data.
#'
#' @param arr Numeric vector of time series observations.
#' @param initialyear Starting year for the time series.
#' @param freq Seasonal frequency (e.g., 4 for quarterly, 12 for monthly).
#' @return Displays plots and prints trend equation to console.
#' @examples
#' data_vals <- c(10, 15, 8, 20, 12, 18, 9, 25)
#' time_series(data_vals, initialyear = 2020, freq = 4)
#' @export
time_series <- function(arr, initialyear, freq){
  
  uk <- ts ( arr , start = c (initialyear , 1) , frequency = freq)
  
  tend <- moving_average(uk, freq)
  
  plot (uk)
  
  est_aleM <- uk / tend
  
  est1M <- colMeans ( matrix ( est_aleM , ncol =freq , byrow = T ) ,
                      na.rm = T )
  
  estM <- est1M / mean ( est1M )
  
  aleM <- est_aleM / estM
  
  desestM <- uk / estM
  
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

#' Autocorrelation for a Specific Lag
#'
#' Calculates the correlation between a time series and itself lagged by n periods.
#'
#' @param data Numeric vector of observations.
#' @param n Integer lag/frequency.
#' @return Numeric correlation value.
#' @examples
#' autoCorrel(c(1, 3, 2, 5, 4, 7, 6, 9), n = 2)
#' @export
autoCorrel <- function ( data , n ) {
  # Remove the last n data points
  xx1 <- head ( data , -n )
  # Remove the first n data points
  xx2 <- tail ( data , -n )
  # correlation
  sqrt ( summary ( lm ( xx1 ~ xx2 )) $ r.squared )
}

#' Find Dominant Frequency by Maximum Autocorrelation
#'
#' Checks autocorrelation for lags from 1 up to 12 (or length-1) to find the frequency with the highest correlation.
#'
#' @param data Numeric vector of time series observations.
#' @return Prints the highest correlation frequency to console.
#' @examples
#' calculateCorrel(c(1, 4, 2, 8, 2, 4, 2, 8, 3, 5, 2, 9))
#' @export
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
