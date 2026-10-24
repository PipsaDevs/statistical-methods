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
pipsa_moving_average <- function(arr, order) {
  tmparr <- c(1)

  for (i in 1:order) {
    tmparr[i] <- 1
  }

  if (order %% 2 == 0) {
    tmparr[1] <- 0.5
    tmparr[order + 1] <- 0.5
  }

  stats::filter(arr, tmparr / order)
}

#' Classical Time Series Decomposition & Trend Fitting
#'
#' Performs seasonal decomposition
#' using moving averages
#' and fits a linear trend to deseasonalized data.
#'
#' @param arr Numeric vector of time series observations.
#' @param initialyear Starting year for the time series.
#' @param freq Seasonal frequency (e.g., 4 for quarterly, 12 for monthly).
#' @return Displays plots and prints trend equation to console.
#' Also returns a list containing
#' the original time series, trend, seasonal indices,
#' deseasonalized data, and trend model.
#' @examples
#' data_vals <- c(10, 15, 8, 20, 12, 18, 9, 25)
#' time_series(data_vals, initialyear = 2020, freq = 4)
#' @export
pipsa_time_series <- function(data_array, initial_year, freq) {
  # time series data
  ts_data <- ts(data_array, start = c(initial_year, 1), frequency = freq)

  trend <- pipsa_moving_average(ts_data, freq)

  plot(ts_data, main = "Original Time Series", ylab = "Values", xlab = "Time")

  season_random <- ts_data / trend

  raw_seasonal_indices <- colMeans(
    matrix(
      season_random,
      ncol = freq, byrow = TRUE
    ),
    na.rm = TRUE
  )

  seasonal_indices <- raw_seasonal_indices / mean(raw_seasonal_indices)

  random_component <- season_random / seasonal_indices

  deseasonalized_data <- ts_data / seasonal_indices

  time_index <- seq_along(ts_data)
  trend_model <- lm(deseasonalized_data ~ time_index)

  trend_line <- ts(
    predict.lm(trend_model),
    start = c(initial_year, 1), frequency = freq
  )

  cat("\n-------- TIME SERIES DECOMPOSITION SUMMARY --------\n")

  cat("\nADJUSTED SEASONAL INDICES (Multiplicative):\n")
  print(round(seasonal_indices, 4))
  cat(
    "Note: Values > 1 indicate peaks, < 1 indicate troughs for that period.\n"
  )

  cat("\nLINEAR TREND MODEL (On Deseasonalized Data):\n")
  coeffs <- coef(trend_model)
  cat(sprintf("Equation: Y = %.4f + %.4f * t\n", coeffs[1], coeffs[2]))
  cat(sprintf("Intercept (Beta_0): %.4f\n", coeffs[1]))
  cat(sprintf("Slope     (Beta_1): %.4f\n", coeffs[2]))
  cat(sprintf("R-squared:          %.4f\n", summary(trend_model)$r.squared))

  cat("\nFORMULA:\n")
  pipsa_obtain_function_from_model(trend_model)
  cat("----------------------------------\n\n")

  plot(deseasonalized_data,
    main = "Deseasonalized Data with Trend Line",
    ylab = "Deseasonalized Values", xlab = "Time"
  )
  lines(trend_line, col = 6, lwd = 2)

  list(
    ts_data = ts_data,
    trend = trend,
    seasonal_indices = seasonal_indices,
    deseasonalized_data = deseasonalized_data,
    trend_model = trend_model
  )
}
#' auto_correlation for a Specific Lag
#'
#' Calculates the correlation between a time
#' series and itself lagged by n periods.
#'
#' @param data Numeric vector of observations.
#' @param n Integer lag/frequency.
#' @return Numeric correlation value.
#' @examples
#' auto_correl(c(1, 3, 2, 5, 4, 7, 6, 9), n = 2)
#' @export
pipsa_auto_correl <- function(data, n) {
  # Remove the last n data points
  xx1 <- head(data, -n)
  # Remove the first n data points
  xx2 <- tail(data, -n)
  # correlation
  sqrt(summary(lm(xx1 ~ xx2))$r.squared)
}

#' Find Dominant Frequency by Maximum auto_correlation
#'
#' Checks auto_correlation for lags from 1 up to 12
#' (or length-1) to find the frequency with the highest correlation.
#'
#' @param data Numeric vector of time series observations.
#' @return Prints the highest correlation frequency to console.
#' @examples
#' calculate_correl(c(1, 4, 2, 8, 2, 4, 2, 8, 3, 5, 2, 9))
#' @export
pipsa_calculate_correl <- function(data) {
  biggest <- 0
  bigfreq <- 1
  top <- 12

  if (length(data) < 12) {
    top <- length(data) - 1
  }

  for (i in 1:top) {
    current <- pipsa_auto_correl(data, i)

    if (current > biggest) {
      biggest <- current
      bigfreq <- i
    }
  }

  print(
    paste(
      "The frequency will be: ", bigfreq,
      ", with correlation: ", round(biggest, 5)
    )
  )
}

#' Obtain Function from Model Coefficients
#'
#' Creates a string from a linear model's coefficients
#' in the form "intercept + slope * x".
#'
#' @param x A model (obtained from lm())
#' @return A string representation of the linear function.
#' @examples
#' obtain_function_from_model(lm(c(1, 2, 3) ~ c(4, 5, 6)))
#' @export
pipsa_obtain_function_from_model <- function(model) {
  intercept <- round(model$coefficients[1], 5)
  xslope <- round(model$coefficients[2], 5)

  if (xslope >= 0) {
    print(paste(intercept, "+", xslope, "x"))
  } else {
    print(paste(intercept, " ", xslope, "x"))
  }
}

#' Deflate Nominal Monetary Series to Real Values
#'
#' Converts nominal monetary observations
#' into real base-year values using CPI index figures.
#'
#' @param nominal_values Numeric vector of
#' observed nominal figures across time periods.
#' @param cpi_index Numeric vector of
#' Consumer Price Index values (base year = 100).
#' @return A data frame containing
#' Nominal Values, CPI, Real Values, and Purchasing Power factor.
#' @export
pipsa_calculate_deflation <- function(nominal_values, cpi_index) {
  if (length(nominal_values) != length(cpi_index)) {
    stop(
      "Nominal values vector and CPI index vector must have the same length."
    )
  }

  cpi_proportion <- cpi_index / 100
  real_values <- nominal_values / cpi_proportion
  purchasing_power_factor <- 1 / cpi_proportion

  data.frame(
    Nominal = nominal_values,
    CPI = cpi_index,
    Real_Value = real_values,
    Purchasing_Power = purchasing_power_factor
  )
}
