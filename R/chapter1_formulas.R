# Basic stuff


#' Absolute frequency
#' 
#' @param x The raw vector containing all data
#' @return A vector containing the absolute frequency of each thing
#' @export
absolute_freq <- function(x) table(x)

#' Statistical mode
#' 
#' Computes the mode out of a vector of data because the R function Mode() returns the type of object
#' 
#' @param x The raw data as a vector
#' @return The mode
#' @export
stat_mode <- function(x) {
  tbl <- absolute_freq(x)
  as.numeric(names(tbl)[tbl == max(tbl)])
}

#' Absolute frequency of a number
#' 
#' Calculates the abs frequency of a number
#' 
#' @param x The raw data
#' @param y The number to look for
#' @return The amount of times y appears in x
#' @export

absolute_freq_of_number <- function(x, y) {
  length(which(x == y))
}

#' Relative frequency
#' 
#' @param x Raw data vector
#' @return Table of relative frequencies
#' @export
relative_freq <- function(x) prop.table(table(x))

# Relative frequency of a specific number
relative_freq_of_number <- function(x, y) {
  table <- prop.table(table(x))
  as.numeric(table)[which(names(table) == y)]
}

# Cumulative absolute frequency (N_i = sum_{j<=i} n_j)
cum_absolute_freq <- function(x) cumsum(table(x))

# Cumulative relative frequency (F_i = N_i / N)
cum_relative_freq <- function(x) cumsum(prop.table(table(x)))

# Position measures are all covered by R
# Dispersion measures

mean_deviation <- function(x, p = mean(x)) {
  mean(abs(x - p))
}

# Mean Squared Error
mse <- function(x, p = mean(x)) {
  mean((x - p)^2)
}


# Warning: Do not mistake with var() function for quasi-variance
variance <- function(x) {
  mean((x - mean(x))^2)
}

std_deviation <- function(x) {
  sqrt(variance(x))
}

std_variable <- function(x) {
  (x - mean(x)) / std_deviation(x)
}

cv <- function(x) {
  std_deviation(x) / abs(mean(x))
}

# Moments & Shape measures

# Moment of order r w.r.t. c
moment <- function(x, r, c) {
  mean((x - c)^r)
}

# Ordinary moment
ord_moment <- function(x, r) moment(x, r, 0)

# Central moment
cnt_moment <- function(x, r) moment(x, r, mean(x))

# Pearson's skewness coefficient
p_skewness <- function(x) (mean(x) - stat_mode(x))/std_deviation(x)

# Fisher's skewness coefficient
f_skewness <- function(x) {
  cnt_moment(x, 3)/(std_deviation(x)^3)
}

# Fisher's kurtosis coefficient
kurtosis <- function(x) {
  g <- cnt_moment(x, 4)/(std_deviation(x)^4)
  g - 3
}

