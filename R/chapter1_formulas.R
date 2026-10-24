# Basic stuff

#' Absolute frequency
#'
#' Computes the absolute frequency table (counts) for a sample vector.
#'
#' @param x A vector of values.
#' @return A frequency table (`table` object) with counts of each unique value.
#' @examples
#' absolute_freq(c(1, 2, 2, 3, 3, 3, 4))
#' @export
pipsa_absolute_freq <- function(x) table(x)

#' Statistical mode
#'
#' Computes the mode (most frequent value) of a numeric sample.
#'
#' @param x A numeric vector.
#' @return The mode of the vector provided
#' @examples
#' stat_mode(c(1, 2, 2, 3, 3, 4))
#' @export
pipsa_stat_mode <- function(x) {
  tbl <- pipsa_absolute_freq(x)
  as.numeric(names(tbl)[which.max(tbl)])
}

#' Interquartile range
#'
#' Computes the Interquartile Range of a given array
#'
#' @param x A vector of values
#' @return The Interquartile Range of said array
#' @examples
#' iqr(c(1, 2, 3, 4, 5))
#' @export
pipsa_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75), type = 2)
  iqr <- q[2] - q[1]
  return(iqr)
}

#' Absolute frequency of a number
#'
#' Counts how many times a specific value appears in a sample.
#'
#' @param x A vector of values.
#' @param y The specific value to count.
#' @return An integer representing the count of `y` in `x`.
#' @examples
#' absolute_freq_of_number(c(1, 2, 2, 3, 4), 2)
#' @export
pipsa_absolute_freq_of_number <- function(x, y) {
  length(which(x == y))
}

#' Relative frequency
#'
#' Computes the relative frequency distribution (f_i = n_i / N) of a sample.
#'
#' @param x A vector of values.
#' @return A frequency table (`table` object) of proportions summing to 1.
#' @examples
#' relative_freq(c(1, 2, 2, 3, 4))
#' @export
pipsa_relative_freq <- function(x) prop.table(table(x))

#' Relative frequency of a specific number
#'
#' Computes the relative frequency proportion
#' (f_i = n_i / N) of a specific value.
#'
#' @param x A vector of values.
#' @param y The specific value whose relative frequency is to be found.
#' @return A numeric proportion between 0 and 1.
#' @examples
#' relative_freq_of_number(c(1, 2, 2, 3, 4), 2)
#' @export
pipsa_relative_freq_of_number <- function(x, y) {
  table <- prop.table(table(x))
  as.numeric(table)[which(names(table) == y)]
}

#' Cumulative absolute frequency
#'
#' Computes cumulative absolute frequencies (N_i = sum_{j<=i} n_j).
#'
#' @param x A vector of values.
#' @return A cumulative sum vector of counts.
#' @examples
#' cum_absolute_freq(c(1, 2, 2, 3, 4))
#' @export
pipsa_cum_absolute_freq <- function(x) cumsum(table(x))

#' Cumulative relative frequency
#'
#' Computes cumulative relative frequencies (F_i = N_i / N).
#'
#' @param x A vector of values.
#' @return A cumulative sum vector of proportions ending at 1.
#' @examples
#' cum_relative_freq(c(1, 2, 2, 3, 4))
#' @export
pipsa_cum_relative_freq <- function(x) cumsum(prop.table(table(x)))

# Position measures are all covered by R
# Dispersion measures

#' Mean deviation w.r.t. a point
#'
#' Computes the mean absolute deviation
#' of a sample with respect to a point p
#' (defaults to the mean).
#'
#' @param x A numeric vector.
#' @param p A reference numeric value (defaults to mean(x)).
#' @return The mean absolute deviation.
#' @examples
#' mean_deviation(c(1, 2, 3, 4, 5))
#' @export
pipsa_mean_deviation <- function(x, p = mean(x)) {
  mean(abs(x - p))
}

#' Mean Squared Error
#'
#' Computes the mean squared error
#' of a sample with respect to a point p
#' (defaults to the mean).
#'
#' @param x A numeric vector.
#' @param p A reference numeric value (defaults to mean(x)).
#' @return The mean squared error.
#' @examples
#' mse(c(1, 2, 3, 4, 5))
#' @export
pipsa_mse <- function(x, p = mean(x)) {
  mean((x - p)^2)
}

#' Population Variance
#'
#' Computes the population variance (divided by N, not quasi-variance N-1).
#'
#' @param x A numeric vector.
#' @return The population variance.
#' @examples
#' variance(c(1, 2, 3, 4, 5))
#' @export
pipsa_variance <- function(x) {
  mean((x - mean(x))^2)
}

#' Population Standard Deviation
#'
#' Computes the population standard deviation (sqrt of population variance).
#'
#' @param x A numeric vector.
#' @return The population standard deviation.
#' @examples
#' std_deviation(c(1, 2, 3, 4, 5))
#' @export
pipsa_std_deviation <- function(x) {
  sqrt(pipsa_variance(x))
}

#' Standardized variable
#'
#' Transforms data into standardized values: z = (x - mean) / std_dev.
#'
#' @param x A numeric vector.
#' @return A numeric vector of standardized z-scores.
#' @examples
#' std_variable(c(10, 20, 30, 40, 50))
#' @export
pipsa_std_variable <- function(x) {
  (x - mean(x)) / pipsa_std_deviation(x)
}

#' Coefficient of variation
#'
#' Computes the Pearson coefficient of variation: cv = std_dev / |mean|.
#'
#' @param x A numeric vector.
#' @return The coefficient of variation.
#' @examples
#' cv(c(10, 20, 30, 40, 50))
#' @export
pipsa_cv <- function(x) {
  pipsa_std_deviation(x) / abs(mean(x))
}

# Moments & Shape measures

#' Moment of order r w.r.t. constant c
#'
#' Computes the sample moment of order
#' r with respect to constant c: mean((x - c)^r).
#'
#' @param x A numeric vector.
#' @param r Integer order of the moment.
#' @param c Reference numeric constant.
#' @return The moment value.
#' @examples
#' moment(c(1, 2, 3, 4, 5), r = 2, c = 0)
#' @export
pipsa_moment <- function(x, r, c) {
  mean((x - c)^r)
}

#' Ordinary moment
#'
#' Computes the ordinary moment of order r (w.r.t. origin 0): a_r = mean(x^r).
#'
#' @param x A numeric vector.
#' @param r Integer order of the moment.
#' @return The ordinary moment value.
#' @examples
#' ord_moment(c(1, 2, 3, 4, 5), r = 2)
#' @export
pipsa_ord_moment <- function(x, r) pipsa_moment(x, r, 0)

#' Central moment
#'
#' Computes the central moment of order r
#'  (w.r.t. the mean): m_r = mean((x - mean)^r).
#'
#' @param x A numeric vector.
#' @param r Integer order of the moment.
#' @return The central moment value.
#' @examples
#' cnt_moment(c(1, 2, 3, 4, 5), r = 2)
#' @export
pipsa_cnt_moment <- function(x, r) pipsa_moment(x, r, mean(x))

#' Pearson's skewness coefficient
#'
#' Computes Pearson's mode skewness coefficient: (mean - mode) / std_deviation.
#'
#' @param x A numeric vector.
#' @return The Pearson skewness coefficient.
#' @examples
#' p_skewness(c(1, 2, 2, 3, 4))
#' @export
pipsa_p_skewness <- function(x) (mean(x) - pipsa_stat_mode(x)) / pipsa_std_deviation(x)

#' Fisher's skewness coefficient
#'
#' Computes Fisher's skewness coefficient: m_3 / (std_dev^3).
#'
#' @param x A numeric vector.
#' @return The Fisher skewness coefficient.
#' @examples
#' f_skewness(c(1, 2, 3, 4, 5))
#' @export
pipsa_f_skewness <- function(x) {
  pipsa_cnt_moment(x, 3) / (pipsa_std_deviation(x)^3)
}

#' Fisher's kurtosis coefficient
#'
#' Computes Fisher's excess kurtosis: (m_4 / (std_dev^4)) - 3. Mesokurtic = 0.
#'
#' @param x A numeric vector.
#' @return The excess kurtosis value.
#' @examples
#' kurtosis(c(1, 2, 3, 4, 5))
#' @export
pipsa_kurtosis <- function(x) {
  g <- pipsa_cnt_moment(x, 4) / (pipsa_std_deviation(x)^4)
  g - 3
}

#' Detect Moderate and Extreme Outliers using Interquartile Range
#'
#' @param x A numeric vector of observations.
#' @return A list containing moderate and extreme outliers.
#' @examples
#' detect_outliers(c(1, 2, 3, 4, 5, 100, 200))
#' @export
pipsa_detect_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1

  mod_lower <- q1 - 1.5 * iqr
  mod_upper <- q3 + 1.5 * iqr
  ext_lower <- q1 - 3.0 * iqr
  ext_upper <- q3 + 3.0 * iqr

  mod_outliers <- x[x < mod_lower | x > mod_upper]
  ext_outliers <- x[x < ext_lower | x > ext_upper]

  list(
    iqr = iqr,
    moderate_bounds = c(lower = mod_lower, upper = mod_upper),
    extreme_bounds = c(lower = ext_lower, upper = ext_upper),
    moderate_outliers = mod_outliers,
    extreme_outliers = ext_outliers
  )
}

#' Root mean square (rms)
#'
#' Computes the root mean square AKA the quadratic mean
#'
#' @param x Raw data vector
#' @return Do i really need to explain this
#' @examples
#' rms(c(2, 0, 0, 4, 6, 9, 6, 7, 6, 6, 1))
#' @export

pipsa_rms <- function(x) {
  sqrt(mean(x^2))
}


#' Quantile on continuous case
#'
#' Calculates the quantile on the continuous case
#' NOTE: For the discrete (normal) case, use R's built-in quantile() function
#'
#' @param c A value between 0 and 1
#' @param N Size of the array
#' @param L Lower limit of the interval
#' @param N_prev The value in the cumulative absolute frequencies such that N_prev <= cN
#' @param ni Absolute frequency of the interval
#' @param ai Interval width
#'
#' @export

pipsa_quantile_continuous <- function(c, N, L, N_prev, ni, ai) {
  L + ((c * N - N_prev) / ni) * ai
}
