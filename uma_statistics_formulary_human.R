# =====================================================================
# UMA STATISTICS FORMULARY | Chapters 1, 2 and 3
# File: uma_statistics_formulary_human.R
# Base R only (no extra packages). No solved exercises, no exam datasets.
# =====================================================================
# CONVENTIONS (UMA course)
#   * Variance, standard deviation and covariance divide by N.
#   * Quasi-variance divides by N - 1.
#   * Regression errors (SSE, MSE, RMSE, R2) are measured on the ORIGINAL
#     scale of Y (never on ln(y) or 1/y).
#   * Moodle answers are CUT to 4 decimals, NEVER rounded.
#   * Frequencies: x = values x_i and ni = absolute frequencies n_i.
#     With raw data just write ni = NULL (the default).
#   * Use a DOT for decimals when you type data (3.5, not 3,5).
#
# HOW TO USE: paste options(digits = 15), trunc_4() and, if you type
# data as text, parse_input() first. Then paste only the functions you
# need. Each header says which other functions it calls (DEPENDENCIES).
#
# SEARCH-TAG INDEX (Ctrl+F the tag, e.g.  [CAP1-QUANTILE-DISCRETE])
#   HEADER   : [HEADER-OPTIONS] [HEADER-TRUNC4] [HEADER-PARSE-INPUT]
#   CHAPTER 1: [CAP1-FREQ-TABLE] [CAP1-FREQ-TABLE-INTERVALS] [CAP1-MEAN]
#              [CAP1-RMS] [CAP1-MEDIAN-DISCRETE] [CAP1-MEDIAN-CONTINUOUS]
#              [CAP1-MEDIAN-INTERVALS] [CAP1-QUANTILE-DISCRETE]
#              [CAP1-QUANTILE-CONTINUOUS] [CAP1-MODE] [CAP1-MODE-INTERVALS]
#              [CAP1-VARIANCE] [CAP1-SD] [CAP1-QUASI-VARIANCE] [CAP1-CV]
#              [CAP1-ZSCORE] [CAP1-RANGE] [CAP1-IQR] [CAP1-DESTANDARIZE-Z]
#              [CAP1-LINEAR-TRANSFORM] [CAP1-SYMMETRY]
#              [CAP1-MOMENTS-ORDINARY] [CAP1-MOMENTS-CENTRAL]
#              [CAP1-SKEWNESS-G1] [CAP1-KURTOSIS-G2] [CAP1-OUTLIERS-IQR]
#   CHAPTER 2: [CAP2-CONTINGENCY-TABLE] [CAP2-TABLE-FROM-VECTOR]
#              [CAP2-TABLE-TO-VECTORS] [CAP2-MARGINALS] [CAP2-CONDITIONALS]
#              [CAP2-EXPECTED-FREQ] [CAP2-INDEPENDENCE] [CAP2-CHI-SQUARE]
#              [CAP2-CONTINGENCY-C] [CAP2-COVARIANCE] [CAP2-CORR-PEARSON]
#              [CAP2-CORR-SPEARMAN] [CAP2-REG-Y-ON-X] [CAP2-REG-X-ON-Y]
#              [CAP2-REG-EXPONENTIAL] [CAP2-REG-HYPERBOLIC]
#              [CAP2-REG-LOGARITHMIC] [CAP2-REG-POWER]
#              [CAP2-REG-POLYNOMIAL] [CAP2-REG-CUSTOM-LINEAR]
#              [CAP2-REG-PLANE-3D] [CAP2-COMPARE-MODELS]
#   ALGEBRA  : [ALGEBRA-HISTOGRAM] [ALGEBRA-TWO-OBSERVATIONS]
#              [ALGEBRA-REGRESSION-LINES]
#   CHAPTER 3: [CAP3-TS-CREATE] [CAP3-MOVING-AVERAGE]
#              [CAP3-DECOMP-MULTIPLICATIVE] [CAP3-DECOMP-ADDITIVE]
#              [CAP3-FORECAST] [CAP3-AUTOCORRELATION] [CAP3-SIMPLE-INDEX]
#              [CAP3-DEFLATE]
# =====================================================================


# =====================================================================
# HEADER & UTILITIES
# =====================================================================

# [HEADER-OPTIONS]
# Show up to 15 significant digits so print() hides nothing.
options(digits = 15)


# ---------------------------------------------------------------------
# [HEADER-TRUNC4]  trunc_4(x)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Moodle CUTS the answer to 4 decimals (it does not round):
#       trunc_4(x) = trunc(x * 10^4) / 10^4
#   Example: 0.96806 -> 0.9680 (rounding would give 0.9681, WRONG).
#   Negative numbers are cut towards zero: -1.23459 -> -1.2345.
#   To PRINT four decimals use sprintf("%.4f", trunc_4(x)).
#   Floating-point note: 0.29 is stored as 0.28999999999999998, so in very
#   rare cases the last digit can be lost. If a result looks one unit too
#   small, check it with trunc(x * 1e4 + 1e-8) / 1e4.
# INPUTS & OUTPUTS:
#   x : number, vector or matrix.  Returns x cut to 4 decimals.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
trunc_4 <- function(x) {
  # Step 1: Move 4 decimals to the left of the point, cut, move them back
  result <- trunc(x * 1e4) / 1e4
  return(result)
}


# ---------------------------------------------------------------------
# [HEADER-PARSE-INPUT]  parse_input(text_string)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   No formula: it saves typing. Paste the data of the exam as one piece
#   of text and get a numeric vector c(...). Spaces, tabs, new lines,
#   commas and semicolons are separators. Decimals must use a DOT.
# INPUTS & OUTPUTS:
#   text_string : text, e.g. "3, 5; 7   9\n10".  Returns c(3, 5, 7, 9, 10).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
parse_input <- function(text_string) {
  # Step 1: Replace commas, semicolons, spaces, tabs and new lines by one space
  clean_text <- gsub("[,;[:space:]]+", " ", text_string)
  # Step 2: Remove the spaces at the beginning and at the end
  clean_text <- trimws(clean_text)
  # Step 3: Split into pieces and convert each piece into a number
  pieces <- strsplit(clean_text, " ", fixed = TRUE)[[1]]
  numbers <- as.numeric(pieces)
  return(numbers)
}


# =====================================================================
# BLOCK 1: UNIVARIATE STATISTICS (CHAPTER 1)
# =====================================================================

# ---------------------------------------------------------------------
# [CAP1-FREQ-TABLE]  frequency_table(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   One row per different value x_i:
#       n_i = absolute frequency          f_i = n_i / N
#       N_i = n_1 + ... + n_i             F_i = N_i / N
#   with N = sum(n_i). The last N_i is N and the last F_i is 1.
# INPUTS & OUTPUTS:
#   x : raw data (or the values x_i if ni is given).  ni : frequencies.
#   Returns data.frame with columns x_i, n_i, f_i, N_i, F_i.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
frequency_table <- function(x, ni = NULL) {
  # Step 1: Values x_i and absolute frequencies n_i (sorted by x_i)
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x[order(x)]
    ni <- ni[order(x)]
  }
  # Step 2: Relative and cumulative frequencies
  N <- sum(ni)
  fi <- ni / N
  Ni <- cumsum(ni)
  Fi <- Ni / N
  # Step 3: Put everything in a table
  table_result <- data.frame(x_i = values, n_i = ni, f_i = fi, N_i = Ni, F_i = Fi)
  return(table_result)
}


# ---------------------------------------------------------------------
# [CAP1-FREQ-TABLE-INTERVALS]  frequency_table_intervals(L_inf, L_sup, ni)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Data grouped in classes [L_(i-1), L_i):
#       class mark  x_i = (L_(i-1) + L_i) / 2
#       width       a_i = L_i - L_(i-1)
#       height      h_i = n_i / a_i      (histogram density, area = n_i)
#       f_i = n_i / N,  N_i = cumulative n_i,  F_i = N_i / N
# INPUTS & OUTPUTS:
#   L_inf, L_sup : lower and upper limits.  ni : frequency of each class.
#   Returns data.frame with L_inf, L_sup, x_i, n_i, f_i, N_i, F_i, a_i, h_i.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
frequency_table_intervals <- function(L_inf, L_sup, ni) {
  # Step 1: Class marks, widths and total N
  class_marks <- (L_inf + L_sup) / 2
  widths <- L_sup - L_inf
  N <- sum(ni)
  # Step 2: Relative and cumulative frequencies
  fi <- ni / N
  Ni <- cumsum(ni)
  Fi <- Ni / N
  # Step 3: Histogram heights h_i = n_i / a_i
  heights <- ni / widths
  table_result <- data.frame(L_inf = L_inf, L_sup = L_sup, x_i = class_marks, n_i = ni,
                             f_i = fi, N_i = Ni, F_i = Fi, a_i = widths, h_i = heights)
  return(table_result)
}


# ---------------------------------------------------------------------
# [CAP1-MEAN]  mean_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Arithmetic mean = centre of gravity of the data:
#       mean = sum(n_i * x_i) / N        (N = sum(n_i))
#   For grouped data use the class marks as x_i.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the mean.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
mean_desc <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Total number of data N
  N <- sum(ni)
  # Step 3: Mean = sum(n_i * x_i) / N
  mean_x <- sum(ni * values) / N
  return(mean_x)
}


# ---------------------------------------------------------------------
# [CAP1-RMS]  rms_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Root mean square (quadratic mean):
#       RMS = sqrt( sum(n_i * x_i^2) / N )
#   It links variance and mean:  s_n^2 = RMS^2 - mean^2.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the RMS.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
rms_desc <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean of the squares sum(n_i * x_i^2) / N
  N <- sum(ni)
  mean_of_squares <- sum(ni * values^2) / N
  # Step 3: Square root
  rms_x <- sqrt(mean_of_squares)
  return(rms_x)
}


# ---------------------------------------------------------------------
# [CAP1-MEDIAN-DISCRETE]  median_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Median = quantile of order p = 0.5 with the UMA rule  c*N = E + D
#   (see quantile_discrete): D > 0 -> x_(E+1);  D = 0 -> (x_E + x_(E+1))/2.
#   R's median() gives the same for raw data, but use this for tables.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the median.
# DEPENDENCIES: quantile_discrete().
# ---------------------------------------------------------------------
median_desc <- function(x, ni = NULL) {
  # Step 1: The median is the quantile of order 0.5
  median_x <- quantile_discrete(x, 0.5, ni)
  # Step 2: Remove the name "p=0.5"
  median_x <- unname(median_x)
  return(median_x)
}


# ---------------------------------------------------------------------
# [CAP1-MEDIAN-CONTINUOUS]  median_continuous(L_inf, L_sup, ni)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Grouped data. Median class = first class with N_i >= N/2. Inside it,
#   linear interpolation:
#       Me = L_(i-1) + ( (N/2 - N_(i-1)) / n_i ) * a_i
# INPUTS & OUTPUTS:
#   L_inf, L_sup : class limits.  ni : class frequencies.
#   Returns one number: the median.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
median_continuous <- function(L_inf, L_sup, ni) {
  # Step 1: Total N, cumulative frequencies N_i and previous cumulative N_(i-1)
  N <- sum(ni)
  cumulative <- cumsum(ni)
  previous_cumulative <- c(0, cumulative)[1:length(ni)]
  # Step 2: Median class = first class with N_i >= N/2
  i <- which(cumulative >= N / 2 - 1e-9 & ni > 0)[1]
  # Step 3: Linear interpolation inside the class
  width <- L_sup[i] - L_inf[i]
  median_x <- L_inf[i] + (N / 2 - previous_cumulative[i]) / ni[i] * width
  return(median_x)
}


# ---------------------------------------------------------------------
# [CAP1-MEDIAN-INTERVALS]  median_intervals(L_inf, L_sup, ni)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Same as median_continuous(): wrapper with the name used for grouped
#   (interval) data.  Me = L_(i-1) + ((N/2 - N_(i-1)) / n_i) * a_i
# INPUTS & OUTPUTS:
#   L_inf, L_sup : class limits.  ni : class frequencies.
#   Returns one number: the median.
# DEPENDENCIES: median_continuous().
# ---------------------------------------------------------------------
median_intervals <- function(L_inf, L_sup, ni) {
  # Step 1: Linear interpolation for p = 0.5
  median_x <- median_continuous(L_inf, L_sup, ni)
  return(median_x)
}


# ---------------------------------------------------------------------
# [CAP1-QUANTILE-DISCRETE]  quantile_discrete(x, p, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Strict UMA rule for the quantile of order p:
#       write  c*N = E + D   (c = p; E = integer part, D = decimal part)
#       D > 0  ->  quantile = x_(E+1)                 (next ordered datum)
#       D = 0  ->  quantile = ( x_(E) + x_(E+1) ) / 2
#   x_(k) is the datum in position k of the SORTED data (all N data, not
#   only the different values). Median p = 0.5, quartiles p = 0.25, 0.75.
#   This differs from R's quantile(): do not use quantile() in the exam.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  p : one or several orders in [0, 1].
#   ni : optional frequencies (whole numbers).
#   Returns a named vector (names "p=0.25", ...).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
quantile_discrete <- function(x, p, ni = NULL) {
  # Step 1: List all N data in increasing order
  if (is.null(ni)) sorted_data <- sort(x) else sorted_data <- rep(x[order(x)], ni[order(x)])
  N <- length(sorted_data)
  result <- numeric(length(p))
  for (j in seq_along(p)) {
    # Step 2: c*N = E + D (rounded to avoid floating-point noise)
    cN <- round(p[j] * N, 9)
    E <- floor(cN)
    D <- cN - E
    # Step 3: UMA rule (positions kept between 1 and N)
    if (D > 0) result[j] <- sorted_data[E + 1] else result[j] <- (sorted_data[max(E, 1)] + sorted_data[min(E + 1, N)]) / 2
  }
  names(result) <- paste0("p=", p)
  return(result)
}


# ---------------------------------------------------------------------
# [CAP1-QUANTILE-CONTINUOUS]  quantile_continuous(L_inf, L_sup, ni, p)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Grouped data. Quantile class = first class with N_i >= p*N. Inside it
#   interpolate linearly:
#       Q_p = L_(i-1) + ( (p*N - N_(i-1)) / n_i ) * a_i
#   L_(i-1) = lower limit, N_(i-1) = cumulative frequency of the previous
#   class, n_i = frequency of the class, a_i = its width.
# INPUTS & OUTPUTS:
#   L_inf, L_sup : class limits.  ni : class frequencies.
#   p : one or several orders in [0, 1].
#   Returns a named vector with the quantiles.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
quantile_continuous <- function(L_inf, L_sup, ni, p) {
  # Step 1: Total N, cumulative frequencies N_i and previous cumulative N_(i-1)
  N <- sum(ni)
  cumulative <- cumsum(ni)
  previous_cumulative <- c(0, cumulative)[1:length(ni)]
  result <- numeric(length(p))
  for (j in seq_along(p)) {
    # Step 2: Quantile class = first class with N_i >= p*N
    target <- p[j] * N
    i <- which(cumulative >= target - 1e-9 & ni > 0)[1]
    # Step 3: Linear interpolation inside the class
    result[j] <- L_inf[i] + (target - previous_cumulative[i]) / ni[i] * (L_sup[i] - L_inf[i])
  }
  names(result) <- paste0("p=", p)
  return(result)
}


# ---------------------------------------------------------------------
# [CAP1-MODE]  mode_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Mode = value(s) with the highest frequency. If several values share
#   the maximum frequency the distribution is multimodal: ALL are returned.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns a numeric vector with every mode (sorted).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
mode_desc <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Values that reach the maximum frequency
  modes <- sort(values[ni == max(ni)])
  return(modes)
}


# ---------------------------------------------------------------------
# [CAP1-MODE-INTERVALS]  mode_intervals(L_inf, L_sup, ni)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Modal class = class with the highest HEIGHT h_i = n_i / a_i (not the
#   highest n_i when the widths are different). Inside it:
#       Mo = L_(i-1) + d1 / (d1 + d2) * a_i
#       d1 = h_i - h_(i-1)      d2 = h_i - h_(i+1)
#   Outside the table the heights are 0. Several classes with the same
#   maximum height give several modes.
# INPUTS & OUTPUTS:
#   L_inf, L_sup : class limits (in increasing order).  ni : frequencies.
#   Returns a numeric vector with the mode(s).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
mode_intervals <- function(L_inf, L_sup, ni) {
  # Step 1: Widths and heights h_i = n_i / a_i
  widths <- L_sup - L_inf
  heights <- ni / widths
  k <- length(heights)
  # Step 2: Height of the previous and of the next class (0 outside the table)
  previous_heights <- c(0, heights)[1:k]
  next_heights <- c(heights, 0)[2:(k + 1)]
  # Step 3: Modal class = maximum height
  i <- which(abs(heights - max(heights)) < 1e-12)
  # Step 4: Mo = L_(i-1) + d1 / (d1 + d2) * a_i
  d1 <- heights[i] - previous_heights[i]
  d2 <- heights[i] - next_heights[i]
  modes <- L_inf[i] + d1 / (d1 + d2) * widths[i]
  return(modes)
}


# ---------------------------------------------------------------------
# [CAP1-VARIANCE]  var_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Variance = average squared distance to the mean, dividing STRICTLY by N:
#       s_n^2 = sum( n_i * (x_i - mean)^2 ) / N  =  RMS^2 - mean^2
#   R's var() divides by N - 1: do not use it here.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the variance.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
var_desc <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean
  N <- sum(ni)
  mean_x <- sum(ni * values) / N
  # Step 3: Sum of squared deviations divided by N
  var_x <- sum(ni * (values - mean_x)^2) / N
  return(var_x)
}


# ---------------------------------------------------------------------
# [CAP1-SD]  sd_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Standard deviation = square root of the variance that divides by N:
#       s_n = sqrt( sum( n_i * (x_i - mean)^2 ) / N )
#   Same units as the data. R's sd() divides by N - 1: do not use it.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the standard deviation.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
sd_desc <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean and variance (divide by N)
  N <- sum(ni)
  mean_x <- sum(ni * values) / N
  var_x <- sum(ni * (values - mean_x)^2) / N
  # Step 3: Square root of the variance
  sd_x <- sqrt(var_x)
  return(sd_x)
}


# ---------------------------------------------------------------------
# [CAP1-QUASI-VARIANCE]  quasi_variance(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Quasi-variance (sample variance) divides by N - 1:
#       s^2 = sum( n_i * (x_i - mean)^2 ) / (N - 1) = N / (N - 1) * s_n^2
#   For raw data it equals R's var().
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the quasi-variance (N >= 2).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
quasi_variance <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean
  N <- sum(ni)
  mean_x <- sum(ni * values) / N
  # Step 3: Sum of squared deviations divided by N - 1
  quasi_var_x <- sum(ni * (values - mean_x)^2) / (N - 1)
  return(quasi_var_x)
}


# ---------------------------------------------------------------------
# [CAP1-CV]  cv_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Coefficient of variation = relative dispersion, without units:
#       CV = s_n / |mean|
#   It compares the dispersion of variables with different units.
#   Meaningless if the mean is 0. Multiply by 100 for a percentage.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the CV.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
cv_desc <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean and standard deviation (divide by N)
  N <- sum(ni)
  mean_x <- sum(ni * values) / N
  sd_x <- sqrt(sum(ni * (values - mean_x)^2) / N)
  # Step 3: CV = s_n / |mean|
  cv_x <- sd_x / abs(mean_x)
  return(cv_x)
}


# ---------------------------------------------------------------------
# [CAP1-ZSCORE]  z_score(x)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Standardised value = number of standard deviations from the mean:
#       z_i = (x_i - mean) / s_n          (s_n divides by N)
#   The z values have mean 0 and variance 1.
# INPUTS & OUTPUTS:
#   x : raw data.  Returns a vector with the z value of each datum.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
z_score <- function(x) {
  # Step 1: Sample mean
  mean_x <- sum(x) / length(x)
  # Step 2: Standard deviation (sum of squared deviations divided by N)
  sd_x <- sqrt(sum((x - mean_x)^2) / length(x))
  # Step 3: Standardise
  z_values <- (x - mean_x) / sd_x
  return(z_values)
}


# ---------------------------------------------------------------------
# [CAP1-RANGE]  range_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Range = distance between the extremes:  R = max(x) - min(x)
#   (only values with frequency > 0 count).
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: the range.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
range_desc <- function(x, ni = NULL) {
  # Step 1: Keep the values that really appear
  if (!is.null(ni)) x <- x[ni > 0]
  # Step 2: Maximum minus minimum
  range_x <- max(x) - min(x)
  return(range_x)
}


# ---------------------------------------------------------------------
# [CAP1-IQR]  iqr_desc(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Interquartile range = length of the interval with the central 50%:
#       IQR = Q3 - Q1
#   Q1 and Q3 use the UMA rule c*N = E + D (R's IQR() uses another rule).
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: Q3 - Q1.
# DEPENDENCIES: quantile_discrete().
# ---------------------------------------------------------------------
iqr_desc <- function(x, ni = NULL) {
  # Step 1: First and third quartiles
  quartiles <- quantile_discrete(x, c(0.25, 0.75), ni)
  # Step 2: Q3 - Q1
  iqr_x <- unname(quartiles[2] - quartiles[1])
  return(iqr_x)
}


# ---------------------------------------------------------------------
# [CAP1-DESTANDARIZE-Z]  destandarizar_z(z_score, mean_x, sd_x)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Inverse of z = (X - mean) / s_n :
#       X = mean_x + z_score * sd_x
#   Use it when the exam gives the z value and asks for the original datum.
# INPUTS & OUTPUTS:
#   z_score : standardised value(s).  mean_x, sd_x : mean and standard
#   deviation of the original variable.  Returns the original value(s).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
destandarizar_z <- function(z_score, mean_x, sd_x) {
  # Step 1: X = mean + z * sd
  original_values <- mean_x + z_score * sd_x
  return(original_values)
}


# ---------------------------------------------------------------------
# [CAP1-LINEAR-TRANSFORM]  transform_linear_variable(mean_x, var_x, a, b)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Linear change of variable  Y = a*X + b :
#       mean_Y = a * mean_X + b       (the mean feels a and b)
#       var_Y  = a^2 * var_X          (the variance ignores b: a shift does
#                                      not change the dispersion)
#       s_Y    = |a| * s_X
# INPUTS & OUTPUTS:
#   mean_x, var_x : mean and variance of X.  a, b : coefficients.
#   Returns list(mean_y, var_y, sd_y).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
transform_linear_variable <- function(mean_x, var_x, a, b) {
  # Step 1: New mean
  mean_y <- a * mean_x + b
  # Step 2: New variance (b disappears) and standard deviation
  var_y <- a^2 * var_x
  sd_y <- sqrt(var_y)
  return(list(mean_y = mean_y, var_y = var_y, sd_y = sd_y))
}


# ---------------------------------------------------------------------
# [CAP1-SYMMETRY]  check_symmetry_criteria(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Three classical criteria to judge the shape of a distribution:
#   1) mean vs median vs mode: all equal -> symmetric; mean > median
#      (> mode) -> positive asymmetry (long right tail); mean < median
#      (< mode) -> negative asymmetry (long left tail).
#   2) quartiles: compare (Q3 - Me) with (Me - Q1); Q3 - Me bigger ->
#      positive asymmetry.
#   3) Fisher skewness g1 = mu_3 / s_n^3: 0 symmetric, > 0 positive,
#      < 0 negative.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns list(mean, median, modes, Q1, Q3, Q3_minus_Me, Me_minus_Q1,
#   g1, criterion_mean_median, criterion_mean_mode, criterion_quartiles,
#   criterion_fisher); the last four are texts.
# DEPENDENCIES: mean_desc(), median_desc(), mode_desc(),
#   quantile_discrete(), skewness_fisher().
# ---------------------------------------------------------------------
check_symmetry_criteria <- function(x, ni = NULL) {
  # Step 1: Mean, median, mode(s), quartiles and Fisher skewness
  mean_x <- mean_desc(x, ni)
  median_x <- median_desc(x, ni)
  modes <- mode_desc(x, ni)
  q1 <- unname(quantile_discrete(x, 0.25, ni))
  q3 <- unname(quantile_discrete(x, 0.75, ni))
  g1 <- skewness_fisher(x, ni)
  # Step 2: A positive difference means positive asymmetry, negative -> negative
  verdict <- function(d) if (abs(d) < 1e-9) "symmetric" else if (d > 0) "positive asymmetry" else "negative asymmetry"
  # Step 3: Apply the three criteria (the mode is used only if it is unique)
  mean_vs_mode <- if (length(modes) == 1) verdict(mean_x - modes) else "several modes: not applicable"
  return(list(mean = mean_x, median = median_x, modes = modes, Q1 = q1, Q3 = q3,
              Q3_minus_Me = q3 - median_x, Me_minus_Q1 = median_x - q1, g1 = g1,
              criterion_mean_median = verdict(mean_x - median_x),
              criterion_mean_mode = mean_vs_mode,
              criterion_quartiles = verdict((q3 - median_x) - (median_x - q1)),
              criterion_fisher = verdict(g1)))
}


# ---------------------------------------------------------------------
# [CAP1-MOMENTS-ORDINARY]  ordinary_moments(x, order = 1:4, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Ordinary moment (about the origin) of order k:
#       m_k = sum( n_i * x_i^k ) / N
#   m_1 = mean,  m_2 = RMS^2.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  order : orders k wanted.
#   ni : optional frequencies.  Returns a named vector (m_1, m_2, ...).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
ordinary_moments <- function(x, order = 1:4, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: One moment for each order k: sum(n_i * x_i^k) / N
  N <- sum(ni)
  moments <- numeric(length(order))
  for (j in seq_along(order)) moments[j] <- sum(ni * values^order[j]) / N
  names(moments) <- paste0("m_", order)
  return(moments)
}


# ---------------------------------------------------------------------
# [CAP1-MOMENTS-CENTRAL]  central_moments(x, order = 2:4, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Central moment (about the mean) of order k:
#       mu_k = sum( n_i * (x_i - mean)^k ) / N
#   mu_1 = 0,  mu_2 = variance (divides by N); mu_3 and mu_4 give
#   skewness and kurtosis.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  order : orders k wanted.
#   ni : optional frequencies.  Returns a named vector (mu_2, mu_3, ...).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
central_moments <- function(x, order = 2:4, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean and deviations from the mean
  N <- sum(ni)
  mean_x <- sum(ni * values) / N
  deviations <- values - mean_x
  # Step 3: One moment for each order k: sum(n_i * (x_i - mean)^k) / N
  moments <- numeric(length(order))
  for (j in seq_along(order)) moments[j] <- sum(ni * deviations^order[j]) / N
  names(moments) <- paste0("mu_", order)
  return(moments)
}


# ---------------------------------------------------------------------
# [CAP1-SKEWNESS-G1]  skewness_fisher(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Fisher coefficient of skewness (asymmetry):
#       g1 = mu_3 / s_n^3        with s_n = sqrt(mu_2) (divides by N)
#   g1 = 0 symmetric; g1 > 0 long right tail; g1 < 0 long left tail.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: g1.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
skewness_fisher <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean, mu_2 and mu_3
  N <- sum(ni)
  mean_x <- sum(ni * values) / N
  mu_2 <- sum(ni * (values - mean_x)^2) / N
  mu_3 <- sum(ni * (values - mean_x)^3) / N
  # Step 3: g1 = mu_3 / s_n^3
  g1 <- mu_3 / sqrt(mu_2)^3
  return(g1)
}


# ---------------------------------------------------------------------
# [CAP1-KURTOSIS-G2]  kurtosis_fisher(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Fisher coefficient of kurtosis (peakedness compared with the normal):
#       g2 = mu_4 / s_n^4 - 3
#   g2 = 0 mesokurtic (normal-like); g2 > 0 leptokurtic (sharper);
#   g2 < 0 platykurtic (flatter).
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns one number: g2.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
kurtosis_fisher <- function(x, ni = NULL) {
  # Step 1: Values x_i and frequencies n_i
  if (is.null(ni)) {
    values <- sort(unique(x))
    ni <- tabulate(match(x, values))
  } else {
    values <- x
  }
  # Step 2: Mean, mu_2 and mu_4
  N <- sum(ni)
  mean_x <- sum(ni * values) / N
  mu_2 <- sum(ni * (values - mean_x)^2) / N
  mu_4 <- sum(ni * (values - mean_x)^4) / N
  # Step 3: g2 = mu_4 / s_n^4 - 3   (s_n^4 = mu_2^2)
  g2 <- mu_4 / mu_2^2 - 3
  return(g2)
}


# ---------------------------------------------------------------------
# [CAP1-OUTLIERS-IQR]  outliers_iqr(x, ni = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Tukey fences built with the quartiles (UMA rule) and IQR = Q3 - Q1:
#       moderate outlier: outside [Q1 - 1.5*IQR , Q3 + 1.5*IQR]
#       extreme outlier : outside [Q1 - 3*IQR   , Q3 + 3*IQR]
#   "moderate" holds the data beyond the inner fences but NOT beyond the
#   outer fences; "extreme" holds the data beyond the outer fences.
# INPUTS & OUTPUTS:
#   x : raw data (or values x_i).  ni : optional frequencies.
#   Returns list(Q1, Q3, IQR, moderate_fences, extreme_fences, moderate,
#   extreme); fences are c(lower, upper).
# DEPENDENCIES: quantile_discrete().
# ---------------------------------------------------------------------
outliers_iqr <- function(x, ni = NULL) {
  # Step 1: Quartiles and IQR
  quartiles <- unname(quantile_discrete(x, c(0.25, 0.75), ni))
  iqr_x <- quartiles[2] - quartiles[1]
  # Step 2: Fences c(lower, upper)
  moderate_fences <- c(quartiles[1] - 1.5 * iqr_x, quartiles[2] + 1.5 * iqr_x)
  extreme_fences <- c(quartiles[1] - 3 * iqr_x, quartiles[2] + 3 * iqr_x)
  # Step 3: Data to classify (only values that really appear)
  data_values <- if (is.null(ni)) sort(x) else sort(x[ni > 0])
  # Step 4: Extreme = beyond the outer fences; moderate = beyond the inner ones only
  is_extreme <- data_values < extreme_fences[1] | data_values > extreme_fences[2]
  is_moderate <- (data_values < moderate_fences[1] | data_values > moderate_fences[2]) & !is_extreme
  return(list(Q1 = quartiles[1], Q3 = quartiles[2], IQR = iqr_x,
              moderate_fences = moderate_fences, extreme_fences = extreme_fences,
              moderate = data_values[is_moderate], extreme = data_values[is_extreme]))
}


# =====================================================================
# BLOCK 2: BIVARIATE STATISTICS & REGRESSION (CHAPTER 2)
# =====================================================================

# ---------------------------------------------------------------------
# [CAP2-CONTINGENCY-TABLE]  contingency_table(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Double-entry table of absolute frequencies: n_ij = number of pairs
#   with X = x_i and Y = y_j. Rows = values of X, columns = values of Y,
#   both in increasing order.
# INPUTS & OUTPUTS:
#   x, y : two raw vectors of the same length (paired observations).
#   Returns a matrix n_ij with dimnames X and Y.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
contingency_table <- function(x, y) {
  # Step 1: Count how many times each pair (x_i, y_j) appears
  counts <- table(X = x, Y = y)
  # Step 2: Keep a plain matrix
  tab_matrix <- unclass(counts)
  return(tab_matrix)
}


# ---------------------------------------------------------------------
# [CAP2-TABLE-FROM-VECTOR]  table_from_vector(x, NRows)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   No formula: builds the table n_ij from the frequencies copied from the
#   exam ROW BY ROW (first row of the table first). Rows are named 1, 2,
#   3... and columns 1, 2, 3...  To use the real values of X and Y rename:
#       rownames(tab) <- c(1, 2, 3)      colnames(tab) <- c(10, 20)
#   table_to_vectors() reads those names as the values of X and Y.
# INPUTS & OUTPUTS:
#   x : frequencies in reading order.  NRows : number of rows of the table.
#   Returns a matrix with dimnames X and Y.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
table_from_vector <- function(x, NRows) {
  # Step 1: Fill the matrix row by row
  tab_matrix <- matrix(x, nrow = NRows, byrow = TRUE)
  # Step 2: Name rows and columns 1, 2, 3...
  dimnames(tab_matrix) <- list(X = 1:nrow(tab_matrix), Y = 1:ncol(tab_matrix))
  return(tab_matrix)
}


# ---------------------------------------------------------------------
# [CAP2-TABLE-TO-VECTORS]  table_to_vectors(tab)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Inverse of the table: each pair (x_i, y_j) is repeated n_ij times, so X
#   and Y become two raw paired vectors that can be used in every
#   regression / correlation function. The values x_i and y_j are the
#   row / column NAMES of the table (they must be numbers).
# INPUTS & OUTPUTS:
#   tab : table of whole-number frequencies n_ij.
#   Returns list(X, Y, x_i, y_j, n_ij): expanded vectors, values of the
#   rows and columns, and the table as a matrix.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
table_to_vectors <- function(tab) {
  # Step 1: Plain matrix and values of X (row names) and Y (column names)
  n_ij <- unclass(as.matrix(tab))
  x_values <- as.numeric(rownames(n_ij))
  y_values <- as.numeric(colnames(n_ij))
  # Step 2: Frequencies in reading order (row by row)
  counts <- as.vector(t(n_ij))
  # Step 3: Repeat every pair (x_i, y_j) n_ij times
  X <- rep(rep(x_values, each = ncol(n_ij)), times = counts)
  Y <- rep(rep(y_values, times = nrow(n_ij)), times = counts)
  return(list(X = X, Y = Y, x_i = x_values, y_j = y_values, n_ij = n_ij))
}


# ---------------------------------------------------------------------
# [CAP2-MARGINALS]  marginal_distributions(tab)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Marginal distributions = totals of rows and columns:
#       n_i. = sum_j n_ij  (distribution of X)    f_i. = n_i. / N
#       n_.j = sum_i n_ij  (distribution of Y)    f_.j = n_.j / N
# INPUTS & OUTPUTS:
#   tab : table of frequencies n_ij.
#   Returns list(N, X_abs, Y_abs, X_rel, Y_rel).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
marginal_distributions <- function(tab) {
  # Step 1: Plain matrix and total N
  n_ij <- unclass(as.matrix(tab))
  N <- sum(n_ij)
  # Step 2: Marginal absolute frequencies n_i. and n_.j
  x_absolute <- rowSums(n_ij)
  y_absolute <- colSums(n_ij)
  # Step 3: Marginal relative frequencies f_i. and f_.j
  return(list(N = N, X_abs = x_absolute, Y_abs = y_absolute,
              X_rel = x_absolute / N, Y_rel = y_absolute / N))
}


# ---------------------------------------------------------------------
# [CAP2-CONDITIONALS]  conditional_distributions(tab)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Conditional distribution = one row (or column) divided by its total:
#       Y | X = x_i :  f(y_j | x_i) = n_ij / n_i.     (each ROW sums to 1)
#       X | Y = y_j :  f(x_i | y_j) = n_ij / n_.j     (each COLUMN sums to 1)
# INPUTS & OUTPUTS:
#   tab : table of frequencies n_ij (no empty row or column).
#   Returns list(Y_given_X, X_given_Y): two matrices of relative frequencies.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
conditional_distributions <- function(tab) {
  # Step 1: Plain matrix, row totals n_i. and column totals n_.j
  n_ij <- unclass(as.matrix(tab))
  row_totals <- rowSums(n_ij)
  column_totals <- colSums(n_ij)
  # Step 2: Divide each row by its total, each column by its total
  y_given_x <- n_ij / row_totals
  x_given_y <- sweep(n_ij, 2, column_totals, "/")
  return(list(Y_given_X = y_given_x, X_given_Y = x_given_y))
}


# ---------------------------------------------------------------------
# [CAP2-EXPECTED-FREQ]  expected_frequencies(tab)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Frequencies the table WOULD have if X and Y were independent:
#       e_ij = n_i. * n_.j / N
#   They keep the same marginal totals as the observed table.
# INPUTS & OUTPUTS:
#   tab : table of frequencies n_ij.
#   Returns a matrix of expected frequencies e_ij.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
expected_frequencies <- function(tab) {
  # Step 1: Plain matrix, totals and N
  n_ij <- unclass(as.matrix(tab))
  N <- sum(n_ij)
  # Step 2: e_ij = n_i. * n_.j / N
  expected <- outer(rowSums(n_ij), colSums(n_ij)) / N
  dimnames(expected) <- dimnames(n_ij)
  return(expected)
}


# ---------------------------------------------------------------------
# [CAP2-INDEPENDENCE]  is_independent(tab)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   X and Y are independent when the joint relative frequency is the
#   product of the marginals in EVERY cell:
#       f_ij = f_i. * f_.j     for all i, j
#   (equivalently n_ij = e_ij, or chi-square = 0). One failing cell is
#   enough to say "NOT independent".
# INPUTS & OUTPUTS:
#   tab : table of frequencies n_ij.
#   Returns TRUE or FALSE (compared with a tiny numerical tolerance).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
is_independent <- function(tab) {
  # Step 1: Plain matrix and total N
  n_ij <- unclass(as.matrix(tab))
  N <- sum(n_ij)
  # Step 2: Joint relative frequencies f_ij and product of marginals f_i. * f_.j
  joint_relative <- n_ij / N
  product_of_marginals <- outer(rowSums(n_ij) / N, colSums(n_ij) / N)
  # Step 3: Are they equal in every cell?
  result <- isTRUE(all.equal(as.numeric(joint_relative), as.numeric(product_of_marginals)))
  return(result)
}


# ---------------------------------------------------------------------
# [CAP2-CHI-SQUARE]  chi_square(tab)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Distance between the observed table and the independence table:
#       X^2 = sum_ij ( n_ij - e_ij )^2 / e_ij       e_ij = n_i. * n_.j / N
#   X^2 = 0 <=> independence; the bigger X^2, the stronger the association.
# INPUTS & OUTPUTS:
#   tab : table of frequencies n_ij (no empty row or column).
#   Returns one number: the chi-square statistic.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
chi_square <- function(tab) {
  # Step 1: Observed frequencies
  observed <- unclass(as.matrix(tab))
  # Step 2: Expected frequencies under independence
  expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
  # Step 3: Add up (n_ij - e_ij)^2 / e_ij over all cells
  chi_square_value <- sum((observed - expected)^2 / expected)
  return(chi_square_value)
}


# ---------------------------------------------------------------------
# [CAP2-CONTINGENCY-C]  contingency_coefficient_C(tab)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Pearson contingency coefficient: chi-square rescaled to [0, 1):
#       C = sqrt( X^2 / (X^2 + N) )
#   C = 0 -> independence; close to 1 -> strong association (it never
#   reaches exactly 1).
# INPUTS & OUTPUTS:
#   tab : table of frequencies n_ij (no empty row or column).
#   Returns one number: C.
# DEPENDENCIES: none (chi-square is computed inside).
# ---------------------------------------------------------------------
contingency_coefficient_C <- function(tab) {
  # Step 1: Observed frequencies and total N
  observed <- unclass(as.matrix(tab))
  N <- sum(observed)
  # Step 2: Chi-square statistic
  expected <- outer(rowSums(observed), colSums(observed)) / N
  chi_square_value <- sum((observed - expected)^2 / expected)
  # Step 3: C = sqrt( X^2 / (X^2 + N) )
  coefficient_C <- sqrt(chi_square_value / (chi_square_value + N))
  return(coefficient_C)
}


# ---------------------------------------------------------------------
# [CAP2-COVARIANCE]  covariance_desc(x, y, w = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Joint variation of two variables, dividing STRICTLY by N:
#       s_xy = sum( w_i * (x_i - mean_x) * (y_i - mean_y) ) / N
#            = mean(x*y) - mean_x * mean_y
#   w_i = frequency of the pair (1 for raw data), N = sum(w_i).
#   s_xy > 0 direct relation; s_xy < 0 inverse relation.
#   R's cov() divides by N - 1: do not use it.
# INPUTS & OUTPUTS:
#   x, y : paired values.  w : optional frequency of each pair.
#   Returns one number: the covariance.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
covariance_desc <- function(x, y, w = NULL) {
  # Step 1: Frequencies (all 1 for raw data) and total N
  if (is.null(w)) w <- rep(1, length(x))
  N <- sum(w)
  # Step 2: Means
  mean_x <- sum(w * x) / N
  mean_y <- sum(w * y) / N
  # Step 3: Covariance (divide by N)
  covariance_xy <- sum(w * (x - mean_x) * (y - mean_y)) / N
  return(covariance_xy)
}


# ---------------------------------------------------------------------
# [CAP2-CORR-PEARSON]  correlation_pearson(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Linear correlation coefficient (between -1 and 1):
#       r = s_xy / ( s_x * s_y )       (all of them divide by N)
#   r^2 = coefficient of determination of the linear regression.
#   |r| = 1 perfect straight line; r = 0 no LINEAR relation.
# INPUTS & OUTPUTS:
#   x, y : paired raw data.  Returns one number: r.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
correlation_pearson <- function(x, y) {
  # Step 1: Means
  mean_x <- mean(x)
  mean_y <- mean(y)
  # Step 2: Covariance and variances (divide by N)
  cov_xy <- mean((x - mean_x) * (y - mean_y))
  var_x <- mean((x - mean_x)^2)
  var_y <- mean((y - mean_y)^2)
  # Step 3: r = s_xy / (s_x * s_y)
  r <- cov_xy / sqrt(var_x * var_y)
  return(r)
}


# ---------------------------------------------------------------------
# [CAP2-CORR-SPEARMAN]  correlation_spearman(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Rank correlation: Pearson's r computed on the RANKS (rank 1 = smallest;
#   ties get the average of their positions). Without ties:
#       rho = 1 - 6 * sum(d_i^2) / ( N * (N^2 - 1) ),  d_i = rank(x_i) - rank(y_i)
#   It measures any monotonic relation.
# INPUTS & OUTPUTS:
#   x, y : paired raw data.  Returns one number: rho.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
correlation_spearman <- function(x, y) {
  # Step 1: Ranks (ties get the average rank)
  ranks_x <- rank(x)
  ranks_y <- rank(y)
  # Step 2: Covariance and variances of the ranks (divide by N)
  cov_ranks <- mean((ranks_x - mean(ranks_x)) * (ranks_y - mean(ranks_y)))
  var_ranks_x <- mean((ranks_x - mean(ranks_x))^2)
  var_ranks_y <- mean((ranks_y - mean(ranks_y))^2)
  # Step 3: Pearson correlation of the ranks
  rho <- cov_ranks / sqrt(var_ranks_x * var_ranks_y)
  return(rho)
}


# ---------------------------------------------------------------------
# COMMON RULES FOR ALL THE REGRESSIONS BELOW (UMA course)
#   * Each model is fitted by least squares on the transformed data, but
#     SSE, MSE, RMSE and R2 are ALWAYS computed on the ORIGINAL scale of Y:
#         SSE = sum( (y_i - y_hat_i)^2 )     MSE = SSE / N
#         RMSE = sqrt(MSE)                   R2 = 1 - MSE / s_y^2
#     (s_y^2 divides by N, so R2 = 1 - SSE / SST).
#   * Every function returns a list with $a, $b, $SSE, $MSE, $RMSE, $R2,
#     $equation (text, coefficients CUT to 4 decimals) and $predict (a
#     function that predicts y for new x values).
#   * DEPENDENCIES: trunc_4() (it is used in $equation).
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# [CAP2-REG-Y-ON-X]  regression_Y_on_X(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Least-squares line that predicts Y from X:   y = a + b*x
#       b = s_xy / s_x^2            (slope; both divided by N)
#       a = mean_y - b * mean_x     (the line passes through the centre of
#                                    gravity (mean_x, mean_y))
#   R2 = r^2 for a straight line.
# INPUTS & OUTPUTS:
#   x, y : paired raw data.
#   Returns list(a, b, SSE, MSE, RMSE, R2, equation, predict).
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_Y_on_X <- function(x, y) {
  # Step 1: N, means, covariance and variances (all divide by N)
  N <- length(x)
  mean_x <- mean(x)
  mean_y <- mean(y)
  cov_xy <- sum((x - mean_x) * (y - mean_y)) / N
  var_x <- sum((x - mean_x)^2) / N
  var_y <- sum((y - mean_y)^2) / N
  # Step 2: Slope and intercept
  b <- cov_xy / var_x
  a <- mean_y - b * mean_x
  # Step 3: Residuals and errors on the original Y
  residuals <- y - (a + b * x)
  SSE <- sum(residuals^2)
  MSE <- SSE / N
  # Step 4: R2 = 1 - MSE / Var(Y), equation and prediction function
  R2 <- 1 - MSE / var_y
  equation <- sprintf("y = %.4f + (%.4f)*x", trunc_4(a), trunc_4(b))
  predict_function <- function(x_new) a + b * x_new
  return(list(a = a, b = b, SSE = SSE, MSE = MSE, RMSE = sqrt(MSE), R2 = R2,
              equation = equation, predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-X-ON-Y]  regression_X_on_Y(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Least-squares line that predicts X from Y:   x = a + b*y
#       b = s_xy / s_y^2            a = mean_x - b * mean_y
#   Solving for y gives the SAME line as  y = a_y + b_y*x  with
#       b_y = 1 / b       a_y = -a / b
#   Both regression lines cross at (mean_x, mean_y) and r^2 = b_yx * b_xy.
#   SSE, MSE, RMSE, R2 and predict() use the line y = a_y + b_y*x, so the
#   errors are measured on Y (original scale).
# INPUTS & OUTPUTS:
#   x, y : paired raw data (s_xy different from 0).
#   Returns list(a, b, a_y, b_y, r2, SSE, MSE, RMSE, R2, equation,
#   equation_y, predict); a and b belong to x = a + b*y.
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_X_on_Y <- function(x, y) {
  # Step 1: N, means, covariance and variances (all divide by N)
  N <- length(x)
  mean_x <- mean(x)
  mean_y <- mean(y)
  cov_xy <- sum((x - mean_x) * (y - mean_y)) / N
  var_x <- sum((x - mean_x)^2) / N
  var_y <- sum((y - mean_y)^2) / N
  # Step 2: Line x = a + b*y and the same line written as y = a_y + b_y*x
  b <- cov_xy / var_y
  a <- mean_x - b * mean_y
  b_y <- 1 / b
  a_y <- -a / b
  # Step 3: Errors on the original Y, using the line y = a_y + b_y*x
  SSE <- sum((y - (a_y + b_y * x))^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / var_y
  # Step 4: r^2 = b_yx * b_xy, equations and prediction function
  r2 <- cov_xy^2 / (var_x * var_y)
  equation <- sprintf("x = %.4f + (%.4f)*y", trunc_4(a), trunc_4(b))
  equation_y <- sprintf("y = %.4f + (%.4f)*x", trunc_4(a_y), trunc_4(b_y))
  predict_function <- function(x_new) a_y + b_y * x_new
  return(list(a = a, b = b, a_y = a_y, b_y = b_y, r2 = r2, SSE = SSE, MSE = MSE,
              RMSE = sqrt(MSE), R2 = R2, equation = equation, equation_y = equation_y,
              predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-EXPONENTIAL]  regression_exponential(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Model  y = a * e^(b*x). Taking logarithms it becomes a straight line:
#       ln(y) = ln(a) + b*x     (fit  v = A + b*x  with v = ln y)
#       b = s_xv / s_x^2        A = mean_v - b*mean_x      a = e^A
#   The same model can be written  y = a * (b_base)^x  with b_base = e^b.
#   Errors on the ORIGINAL y (not on ln y).
# INPUTS & OUTPUTS:
#   x, y : paired data; all y must be > 0.
#   Returns list(a, b, b_base, SSE, MSE, RMSE, R2, equation (e^(bx) form),
#   equation_base (b^x form), predict).
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_exponential <- function(x, y) {
  # Step 1: Linearize: v = ln(y)
  N <- length(x)
  v <- log(y)
  # Step 2: Least-squares line v = A + b*x, then a = e^A
  b <- sum((x - mean(x)) * (v - mean(v))) / sum((x - mean(x))^2)
  A <- mean(v) - b * mean(x)
  a <- exp(A)
  # Step 3: Errors on the ORIGINAL scale of Y
  SSE <- sum((y - a * exp(b * x))^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / (sum((y - mean(y))^2) / N)
  # Step 4: Same model with base b_base = e^b, equations and prediction function
  b_base <- exp(b)
  equation <- sprintf("y = %.4f * e^((%.4f)*x)", trunc_4(a), trunc_4(b))
  equation_base <- sprintf("y = %.4f * (%.4f)^x", trunc_4(a), trunc_4(b_base))
  predict_function <- function(x_new) a * exp(b * x_new)
  return(list(a = a, b = b, b_base = b_base, SSE = SSE, MSE = MSE, RMSE = sqrt(MSE),
              R2 = R2, equation = equation, equation_base = equation_base,
              predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-HYPERBOLIC]  regression_hyperbolic(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Model  y = 1 / (a + b*x). Inverting both sides gives a straight line:
#       1/y = a + b*x          (fit  v = a + b*x  with v = 1/y)
#       b = s_xv / s_x^2       a = mean_v - b*mean_x
#   Errors on the ORIGINAL y (not on 1/y).
# INPUTS & OUTPUTS:
#   x, y : paired data; no y can be 0.
#   Returns list(a, b, SSE, MSE, RMSE, R2, equation, predict).
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_hyperbolic <- function(x, y) {
  # Step 1: Linearize: v = 1/y
  N <- length(x)
  v <- 1 / y
  # Step 2: Least-squares line v = a + b*x
  b <- sum((x - mean(x)) * (v - mean(v))) / sum((x - mean(x))^2)
  a <- mean(v) - b * mean(x)
  # Step 3: Errors on the ORIGINAL scale of Y
  SSE <- sum((y - 1 / (a + b * x))^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / (sum((y - mean(y))^2) / N)
  # Step 4: Equation and prediction function
  equation <- sprintf("y = 1 / (%.4f + (%.4f)*x)", trunc_4(a), trunc_4(b))
  predict_function <- function(x_new) 1 / (a + b * x_new)
  return(list(a = a, b = b, SSE = SSE, MSE = MSE, RMSE = sqrt(MSE), R2 = R2,
              equation = equation, predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-LOGARITHMIC]  regression_logarithmic(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Model  y = a + b*ln(x): a straight line against u = ln(x).
#       b = s_uy / s_u^2       a = mean_y - b*mean_u
#   Errors on the ORIGINAL y.
# INPUTS & OUTPUTS:
#   x, y : paired data; all x must be > 0.
#   Returns list(a, b, SSE, MSE, RMSE, R2, equation, predict).
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_logarithmic <- function(x, y) {
  # Step 1: Linearize: u = ln(x)
  N <- length(x)
  u <- log(x)
  # Step 2: Least-squares line y = a + b*u
  b <- sum((u - mean(u)) * (y - mean(y))) / sum((u - mean(u))^2)
  a <- mean(y) - b * mean(u)
  # Step 3: Errors on the ORIGINAL scale of Y
  SSE <- sum((y - (a + b * log(x)))^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / (sum((y - mean(y))^2) / N)
  # Step 4: Equation and prediction function
  equation <- sprintf("y = %.4f + (%.4f)*ln(x)", trunc_4(a), trunc_4(b))
  predict_function <- function(x_new) a + b * log(x_new)
  return(list(a = a, b = b, SSE = SSE, MSE = MSE, RMSE = sqrt(MSE), R2 = R2,
              equation = equation, predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-POWER]  regression_power(x, y)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Model  y = a * x^b. Taking logarithms it becomes a straight line:
#       ln(y) = ln(a) + b*ln(x)    (fit  v = A + b*u,  u = ln x, v = ln y)
#       b = s_uv / s_u^2           A = mean_v - b*mean_u        a = e^A
#   b is the ELASTICITY: x up 1% changes y about b%.
#   Errors on the ORIGINAL y.
# INPUTS & OUTPUTS:
#   x, y : paired data; all x and all y must be > 0.
#   Returns list(a, b, SSE, MSE, RMSE, R2, equation, predict).
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_power <- function(x, y) {
  # Step 1: Linearize: u = ln(x), v = ln(y)
  N <- length(x)
  u <- log(x)
  v <- log(y)
  # Step 2: Least-squares line v = A + b*u, then a = e^A
  b <- sum((u - mean(u)) * (v - mean(v))) / sum((u - mean(u))^2)
  A <- mean(v) - b * mean(u)
  a <- exp(A)
  # Step 3: Errors on the ORIGINAL scale of Y
  SSE <- sum((y - a * x^b)^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / (sum((y - mean(y))^2) / N)
  # Step 4: Equation and prediction function
  equation <- sprintf("y = %.4f * x^(%.4f)", trunc_4(a), trunc_4(b))
  predict_function <- function(x_new) a * x_new^b
  return(list(a = a, b = b, SSE = SSE, MSE = MSE, RMSE = sqrt(MSE), R2 = R2,
              equation = equation, predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-POLYNOMIAL]  regression_polynomial(x, y, degree = 2)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Polynomial model (default quadratic)  y = a + b*x + c*x^2 + ...
#   Least squares in matrix form, M = columns (1, x, x^2, ...):
#       coefficients = solve( t(M) %*% M ) %*% t(M) %*% y
#   (the "normal equations"). Errors on the ORIGINAL y.
# INPUTS & OUTPUTS:
#   x, y : paired data (at least degree + 1 different x).
#   degree : degree of the polynomial (2 = quadratic).
#   Returns list(coefficients, a, b, c, SSE, MSE, RMSE, R2, equation,
#   predict); c is NA when degree = 1.
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_polynomial <- function(x, y, degree = 2) {
  # Step 1: Matrix M with columns 1, x, x^2, ...
  N <- length(x)
  M <- outer(x, 0:degree, "^")
  # Step 2: Normal equations
  coefficients <- drop(solve(t(M) %*% M) %*% t(M) %*% y)
  # Step 3: Errors on the ORIGINAL Y
  SSE <- sum((y - M %*% coefficients)^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / (sum((y - mean(y))^2) / N)
  # Step 4: Equation text "y = (a) + (b)*x + (c)*x^2"
  x_terms <- c("", "*x", if (degree >= 2) paste0("*x^", 2:degree))
  equation <- paste("y =", paste0("(", sprintf("%.4f", trunc_4(coefficients)), ")", x_terms, collapse = " + "))
  predict_function <- function(x_new) drop(outer(x_new, 0:degree, "^") %*% coefficients)
  return(list(coefficients = coefficients, a = coefficients[1], b = coefficients[2], c = coefficients[3],
              SSE = SSE, MSE = MSE, RMSE = sqrt(MSE), R2 = R2,
              equation = equation, predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-CUSTOM-LINEAR]  regression_custom_linear(X_matrix, Y_vector)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Any model LINEAR IN ITS COEFFICIENTS:  Y = c1*M1 + c2*M2 + ...
#   where each column Mj is a function of the data that YOU build:
#       coefficients = solve( t(M) %*% M ) %*% t(M) %*% Y
#   Example  Z = c + d*ln(X*Y):      M <- cbind(1, log(x*y)),  Y_vector <- z
#   Example  y = a + b*x + c*ln(x):  M <- cbind(1, x, log(x))
#   Errors on the scale of Y_vector.
# INPUTS & OUTPUTS:
#   X_matrix : matrix M, one column per term (add a column of 1s if the
#   model has a constant).  Y_vector : the variable to explain.
#   Returns list(coefficients, a, b, SSE, MSE, RMSE, R2, equation,
#   predict); a = first coefficient, b = second one; predict(new_matrix)
#   needs a matrix with the same columns.
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
regression_custom_linear <- function(X_matrix, Y_vector) {
  # Step 1: Normal equations
  M <- as.matrix(X_matrix)
  N <- length(Y_vector)
  coefficients <- drop(solve(t(M) %*% M) %*% t(M) %*% Y_vector)
  # Step 2: Errors on the scale of Y
  SSE <- sum((Y_vector - M %*% coefficients)^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / (sum((Y_vector - mean(Y_vector))^2) / N)
  # Step 3: Equation text "Y = (c1)*M1 + (c2)*M2 + ..." and prediction function
  equation <- paste("Y =", paste0("(", sprintf("%.4f", trunc_4(coefficients)), ")*M", seq_along(coefficients), collapse = " + "))
  predict_function <- function(new_matrix) drop(as.matrix(new_matrix) %*% coefficients)
  return(list(coefficients = coefficients, a = coefficients[1], b = coefficients[2],
              SSE = SSE, MSE = MSE, RMSE = sqrt(MSE), R2 = R2,
              equation = equation, predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-REG-PLANE-3D]  plane_regression(x, y, z)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Regression plane that predicts Z from X and Y:  Z = a0 + a1*X + a2*Y
#   Normal equations with M = columns (1, x, y):
#       coefficients = solve( t(M) %*% M ) %*% t(M) %*% z
#   R2 = 1 - MSE / s_z^2 (errors measured on Z).
# INPUTS & OUTPUTS:
#   x, y, z : three vectors of the same length.
#   Returns list(a0, a1, a2, SSE, MSE, RMSE, R2, equation,
#   predict(x_new, y_new)).
# DEPENDENCIES: trunc_4().
# ---------------------------------------------------------------------
plane_regression <- function(x, y, z) {
  # Step 1: Matrix M = [1, x, y] and normal equations
  N <- length(z)
  M <- cbind(1, x, y)
  coefficients <- drop(solve(t(M) %*% M) %*% t(M) %*% z)
  # Step 2: Errors on Z
  SSE <- sum((z - M %*% coefficients)^2)
  MSE <- SSE / N
  R2 <- 1 - MSE / (sum((z - mean(z))^2) / N)
  # Step 3: Equation and prediction function
  equation <- sprintf("z = %.4f + (%.4f)*x + (%.4f)*y", trunc_4(coefficients[1]), trunc_4(coefficients[2]), trunc_4(coefficients[3]))
  predict_function <- function(x_new, y_new) coefficients[[1]] + coefficients[[2]] * x_new + coefficients[[3]] * y_new
  return(list(a0 = coefficients[[1]], a1 = coefficients[[2]], a2 = coefficients[[3]],
              SSE = SSE, MSE = MSE, RMSE = sqrt(MSE), R2 = R2,
              equation = equation, predict = predict_function))
}


# ---------------------------------------------------------------------
# [CAP2-COMPARE-MODELS]  compare_models(x, y, x_predict = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Fits the six Y-models of the course and compares them ON THE ORIGINAL
#   SCALE OF Y:
#       Linear       y = a + b*x
#       Exponential  y = a * e^(b*x)       (needs y > 0)
#       Hyperbolic   y = 1 / (a + b*x)     (needs y different from 0)
#       Logarithmic  y = a + b*ln(x)       (needs x > 0)
#       Power        y = a * x^b           (needs x > 0 and y > 0)
#       Quadratic    y = a + b*x + c*x^2   (needs 3+ different x)
#   Ranking: highest R2 first (R2 = 1 - MSE / s_y^2); ties by lowest MSE.
#   The best model is the one with the highest R2, i.e. the lowest MSE.
#   A model whose data conditions fail shows NaN and goes to the bottom.
# INPUTS & OUTPUTS:
#   x, y : paired data.  x_predict : optional x value(s) to predict with
#   the BEST model.
#   Prints the table and returns (invisibly) list(table, models, best,
#   prediction).
# DEPENDENCIES: trunc_4(), regression_Y_on_X(), regression_exponential(),
#   regression_hyperbolic(), regression_logarithmic(), regression_power(),
#   regression_polynomial().
# ---------------------------------------------------------------------
compare_models <- function(x, y, x_predict = NULL) {
  # Step 1: Fit every model (each one measures its errors on the original Y)
  models <- list(Linear = regression_Y_on_X(x, y), Exponential = regression_exponential(x, y),
                 Hyperbolic = regression_hyperbolic(x, y), Logarithmic = regression_logarithmic(x, y),
                 Power = regression_power(x, y), Quadratic = regression_polynomial(x, y, 2))
  # Step 2: Comparison table sorted by R2 (high to low), then MSE (low to high)
  comparison <- data.frame(Model = names(models), R2 = sapply(models, function(m) m$R2),
                           MSE = sapply(models, function(m) m$MSE), RMSE = sapply(models, function(m) m$RMSE),
                           Equation = sapply(models, function(m) m$equation), row.names = NULL)
  comparison <- comparison[order(-comparison$R2, comparison$MSE), ]
  # Step 3: Print the table with values cut to 4 decimals
  cat("MODEL COMPARISON (sorted by R2, then MSE)\n")
  for (i in 1:nrow(comparison)) {
    cat(sprintf("%d. %-12s R2=%.4f  MSE=%.4f  RMSE=%.4f  %s\n", i, comparison$Model[i], trunc_4(comparison$R2[i]),
                trunc_4(comparison$MSE[i]), trunc_4(comparison$RMSE[i]), comparison$Equation[i]))
  }
  # Step 4: Best model and, if asked, its predictions
  best <- comparison$Model[1]
  prediction <- if (is.null(x_predict)) NULL else models[[best]]$predict(x_predict)
  cat("Best model:", best, "\n")
  if (!is.null(x_predict)) cat(sprintf("Prediction at x=%s: y=%.4f\n", x_predict, trunc_4(prediction)), sep = "")
  return(invisible(list(table = comparison, models = models, best = best, prediction = prediction)))
}


# =====================================================================
# BLOCK 3: EXAM ALGEBRA & DEDUCTION SOLVERS
# =====================================================================

# ---------------------------------------------------------------------
# [ALGEBRA-HISTOGRAM]  reconstruct_histogram(h_i, a_i, N = NULL)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   In a histogram the AREA of each bar is the frequency:
#       n_i = h_i * a_i        (height h_i = density, width a_i)
#   If the heights are only proportional and the total N is known, rescale:
#       n_i = h_i * a_i * N / sum(h_i * a_i)
#   Then f_i = n_i / N and N_i, F_i are the cumulative columns.
# INPUTS & OUTPUTS:
#   h_i : bar heights.  a_i : class widths.  N : optional total.
#   Returns data.frame with h_i, a_i, n_i, f_i, N_i, F_i.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
reconstruct_histogram <- function(h_i, a_i, N = NULL) {
  # Step 1: Absolute frequencies = area of each bar
  ni <- h_i * a_i
  # Step 2: Rescale so that the frequencies add up to N (if N is known)
  if (!is.null(N)) ni <- ni * N / sum(ni)
  # Step 3: Relative and cumulative frequencies
  total <- sum(ni)
  table_result <- data.frame(h_i = h_i, a_i = a_i, n_i = ni, f_i = ni / total,
                             N_i = cumsum(ni), F_i = cumsum(ni) / total)
  return(table_result)
}


# ---------------------------------------------------------------------
# [ALGEBRA-TWO-OBSERVATIONS]  deduce_sample_two_obs(mean_val, sd_val, quasi = FALSE)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Sample of N = 2 observations x1, x2:
#       mean = (x1 + x2) / 2
#       s_n^2 = ((x1 - mean)^2 + (x2 - mean)^2) / 2 = ((x2 - x1) / 2)^2
#   so the two data are symmetric around the mean:
#       x1, x2 = mean_val -/+ sd_val        (sd_val = s_n, divides by N)
#   If the given sd is the QUASI-standard deviation (divides by N - 1 = 1)
#   then s^2 = 2 * s_n^2 and  x1, x2 = mean_val -/+ s / sqrt(2)  (quasi = TRUE).
# INPUTS & OUTPUTS:
#   mean_val : mean.  sd_val : standard deviation.  quasi : TRUE if the
#   standard deviation divides by N - 1.  Returns c(x1, x2), x1 <= x2.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
deduce_sample_two_obs <- function(mean_val, sd_val, quasi = FALSE) {
  # Step 1: Distance from the mean to each observation
  distance <- if (quasi) sd_val / sqrt(2) else sd_val
  # Step 2: The two observations are symmetric around the mean
  observations <- c(mean_val - distance, mean_val + distance)
  return(observations)
}


# ---------------------------------------------------------------------
# [ALGEBRA-REGRESSION-LINES]  identify_regression_lines(line1_coefs, line2_coefs)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Two regression lines are given as  A*x + B*y = C  ->  c(A, B, C).
#   One is Y on X (y = a + b_yx*x), the other is X on Y (x = a' + b_xy*y).
#   If line 1 is Y/X and line 2 is X/Y:
#       b_yx = -A1 / B1        b_xy = -B2 / A2        r^2 = b_yx * b_xy
#   The right assignment gives r^2 <= 1 (the wrong one gives 1 / r^2 > 1).
#   Both lines cross at the CENTRE OF GRAVITY (mean_x, mean_y): solve the
#   2x2 system. The sign of r is the sign of the slopes.
# INPUTS & OUTPUTS:
#   line1_coefs, line2_coefs : c(A, B, C) of each line.
#   Returns list(Y_on_X_line (1 or 2), X_on_Y_line, slope_Y_on_X,
#   slope_X_on_Y, intercept_Y_on_X, intercept_X_on_Y, mean_x, mean_y,
#   r2, r).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
identify_regression_lines <- function(line1_coefs, line2_coefs) {
  # Step 1: Assume line 1 is Y/X and line 2 is X/Y
  y_on_x <- line1_coefs
  x_on_y <- line2_coefs
  y_on_x_line <- 1
  r2 <- (-y_on_x[1] / y_on_x[2]) * (-x_on_y[2] / x_on_y[1])
  # Step 2: r^2 cannot be bigger than 1: if it is, swap the roles
  if (r2 > 1 + 1e-9) {
    y_on_x <- line2_coefs
    x_on_y <- line1_coefs
    y_on_x_line <- 2
  }
  # Step 3: Slopes, r^2 and r (with the sign of the slopes)
  b_yx <- -y_on_x[1] / y_on_x[2]
  b_xy <- -x_on_y[2] / x_on_y[1]
  r2 <- min(b_yx * b_xy, 1)
  # Step 4: Centre of gravity = crossing point of the two lines
  center <- solve(rbind(line1_coefs[1:2], line2_coefs[1:2]), c(line1_coefs[3], line2_coefs[3]))
  return(list(Y_on_X_line = y_on_x_line, X_on_Y_line = 3 - y_on_x_line,
              slope_Y_on_X = b_yx, slope_X_on_Y = b_xy,
              intercept_Y_on_X = y_on_x[3] / y_on_x[2], intercept_X_on_Y = x_on_y[3] / x_on_y[1],
              mean_x = center[[1]], mean_y = center[[2]], r2 = r2, r = sign(b_yx) * sqrt(r2)))
}


# =====================================================================
# BLOCK 4: TIME SERIES & INDEXES (CHAPTER 3)
# =====================================================================

# ---------------------------------------------------------------------
# [CAP3-TS-CREATE]  create_timeseries(data, start, frequency)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   No formula: wraps the observations X_1, ..., X_N in a "ts" object that
#   knows its calendar. frequency = k = observations per year (4 =
#   quarterly, 12 = monthly). The decomposition functions need it.
#   Example: create_timeseries(c(5, 7, 6, 9), start = c(2020, 1),
#   frequency = 4) -> first datum = 1st quarter of 2020.
# INPUTS & OUTPUTS:
#   data : observations in time order.  start : c(year, season) of the
#   first one.  frequency : seasons per year.  Returns a ts object.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
create_timeseries <- function(data, start, frequency) {
  # Step 1: Build the time series
  time_series <- stats::ts(data, start = start, frequency = frequency)
  return(time_series)
}


# ---------------------------------------------------------------------
# [CAP3-MOVING-AVERAGE]  centered_moving_average(x, k)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Moving average of order k = mean of k consecutive data placed at the
#   CENTRE of the window; it smooths the seasonality and shows trend-cycle.
#       k odd :  weights 1/k, 1/k, ..., 1/k
#       k even:  weights 1/(2k), 1/k, ..., 1/k, 1/(2k)   (k + 1 weights, so
#                the average falls exactly on an observation)
#   The first and last values cannot be computed and are NA (k/2 at each
#   end for even k, (k-1)/2 for odd k).
# INPUTS & OUTPUTS:
#   x : numeric vector or ts.  k : order of the moving average.
#   Returns the moving average (a ts if x is a ts) with NA at the ends.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
centered_moving_average <- function(x, k) {
  # Step 1: Weights (k even: half weight at both ends)
  if (k %% 2 == 0) weights <- c(0.5, rep(1, k - 1), 0.5) / k else weights <- rep(1 / k, k)
  # Step 2: Centered moving average
  moving_average <- as.numeric(stats::filter(as.numeric(x), weights, sides = 2))
  # Step 3: Keep the calendar if x was a time series
  if (stats::is.ts(x)) moving_average <- stats::ts(moving_average, start = stats::start(x), frequency = stats::frequency(x))
  return(moving_average)
}


# ---------------------------------------------------------------------
# [CAP3-DECOMP-MULTIPLICATIVE]  seasonal_decomposition_multiplicative(x_ts)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Multiplicative model  X_t = T * C * E * A  (trend T, cycle C, seasonal
#   E, accidental A). UMA method:
#     1) T*C = centered moving average of order k (k = frequency)
#     2) E*A = X_t / (T*C)
#     3) raw seasonal index of each season = mean of its E*A values
#     4) IVE (seasonal variation indices) = raw indices * k / sum(raw)
#        so the k indices ADD UP TO k (average index = 1)
#     5) deseasonalised series D_t = X_t / IVE_season
#     6) trend line D_t = a + b*t (t = 1..N) by least squares
#   Forecast = (a + b*t) * IVE_season  (see forecast_timeseries).
# INPUTS & OUTPUTS:
#   x_ts : ts object (k = frequency, all values > 0, N >= 2k).
#   Returns list(type, k, N, start_position, Trend_Cycle,
#   Seasonal_Irregular, Seasonal_Indices, IVE, Deseasonalized_Series,
#   Trend_Coefficients c(a, b), Fitted_Trend).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
seasonal_decomposition_multiplicative <- function(x_ts) {
  # Step 1: Period k, number of data N, values and season (1..k) of each datum
  k <- stats::frequency(x_ts)
  N <- length(x_ts)
  x_values <- as.numeric(x_ts)
  season <- as.integer(stats::cycle(x_ts))
  # Step 2: Trend-cycle = centered moving average of order k
  weights <- if (k %% 2 == 0) c(0.5, rep(1, k - 1), 0.5) / k else rep(1 / k, k)
  trend_cycle <- as.numeric(stats::filter(x_values, weights, sides = 2))
  # Step 3: Seasonal * irregular = X_t / (T*C), averaged by season
  seasonal_irregular <- x_values / trend_cycle
  raw_indices <- as.numeric(tapply(seasonal_irregular, season, mean, na.rm = TRUE))
  # Step 4: IVE: the k indices add up to k
  IVE <- raw_indices * k / sum(raw_indices)
  # Step 5: Deseasonalised series and its trend line D_t = a + b*t
  deseasonalized <- x_values / IVE[season]
  t_values <- 1:N
  b <- sum((t_values - mean(t_values)) * (deseasonalized - mean(deseasonalized))) / sum((t_values - mean(t_values))^2)
  a <- mean(deseasonalized) - b * mean(t_values)
  # Step 6: Give the calendar back to the series
  to_ts <- function(values) stats::ts(values, start = stats::start(x_ts), frequency = k)
  return(list(type = "multiplicative", k = k, N = N, start_position = season[1],
              Trend_Cycle = to_ts(trend_cycle), Seasonal_Irregular = to_ts(seasonal_irregular),
              Seasonal_Indices = IVE, IVE = IVE, Deseasonalized_Series = to_ts(deseasonalized),
              Trend_Coefficients = c(a = a, b = b), Fitted_Trend = to_ts(a + b * t_values)))
}


# ---------------------------------------------------------------------
# [CAP3-DECOMP-ADDITIVE]  seasonal_decomposition_additive(x_ts)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Additive model  X_t = T + E + A  (trend-cycle T, seasonal E, accidental
#   A). Same method as the multiplicative one, with differences:
#     1) T = centered moving average of order k (k = frequency)
#     2) E + A = X_t - T
#     3) raw seasonal index of each season = mean of its E + A values
#     4) IVE = raw indices - mean(raw indices)  -> they ADD UP TO 0
#     5) deseasonalised series D_t = X_t - IVE_season
#     6) trend line D_t = a + b*t (t = 1..N) by least squares
#   Forecast = (a + b*t) + IVE_season  (see forecast_timeseries).
# INPUTS & OUTPUTS:
#   x_ts : ts object (k = frequency, N >= 2k).
#   Returns list(type, k, N, start_position, Trend_Cycle,
#   Seasonal_Irregular, Seasonal_Indices, IVE, Deseasonalized_Series,
#   Trend_Coefficients c(a, b), Fitted_Trend).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
seasonal_decomposition_additive <- function(x_ts) {
  # Step 1: Period k, number of data N, values and season (1..k) of each datum
  k <- stats::frequency(x_ts)
  N <- length(x_ts)
  x_values <- as.numeric(x_ts)
  season <- as.integer(stats::cycle(x_ts))
  # Step 2: Trend-cycle = centered moving average of order k
  weights <- if (k %% 2 == 0) c(0.5, rep(1, k - 1), 0.5) / k else rep(1 / k, k)
  trend_cycle <- as.numeric(stats::filter(x_values, weights, sides = 2))
  # Step 3: Seasonal + irregular = X_t - T, averaged by season
  seasonal_irregular <- x_values - trend_cycle
  raw_indices <- as.numeric(tapply(seasonal_irregular, season, mean, na.rm = TRUE))
  # Step 4: IVE: the k indices add up to 0
  IVE <- raw_indices - mean(raw_indices)
  # Step 5: Deseasonalised series and its trend line D_t = a + b*t
  deseasonalized <- x_values - IVE[season]
  t_values <- 1:N
  b <- sum((t_values - mean(t_values)) * (deseasonalized - mean(deseasonalized))) / sum((t_values - mean(t_values))^2)
  a <- mean(deseasonalized) - b * mean(t_values)
  # Step 6: Give the calendar back to the series
  to_ts <- function(values) stats::ts(values, start = stats::start(x_ts), frequency = k)
  return(list(type = "additive", k = k, N = N, start_position = season[1],
              Trend_Cycle = to_ts(trend_cycle), Seasonal_Irregular = to_ts(seasonal_irregular),
              Seasonal_Indices = IVE, IVE = IVE, Deseasonalized_Series = to_ts(deseasonalized),
              Trend_Coefficients = c(a = a, b = b), Fitted_Trend = to_ts(a + b * t_values)))
}


# ---------------------------------------------------------------------
# [CAP3-FORECAST]  forecast_timeseries(decomp_obj, periods_future)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Extends the trend line D_t = a + b*t to t = N+1, N+2, ... and puts the
#   seasonality back:
#       multiplicative:  forecast_t = (a + b*t) * IVE_season(t)
#       additive      :  forecast_t = (a + b*t) + IVE_season(t)
#   season(t) = position inside the year of time t (the series does not
#   have to start in season 1).
# INPUTS & OUTPUTS:
#   decomp_obj : result of seasonal_decomposition_multiplicative() or
#   seasonal_decomposition_additive().  periods_future : how many future
#   periods.  Returns data.frame with t, Trend, Seasonal_Index, Forecast.
# DEPENDENCIES: a decomposition object (it is the input).
# ---------------------------------------------------------------------
forecast_timeseries <- function(decomp_obj, periods_future) {
  # Step 1: Future times N+1, N+2, ...
  future_t <- decomp_obj$N + 1:periods_future
  # Step 2: Trend line a + b*t
  future_trend <- decomp_obj$Trend_Coefficients[["a"]] + decomp_obj$Trend_Coefficients[["b"]] * future_t
  # Step 3: Season of each future time and its index IVE
  future_season <- (decomp_obj$start_position - 1 + future_t - 1) %% decomp_obj$k + 1
  future_ive <- unname(decomp_obj$IVE[future_season])
  # Step 4: Put the seasonality back
  if (decomp_obj$type == "multiplicative") forecast_values <- future_trend * future_ive else forecast_values <- future_trend + future_ive
  return(data.frame(t = future_t, Trend = future_trend, Seasonal_Index = future_ive, Forecast = forecast_values))
}


# ---------------------------------------------------------------------
# [CAP3-AUTOCORRELATION]  autocorrelation(data, lag, method = "pearson")
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Correlation of the series with itself shifted k periods (lag k).
#   method "pearson" (UMA slides): ordinary Pearson correlation between
#       the pairs (X_t, X_(t+k)), t = 1..N-k, i.e. the first N-k values
#       against the last N-k values.
#   method "acf": R's classical estimator (stats::acf), which uses the
#       global mean and variance; the numbers are slightly different.
# INPUTS & OUTPUTS:
#   data : numeric vector or ts.  lag : one or several lags k.
#   method : "pearson" (default) or "acf".
#   Returns a named vector (lag_1, lag_2, ...).
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
autocorrelation <- function(data, lag, method = "pearson") {
  # Step 1: Plain vector and its length N
  x <- as.numeric(data)
  N <- length(x)
  result <- numeric(length(lag))
  for (j in seq_along(lag)) {
    # Step 2: Pearson correlation between the first N-k and the last N-k data
    if (method == "pearson") result[j] <- stats::cor(x[1:(N - lag[j])], x[(lag[j] + 1):N])
    else result[j] <- stats::acf(x, lag.max = lag[j], plot = FALSE)$acf[lag[j] + 1]
  }
  names(result) <- paste0("lag_", lag)
  return(result)
}


# ---------------------------------------------------------------------
# [CAP3-SIMPLE-INDEX]  simple_index(x, base_index = 1, base = 100)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Simple index number: value of each period relative to the BASE period:
#       I_t = X_t / X_base * 100
#   The base period always has index 100. I_t = 125 means +25% with
#   respect to the base.
# INPUTS & OUTPUTS:
#   x : values in time order.  base_index : POSITION (1, 2, 3...) of the
#   base period in x.  base : value given to the base (100).
#   Returns a vector with the index of every period.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
simple_index <- function(x, base_index = 1, base = 100) {
  # Step 1: I_t = X_t / X_base * 100
  index_values <- as.numeric(x) / x[base_index] * base
  return(index_values)
}


# ---------------------------------------------------------------------
# [CAP3-DEFLATE]  deflate(nominal_values, price_index)
# MATHEMATICAL INTUITION & UMA FORMULA:
#   Deflating removes the effect of prices: REAL (constant-price) values
#   from NOMINAL (current-price) values:
#       real_t = nominal_t / ( price_index_t / 100 )
#   The price index must have base 100 in the reference period.
# INPUTS & OUTPUTS:
#   nominal_values : nominal values in time order.  price_index : price
#   index (base 100) of each period.  Returns the real values.
# DEPENDENCIES: none.
# ---------------------------------------------------------------------
deflate <- function(nominal_values, price_index) {
  # Step 1: real = nominal / (index / 100)
  real_values <- nominal_values / (price_index / 100)
  return(real_values)
}
