# Basic stuff
# Absolute frequency (count how many times x number appears in array)
absolute_freq <- function(x) table(x)

# How many times a number appears in an array
absolute_freq_of_number <- function(x, y) {
  length(which(x == y))
}

# Relative frequency (f_i = n_i / N)
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

