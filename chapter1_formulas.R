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
  return(mean(abs(x - p)))
}


