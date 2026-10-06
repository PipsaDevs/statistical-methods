# Get data "x" (Ex: c(1,2,3,4,5,6)) from a vector to a table in R, by row, and with "NRows" rows
tableFromVector <- function(x, NRows) {
  as.table(matrix(x, nrow = NRows, byrow = TRUE))
}

# Get data from the two variables "X" and "Y" from their vectors to a table in R
tableFromVariables <- function(X, Y) {
  table(X, Y)
}

# Get the marginal X data from your table
marginalX <- function(table) {
  margin.table(table, 1)
}

# Get the marginal Y data from your table
marginalY <- function(table) {
  margin.table(table, 2)
}

# Check if your variables are independent
isIndependent <- function(table) {
  freqs <- relative_freq(table) # Get relative frequencies
  
  relativeMargX <- marginalX(freqs) # Marginal of relative X
  relativeMargY <- marginalY(freqs) # Marginal of relative Y
  
  relativeProducts <- outer(relativeMargX, relativeMargY, "*") # Products of the marginals (what would result if variables were independent)
  
  return(isTRUE(all.equal(c(freqs), c(relativeProducts))))
}

# Get the covariance
covariance <- function(x, y) {
  mean((x - mean(x)) * (y - mean(y)))
}

# Get the linear correlation by using the covariance
linear_correlation <- function(x, y) {
  covariance(x, y) / (std_deviation(x) * std_deviation(y))
}

# Conditional distribution table generator with abs and rel freq
cond_dist_gen <- function(values, condition_var = c("x", "y"), value) {
  condition_var <- match.arg(condition_var)
  if (condition_var == "x") {
    counts <- values[as.character(value), ]
    rel_freq <- counts / sum(counts)
    data.frame(
      y = names(counts),
      absolute = as.vector(counts),
      relative = as.vector(rel_freq)
    )
  } else {
    counts <- values[, as.character(value)]
    rel_freq <- counts / sum(counts)
    data.frame(
      x = names(counts),
      absolute = as.vector(counts),
      relative = as.vector(rel_freq)
    )
  }
}

# Moment of order r, s
mom_ord <- function(values, r, s, a, b) {
  x <- as.numeric(rownames(values))
  y <- as.numeric(colnames(values))
  res <- 0
  for(i in 1:length(x)) {
    for(j in 1:length(y)){
      res <- res + (((x[i] - a)^r)*((y[j] - b)^s)*values[i, j])
    }
  }
  return(res)
}

# Central moment
mom_cent <- function(values, r, s) {
  mom_ord(values, 
          r, 
          s, 
          mean(as.numeric(rownames(values))), 
          mean(as.numeric(colnames(values)))
          )
}
