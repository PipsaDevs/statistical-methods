#Get data "x" (Ex: c(1,2,3,4,5,6)) from a vector to a table in R, by row, and with "NRows" rows
tableFromVector <- function(x, NRows) {
  as.table(matrix(x, nrow = NRows, byrow = TRUE))
}

#Get data from the two variables "X" and "Y" from their vectors to a table in R
tableFromVariables <- function(X, Y) {
  table(X, Y)
}

#Get the marginal X data from your table
marginalX <- function(table) {
  margin.table(table, 1)
}

#Get the marginal Y data from your table
marginalY <- function(table) {
  margin.table(table, 2)
}

#Check if your variables are independent
isIndependent <- function(table) {
  freqs <- relative_freq(table) #Get relative frequencies

  relativeMargX <- marginalX(freqs) #Marginal of relative X
  relativeMargY <- marginalY(freqs) #Marginal of relative Y

  relativeProducts <- outer(relativeMargX, relativeMargY, "*") #Products of the marginals (what would result if variables were independent)

  return(isTRUE(all.equal(c(freqs), c(relativeProducts))))
}
