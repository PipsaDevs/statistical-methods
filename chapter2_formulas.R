#Get data "x" (Ex: c(1,2,3,4,5,6)) from a vector to a table in R, by row, and with "NRows" rows
tableFromVector <- function(x, NRows) {
  as.table(matrix(x, nrow = NRows, byrow = TRUE))
}
