#' Combinations (n choose k)
#'
#' Computes the number of combinations of x items taken y at a time without repetition: choose(x, y).
#'
#' @param x Total number of items (n).
#' @param y Number of items to choose (k).
#' @return Number of possible combinations.
#' @examples
#' combination(5, 2)
#' @export
pipsa_combination <- function(x, y) {
  choose(x, y)
}

#' Conditional Probability
#'
#' Computes the conditional probability between two arrays of numbers
#'
#' @param x An array
#' @param y An array
#' @return The percentage
#' @examples
#' conditional_probability(c(0.1, 0.2, 0.3), c(0.01, 0.02, 0.03))
#' @export
pipsa_conditional_probability <- function(x, y) {
  # both x and y will be an array of probabilities, c(a, b, c, ...)

  d <- 0

  for (i in 1:length(x)) {
    d <- d + x[i] * y[i]
  }

  print(paste(d, "%"))

  return(d)

}
