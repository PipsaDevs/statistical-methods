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
combination <- function(x, y) {
  choose(x, y)
}

conditional_probability <- function(x, y) {
  # both x and y will be an array of probabilities, c(a, b, c, ...)

  d <- 0

  for (i in 1:length(x)) {
    d <- d + x[i] * y[i]
  }

  print(paste(d, "%"))

}
