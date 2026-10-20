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
combination <- function(x, y){
  choose(x, y)
}
