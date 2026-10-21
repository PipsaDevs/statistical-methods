#' Matrix / Vector to Contingency Table
#'
#' Converts a raw vector into a contingency table by row with a specified number of rows.
#'
#' @param x A vector of table cell counts.
#' @param NRows Integer number of rows for the matrix.
#' @return An R `table` object.
#' @examples
#' tableFromVector(c(10, 20, 30, 40), NRows = 2)
#' @export
tableFromVector <- function(x, NRows) {
  as.table(matrix(x, nrow = NRows, byrow = TRUE))
}

#' Two-Variable Contingency Table
#'
#' Creates a 2-way frequency table from two vectors X and Y.
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return A contingency table.
#' @examples
#' tableFromVariables(c(1, 1, 2, 2), c("A", "B", "A", "B"))
#' @export
tableFromVariables <- function(x, y) {
  table(x, y)
}

#' Reconstruct Observation Vectors from Contingency Table
#'
#' Expands a contingency table back into individual observation vectors (x and y).
#'
#' @param tbl A contingency table.
#' @return A data.frame with two columns: `x` and `y`.
#' @examples
#' tbl <- table(c(1, 1, 2), c(3, 4, 4))
#' tableToVectors(tbl)
#' @export
tableToVectors <- function(tbl) {
  df_freq <- as.data.frame(tbl)

  x_vec <- rep(df_freq[[1]], times = df_freq$Freq)
  y_vec <- rep(df_freq[[2]], times = df_freq$Freq)

  if (!any(is.na(suppressWarnings(as.numeric(as.character(x_vec)))))) {
    x_vec <- as.numeric(as.character(x_vec))
  }
  if (!any(is.na(suppressWarnings(as.numeric(as.character(y_vec)))))) {
    y_vec <- as.numeric(as.character(y_vec))
  }

  return(data.frame(x = x_vec, y = y_vec)) # Returns data frame d. You can access d$x and d$y for the variable x vector and the variable y vector
}

#' Marginal Distribution of X
#'
#' Computes the marginal frequency distribution for variable X (rows).
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return A 1D table of marginal frequencies for X.
#' @examples
#' marginalX(c(1, 1, 2, 2), c(3, 4, 3, 4))
#' @export
marginalX <- function(x, y) {
  margin.table(tableFromVariables(x, y), 1)
}

#' Marginal Distribution of Y
#'
#' Computes the marginal frequency distribution for variable Y (columns).
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return A 1D table of marginal frequencies for Y.
#' @examples
#' marginalY(c(1, 1, 2, 2), c(3, 4, 3, 4))
#' @export
marginalY <- function(x, y) {
  margin.table(tableFromVariables(x, y), 2)
}

#' Test for Statistical Independence
#'
#' Checks whether two variables X and Y are statistically independent (f_ij == f_i * f_j).
#'
#' @param x A vector for variable X.
#' @param y A vector for variable Y.
#' @return Logical `TRUE` if independent, `FALSE` otherwise.
#' @examples
#' isIndependent(c(1, 1, 2, 2), c(3, 4, 3, 4))
#' @export
isIndependent <- function(x, y) {
  tbl <- tableFromVariables(x, y)
  freqs <- prop.table(tbl)

  relativeProducts <- outer(margin.table(freqs, 1), margin.table(freqs, 2), "*")

  return(isTRUE(all.equal(c(freqs), c(relativeProducts))))
}

#' Population Covariance
#'
#' Computes the population covariance between variables X and Y: mean((x - mean(x)) * (y - mean(y))).
#'
#' @param x Numeric vector for X.
#' @param y Numeric vector for Y.
#' @return Numeric covariance value.
#' @examples
#' covariance(c(1, 2, 3), c(2, 4, 6))
#' @export
covariance <- function(x, y) {
  mean((x - mean(x)) * (y - mean(y)))
}

#' Pearson Linear Correlation
#'
#' Computes the Pearson linear correlation coefficient: cov(X, Y) / (std_dev(X) * std_dev(Y)).
#'
#' @param x Numeric vector for X.
#' @param y Numeric vector for Y.
#' @return Numeric correlation between -1 and 1.
#' @examples
#' linear_correlation(c(1, 2, 3, 4), c(2, 4, 6, 8))
#' @export
linear_correlation <- function(x, y) {
  covariance(x, y) / (std_deviation(x) * std_deviation(y))
}

#' Conditional Distribution Table Generator
#'
#' Computes the conditional distribution
#' (absolute and relative frequencies) given a condition variable.
#'
#' @param values A contingency table or matrix with row/column names.
#' @param condition_var Either "x" (conditioning on row value)
#' or "y" (conditioning on col value).
#' @param value The specific value conditioned on (character or numeric).
#' @return A data.frame with counts and relative frequencies.
#' @examples
#' tbl <- table(c(1, 1, 2, 2), c("A", "B", "A", "B"))
#' cond_dist_gen(tbl, condition_var = "x", value = "1")
#' @export
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

#' Bivariate Joint Moment of Order (r, s) w.r.t. (a, b)
#'
#' Computes bivariate joint moment sum((x - a)^r * (y - b)^s * `values[i, j]`).
#'
#' @param values A matrix/table of joint frequencies
#' with numeric row and column names.
#' @param r Integer order for X.
#' @param s Integer order for Y.
#' @param a Numeric reference point for X.
#' @param b Numeric reference point for Y.
#' @return Numeric moment value.
#' @export
mom_ord <- function(values, r, s, a, b) {
  x <- as.numeric(rownames(values))
  y <- as.numeric(colnames(values))
  res <- 0
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      res <- res + (((x[i] - a)^r) * ((y[j] - b)^s) * values[i, j])
    }
  }
  return(res)
}

#' Bivariate Joint Central Moment
#'
#' Computes the bivariate central moment
#' of order (r, s) about means of row/col names.
#'
#' @param values A matrix/table of joint frequencies
#' with numeric row and column names.
#' @param r Integer order for X.
#' @param s Integer order for Y.
#' @return Numeric central moment value.
#' @export
mom_cent <- function(values, r, s) {
  mom_ord(
    values,
    r,
    s,
    mean(as.numeric(rownames(values))),
    mean(as.numeric(colnames(values)))
  )
}

#' Regression Curve Fitting & Evaluation
#'
#' Fits a mathematical model to bivariate data (X, Y)
#' and calculates Model, MSE, Var(Y), and R^2.
#'
#' @param dataX Numeric vector for independent variable X.
#' @param dataY Numeric vector for dependent variable Y.
#' @param fitting Character specifying model type:
#'   - "l": Linear (y = a + bx)
#'   - "e": Exponential (y = a * x^b)
#'   - "i": Hyperbolic/Inverse (y = 1 / (a + bx))
#'   - "q": Quadratic (y = ax^2 + bx + c)
#'   - "t": Trigonometric (y = a·sin(x) + b·cos(x))
#' @return A named list: Model, MSE, Variance of Y, R^2, Plot, Curve.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 5, 8, 10)
#' modelFunc(x, y, fitting = "l")
#' @export
modelFunc <- function(dataX, dataY, fitting) {
  n <- length(dataY)
  err <- dataY

  # preparing graphical info
  p <- plot(dataX, dataY)
  curve <- NULL

  # the function must perform different
  # operations depending on the type of fitting specified,
  # but it mostly follows the same steps for each type
  if (fitting == "l") {
    # LINEAR
    model <- lm(dataY ~ dataX)

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve <- curve(
      (a + b * x),
      from = min(dataX), to = max(dataX), col = "blue", add = TRUE
    )

    yEst <- a + b * dataX
    err <- err - yEst
  } else if (fitting == "e") {
    # EXPONENTIAL
    lnX <- log(dataX)
    lnY <- log(dataY)

    model <- lm(lnY ~ lnX)

    a <- exp(coef(model))[1]
    b <- coef(model)[2]

    curve <- curve(
      (a * x^b),
      from = min(dataX), to = max(dataX), col = "green", add = TRUE
    )

    yEst <- a * x^b
    err <- err - yEst
  } else if (fitting == "i") {
    # HYPERBOLIC / INVERSE
    model <- lm(1 / dataY ~ dataX)

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve <- curve(
      (1 / (a + b * x)),
      from = min(dataX), to = max(dataX), col = "red", add = TRUE
    )

    yEst <- 1 / (a + b * dataX)
    err <- err - yEst
  } else if (fitting == "q") {
    # QUADRATIC
    dataX2 <- dataX^2

    model <- lm(dataY ~ dataX + dataX2)

    a <- coef(model)[3]
    b <- coef(model)[2]
    c <- coef(model)[1]

    curve <- curve(
      (a * (x^2) + (b * x) + c),
      from = min(dataX), to = max(dataX), col = "black", add = TRUE
    )

    yEst <- a * (dataX2) + (b * dataX) + c
    err <- err - yEst
  } else if (fitting == "t") {
    model <- lm(dataY ~ 0 + sin(dataX) + cos(dataX))

    a <- coef(model)[1]
    b <- coef(model)[2]

    curve <- curve(
      (a * sin(x) + b * cos(x)),
      from = min(dataX), to = max(dataX), col = "purple", add = TRUE
    )

    yEst <- a * sin(dataX) + b * cos(dataX)
    err <- err - yEst
  }

  # calculation of final variables
  MSE <- mean(err^2)
  varY <- var(dataY) * (n - 1) / n
  R2 <- 1 - (MSE / varY)

  result <- list(
    "Model" = model,
    "MSE" = MSE,
    "VarianceY" = varY,
    "R^2" = R2,
    "Plot" = p,
    "Curve" = curve
  )
  return(result)
}
