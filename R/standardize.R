#' Standardize a numeric vector
#'
#' For a numeric vector, center the values such that the mean is zero, and scale such that the standard deviation is one.
#'
#' @param x A numeric vector.
#'
#' @examples
#' standardize(100:104)
#'
#' @export
standardize <- function(x) {

  checkmate::assert_numeric(x)

  stddev <- stats::sd(x, na.rm = TRUE)
  avg <- mean(x, na.rm = TRUE)
  out <- (x - avg) / stddev

  return(out)
}

