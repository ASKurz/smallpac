#' Standardize a numeric vector
#'
#' @export
standardize <- function(x, center_only = FALSE) {

  checkmate::assert_numeric(x)
  checkmate::assert_logical(center_only)

  stddev <- stats::sd(x, na.rm = TRUE)
  avg <- mean(x, na.rm = TRUE)

  if(isTRUE(center_only)) {
    out <- x - avg
  } else {
    out <- (x - avg) / stddev
  }
  return(out)
}

