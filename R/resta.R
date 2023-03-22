#' Subtract numeric values
#'
#' @param x Numeric vector
#' @param y Numeric vector. Values to subtract from x
#' @param warn.negative Logical Warn if result is negative
#'
#' @return A number or numeric vector of the same length as x
#' @export
#'
#' @examples
#' resta(2, 3)
#' resta(c(2, 2), c(3, 3))
#'
#' \dontrun{
#' resta(c(2, 2), c(3, 3), warn.negative = TRUE)  # gives warning
#' }


resta <- function(x = NULL, y = NULL, warn.negative = FALSE) {

  ## Check arguments

  stopifnot(is.numeric(x), is.numeric(y))

  if (length(x) != length(y)) {
    stop("x and y must have same length")
  }

  ## Calculate value
  valor <- x - y

  ## Check if negative values are not allowed
  if (isTRUE(warn.negative)) {
    if (any(valor < 0)) {
      warning("This function returned a negative value")
    }
  }

  return(valor)


}
