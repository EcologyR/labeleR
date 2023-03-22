#' Make a scatterplot of two numeric variables
#'
#' Make a scatterplot of two numeric variables using [ggplot2::ggplot()].
#'
#' @param var1 Numeric vector
#' @param var2 Numeric vector
#' @param ... Optional arguments to be passed to [ggplot2::labs()] such as `title`,
#' `x` or `y` to define plot title, horizontal axis or vertical axis names,
#' respectively
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' data("exampledata")
#' make_scatterplot(exampledata$x, exampledata$y, title = "My scatterplot")

make_scatterplot <- function(var1 = NULL,
                             var2 = NULL,
                             ...) {

  df <- tibble::tibble(var1, var2)
  df.arrange <- dplyr::arrange(df, var1)

  ggplot(df.arrange) +
    geom_point(aes(var1, var2)) +
    labs(...)

}
