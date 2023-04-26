
#' Title
#'
#' @param x an object of type Rttest
#' @param ... other parameters
#'
#' @return nothing - prints the object
#' @export
#'
#' @examples
#' \dontrun{print(x)}
#'
print.Rttest <- function(x, ...)
{
  print(x$conf.int)
}
