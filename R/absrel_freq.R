#' absrel_freq
#'
#' Computes the absolute or relative frequency of elements of a vector
#'
#' @param x vector. For computing frequency
#' @param rel logical. If TRUE, returns the relative frequency
#'
#' @return table
#'
#' @examples
#' \dontrun{
#' absrel_fre(x=abortion,rel=TRUE)
#' }
#'
#' @export
#'

absrel_freq <- function(x,rel=FALSE) {
  if(rel==T) {
    y <- table(x,dnn='relative frequency')/sum(table(x))
    return(y)
  }
  else {
    y <- table(x,dnn='absolute frequency')
    return(y)
  }
}
