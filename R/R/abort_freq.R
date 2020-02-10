#' abort_freq
#'
#' Computes the absolute or relative frequency of the view on abortion from the dataset 'canada_ab'
#'
#' @param rel logical. If TRUE, it returns the relative frequency
#'
#' @return table
#'
#' @examples
#' \dontrun{
#' abort_freq(rel=TRUE)
#' }
#'
#' @export
#'

abort_freq <- function(rel=FALSE) {
  if(rel==T) {
    y <- table(abortion,dnn='relative frequency of view on abortion')/sum(table(abortion))
    return(y)
  }
  else {
    x <- table(abortion,dnn='absolute frequency of view on abortion')
    return(x)
  }
}
