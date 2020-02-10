#' condfreq_abort
#'
#' Computes the frequency of the view on abortion from the dataset 'canada_ab',
#' conditioned to one of the other variables in the dataset.
#'
#' @param x character. Name of a column from the dataset 'canada_ab' for computing the conditioned frequency
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' confreq_abort(x=gender)
#' }
#'
#' @export
#'

condfreq_abort <- function(x) {
  y <- substitute(x)
  if(as.character(y) %in% colnames(canada_ab)[-c(1,6)]) {
    tapply(abortion,x,table)
  } else {
    print('Please insert one of the followings: province, population, weights, gender, importance, education, urban')
  } #if the correct name for the argument x is not used, it returns a hint
}
