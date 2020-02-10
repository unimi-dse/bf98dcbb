#' abort_education
#'
#' Computes the absolute frequency of the view on abortion given the education level from the dataset 'canada_ab'
#'
#' @param x str. Level of education from the education column in the dataset 'canada_ab'
#'
#' @return table
#'
#' @examples
#' \dontrun{
#' abort_education('college')
#' }
#'
#' @export
#'

abort_education <- function(x) {
  y <- substitute(x)
  if(as.character(y) %in% unique(education)) {
    cat('Abortion view conditioned on level of education:',y)
    table(abortion[education==x])/sum(table(abortion[education==x] ))
  }
  else {cat('Please insert one of the followings: somePS, bachelors, college, lessHS, HS, higher')} #if the correct name for the argument x is not
  #used, it returns a hint
  }
