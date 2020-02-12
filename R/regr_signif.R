#' regr_signif
#'
#' Regress a variable y on a variable x
#' If the intercept value is non-significant, it computes again the regression without the intercept. It also returns if the regressed variable is significant or not
#'
#' @param y numeric vector. Dependent variable
#' @param x numeric vector. Independent variable
#'
#' @return table, str
#'
#' @examples
#' \dontrun{
#' regr_signif(y=chicken_weight$weight,x=chicken_weight$Time)
#' }
#'
#' @export
#'

regr_signif <- function(y,x) {
      s <- as.character(substitute(x))
      l <- lm(y~x)
      print(l)
      z1 <- summary(l)$coefficients
      if(z1['x','Pr(>|t|)']<0.5) {
        cat('Significant estimated coefficient for',s,'\n\n') #x
        } else {
          cat('Non-significant estimated coefficient for',s,'\n\n')
        }
      if(z1['(Intercept)','Pr(>|t|)']<0.5) {
        cat('Significant estimated Intercept')
        } else {
          cat('Non-significant estimated Intercept\nEvaluating new regression with intercept = 0\n')
          nr <- lm(y~0+x)
          print(nr)
          z2 <- summary(nr)$coefficients
            if(z2['x','Pr(>|t|)']<0.5) {
            cat('New estimated coefficient for',s,'is significant')
            } else {
              cat('New estimated coefficient for',s,'is not significant')
            }
  }
}
