regr_chickw <- function(z) {
  s <- substitute(z)
  s2 <- as.character(s)
  if(s2 %in% colnames(chicken_weight)[-1]) {
    l <- lm(weight~z)
    names(l$coefficients) <- c(names(l$coefficients)[1],s) ##??
    print(l)
    x <- summary(l)$coefficients
    if(x[s2,'Pr(>|t|)']<0.5) {
      cat('Significant estimated coefficient for',s,'with value:',x[s2,'Estimate'],'\n\n') #x
    } else {
      cat('Non-significant estimated coefficient for',s,'\n\n')
    }
    if(x['(Intercept)','Pr(>|t|)']<0.5) {
      cat('Significant estimated Intercept, with value:',x['(Intercept)','Estimate'])
    } else {
      cat('Non-significant estimated Intercept\nEvaluating new regression with intercept = 0\n')
      nr <- lm(weight~0+z)
      names(nr$coefficients) <- s2
      print(nr)
      y <- summary(nr)$coefficients
      if(y[s2,'Pr(>|t|)']<0.5) {
        cat('New estimated coefficient for',s,'is significant, with value',y[s2,'Estimate'])
      } else {
        cat('New estimated coefficient for',s,'is not significant')
      }
    }
  } else {
    cat('Please insert one of the following: Time, Chick, Diet, random') #if the argument
    #for z is not correct it gives a hint
  }
}
