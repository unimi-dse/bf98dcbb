#' helloyou
#'
#' Asks the user his name and surname and greets him, returning a jpg.
#'
#' @return your name, surname, jpg file
#'
#' @export

helloyou <- function() {
  n <- readline(cat('What\'s your name?\n'))
  s <- readline(cat('What\'s your surname?\n'))
  cat(c('Buongiorno',n,s,'!\nLet me greet you like this!\n'))
  magick::image_read('https://designshop-6aa0.kxcdn.com/photos/hello-cartoons-comic-send-greeting-card-online-2526_2.jpg')
}
