#' online_dataset
#'
#' Gets online dataset in csv format from 'https://vincentarelbundock.github.io/Rdatasets/datasets.html'
#'
#' @param URL url from 'https://vincentarelbundock.github.io/Rdatasets/datasets.html'
#'
#' @return data frame
#'
#' @examples
#' \dontrun {
#' online_dataset('https://vincentarelbundock.github.io/Rdatasets/csv/boot/amis.csv')
#' }
#'
#' @export
#'

online_dataset <- function(URL) {
  data <- read.csv(file=url(URL),header = TRUE,sep=',')[-1]
  return(data)
}
