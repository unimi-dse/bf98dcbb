#' online_dataset
#'
#' Gets online dataset in csv format from 'https://vincentarelbundock.github.io/Rdatasets/datasets.html'
#'
#' @param URL url from 'https://vincentarelbundock.github.io/Rdatasets/datasets.html'
#' @param dataname character with the name to save the dataset
#'
#' @return data frame
#'
#' @examples
#' \dontrun {
#' online_dataset('https://vincentarelbundock.github.io/Rdatasets/csv/boot/amis.csv','car_speedwarn')
#' }
#'
#' @export
#'

online_dataset <- function(URL=NULL,dataname=NULL) {
  if(is.null(URL)) {
    return(print('Please insert a valid URL argument'))
  }
  else
    if(is.null(dataname)) {
      return(print('assign a name to save the dataset'))
      }
  else {
    data <- read.csv(file=url(URL),header = TRUE,sep=',')[-1]
    assign(dataname,data,envir=.GlobalEnv)
  return(data) }
}


