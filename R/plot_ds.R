#' plot_ds
#'
#' It plots the data depending on the data used
#'
#' @param dataset dataframe. Name of the dataset to plot
#' @param x numeric vector or vector of factors Name of the column
#' @param y numeric vector. Name of the column for scatterplot
#' @param colour numeric or vector of factors. Name of the column to paint the data in the plot
#'
#' @return scatterplot if numeric vector, barplot if vector of factors
#'
#' @examples
#' \dontrun{
#' plot_ds(dataset=canada_ab,x=canada_ab$abortion,colour=canada_ab$importance)
#' }
#'
#' @export
#'

plot_ds <- function(dataset,x,y=NULL,colour=NULL) {
    ds <- substitute(dataset)
    xsub <- substitute(x)

    if(is.numeric(x) && is.numeric(y)) {  #it builds a scatterplot
      ggplot2::ggplot(dataset,mapping=ggplot2::aes(x=x,y=y,colour=colour))+
        ggplot2::geom_point(size=1)+
        ggplot2::labs(title=paste('Scatterplot of',ds),
           x = xsub)}

    else
      if(is.factor(x)) {   #it builds a barplot
          ggplot2::ggplot(dataset,mapping=ggplot2::aes(x=x,fill=colour,colour=colour))+
            ggplot2::geom_bar(position='fill',alpha=0.7,size=0.8)+
            ggplot2::scale_fill_brewer(palette='RdBu')+
            ggplot2::scale_colour_brewer(palette='RdBu')+
            ggplot2::labs(title=paste('Barplot of',ds),
               x=xsub)
      }
}
