plot_ds <- function(dataset,x=NULL) {
  ds <- substitute(dataset)
  xsub <- substitute(x)

  if(ds=='electric_d') {  #it builds a scatterplot for the database electric_d,
    #divided by work days and non-work days
    ggplot2::ggplot(electric_d,aes(x=Demand,y=Temperature,colour=WorkDay))+
      ggplot2::geom_point(size=1)+
      ggplot2::scale_colour_manual(values=c('0'='#999999','1'='#33FF66'))+
      ggplot2::scale_y_continuous(breaks=seq(0,max(Temperature),by=5))+
      ggplot2::scale_x_continuous(breaks=seq(0,max(Demand),by=1))+
      ggplot2::labs(title='Total electricity demand for Victoria (GW)',
           subtitle = 'Divided for work days and non-work days')}

  else
    if(ds=='canada_ab') {   #it builds a barplot for the database canada_ab,
      #it requires as x argument one of the columns
      if(as.character(xsub) %in% colnames(canada_ab)[-c(1,3,4,6)] && !purrr::is_empty(x)) {  #it evaluates whether
        #x is not empty and it is the name of a column od the dataset
        ggplot2::ggplot(canada_ab,aes(x=x,fill=abortion,colour=abortion))+
          ggplot2::geom_bar(position='fill',alpha=0.7,size=0.8)+
          ggplot2::scale_fill_manual(values=c('No'='indianred2','Yes'='lightgreen'))+
          ggplot2::scale_colour_manual(values=c('No'='indianred2','Yes'='lightgreen'))+
          ggplot2::labs(title=paste('Percentage of view on abortion conditioned to',xsub),
               x=xsub)}
      else {cat('please specify the x argument for the plot among the following:\n province, gender, importance, education, urban')}
      #it gives a hint on the name of the x argument
    }

  else
    if(ds=='chicken_weight') {    #it builds a regression for the database chicken_weight,
      #it requires as x argument the name of a column of the dataset
      if(as.character(xsub) %in% colnames(chicken_weight)[-c(3)] && !purrr::is_empty(x)) {
        ggplot2::ggplot(chicken_weight,aes(y=weight,x=x))+
          ggplot2::theme_light()+
          ggplot2::geom_smooth(method='lm',colour='indianred2')+
          ggplot2::labs(title=paste('Relation between chickens\' weight and',xsub),
               x=xsub)
      }
      else {cat('please specify the x argument for the plot among the following:\n weight, Time, Diet, random')}
      #it gives a hint on the name of columns the dataset
    }
  else {cat('please specify a dataset among \'chicken_weight\',\'canada_ab\',\'electric_d\' and a x argument')}
}

