########################################################################
########################## PROJECT R DSE ############################
#1) Data Acquisition: Import data via local/remote files, R packages,
#APIs or web scraping. v
#2) Data Visualization: Visualize data with ggplot2 or plotly 
#3) Data Analysis: Use R packages to perform some kind of data analysis. 
#The complexity of the analysis is not relevant. The aim is to show your 
#ability to work with R packages v
#4) Interactive Interface Code Optimization: Build an interactive 
#interface with shiny
#5) Code Optimization: Speed up the code using Rcpp or parallel computing

#1) README: Use the Markdown syntax to provide a high 1 quality README.md file
#2) Documentation: Document your functions using roxygen2. It must be possible
#to access the documentation of your function(s) using the standard syntax 
#?functioname. If you are developing an inter- active interface you still 
#need to document the function which runs the interface.
#3) Code quality: Coding style and modularity

#this function imports online datasets
set_data=function() {
  canada_ab<<-read.csv(url('https://raw.githubusercontent.com/unimi-dse/bf98dcbb/master/project%20DSE/canada_ab.csv'),sep=',',header = T)[-1]
  colnames(canada_ab)[4]<<-'weights'
  cw=read.csv(url('https://raw.githubusercontent.com/unimi-dse/bf98dcbb/master/project%20DSE/chicken_weight.csv'), sep=',',header=T)[-1]
  chicken_weight<<-cbind(cw,random=rnorm(length(cw$weight)))
  electric_d<<-read.csv(url('https://raw.githubusercontent.com/unimi-dse/bf98dcbb/master/project%20DSE/electric_d.csv'),header=T)[-1]
  #Total electricity demand in GW for Victoria, Australia, every half-hour during 2014.
  #WorkDay:	taking value 1 on work days, and 0 otherwise.
  electric_d$WorkDay<<-as.character(electric_d$WorkDay)
  attach(canada_ab)
  attach(chicken_weight)
  attach(electric_d)
}


#################

#this function computes the relative or absolute frequency of the view 
#on abortion
abort_freq=function(rel=F) {
  if(rel==T) {
    y=table(abortion,dnn='relative frequency of view on abortion')/sum(table(abortion))
    return(y)
    }
  else { 
    x=table(abortion,dnn='absolute frequency of view on abortion')
    return(x)
  }
}
############### ok #########

############
#this function returns the frequency of the view on abortion conditioned
#on the other variables in the dataframe canada_ab, which are taken as the
#argument x. the variables excluded are 'abortion' itself and 'id' which
#cannot be used for conditioning
cond_freq=function(x) {
  y=substitute(x)
  if(as.character(y) %in% colnames(canada_ab)[-c(1,6)]) {
    tapply(abortion,x,table)
  } else {
    print('Please insert one of the followings: province, population, weights, gender, importance, education, urban')
  } #if the correct name for the argument x is not used, it returns a hint
}
################# ok ##############


#this function returns the percentage of the view on abortion among people conditioned 
#to their level of education in dataframe canada_ab, which is taken as the argument x: 
#somePS, bachelors, college, lessHS,HS or higher
ab_education=function(x) {
  y=substitute(x)
  if(as.character(y) %in% unique(education)) {
    cat('Abortion view conditioned on level of education:',y)
    table(abortion[education==x])/sum(table(abortion[education==x] ))
  }
  else {print('Please insert one of the followings: somePS, bachelors, college,
               lessHS, HS, higher')} #if the correct name for the argument x is not 
  #used, it returns a hint
}
################# ok ################


#####################
#it asks the name and surname and greet the user
hello=function() {
  n=readline(cat('What\'s your name?\n'))
  s=readline(cat('What\'s your surname?\n'))
  cat(c('Buongiorno',n,s,'!\nLet me greet you like this!\n'))
  image_read('https://designshop-6aa0.kxcdn.com/photos/hello-cartoons-comic-send-greeting-card-online-2526_2.jpg')
}
#################### ok #############

#it imports the packages
set_pack=function() {
  install.packages('magick')
  require(magick)
  install.packages('tidyverse')
  require(tidyverse)
}

#stats
#dplyr



########################
#this function computes a regression of a variable in the dataset 
#chicken weight, given as the argument z, on the weight data. it returns
#if the coefficients are are significant or not, if the intercept is not
#statistically significant, it computes a new regression with no intercept
regr_weight=function(z) {
  s=substitute(z)
  s2=as.character(s)
  if(s2 %in% colnames(chicken_weight)[-1]) {
    l=lm(weight~z)
    names(l$coefficients)=c(names(l$coefficients)[1],s)
    print(l)
    x=summary(l)$coefficients
    if(x[s2,'Pr(>|t|)']<0.5) {
      cat('Significant estimated coefficient for',s,'with value:',x[s2,'Estimate'],'\n\n') #x
      } else {
        cat('Non-significant estimated coefficient for',s,'\n\n')
        }  
    if(x['(Intercept)','Pr(>|t|)']<0.5) {
      cat('Significant estimated Intercept, with value:',x['(Intercept)','Estimate'])
      } else {
        cat('Non-significant estimated Intercept\nEvaluating new regression with intercept = 0\n')
        nr=lm(weight~0+z)
        names(nr$coefficients)=s2
        print(nr)
        y=summary(nr)$coefficients
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

################## ok ################

plot_ds=function(dataset,x=NULL) {
  ds=substitute(dataset)
  xsub=substitute(x)
  if(ds=='electric_d') {
    ggplot(electric_d,aes(x=Demand,y=Temperature,colour=WorkDay))+
      geom_point(size=1)+
      scale_colour_manual(values=c('0'='#999999','1'='#33FF66'))+
      scale_y_continuous(breaks=seq(0,max(Temperature),by=5))+
      scale_x_continuous(breaks=seq(0,max(Demand),by=1))+
      labs(title='Total electricity demand for Victoria (GW)',
           subtitle = 'Divided for work days and non-work days')}
  else
    if(ds=='canada_ab') {
    if(as.character(xsub) %in% colnames(canada_ab)[-c(1,3,4,6)] && !is_empty(x)) { 
      ggplot(canada_ab,aes(x=x,fill=abortion,colour=abortion))+
        geom_bar(position='fill',alpha=0.7,size=0.8)+
        scale_fill_manual(values=c('No'='indianred2','Yes'='lightgreen'))+
        scale_colour_manual(values=c('No'='indianred2','Yes'='lightgreen'))+
        labs(title=paste('Percentage of view on abortion conditioned to',xsub),
             x=xsub)} 
    else {cat('please specify the x argument for the plot among the following:\n province, gender, importance, education, urban')}
    }
  else
    if(ds=='chicken_weight') {
      if(as.character(xsub) %in% colnames(chicken_weight)[-c(3)] && !is_empty(x)) {
        ggplot(chicken_weight,aes(y=weight,x=x))+
          theme_light()+
          geom_smooth(method='lm',colour='indianred2')+
          labs(title=paste('Relation between chickens\' weight and',xsub),
                           x=xsub)
      }
      else {cat('please specify the x argument for the plot among the following:\n weight, Time, Diet, random')}
    }
  else {cat('please specify a dataset among \'chicken_weight\',\'canada_ab\',\'electric_d\' and a x argument')}
}

############################################

