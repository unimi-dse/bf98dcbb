set_data=function() {

  path1 <- system.file('extdata','canada_ab.csv',package='PackNy')
  canada_ab <<- read.csv(path1, sep=',',header = T)[-1]
  attach(canada_ab)

  path2 <- system.file('extdata','chicken_weight.csv',package='PackNy')
  chicken_weight=read.csv(path2, sep=',',header=T)[-1]
  attach(chicken_weight)

  path3 <- system.file('extdata','electric_d.csv',package='PackNy')
  electric_d <<- read.csv(path3,header=T)[-1]
  electric_d$WorkDay <<- as.character(electric_d$WorkDay)
  attach(electric_d)
}

