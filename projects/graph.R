# Писал для себя. Интересно было поигратся с визуализацией данных.
# Часть программы(выкачивание данных и предварительная очистка) была сделана на Ruby.
# Суть - по времени комментариев пользователя на ресусрсе выяснить график работы/учебы/выходных. Активные/неактивные месяцы.
require(ggplot2)

get_linn <- function(){
  setwd("~/Documents/prog/check_date_time")
  fileName="dates.txt"
  conn=file(fileName,open="r")
  linn=readLines(conn)
  close(conn)
  linn
}

my_plot_1<- function(){
  require(ggplot2)
  linn = get_linn()
  for(mnt in 6:9){
    time.posix <- as.POSIXlt(linn,"%Y-%m-%dT%H:%M:%S",tz="UTC")
    time.posix = time.posix[as.numeric(format(time.posix,"%m")) %in% c(mnt)]
    wd = factor(weekdays(time.posix),level = c('понеділок','вівторок','середа','четвер',"п'ятниця",'субота','неділя'))
    t = time.posix$hour+time.posix$min/60
    q = qplot(wd,t,main = mnt)+scale_y_continuous(breaks=seq(0, 24, 1))
    print(q)
  }
}

my_plot_2 <- function(mnt){
  linn = get_linn()
  time.posix <- as.POSIXlt(linn,"%Y-%m-%dT%H:%M:%S",tz="UTC")
  time.posix = time.posix[as.numeric(format(time.posix,"%m")) %in% c(mnt)]
  r = tapply(time.posix,format(time.posix,"%y:%m:%d"),length)
  r = tapply(r,weekdays(as.POSIXlt(names(r),"%y:%m:%d",tz="UTC"),abbreviate = TRUE),mean)
  plot(factor(names(r),level = c('пн','вт','ср','чт','пт','сб','нд')),r)
}

my_plot_3 <- function(mnt){
  linn = get_linn()
  time.posix <- as.POSIXlt(linn,"%Y-%m-%dT%H:%M:%S",tz="UTC")
  r = tapply(time.posix,format(time.posix,"%m"),length)
  plot(names(r),r,type = 'h')
}

my_plot_4 <-function(){
  require(ggplot2)
  linn = get_linn()
  time.posix <- as.POSIXlt(linn,"%Y-%m-%dT%H:%M:%S",tz="UTC")
  wd = factor(weekdays(time.posix),level = c('понеділок','вівторок','середа','четвер',"п'ятниця",'субота','неділя'))
  t = time.posix$hour+time.posix$min/60
  qplot(wd,t,color = as.numeric(format(time.posix,"%m")))+scale_y_continuous(breaks=seq(0, 24, 1))
}
my_plot_5 <-function(){
  require(ggplot2)
  time.posix <- as.POSIXlt(linn,"%Y-%m-%dT%H:%M:%S",tz="UTC")
  wd = factor(weekdays(time.posix),level = c('понеділок','вівторок','середа','четвер',"п'ятниця",'субота','неділя'))
  t = time.posix$hour
  mnth = as.numeric(format(time.posix,"%m"))
  mnth = (mnth - mean(unique(mnth)))
  mnth = mnth/(3*max(abs(mnth)))
  x = as.numeric(wd)*2+mnth
  y = t+0.5
  qplot(x,y,color = as.numeric(format(time.posix,"%m")))+
    scale_y_continuous(breaks=seq(0, 24, 1))+
    scale_x_continuous(breaks=seq(0, 16, 2))+
    geom_vline(xintercept=c(seq(3,13,by = 2)),color='white',size=5)+
    stat_sum()
}

