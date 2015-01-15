#Лабортаторная работа - задача которой генератор СВ, 
#а потом проверить свой генератор.


#генератор нормальной величины
create_norm <- function(){
  s = 0
  while(s>1||s==0){
    r = unlist(runif(2,min=-1,max=1))
    s=sum(r**2)
  }
  z = r[1]*sqrt(-2*log(s)/s)
}
#генератор логнормальных величин
lognorm_generator <- function(n=100){
  unlist(lapply(1:n,function(x){(create_norm())}))
}

#плотность теоритического распределения
lognorm_density<-function(x){
  exp(-log(x)^2/2)/(x*sqrt(2*pi))
}

#вероятность оказаться на отрезке a,b
lognorm_prob<-function(a,b,n){
    if(a==0) a=0.00001
    x = seq(a,b,(b-a)/n)
    (b-a)/n*sum(lognorm_density(x))
}

#проверка гипотезы распределения указанной в lognorm_density
lognorm_pirson_check <- function(left=0,right = 5,groups=20, dots = 1000){
  require('PEIP')
  n = groups
  val = lognorm_generator(dots)
  step = (right-left)/n
  checker<-function(v){
    if(v<left) return(NA)
    if(v>right) return(NA)
    step = (right - left)/n
    floor((v-left)/step)
  }
  grouped = tapply(val,sapply(val,checker),length)
  scale = left + as.numeric(names(grouped))*step
  par(mfrow=c(2,1))
  plot(scale,grouped,type = 'l')
  x = seq(left,right,by = 0.01)
  y = lognorm_density(x)
  plot(x,y)
  got = grouped/sum(grouped)
  expect = sapply(scale,function(e){lognorm_prob(e,e+step,100)})
  exp_xi = n*sum((got-expect)^2/expect)
  teor_xi = chi2inv(1-0.05,n-1)
  print(exp_xi)
  print(teor_xi)
  return(teor_xi>exp_xi)
}
