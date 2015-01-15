# генераторы другого распределения для пред лабы
generate_poisson<- function(lambda){
  l = exp(-lambda)
  k =0
  p =1
  while(p>l){
    k=k+1
    p=p*runif(1)
  }
  k-1
}
poisson_generator <- function(n=100, lambda = 10){
  val = unlist(lapply(1:n,function(x){generate_poisson(lambda)}))
  hist(val,probability=TRUE,breaks = "FD")
  lines(density(val), lwd = 3)
  
}