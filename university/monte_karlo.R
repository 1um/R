#интегралл методом монтекарло, сравнение с аналитическим значением
f <- function(x,p,q){
  (x**(p-1))/((1-x**q)**(p/q))
}

monte_carlo <- function(p,q,a = 0, b = 1, n = 1000){
  rez = (b-a)/n*sum(f(runif(n),p,q))
  print(rez)
  print(pi/(q*sin(p*pi/q)))
}