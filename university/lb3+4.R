#эмулирование системы массового обслуживания + расчет основных параметров.
# Заявки поступаю по заданному закону.
create_norm <- function(m=0,s=1){
  s = 0
  while(s>1||s==0){
    r = unlist(runif(2,min=-1,max=1))
    s=sum(r**2)
  }
  z = r[1]*sqrt(-2*log(s)/s)
  z=z*s+m
}

lognorm_generator <- function(n=100,m=0,s=1){
  sapply(1:n,function(x){(create_norm(m,s))})
}

printf <- function(...)print(sprintf(...))

smo <- function(t1,t2,with_queue=T,n=10000,print=T){
  #preparing data
  x <- lognorm_generator(n,t1)
  y <- lognorm_generator(n,t2)
  
  size = sum(x>0&y>0)
  t_wait = x[x>0&y>0]
  t_serv = y[x>0&y>0]
  
  come_in <- matrix(-1,1,size)[1,]
  come_in[1]=t_wait[1]
  for(i in 2:size){
    come_in[i]=come_in[i-1]+t_wait[i]
  }
  
  start <- matrix(-1,1,size)[1,]
  end <- matrix(-1,1,size)[1,]
  start[1]=come_in[1]
  end[1]=start[1]+t_serv[1]
  
  #serving
  if(with_queue){ #with waiting
    for(i in 2:size){
      start[i]=max(end[i-1],come_in[i])
      end[i]=start[i]+t_serv[i]
    }
  }
  else{
    for(i in 2:size){
      if(end[i-1] <= come_in[i]){
        start[i] = come_in[i]
        end[i] = come_in[i]+t_serv[i]
      }else{
        start[i]=-1
        end[i]=end[i-1]
      }
    }
  }
  served = start!=-1
  avg_time_in_q = mean(start[served]-come_in[served])
  
  if(print){
    #printing
    printf("come in intensivity=%f",1/mean(t_wait))
    printf("serve intensivity=%f",1/mean(t_serv))
    printf("not_serve propability=%f",sum(start==-1)/length(start))
    printf("average time of serving=%f",mean(end[served]-come_in[served]))
    printf("average time of waiting in queue=%f",avg_time_in_q)
  }
  avg_time_in_q
}

smo_cheker <-  function(n=20, accuracy=0.1, smo_t1,smo_t2,smo_with_queue=T,smo_n=10000){
  param = sapply(1:n,function(x){(smo(smo_t1,smo_t2,smo_with_queue,smo_n,print=F))})
  X = mean(param)
  S = sqrt(sum((param - X)^2)/(n-1))
  spreading = qt(1-accuracy/2,n-1)*S/sqrt(n)
  printf("%.f%% [%f - %f]",(1-accuracy)*100,X - spreading,X+spreading)
}
