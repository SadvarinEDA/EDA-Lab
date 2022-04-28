data_mtcars <-mtcars
rm(list = ls())
mgd <-function(x1,x2, y, m1,m2, c, alpha, gamma, iter) 
  {iterations <-0
  u_m1<-0
  u_m2<-0
  u_c<-0
  while(iterations<=iter){
    y_pred=m1*x1+m2*x2+c
    loss_new<-0.5*sum((y_pred-y)^2)
    nu_m1<-gamma*u_m1+alpha*sum((y_pred-y)*x1)
    nu_m2<-gamma*u_m2+alpha*sum((y_pred-y)*x2)
    nu_c<-gamma*u_c+alpha*sum(y_pred-y)
    m1<-m1-nu_m1
    m2<-m2-nu_m2
    c<-c-nu_c
    u_m1<-nu_m1
    u_m2<-nu_m2
    u_c<-nu_c
    loss<-loss_new
    iterations<-iterations+1}
  return(paste("Optimal intercept: ", c, " Optimal slope m1: ", m1, " Optimal slope m2: ", m2," Loss function: ", loss," iterations: ",iterations))
  } 

mgd(data_mtcars$wt, data_mtcars$hp, data_mtcars$mpg, -0.2, -0.2, 32, 0.0000045,0.98,15000)
model<-lm(data_mtcars$mpg~data_mtcars$wt+data_mtcars$hp)
model