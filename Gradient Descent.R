rm(list=ls())
gd <- function(x1, x2, y, m1, m2, c, alpha, con_thr, iter){
  iterations = 0
  Lf <- 0
  while(iterations < iter) {
    y_pred <- m1*x1 + m2*x2 + c
    Lf_new <- 0.5*sum((y_pred - y)^2)
    m1 <- m1-alpha*sum((y_pred - y)*x1)
    m2 <- m2-alpha*sum((y_pred - y)*x2)
    c <- c-alpha*sum(y_pred-y)
    #if(abs(Lf-Lf_new) < con_thr) {
    # break
    #}
    Lf <- Lf_new
    iterations = iterations + 1
  }
  return(paste("Optimal intercept:", c, "Optimal slope 1:", m1, "Optimal slope 2:", m2,
               "Loss Fucntion:", Lf, "Iterations:", iterations))
}
data <- mtcars
#View(mtcars)
gd(data$wt, data$hp, data$mpg, -0.2, -0.2, 32, 0.00000002, 0.00000001, 100000000)
lr <- lm(data$mpg ~ data$wt+data$hp)
lr
