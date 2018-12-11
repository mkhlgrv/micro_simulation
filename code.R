library(dplyr)

P_fun <- function(Q, a, b, eps) {
   max((Q - a + eps)/ b, 0)
}
q_fun <- function(a, eps, n) {
  (a-eps)/ n
}
pi_fun <- function(P, q, c, v) {
  P*q - c + v
}
Q_fun <- function(q, n) {
  q * n
}



a <-  10 #автономный спрос
b <-  1 # чувствительность спроса
c <-  3 # FC
N <- 100 # максимум фирм
k <- 1000 # количество прогонов Монте-Карло
df <- data.frame()
set.seed(1)
for(j in 1:k){
  n_star <- c()
  Q_star <- c()
  P_star <- c()
  eps <- rnorm(1) # шок спроса
  V <- sort(rnorm(N), decreasing = TRUE) # шок предложения
  n <- 1
  
  v <- V[n]
  q <- q_fun(a, eps, n)
  Q <-  Q_fun(q, n)
  P <- P_fun(Q, a, b, eps)
  pi <- pi_fun(P, q, c, v)
  if(pi < 0) {
    n_star <- c(n_star, 0)
    Q_star <- c(Q_star,0)
    P_star <- c(P_star,0)
  } else {
    while(pi >= 0) {
      n_star <- c(n_star,n)
      Q_star <- c(Q_star,Q)
      P_star <- c(P_star,P)
        if(n<N){
        n <- n+1
        v <- V[n]
        q <- q_fun(a, eps, n)
        Q <-  Q_fun(q, n)
        P <- P_fun(Q, a, b, eps)
        pi <- pi_fun(P, q, c, v)
        
      } else{
        break()
      }
    }
  }
  df <- rbind(df, c(last(n_star), last(Q_star), last(P_star)))
}
colnames(df) <- c("n", "Q", "P")
library(ggplot2)
qplot(x = n, y = P, data = df)
