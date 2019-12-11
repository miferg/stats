
GAM_spline <- function(x,y,n=5,trials=100){
  x <- x-min(x)
  x <- x/max(x)
  knts <- 1:4/5
  rk <- function(x,z) {
    ((((z-1/2)^2)-1/2)*(((x-1/2)^2)-1/2)/4) - (((abs(x-z)-1/2)^4)-1/2*((abs(x-z)-1/2)^2)+7/240)/24
  }
  S1 <- matrix(1,nrow = length(x),ncol = 6)
  #S1 <- as.data.frame(S1)
  # S1[,1] is already a vector with 1s only
  S1[,2] <- x
  S1[,3] <- rk(x,knts[1])
  S1[,4] <- rk(x,knts[2])
  S1[,5] <- rk(x,knts[3])
  S1[,6] <- rk(x,knts[4])
  m1 <- lm(y ~ S1 - 1)
  xp <- (0:100)/100
  Sp <- matrix(NA,length(xp),length(knts)+2)
  Sp[,1] <- 1
  Sp[,2] <- xp
  Sp[,3] <- rk(xp,knts[1])
  Sp[,4] <- rk(xp,knts[2])
  Sp[,5] <- rk(xp,knts[3])
  Sp[,6] <- rk(xp,knts[4])
  plot(x,y)
  lines(xp,Sp%*%coefficients(m1))
  cv <- 1:length(x)
  n <- 5
  for (i in 1:trials) {
    r <- sample(1:length(x),n)
    mod <- lm(y[-r] ~ S1[-r,] - 1)
    cv[r] <- S1[r,]%*%coefficients(mod)
  }
  print('Mean error: ')
  print(mean(y-cv))
  return(m1)
}
