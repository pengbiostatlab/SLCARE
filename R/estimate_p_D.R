p_D <- function(d, beta, Z, mu_censor, gamma = 0){
  #n <- nrow(Z)
  #K <- nrow(beta)
  Z1 <- cbind(1, Z)
  exp_Zbeta <- exp(Z1 %*% t(beta)) # n*(p+1) (p+1)*k
  r <- gamma
  
  if(gamma == 0){
    p_D <- apply(exp_Zbeta, 2, function(x) ((x*mu_censor)^d) * exp(-x*mu_censor)/factorial(d))
  }else{
    #p_D <- apply(exp_Zbeta, 2, function(x) ((x*mu_censor)^d) * (gamma^gamma) * (factorial(d + gamma - 1) / factorial(d))/(( x*mu_censor + gamma-1 )^(d+gamma))/factorial(gamma -1))
    p_D <- apply(exp_Zbeta, 2, 
                 function(x) factorial(d + r - 1) / factorial(d) / factorial(r - 1) * (r^r) * (x*mu_censor)^d / (r + x*mu_censor)^(d + r)   
                 )
  }
  return(p_D)
}
