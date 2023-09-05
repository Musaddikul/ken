#=====Question 1=====
data1 = c(1, 5, 10, 4)

EM = function(data, N, k, m) {
  mu = vector("numeric")
  mu[1] = m
  for (i in 1:1000) {
    m = (sum(data) + (N - k) * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  cat("All mu:", mu, "\n")
  cat("Final estimated mean (mu):", mu[length(mu)], "\n")
}

EM(data1, 6, 4, 3)


## All_mu: 4.333333 4.777778 4.925926 4.975309 4.99177 4.997257 4.999086 
## Final estimated mean (mu): 4.999086 


#=====Question 2=====
set.seed(1810052)
mis = 2
N = 10
lambda2 = 9
pois2 = rpois(N, lambda2)
pois2[9:10] = NA
m = 10

mu = vector("numeric")

EM = function(x, N) {
  mu[1] = m
  for (i in 1:N) {
    m = (sum(x, na.rm = T) + mis * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  cat("All mu:", mu, '\n')
  cat("Final Mean (mu):", mu[length(mu)], '\n')
  cat('No of Iterations:', i, '\n')
}
EM(pois2, 10)

## All mu: 9.8 9.76 9.752 9.7504 
## Final Mean (mu): 9.7504 
## No of Iterations: 5

#=====Question 3=====
EM = function(N, lambda, mis) {
  set.seed(1810052)
  data = rpois(N, lambda)
  data[19:20] = NA
  m = round(sum(data, na.rm = T) / N)
  mu = vector("numeric")
  mu[1] = m
  
  for (i in 1:1000) {
    m = (sum(data, na.rm = T) + mis * m) / N
    if (abs(mu[length(mu)] - m) < 0.001)
      break
    else
      mu[i] = m
  }
  
  data[19:20] = mu[length(mu)]

  cat("Estimated Mean (mu):",  mu[length(mu)], "\n")
  cat("Estimated Standard Deviation (Sigma):",  sd(data), "\n")
}
EM(20, 15, 2)


## Estimated Mean (mu): 15.8887 
## Estimated Standard Deviation (Sigma): 4.089639 

#=====Question 4=====
EM = function(N, lambda, mis) {
  set.seed(1810052)
  data = rexp(N, rate = 1 / lambda)
  data[5:6] = NA
  m = round(N/sum(data, na.rm = T))
  mu = vector("numeric")
  mu[1] = m
  for (i in 1:1000) {
    m = (sum(data, na.rm = T) + mis * m) / N
 
    if (abs(mu[length(mu)] - m) < 0.001)
      break
    else
      mu[i] = m
  }
  
  data[5:6]  = mu[length(mu)]
  
  cat("Answer (a):
Minimum Variance Unbiased Estimate:", mean(data), "\n", '\n')
 
  cat("Answer (b):
Unbiased Variance of Sample Mean:", var(data)/N, "\n")
  
}
results = EM(10, 15, 2)

## Answer (a):
## Minimum Variance Unbiased Estimate: 16.31045 
##
## Answer (b):
## Unbiased Variance of Sample Mean: 8.871301


#======================THE END=====================#