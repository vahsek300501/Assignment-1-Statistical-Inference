rm(list = ls())
set.seed(12345)

n <- 1000
r <- 5
lambda <- 2
data <- round(rgamma(n,shape = r,rate = lambda),2)
print(data)


# Question-1 Part A
maxLikelyhoodFunction.gamma <- function(params) {
  return (-1*n*params[1]*log(params[2]) + n*lgamma(params[1]) + sum(data)*params[2] - (params[1]-1)*sum(log(data)))
}


# Question-1 Part B
#Computing using method of moments
# moment alpha of gamma data is mean square/variance
momentAlpha <- mean(data)^2/var(data)
# moment beta of gamma data is mean/variance
momentBeta <- mean(data)/var(data)
output <- nlm(maxLikelyhoodFunction.gamma,c(momentAlpha,momentBeta),hessian = T)
print(output)

#Computing using random initial value-1
output2 <- nlm(maxLikelyhoodFunction.gamma,c(8,0.8),hessian = T)
print(output2)

#Computing using random initial value-3
output3 <- nlm(maxLikelyhoodFunction.gamma,c(3,3),hessian = T)
print(output3)


# Question-1 Part C
lambdaVals = seq(0.1,5,by = 0.2)
mleOutput <- list()
i = 1
for (lVal in lambdaVals) {
  output = -1*maxLikelyhoodFunction.gamma(c(5,lVal))
  mleOutput[i] <- output
  i <- i +1
}
plot(lambdaVals,mleOutput,xlab = "Lambda Values",ylab = "Log-Likelyhood Function", main = "Log Likelyhood vs lamda values")

# Question-1 Part D
rVals = seq(0.1, 10, by=0.2)
lambdaVals = seq(0.1,4,by = 0.1)
maxLikelyhoodFunction.gammaNew <- function(alpha,beta) {
  tmp <- -1*n*alpha*log(beta) + n*lgamma(alpha) + sum(data)*beta - (alpha-1)*sum(log(data))
  return (tmp)
}
mleOutput2 = -1*outer(rVals,lambdaVals,maxLikelyhoodFunction.gammaNew)
print(mleOutput2)
persp(rVals,lambdaVals,mleOutput2,xlab = "r Vals", ylab = "Lambda vals",zlab = "Log-Likelihood",main = "Log Likelyhood vs r and lambda")