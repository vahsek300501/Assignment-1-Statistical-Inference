rm(list = ls())
set.seed(12345)

#Question-2 Part A
data = unlist((read.csv(file = "C:\\Users\\Keshav Gambhir\\Desktop\\Assignment-1 SI\\data_Q2.csv"))['x'])
dataMean = mean(data)
dataVar = var(data)
print(dataMean)
n = 1000

mle.norm<-function(params)
{
  loglikelihood.norm <- ((n/2)*log(2*pi*params[2])) - (-1/(2*params[2]))*(sum(data^2) + (2*params[1]*sum(data)) - n*(params[1]^2))
  return (-1*loglikelihood.norm)
}


output <- nlm(mle.norm,c(dataMean,dataVar),hessian = T)
output

# Question-2
meanVals = seq(40, 100, by=5)
outputVals <- list()
i = 1
for (val in meanVals) {
    tmp <- -1*mle.norm(c(val,dataVar))
    outputVals[i] = tmp
    i <- i + 1
}

plot(meanVals,outputVals,xlab = "mean Values",ylab = "Log MLE",main = "Log MLE vs mean values")

mle.norm2<-function(a,b)
{
  loglikelihood.norm <- ((n/2)*log(2*pi*b)) - (-1/(2*b))*(sum(data^2) + (2*a*sum(data)) - n*(a^2))
  return (loglikelihood.norm)
}

meanVals = seq(40, 100, by=5)
varVals = seq(1,10,by = 1)
mleOutput2 = -1*outer(meanVals,varVals,mle.norm2)
persp(meanVals,varVals,mleOutput2,xlab = "mean Vals", ylab = "variance vals",zlab = "Log-Likelihood",main = "Log Likelyhood vs mu and sigma square")
