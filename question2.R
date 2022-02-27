rm(list = ls())
set.seed(12345)

data = unlist((read.csv(file = "C:\\Users\\Keshav Gambhir\\Desktop\\Assignment-1 SI\\data_Q2.csv"))['x'])
dataMean = mean(data)
dataVar = var(data)
n = 1000

mle.norm<-function(params)
{
  loglikelihood.norm <- ((n/2)*log(2*pi*params[2])) - (-1/(2*params[2]))*(sum(data^2) + 2*params[1]*sum(data) - n*(params[1]^2))
  return (-1*loglikelihood.norm)
}

output <- nlm(mle.norm,c(dataMean,dataVar),hessian = T)
output
