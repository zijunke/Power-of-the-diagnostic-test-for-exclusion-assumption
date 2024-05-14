wd.global = 'D:/Research/2023/Necessity_of_multiple_predictors/RCode/Study2/'
setwd(wd.global)
library(doParallel)

no_cores <- detectCores(logical = F) - 2
cl <- makeCluster(no_cores,outfile = 'debug.txt')
registerDoParallel(cl)

foreach(i=1:160) %dopar% {
	print(i)
	source(paste0(wd.global,'S2C',i,'.r')) 
}

stopCluster(cl)

