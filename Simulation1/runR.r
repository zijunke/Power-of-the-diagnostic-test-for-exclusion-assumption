wd.global = 'D:/Research/2023/PowerofChisq/RCode/Study1/'
setwd(wd.global)
library(doParallel)

no_cores <- detectCores(logical = F) - 2
cl <- makeCluster(no_cores,outfile = 'debug.txt')
registerDoParallel(cl)

foreach(i=1:240) %dopar% {
	print(i)
	source(paste0(wd.global,'S1C',i,'.r')) 
}

stopCluster(cl)

