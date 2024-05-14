wd = 'D:/Research/2023/Necessity_of_multiple_predictors/'
wd.res = paste0(wd,'Results/Study2/')
library(xlsx)
source(paste0(wd,'RCode/RFuncs2.r'))

Cond.l <- read.xlsx(paste0(wd,'RCode/Study2/CondList.xlsx'),1)
ConvergenceR = Cond.l
ConvergenceR$ConvR = rep(1,160)

for(si in 1:160){
	setwd(paste0(wd.res,'Cond',si))
	Convergence = read.xlsx(paste0('S2C',si,'.Conv.xlsx'),1)
	sim.res = read.xlsx(paste0('S2C',si,'.Res.xlsx'),1)
	
	cr1 = which(Convergence$Converged == FALSE)
	cr2 = which(is.na(Convergence$Converged) == TRUE)
	del.id = unique(c(cr1,cr2))
	if(length(del.id)>0){
		sim.res = as.data.frame(sim.res[-del.id,])
		NegVar = Convergence$NegVar[-del.id]
		ConvergenceR$ConvR[si] = 1-length(del.id)/1000
	}else{
		sim.res = as.data.frame(sim.res)
		NegVar = Convergence$NegVar
		ConvergenceR$ConvR[si] = 1
	}

	# Results for X1
	X1.res = Sim.Res.Summary(NegVar = NegVar,simM=sim.res,
		par.name='x1',TV=as.numeric(Cond.l[si,'byx1']))

	#X1.res
	#X2.res
	fn = paste0('S2C',si,'.ResSumN.xlsx')
	write.xlsx(X1.res,fn,sheetName = 'X1.res',append = T)
}

fn.conv = paste0(wd.res,'ConvergenceR.xlsx')
write.xlsx(ConvergenceR,fn.conv)
