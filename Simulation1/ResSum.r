wd = 'D:/Research/2023/PowerofChisq/'
wd.res = paste0(wd,'Results/Study1/')
library(xlsx)
source(paste0(wd,'RCode/RFuncs.r'))

Cond.l <- read.xlsx(paste0(wd,'RCode/Study1/CondList.xlsx'),1)
ConvergenceR = Cond.l
ConvergenceR$ConvR = rep(1,240)

for(si in 1:240){
	setwd(paste0(wd.res,'Cond',si))
	Convergence = read.xlsx(paste0('S1C',si,'.Conv.xlsx'),1)
	sim.res = read.xlsx(paste0('S1C',si,'.Res.xlsx'),1)

	cr1 = which(Convergence$Converged == FALSE)
	cr2 = which(is.na(Convergence$Converged) == TRUE)
	#cr3 = which(Convergence$NegVar >0)
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
	
	# Results
	res = Sim.Res.Summary(NegVar = NegVar,simM=sim.res,
		TV=as.numeric(Cond.l[si,'byx']))

	#X1.res
	#X2.res
	fn = paste0('S1C',si,'.ResSumN.xlsx')
	write.xlsx(res,fn,sheetName = 'Results',append = T)
}

fn.conv = paste0(wd.res,'ConvergenceR.xlsx')
write.xlsx(ConvergenceR,fn.conv)
