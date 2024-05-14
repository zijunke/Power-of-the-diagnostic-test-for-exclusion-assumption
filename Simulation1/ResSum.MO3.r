library(xlsx)

wd = 'D:/Research/2023/PowerofChisq/'
source(paste0(wd,'RCode/RFuncs.r'))
STUDYii = 1
wd.res = paste0(wd,'Results/Study',STUDYii,'/')
Cond.l <- read.xlsx(paste0(wd,'RCode/Study',STUDYii,'/CondList.xlsx'),1)

N.all = c(50,100,200,500,1000)
ryz1.all = c(0.05,0.1,0.2)
bxz.all = c(0.1,0.4)

n.ryz1 = length(ryz1.all)
n.bxz = length(bxz.all)
n.N = length(N.all)

res.list = vector('list',240)
names(res.list) = paste0('Cond',1:240)
for(si in 1:240){
	setwd(paste0(wd.res,'Cond',si))
	fn = paste0('S',STUDYii,'C',si,'.ResSumN.xlsx')
	res.list[[si]] = read.xlsx(fn,sheetName = 'Results')
}
fn = paste0(wd.res,'ConvergenceR.xlsx')
ConvR = read.xlsx(fn,1)[,-c(1)]

#----------------------------------------------------------
method.names = c('All','ChisqNS')
cond.names = c('N','ryz1','bxz')
item.names = c('median','min','max')
PowerMisFit = GetResM(n.N*n.ryz1*n.bxz,item.names,method.names,cond.names)
Bias =RBias= rMSE = SE = SEBias = RejectionR = CoverageR = PowerMisFit
ConvergenceR = GetResM(n.N*n.ryz1*n.bxz,item.names,'All',cond.names)

counti = 1
for(ni in N.all){
for(ryz1i in ryz1.all){
for(bxzi in bxz.all){

	cr1 = (Cond.l$N==ni)
	cr2 = (Cond.l$ryz1 == ryz1i)
	cr3 = (Cond.l$bxz == bxzi)
	sel.id = which(cr1*cr2*cr3==1)
	res.sublist = res.list[sel.id]
	ConvR.sub = ConvR[sel.id,]
	
	Bias[counti,1:3] = rMSE[counti,1:3] = SE[counti,1:3] = c(ni,ryz1i,bxzi)
	RBias[counti,1:3] = SEBias[counti,1:3] = PowerMisFit[counti,1:3] = c(ni,ryz1i,bxzi)
	CoverageR[counti,1:3] = RejectionR[counti,1:3] = c(ni,ryz1i,bxzi)
	ConvergenceR[counti,1:3] = c(ni,ryz1i,bxzi)
	
	tmp.res = Org.Res(Resl=res.sublist,method.n=method.names)
	Bias[counti,(1:length(tmp.res$biasv))+3] = tmp.res$biasv
	RBias[counti,(1:length(tmp.res$rbiasv))+3] = tmp.res$rbiasv
	rMSE[counti,(1:length(tmp.res$rmsev))+3] = tmp.res$rmsev
	SE[counti,(1:length(tmp.res$esev))+3] = tmp.res$esev
	SEBias[counti,(1:length(tmp.res$seBias))+3] = tmp.res$seBias	
	PowerMisFit[counti,(1:length(tmp.res$srv))+3] = c(tmp.res$srv)
	CoverageR[counti,(1:length(tmp.res$crv))+3] = c(tmp.res$crv)
	RejectionR[counti,(1:length(tmp.res$rrv))+3] = c(tmp.res$rrv)
	ConvergenceR[counti,(1:3)+3] = c(median(ConvR.sub$ConvR,na.rm = T),
		min(ConvR.sub$ConvR,na.rm = T),max(ConvR.sub$ConvR,na.rm = T))
	counti = counti +1

}}}

source(paste0(wd,'RCode/RFuncs.r'))
wd.res = paste0(wd,'Results/Study',STUDYii,'/RAW/')

plotRes(resm=ConvergenceR,'All',item.names,ypanel.n = 'ryz1',ylabel='Convergence Rate',
	pic.names='ConvR.Overall',wd=wd.res)
	
plotRes(resm=PowerMisFit[,-c(4:6)],'ChisqNS',item.names,ypanel.n = 'ryz1',
	ylabel='Power of Chi-square Test of Model Fit',pic.names='PowerMisFit',wd=wd.res)
	
plotRes(resm=Bias,method.names,item.names,ypanel.n = 'ryz1',ylabel='Bias',
	pic.names='Bias',wd=wd.res)

plotRes(resm=RBias,method.names,item.names,ypanel.n = 'ryz1',ylabel='Relative Bias',
	pic.names='RBias',wd=wd.res)

biasv = unlist(lapply(res.list,function(x){ x[2,'Bias'] }))
TVv = unlist(lapply(res.list,function(x){ x[2,'TV'] }))
sum(abs(biasv/TVv)>.1,na.rm = T)
	
plotRes(resm=rMSE,method.names,item.names,ypanel.n = 'ryz1',
	ylabel='Root Mean Square Error',pic.names='rMSE',wd=wd.res)

plotRes(resm=SEBias,method.names,item.names,ypanel.n = 'ryz1',ylabel='SE Bias',
	pic.names='SE_Bias',wd=wd.res)
	
plotRes(resm=CoverageR,method.names,item.names,ypanel.n = 'ryz1',
	ylabel='Coverage Rate',pic.names='Coverage_Rate',wd=wd.res)
	


plotRes(resm=RejectionR,method.names,item.names,ypanel.n = 'ryz1',
	ylabel='Rejection Rate',pic.names='Rejection_Rate',wd=wd.res)

fn = paste0(wd.res,'Conv.xlsx')
write.xlsx(ConvergenceR,fn)

fn = paste0(wd.res,'Bias.xlsx')
write.xlsx(Bias,fn)

fn = paste0(wd.res,'rMSE.xlsx')
write.xlsx(rMSE,fn)

fn = paste0(wd.res,'SEBias.xlsx')
write.xlsx(SEBias,fn)

fn = paste0(wd.res,'CoverageR.xlsx')
write.xlsx(CoverageR,fn)

fn = paste0(wd.res,'PowerMisFit.xlsx')
write.xlsx(PowerMisFit,fn)

fn = paste0(wd.res,'RejectionR.xlsx')
write.xlsx(RejectionR,fn)

fn = paste0(wd.res,'Simulation1.RBias4AllConds.xlsx')
Bias4AllC = data.frame(Condition= 1:240,N=Cond.l$N,byx=Cond.l$byx,
	ryz1=Cond.l$ryz1,bxz=Cond.l$bxz,rxy=Cond.l$rxy,AllBias = rep(NA,240),
	ChisqNSBias=rep(NA,240))
Bias4AllC$AllBias = as.numeric(lapply(res.list,function(x){x[1,'Bias']}))/as.numeric(Cond.l$byx)
Bias4AllC$ChisqNSBias = as.numeric(lapply(res.list,function(x){x[2,'Bias']}))/as.numeric(Cond.l$byx)
write.xlsx(Bias4AllC,fn)
