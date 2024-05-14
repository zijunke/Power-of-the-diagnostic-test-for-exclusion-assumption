library(xlsx)

wd = 'D:/Research/2023/Necessity_of_multiple_predictors/'
source(paste0(wd,'RCode/RFuncs2.r'))
STUDYii = 2
wd.res = paste0(wd,'Results/Study',STUDYii,'/')
Cond.l <- read.xlsx(paste0(wd,'RCode/Study',STUDYii,'/CondList.xlsx'),1)

N.all = c(50,100,200,500,1000)
bx2zj.all = c(0.2,0.4)
bx1zj.all = c(0.1,0.4)

n.bx2zj = length(bx2zj.all)
n.bx1zj = length(bx1zj.all)
n.N = length(N.all)

X1.list = vector('list',160)
names(X1.list) = paste0('Cond',1:160)
for(si in 1:160){
	setwd(paste0(wd.res,'Cond',si))
	fn = paste0('S',STUDYii,'C',si,'.ResSumN.xlsx')
	X1.list[[si]] = read.xlsx(fn,sheetName = 'X1.res')
}
fn = paste0(wd.res,'ConvergenceR.xlsx')
ConvR = read.xlsx(fn,1)[,-c(1)]

#----------------------------------------------------------
method.names = c('All','ChisqNS')
cond.names = c('N','bx2zj','bx1zj')
item.names = c('median','min','max')
Bias = GetResM(n.N*n.bx2zj*n.bx1zj,item.names,method.names,cond.names)
RBias=rMSE = SE = SEBias = RejectionR = Bias
CoverageR = PowerMisFit = Bias
ConvergenceR = GetResM(n.N*n.bx2zj*n.bx1zj,item.names,c('X1'),cond.names)$X1

counti = 1
for(ni in N.all){
for(bx2zj in bx2zj.all){
for(bx1zj in bx1zj.all){
	
	cr1 = (Cond.l$N==ni)
	cr2 = (Cond.l$bx2zj == bx2zj)
	cr3 = (Cond.l$bx1zj  == bx1zj)
	sel.id = which(cr1*cr2*cr3==1)
	X1.sublist = X1.list[sel.id]
	ConvR.sub = ConvR[sel.id,]
	
	Bias$X1[counti,1:3] = RBias$X1[counti,1:3] = rMSE$X1[counti,1:3] = c(ni,bx2zj,bx1zj)
	SE$X1[counti,1:3] = SEBias$X1[counti,1:3] = c(ni,bx2zj,bx1zj)
	PowerMisFit$X1[counti,1:3]=CoverageR$X1[counti,1:3] = c(ni,bx2zj,bx1zj)
	RejectionR$X1[counti,1:3]=ConvergenceR[counti,1:3] = c(ni,bx2zj,bx1zj)
	
	tmp.res = Org.Res(Resl=X1.sublist,method.n=method.names)
	Bias$X1[counti,(1:length(tmp.res$biasv))+3] = tmp.res$biasv
	RBias$X1[counti,(1:length(tmp.res$rbiasv))+3] = tmp.res$rbiasv
	rMSE$X1[counti,(1:length(tmp.res$rmsev))+3] = tmp.res$rmsev
	SE$X1[counti,(1:length(tmp.res$esev))+3] = tmp.res$esev
	SEBias$X1[counti,(1:length(tmp.res$seBias))+3] = tmp.res$seBias	
	CoverageR$X1[counti,(1:length(tmp.res$crv))+3] = c(tmp.res$crv)
	RejectionR$X1[counti,(1:length(tmp.res$rrv))+3] = c(tmp.res$rrv)
	PowerMisFit$X1[counti,(1:length(tmp.res$srv))+3] = c(tmp.res$srv)
	ConvergenceR[counti,(1:3)+3] =c(
		median(ConvR.sub$ConvR,na.rm = T),
		min(ConvR.sub$ConvR,na.rm = T),
		max(ConvR.sub$ConvR,na.rm = T) )
	counti = counti +1

}}}

source(paste0(wd,'RCode/RFuncs2.r'))
wd.res = paste0(wd,'Results/Study',STUDYii,'/RAW/')
plotRes(resm=ConvergenceR,c('X1','X2'),
	item.names,ypanel.n = 'bx2zj',ylabel='Convergence Rate',
	pic.names='ConvR.Overall',wd=wd.res)
	
plotRes(resm=PowerMisFit$X1[,-c(4:6)],'ChisqNS',item.names,ypanel.n = 'bx2zj',
	ylabel='Power of Chi-square Test of Model Fit',pic.names='PowerMisFit',wd=wd.res)
	
plotRes(resm=Bias$X1,method.names,item.names,ypanel.n = 'bx2zj',
	ylabel='Bias',pic.names='Bias',wd=wd.res)
	
plotRes(resm=RBias$X1,method.names,item.names,ypanel.n = 'bx2zj',ylabel='Relative Bias',
	pic.names='RBias',wd=wd.res)
	
biasv = unlist(lapply(X1.list,function(x){ x[2,'Bias'] }))
TVv = unlist(lapply(X1.list,function(x){ x[2,'TV'] }))
sum(abs(biasv/TVv)>.1,na.rm = T)

	
plotRes(resm=rMSE$X1,method.names,item.names,ypanel.n = 'bx2zj',
	ylabel='Root Mean Square Error',pic.names='rMSE',wd=wd.res)

plotRes(resm=SEBias$X1,method.names,item.names,ypanel.n = 'bx2zj',ylabel='SE Bias',
	pic.names='SE_Bias',wd=wd.res)
	
plotRes(resm=CoverageR$X1,method.names,item.names,ypanel.n = 'bx2zj',
	ylabel='Coverage Rate',pic.names='Coverage_Rate',wd=wd.res)

plotRes(resm=RejectionR$X1,method.names,item.names,ypanel.n = 'bx2zj',
	ylabel='Rejection Rate',pic.names='Rejection_Rate',wd=wd.res)
	
fn = paste0(wd.res,'Bias.xlsx')
write.xlsx(Bias$X1,fn,sheetName = 'X1.res',append = T)

fn = paste0(wd.res,'rMSE.xlsx')
write.xlsx(rMSE$X1,fn,sheetName = 'X1.res',append = T)

fn = paste0(wd.res,'SEBias.xlsx')
write.xlsx(SEBias$X1,fn,sheetName = 'X1.res',append = T)

fn = paste0(wd.res,'CoverageR.xlsx')
write.xlsx(CoverageR$X1,fn,sheetName = 'X1.res',append = T)

fn = paste0(wd.res,'PowerMisFit.xlsx')
write.xlsx(PowerMisFit$X1,fn,sheetName = 'X1.res',append = T)

fn = paste0(wd.res,'RejectionR.xlsx')
write.xlsx(RejectionR$X1,fn,sheetName = 'X1.res',append = T)

fn = paste0(wd.res,'Simulation2.RBias4AllConds.xlsx')
Bias4AllC = Cond.l
Bias4AllC$s2e.y = NULL
Bias4AllC$s2e.x1 = NULL
Bias4AllC$s2e.x2 = NULL
Bias4AllC$AllBias = as.numeric(lapply(X1.list,function(x){x[1,'Bias']}))/as.numeric(Cond.l$byx1)
Bias4AllC$ChisqNSBias = as.numeric(lapply(X1.list,function(x){x[2,'Bias']}))/as.numeric(Cond.l$byx1)
write.xlsx(Bias4AllC,fn)