library(xlsx)
wd = 'D:/Research/2023/Necessity_of_multiple_predictors/RCode/'
source(paste0(wd,'RFuncs2.r'))
setwd(paste0(wd,'Study2'))

Cond.list = matrix(NA,160,11)
colnames(Cond.list) = c('Condition','N','byx1','bx1zj','rx1y','rx2y',
	'rzizj','bx2zj','s2e.y','s2e.x1','s2e.x2')
Cond.list = as.data.frame(Cond.list)

byx.all = c(0.1,0.4)
N.all = c(50,100,200,500,1000)
rzizj.all = c(0.1,0.5)         # correlation between instruments
rxy.all = matrix(c(0.2,0.1,0.1,0.2),nrow = 2,2,byrow = T)
bxz.all = c(0.1,0.4)
bxz2.all = c(0.2,0.4)

var.names = c('y','x1','x2','z1','z2','z3','z4')
Ve = diag(1,7)
dimnames(Ve) = list(var.names,var.names)
Ve['z1','z3'] = Ve['z3','z1'] = 0.5
Ve['z2','z4'] = Ve['z4','z2'] = 0.3

Condi = 0
for(Ni in N.all){
for(byx in byx.all){
for(rzizj in rzizj.all){
for(bxz in bxz.all){
for(rxyi in 1:2){
for(bxz2 in bxz2.all){

	Ve['z1','z2'] = Ve['z2','z1']  = Ve['z3','z4'] = Ve['z4','z3'] = rzizj
	Ve['x1','y'] = Ve['y','x1'] = rxy.all[rxyi,1]
	Ve['x2','y'] = Ve['y','x2'] = rxy.all[rxyi,2]

	B = matrix(c(
		0,byx,0.4,0,0,0,0,
		0,0,0,bxz,bxz,0,0,
		0,0,0,0,0,bxz2,bxz2,
		rep(0,7*4)),7,7,byrow = T)
	dimnames(B) = list(var.names,var.names)
	
	PopPars = GetPars(B,Ve)
	Condi = Condi + 1
	Cond.list[Condi,] = c(paste0('Cond',Condi),Ni,byx,bxz,rxy.all[rxyi,],	
		rzizj,bxz2,PopPars$s2e.y,PopPars$s2e.x1,PopPars$s2e.x2)
	if(min(eigen(PopPars$mP)$values)<0){break}
	
}}}}}}

write.xlsx(Cond.list,'CondList.xlsx',row.names = F)