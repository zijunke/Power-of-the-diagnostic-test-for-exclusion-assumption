library(xlsx)
wd = 'D:/Research/2023/PowerofChisq/RCode/'
source(paste0(wd,'RFuncs.r'))
setwd(paste0(wd,'Study1'))

Cond.list = matrix(NA,240,9)
colnames(Cond.list) = c('Condition','N','byx','ryz1','bxz','rxy',
	'rz1z2','s2e.y','s2e.x')
Cond.list = as.data.frame(Cond.list)

byx.all = c(0.1,0.4)
N.all = c(50,100,200,500,1000)
rz1z2.all = c(0.1,0.5) # correlation between instruments
rxy.all = c(0.1,0.2)
bxz.all = c(0.1,0.4)
ryz1.all = c(0.05,0.1,0.2)

var.names = c('y','x','z1','z2')
Ve = diag(1,4)
dimnames(Ve) = list(var.names,var.names)

Condi = 0
for(Ni in N.all){
for(ryz1i in ryz1.all){
for(byxi in byx.all){
for(rz1z2 in rz1z2.all){
for(bxzi in bxz.all){
for(rxyi in rxy.all){

	B = matrix(c(0,byxi,0,0,
		0,0,bxzi,bxzi,
		rep(0,4*2)),4,4,byrow = T)
	dimnames(B) = list(var.names,var.names)

	Ve['z1','z2'] = Ve['z2','z1']  = rz1z2
	Ve['z1','y'] = Ve['y','z1']  = ryz1i
	Ve['x','y'] = Ve['y','x']  = rxyi
	PopPars = GetPars(B,Ve)

	Condi = Condi + 1
	Cond.list[Condi,] = c(paste0('Cond',Condi),Ni,byxi,ryz1i,bxzi,rxyi,rz1z2,
		PopPars$s2e.y,PopPars$s2e.x)
	
}}}}}}

write.xlsx(Cond.list,'CondList.xlsx',row.names = F)