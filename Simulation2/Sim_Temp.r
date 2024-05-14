# Study1
library(MASS)
library(lavaan)
library(xlsx)
library(expm)
wd = 'D:/Research/2023/Necessity_of_multiple_predictors/'
wd.win = 'D:\\Research\\2023\\Necessity_of_multiple_predictors\\'
source(paste0(wd,'RCode/RFuncs2.r'))

# Condition specific parameter values
STUDYii = 2
nsim = 1000

N = Nii
byx1 = BYXii
rzizj = RZIZJii
rx1y = RX1Yii
rx2y = RX2Yii
bxz = BXZii
s2e.y = VEYii
s2e.x1 = VEX1ii
s2e.x2 = VEX2ii
bxz2 = BXZ2ii

var.names = c('y','x1','x2','z1','z2','z3','z4')
nvar = length(var.names) 
nz = 4; nx = 2

B = matrix(c(
	0,byx1,0.4,0,0,0,0,
	0,0,0,bxz,bxz,0,0,
	0,0,0,0,0,bxz2,bxz2,
	rep(0,7*4)),7,7,byrow = T)
dimnames(B) = list(var.names,var.names)

Ve = diag(c(s2e.y,s2e.x1,s2e.x2,1,1,1,1))
dimnames(Ve) = list(var.names,var.names)
Ve['z1','z3'] = Ve['z3','z1'] = 0.5
Ve['z2','z4'] = Ve['z4','z2'] = 0.3
Ve['z1','z2'] = Ve['z2','z1']  = Ve['z3','z4'] = Ve['z4','z3'] = rzizj
Ve['x1','y'] = Ve['y','x1'] = rx1y
Ve['x2','y'] = Ve['y','x2'] = rx2y
mP = get.P(B,Ve)

newf = paste0('mkdir ',wd.win,'Data\\Study',STUDYii,'\\Cond',CONDii)
try(shell(newf))
wd.d = paste0(wd,'Data/Study',STUDYii,'/Cond',CONDii,'/')
#setwd(wd.d)
for(si in 1:nsim){
	dat = mvrnorm(N,rep(0,nvar),mP)
	colnames(dat) = var.names
	fn = paste0(wd.d,'dat',si,'.xlsx')
	write.xlsx(dat,fn,row.names = F)	
}

# Analyze data
newf = paste0('mkdir ',wd.win,'Results\\Study',STUDYii,'\\Cond',CONDii)
try(shell(newf))
wd.out = paste0(wd,'Results/Study',STUDYii,'/Cond',CONDii)
#setwd(wd.out)
fn.out = paste0(wd.out,'/S',STUDYii,'C',CONDii)

IVRM <- ' y ~ x1
	x1 ~ z1 + z2
	y ~~ x1 ' 
res.l = simF2(wd.d,nsim,IVRM,nx,nz,Yn='y',Xn='x',Zn='z')

fn = paste0(fn.out,'.Res.xlsx')
write.xlsx(res.l$sim.res,fn,row.names = F)

fn = paste0(fn.out,'.Conv.xlsx')
write.xlsx(res.l$Convergence,fn,row.names = F)
