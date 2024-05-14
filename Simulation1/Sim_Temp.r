# Study1
library(MASS)
library(lavaan)
library(xlsx)
library(expm)
wd = 'D:/Research/2023/PowerofChisq/'
wd.win = 'D:\\Research\\2023\\PowerofChisq\\'
source(paste0(wd,'RCode/RFuncs.r'))

# Condition specific parameter values
STUDYii = 1
nsim = 1000

N = Nii
byx = BYXii
rz1z2 = RZ1Z2ii
rxy= RXYii
bxz = BXZii
ryz1 = RYZ1ii
s2e.y = VEYii
s2e.x = VEXii

var.names = c('y','x','z1','z2')
nvar = length(var.names) 
nz = 2;nx = 1;nv = 1

B = matrix(c(0,byx,0,0,
		0,0,bxz,bxz,
		rep(0,4*2)),4,4,byrow = T)
dimnames(B) = list(var.names,var.names)

Ve = diag(c(s2e.y,s2e.x,1,1))
dimnames(Ve) = list(var.names,var.names)
Ve['z1','z2'] = Ve['z2','z1']  = rz1z2
Ve['z1','y'] = Ve['y','z1']  = ryz1
Ve['x','y'] = Ve['y','x']  = rxy

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

IVRM <- ' y ~ x
	x ~ z1 + z2
	y ~~ x'
res.l = simF(wd.d,nsim,IVRM,nx,nz,Yn='y',Xn='x',Zn='z')

fn = paste0(fn.out,'.Res.xlsx')
write.xlsx(res.l$sim.res,fn,row.names = F)

fn = paste0(fn.out,'.Conv.xlsx')
write.xlsx(res.l$Convergence,fn,row.names = F)
