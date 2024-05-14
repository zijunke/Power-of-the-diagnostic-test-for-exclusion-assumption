library(xlsx)
wd = 'D:/Research/2023/PowerofChisq/RCode/Study1'
setwd(wd)
Cond.list = read.xlsx('CondList.xlsx',1)

nCond = nrow(Cond.list)
N.all = as.numeric(Cond.list$N)
ryz1.all = as.numeric(Cond.list$ryz1)
byx.all = as.numeric(Cond.list$byx)
bxz.all = as.numeric(Cond.list$bxz)
rxy.all = as.numeric(Cond.list$rxy)
s2e.y.all = as.numeric(Cond.list$s2e.y)
s2e.x.all = as.numeric(Cond.list$s2e.x)
rz1z2.all = as.numeric(Cond.list$rz1z2)

for(Condi in 1:nCond){
	newname = paste0('S1C',Condi,'.r')
	tx <- readLines('Sim_Temp.r')
	tx2 <- gsub('Nii',replace = N.all[Condi],x = tx)
	tx3 <- gsub('BYXii',replace = byx.all[Condi],x = tx2)
	tx4 <- gsub('BXZii',replace = bxz.all[Condi],x = tx3)
	tx5 <- gsub('RXYii',replace = rxy.all[Condi],x = tx4)
	tx6 <- gsub('RYZ1ii',replace = ryz1.all[Condi],x = tx5)
	tx7 <- gsub('VEYii',replace = s2e.y.all[Condi],x = tx6)
	tx8 <- gsub('VEXii',replace = s2e.x.all[Condi],x = tx7)
	tx9 <- gsub('RZ1Z2ii',replace = rz1z2.all[Condi],x = tx8)
	txf <- gsub('CONDii',replace = Condi,x = tx9)
	writeLines(txf, con=newname)	
}
