# Исходный код для рисунка 1 в статье
#
#

SCandPC.raw<-readRDS("SC_and_PC.RDa")
total.rows<-dim(SCandPC.raw)[1]
set.seed(118)
test.idx<-sample(1:total.rows,total.rows/4)
SCandPC.test<-SCandPC.raw[test.idx,]
SCandPC.mdl<-SCandPC.raw[-test.idx,]

set.seed(2014)
rows<-nrow(SCandPC.mdl)
iid1<-sample(1:rows,2000,replace=T)
iid2<-sample(1:rows,2000,replace=T)

test<-data.frame(PC=SCandPC.mdl$PC[iid1]+SCandPC.mdl$PC[iid2],SC=SCandPC.mdl$SC[iid1]+SCandPC.mdl$SC[iid2])

test$type<-"M"
test$type[test$PC<700]<-"S"
test$type[test$PC>1500]<-"L"
lm1.1<-lm(SC~PC-1,data=SCandPC.mdl)
library(lme4)
M1<-lmer(SC~PC+(1+PC|type),data=test)


bmp(file = "pic1.bmp",
    width = 12, height = 12, units="cm",
    pointsize = 12, family="serif",res=150)
old.par<-par(mar=c(5.1,4.1,0.1,0.1),cex.lab=1.5, cex.axis=1.5)
plot(test$SC~test$PC,pch=3,col="lightgrey",cex=0.3,xlab="Обслуживаемые ПК, ед.", ylab="Число обращений, pcs",main="")
points(SCandPC.mdl$SC~SCandPC.mdl$PC,pch=19,cex=0.5,col="darkgrey")

points(test$PC,predict(M1),pch=4,lwd=0.7,cex=0.7)
abline(lm1.1,lwd=2,lty=4)
dev.off()
