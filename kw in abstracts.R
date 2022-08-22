# imena svih fajlova u folderu abstracts
files=list.files(path="./abstracts/", pattern="*.txt", full.names=T, recursive=F)
#redni brojevi u NaRDuS
rb=substr(files,13,16)
#svi apstrakti u vektoru
ab=vector()
for (f in files) ab=c(ab, readChar(f, file.info(f)$size))
print(paste(length(ab)," abstracts"))

kws=readLines('./staRi/keywords.txt')
print(paste(length(kws)," keywords"))
mkr=matrix(data=0,nrow=length(ab),ncol=length(kws))

i=0
for (kw in kws) {
  i=i+1
  mkr[grep(kw,iconv(ab,to="UTF-8"),ignore.case=T),i]=1}

#groups of OECD keywords for innovation
digital=sign(rowSums(mkr[,11:25]))
environment=sign(rowSums(mkr[,26:42]))
biotech=sign(rowSums(mkr[,43:54]))
materials=sign(rowSums(mkr[,55:61]))

dfOECD=cbind(rb,digital,environment,biotech,materials)
pt=read.table("./staRi/nardus_publisher.txt",sep="\t",encoding="UTF-8",stringsAsFactors=F)
dfm=merge(dfOECD,pt,by.x="rb",by.y="V1")

#plot digital
s=tapply(digital,dfm$V3,sum)
s2=s[order(s,decreasing=T)]
n2=names(table(dfm$V3)[order(s,decreasing=T)])
n2[1:12]=c("ETF Bg","FTN NS","FON Bg","Mas Bg","Farm Bg","PMF Kg",
           "TMF Bg","Mas Ni","TF NS","GF Bg","Mat Bg","SF Bg")
barplot(s2[1:12],names.arg=n2[1:12],las=2,main="Digital")

#plot environment
s=tapply(environment,dfm$V3,sum)
s2=s[order(s,decreasing=T)]
n2=names(table(dfm$V3)[order(s,decreasing=T)])
n2[1:12]=c("PMF NS","Biol Bg","TMF Bg","FFH Bg","Sum Bg","ETF Bg",
           "Mas Bg","FEIM UPA","GF Bg","Med Bg","Polj Bg","FON Bg")
barplot(s2[1:12],names.arg=n2[1:12],las=2,main="Energy/Environment")

#plot biotech
s=tapply(biotech,dfm$V3,sum)
s2=s[order(s,decreasing=T)]
n2=names(table(dfm$V3)[order(s,decreasing=T)])
n2[1:12]=c("Bio Bg","ETF Bg","Med Bg","TMF Bg","Stom Bg","Farm Bg",
           "FFH Bg","Mat Bg","Mas Bg","FVM Bg","FPN Bg","Fiz Bg")
barplot(s2[1:12],names.arg=n2[1:12],las=2,main="Biotech")

#plot materials
s=tapply(materials,dfm$V3,sum)
s2=s[order(s,decreasing=T)]
n2=names(table(dfm$V3)[order(s,decreasing=T)])
n2[1:12]=c("TMF Bg","PMF NS","FFH Bg","Bio Bg","ETF Bg","Fiz Bg",
           "Mas Bg","UB","Farm Bg","Mas Ni","Med Ni","TF Le")
barplot(s2[1:12],names.arg=n2[1:12],las=2,main="Advanced materials")

#plot dendrogram
cs=colSums(mkr)
ocs=order(cs)
mkro=mkr[,rev(ocs)]
kwso=kws[rev(ocs)]
#head(kwso)
dkro=as.data.frame(mkro)
names(dkro)=kwso
d2=dkro[,1:60]
d2=d2[,-c(1,2,3)] # uklanjam opsta mesta
dd <- dist(scale(t(d2)), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc,main="",xlab="",ylab="Broj disertacija",sub="",cex=1.2)
