library(XML)
library(RCurl)

zz=file("nardus_keywords.txt", "a")
for (i in 8052:1000) {
  tabelaURL=paste("http://nardus.mpn.gov.rs/handle/123456789/",i,"?show=full",sep="")
  if (!url.exists(tabelaURL)) tb=c() else tb=readHTMLTable(tabelaURL)
  if (length(tb)==0) tb=c()
  tmp=tb[[1]]
  dcsubject=as.character(tmp[tmp[,1]=='dc.subject' & tmp[,3]=='en',2])
  pastedline=paste(i,dcsubject,sep="\t")
  cat(iconv(c(pastedline,"\n"),to="UTF-8"),file=zz)
  if (i%%50==0) print(paste(i," ",Sys.time()))
  }
close(zz)

s=sort(table(df[,3]),decreasing = T)
s=s[-1] # delete those with blanco field
head(s,10)
# 
#
# let's see how many were there in 2015 and 2016
datumi=df[nchar(as.character(df[,2]))==10,2]
datumi2=as.vector(datumi)
op=par(mfrow=c(2,1))
hist(as.Date(datumi2),breaks="week", ylim=c(0,180),
     xlim=c(as.Date("2015-01-01"),as.Date("2015-12-31")),freq=T,xlab="",
     main="2015", ylab="Broj odbrana")
grid()
hist(as.Date(datumi2),breaks="week", ylim=c(0,180),
     xlim=c(as.Date("2016-01-01"),as.Date("2016-12-31")),freq=T,xlab="",
     main="2016",ylab="Broj odbrana")
grid()
par(op)
# 
#
# how many were there by universities and faculties in 2016
df=read.table("nardus_publisher.txt",sep="\t",encoding="UTF-8")
dfd=df[nchar(as.character(df[,2]))==10,]
datumi2=as.vector(dfd[,2])
df2016=dfd[datumi2>as.Date("2015-12-31") & datumi2<as.Date("2017-01-01"),]
n=dim(df2016)[1]
nazivi=df2016[,3]
naziviv=as.vector(nazivi)
strnaziv=sapply(naziviv,strsplit,',')
univ=vector()
for (i in 1:n) univ[i]=strnaziv[[i]][1]
s2016=sort(table(df2016[,3]),decreasing = T)
head(s2016,10)
write.table(iconv(cbind(names(s2016),as.vector(s2016)),to="UTF-8"),file="s2016.txt",sep="\t")
