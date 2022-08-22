library(XML)
library(RCurl)

zz=file("nardus_publisher2.txt", "a")
for (i in 8052:7407) {
  tabelaURL=paste("http://nardus.mpn.gov.rs/handle/123456789/",i,"?show=full",sep="")
  if (!url.exists(tabelaURL)) tb=c() else tb=readHTMLTable(tabelaURL)
  if (length(tb)==0) tb=c()
  tmp=tb[[1]]
  dcdateissued=as.character(tmp[tmp[,1]=='dc.date.issued',2])
  dcpublisher=as.character(tmp[tmp[,1]=='dc.publisher' & tmp[,3]=='sr',2])
  dctitle=as.character(tmp[tmp[,1]=='dc.title' & tmp[,3]=='en',2])
  if (length(dctitle)==0) dctitle=as.character(tmp[tmp[,1]=='dc.title.alternative' & tmp[,3]=='en',2])
  pastedline=paste(i,dcdateissued,dcpublisher,dctitle,sep="\t")
  cat(iconv(c(pastedline,"\n"),to="UTF-8"),file=zz)
  if (i%%50==0) print(paste(i," ",Sys.time()))
  }
close(zz)

df=read.table("./staRi/nardus_publisher.txt",sep="\t",encoding="UTF-8")
s=sort(table(df[,3]),decreasing = T)
s=s[-1] # delete those with blanco field
head(s,10)

datumi=df[nchar(as.character(df[,2]))==10,2]
datumi2=as.vector(datumi)
hist(as.Date(datumi2),breaks="weeks",xlim=c(as.Date("2015-12-31"),as.Date("2016-12-31")),freq=T,xlab="",main="Broj odbrana po nedeljama (2016)")
hist(as.Date(datumi2),breaks="weeks",xlim=c(as.Date("2015-12-31"),as.Date("2016-12-31")),freq=T,xlab="",ylab="Number of defenses", main="Year 2016")
h=hist(as.Date(datumi2),breaks="months",xlim=c(as.Date("2015-12-31"),as.Date("2017-01-01")),freq=T,xlab="",main="Broj odbrana po nedeljama (2016)",las=2)
grid()
