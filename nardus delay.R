library(XML)
library(RCurl)

zz=file("nardus_delay2.txt", "a")
for (i in 8052:1000) {
  tabelaURL=paste("http://nardus.mpn.gov.rs/handle/123456789/",i,"?show=full",sep="")
  if (!url.exists(tabelaURL)) tb=c() else tb=readHTMLTable(tabelaURL)
  if (length(tb)==0) tb=c()
  tmp=tb[[1]]
  dcdateavailable=as.character(tmp[tmp[,1]=='dc.date.available',2])
  dcdateissued=as.character(tmp[tmp[,1]=='dc.date.issued',2])
  dcpublisher=as.character(tmp[tmp[,1]=='dc.publisher' & tmp[,3]=='sr',2])
  pastedline=paste(i,dcdateavailable,dcdateissued,dcpublisher,sep="\t")
  cat(iconv(c(pastedline,"\n"),to="UTF-8"),file=zz)
  if (i%%50==0) print(paste(i," ",Sys.time()))
  }
close(zz)

df=read.table("nardus_publisher.txt",sep="\t",encoding="UTF-8")
s=sort(table(df[,3]),decreasing = T)
s=s[-1] # delete those with blanco field
head(s,10)

datumi=df[nchar(as.character(df[,2]))==10,2]
datumi2=as.vector(datumi)
h15=hist(as.Date(datumi2)+365,breaks="week")
h16=hist(as.Date(datumi2),breaks="week",col=rgb(1,0,0,0.6),
         xlim=c(as.Date("2015-12-31"),as.Date("2016-12-31")),freq=T,
         xlab="",main="",ylab="")
plot(h15,col=rgb(0,1,0,.6),add=T)
legend(x=as.Date("2016-01-01"),y=180,c("2015","2016"),fill=c("green","red"),bty="n")


# delay 
df=read.table("./staRi/nardus_delay.txt",sep="\t",encoding="UTF-8")
plot(as.Date(df[,2]),ifelse(as.Date(df[,3])>as.Date("2014-12-31"),
                            as.Date(df[,2])-as.Date(df[,3]),NA),
     ylim=c(0,1000),pch=20,ylab="",xlab="",
     bty="l",main="Kasnjenje sa dostavljanjem disertacije (u danima)")
abline(90,0,lty=2)
