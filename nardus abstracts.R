# web scrapping of disseration abstracts
library(XML)
library(RCurl)

for (i in 8052:1000) { # read from the last available abstract backward
  tabelaURL=paste("http://nardus.mpn.gov.rs/handle/123456789/",i,"?show=full",sep="")
  if (!url.exists(tabelaURL)) tb=c() else tb=readHTMLTable(tabelaURL)
  if (length(tb)==0) tb=c()
  tmp=tb[[1]]
  dcdescription=as.character(tmp[tmp[,1]=='dc.description' & tmp[,3]=='en',2])
  if (length(dcdescription)==0) dcdescription=as.character(tmp[tmp[,1]=='dc.description.abstract' & tmp[,3]=='en',2])
  imefajla=paste("./abstracts/",i,".txt",sep="")
  if (length(dcdescription)!=0) cat(iconv(dcdescription,to="UTF-8"),file=imefajla)
  if (i%%50==0) print(paste(i," ",Sys.time()))
  }

