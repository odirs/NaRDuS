library(XML)
library(RCurl)

sink("nardus_titles.txt")
for (i in 7176:1000) {
  tabelaURL=paste("http://nardus.mpn.gov.rs/handle/123456789/",i,"?show=full",sep="")
  if (!url.exists(tabelaURL)) tb=c()
  else tb=readHTMLTable(tabelaURL)
  if (length(tb)==0) tb=c()
  tmp=tb[[1]]
  a=as.character(tmp[tmp[,1]=='dc.title' & tmp[,3]=='en',2])
  cat(i,'\t',toString(a),'\n')
  }
sink()
