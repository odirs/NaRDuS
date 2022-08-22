library(stringr)
brkw=vector()
df=read.table('./staRi/nardus_keywords.txt',sep='\t',encoding="UTF-8",stringsAsFactors=F)
kw=as.character(df[,2])
n=dim(df)[1]
for (i in 1:n) {
  brkw[i]=length(strsplit(kw[i],',')[[1]])
}
mt=matrix(nrow=n,ncol=45)
for (i in 1:n) {
  ki=str_trim(strsplit(kw[i],';')[[1]],side='both') #req stringr
  li=length(ki)
  if (li==1) {
    ki=str_trim(strsplit(kw[i],',')[[1]],side='both')
    li=length(ki)
    }
  for (j in 1:li) mt[i,j]=ki[j];
}
mt[mt=='']=NA
mt=tolower(mt)

s=sort(table(mt),decreasing = T)
barplot(rev(head(s,20)),horiz = T,las=2)

t=table(mt); write.table(iconv(cbind(names(t),as.vector(t)),to="UTF-8"),file="t.txt")
write.table(iconv(cbind(names(s),as.vector(s)),to="UTF-8"),file="s.txt",sep="\t")
write.table(iconv(cbind(names(s)),to="UTF-8"),file="s_names.txt")

