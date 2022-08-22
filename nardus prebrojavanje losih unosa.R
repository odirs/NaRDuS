# counting bad records
df=read.table("./staRi/nardus_publisher.txt",sep="\t",encoding="UTF-8")

dfa=cbind(df,0)
names(dfa)=c("nrd","datum","vsu","title","abstract")
rbn=strtoi(rb)
j=1
for (i in 1000:8052) {
  print(i)
  if (i==rbn[j]) {
    dfa$abstract[8052+1-i]=1
    if (j==4991) break else j=j+1}
}
su=tapply(dfa$vsu!="",dfa$vsu,sum)
sp=tapply(dfa$title==" ",dfa$vsu,sum)
sa=tapply(dfa$abstract=="0",dfa$vsu,sum)
write.table(iconv(cbind(names(su),as.vector(su),as.vector(sp),as.vector(sa)),to="UTF-8"),
            file="pokvareni.txt",sep="\t")

