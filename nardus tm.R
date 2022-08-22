#text mining and files' analysis
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
cname <- file.path("C:", "texts")
cname

docs <- Corpus(DirSource(cname,encoding = "UTF-8"))
docs <- tm_map(docs, removePunctuation, preserve_intra_word_dashes = T)
for(j in seq(docs)) {
  docs[[j]][[1]] <- gsub("\"", "", docs[[j]][[1]])
  docs[[j]][[1]] <- gsub("\'", "", docs[[j]][[1]])
}
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("one", "two","can","using","used",
                                    "also","well","first","new","different"))   
docs <- tm_map(docs, stemDocument,"english")
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)
freq=vector()
noabs=dim(dtm)[1]
i=1
while (i+49<noabs) {
  freq <- append(freq,colSums(as.matrix(dtm[i:(i+49),])))
  freq=tapply(freq,names(freq),sum)
  i=i+50
  print(c(i, length(freq)))
  }
freq <- append(freq,colSums(as.matrix(dtm[i:noabs,])))
freq=tapply(freq,names(freq),sum)

ord <- order(freq)
freq[tail(ord,50)]



