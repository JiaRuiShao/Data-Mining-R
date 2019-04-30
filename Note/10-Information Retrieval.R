library(NLP)
library(tm)

source <- DirSource(directory = "I:\\CrudeCorpusPackagetm", encoding = "UTF-8") #input path for documents

crude20 <- Corpus(source, readerControl=list(reader=readPlain)) 

strwrap(crude20[[1]])   ###show the first text###

corp = VCorpus(VectorSource(crude20))

q= c("what","the","level","crude","oil","price") ###terms in the 20th text##
dtm = DocumentTermMatrix(corp,control=list(tolower=TRUE,removePunctuation=TRUE,removeNumbers=TRUE)) ##Document Term Matrix##
newdtm = as.matrix(dtm)
dist = sqrt(rowSums((scale(newdtm,center=newdtm[20,],scale=F)^2))) ####Distance###
mat = cbind(newdtm[,q],dist)
colnames(mat) = c(q,"dist")
mat

# Document length normalization
newdtm.dl = newdtm/rowSums(newdtm)
dist.dl = sqrt(rowSums((scale(newdtm.dl,center=newdtm.dl[20,],scale=F)^2))) 
mat.dl = cbind(newdtm.dl[,q],dist.dl)
colnames(mat.dl) = c(q,"dist.dl")

# l2 length normalization
newdtm.l2 = newdtm/sqrt(rowSums(newdtm^2))
dist.l2 = sqrt(rowSums((scale(newdtm.l2,center=newdtm.l2[20,],scale=F)^2)))  
mat.l2 = cbind(newdtm.l2[,q],dist.l2)

cbind(mat[,7],mat.dl[,7],mat.l2[,7])

###Inverse document frequency### 
dtm.IDF = DocumentTermMatrix(corp,
control=list(tolower=TRUE,removePunctuation=TRUE,removeNumbers=TRUE,weighting=weightTfIdf))

newdtm.IDF = as.matrix(dtm.IDF)
dist.IDF = sqrt(rowSums((scale(newdtm.IDF,center=newdtm.IDF[20,],scale=F)^2)))
mat.IDF = cbind(newdtm.IDF[,q],dist.IDF )
colnames(mat.IDF ) = c(q,"dist")

stopwords("english")

#####Stop Words###
dtm.Stop = DocumentTermMatrix(corp,control=list(tolower=TRUE,
removePunctuation=TRUE,removeNumbers=TRUE,stopwords = TRUE))
newdtm.Stop = as.matrix(dtm.Stop)

dist.Stop = sqrt(rowSums((scale(newdtm.Stop,center=newdtm.Stop[20,],scale=F)^2)))
q1 = c("level","crude","oil","price")  ###terms in the 20th text after removing stop words##
mat.Stop= cbind(newdtm.Stop[,q1],dist.Stop)
colnames(mat.Stop) = c(q1,"dist")

#####Information Preprocessing/Cleaning###
crude20woPunctuation <- tm_map(crude20, removePunctuation)
crude20lower <- tm_map(crude20, tolower)
crude20removeNumbers <- tm_map(crude20, removeNumbers)
findFreqTerms(dtm.Stop, lowfreq=10)
findAssocs(dtm.Stop, c("oil"), corlimit=0.7)

# generate wordcloud
library(wordcloud)
freq <- colSums(as.matrix(dtm.Stop[,findFreqTerms(dtm.Stop, lowfreq=10)]))  
wordcloud(names(freq), freq, min.freq=5,random.color=TRUE,colors=rainbow(7)) 

###Clustering###
### compute distances###
distMatrix <- dist(scale(dtm.Stop))
fit <- hclust(distMatrix)
plot(fit)
clusterdtm.Stop=kmeans(dtm.Stop,3)
clusterdtm.Stop$cluster

layout(matrix(c(1,2,3),1,3))
for(k in 1:3){
 cl <- which(clusterdtm.Stop$cluster == k )
 tdmk <- t(dtm.Stop[cl,])
v = sort(rowSums(as.matrix(tdmk)), decreasing=TRUE)
 d = data.frame(word=names(v), freq=v)
 wordcloud(d$word, d$freq, min.freq=5,
 random.color=TRUE,colors=rainbow(7))
 }

