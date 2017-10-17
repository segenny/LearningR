library(tm)

getwd()

tweets.mandrill<-read.csv('Mandrill.csv',header=T)
tweets.other<-read.csv('Other.csv',header=T)
tweets.test<-read.csv('Test.csv',header=T)
tweets.mandrill["class"]<-rep("App",nrow(tweets.mandrill))
tweets.other["class"]<-rep("Other",nrow(tweets.other))

replacePunctuation <- function(x)
{
  x <- tolower(x)
  x <- gsub("[.]+[ ]"," ",x)
  x <- gsub("[:]+[ ]"," ",x)
  x <- gsub("[?]"," ",x)
  x <- gsub("[!]"," ",x)
  x <- gsub("[;]"," ",x)
  x <- gsub("[,]"," ",x)
  return(x)
}

tweets.mandrill$Tweet <- replacePunctuation(tweets.mandrill$Tweet)
tweets.other$Tweet <- replacePunctuation(tweets.other$Tweet)
tweets.test$Tweet <- replacePunctuation(tweets.test$Tweet)
tweets.mandrill.corpus <- Corpus(VectorSource(as.vector(tweets.mandrill$Tweet)))
tweets.other.corpus <- Corpus(VectorSource(as.vector(tweets.other$Tweet)))
tweets.test.corpus <- Corpus(VectorSource(as.vector(tweets.test$Tweet)))
tweets.mandrill.matrix<-t(TermDocumentMatrix(tweets.mandrill.corpus,control=
                                               list(wordLengths=c(4,Inf))));
tweets.other.matrix<-t(TermDocumentMatrix(tweets.other.corpus,control=list(wordLengths=c(4,Inf))));
tweets.test.matrix <- t(TermDocumentMatrix(tweets.test.corpus,control = list(wordLengths=c(4,Inf))));

probabilityMatrix <-function(docMatrix)
{
  termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
  termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
  termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
  termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
  colnames(termSums)<-c("term","count","additive","probability","lnProbability")
  termSums
}

tweets.mandrill.pMatrix<-probabilityMatrix(tweets.mandrill.matrix)
tweets.other.pMatrix<-probabilityMatrix(tweets.other.matrix)

getProbability <- function(testChars,probabilityMatrix)
{
  charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
  charactersNotFound<-length(testChars)-length(charactersFound)
  charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
  charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
  prob<-charactersFoundSum+charactersNotFoundSum
  prob
}

tweets.test.matrix<-as.matrix(tweets.test.matrix)
classified<-NULL

for(documentNumber in 1:nrow(tweets.test.matrix))
{
  tweets.test.chars<-names(tweets.test.matrix[documentNumber,tweets.test.matrix[documentNumber,] %in% 1])
  mandrillProbability <- getProbability(tweets.test.chars,tweets.mandrill.pMatrix)
  otherProbability <- getProbability(tweets.test.chars,tweets.other.pMatrix)
  classified<-c(classified,ifelse(mandrillProbability>otherProbability,"App","Other"))
}

cbind(classified,tweets.test$Tweet)
