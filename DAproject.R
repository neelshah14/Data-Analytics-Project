install.packages("tm")
library(tm)
getSources()
getReaders()
cname <- "C:/Users/Neel-Megha/Desktop/corpus"
cname
length(dir(cname))
dir(cname)
proxies <- Corpus(DirSource(cname)) 
class(proxies)
proxies
class(proxies[[1]])
summary(proxies)
hp_proxy2010<-readLines("C:/users/Neel-Megha/Desktop/corpus/hp-2010.html")
qa_sentence_start <- "QUESTIONS AND ANSWERS ABOUT OUR PROXY MATERIALS"
grep(qa_sentence_start, hp_proxy2010, ignore.case = TRUE)
qa_sentence_end <- "If you have any questions about the annual meeting or how to vote or revoke your proxy, you should contact HP's proxy solicitor:"
grep(qa_sentence_end,hp_proxy2010,ignore.case = TRUE)
qa_section2010 <- hp_proxy2010[565:13954]
cat(qa_section2010,file="C:/Users/Neel-Megha/Desktop/corpus/qa_section/qa_section2010.txt",sep="n",append=TRUE)
qa_section2010 <- hp_proxy2010[565:13954]
qa_section2010 <- hp_proxy2010[565:13954]
cname <- "C:/Users/Neel-Megha/Desktop/corpus/qa_section"
library(tm)
qa_sections<-Corpus(DirSource(cname))
inspect(qa_sections[1])
dir(cname)
class(qa_sections)[1]
class(qa_sections[[1]])[1]
summary(qa_sections)
install.packages("magrittr")
library(magrittr)
viewDocs <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()} 
viewDocs(qa_sections,1)
install.packages("readr")
library(readr)
qa_sections <- tm_map(qa_sections, content_transformer(tolower)) 
qa_sections <- tm_map(qa_sections, removeNumbers)
qa_sections <- tm_map(qa_sections, removeWords, stopwords("english"))
viewDocs(qa_sections, 1) 
length(stopwords("english"))
stopwords("en") 
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x)) 
qa_sections <- tm_map(qa_sections, toSpace, "/|<|>|"|=|@|\\|:|;|-|\"")
viewDocs(qa_sections,1)
qa_sections <- tm_map(qa_sections, removePunctuation) 
qa_sections <- tm_map(qa_sections, stripWhitespace)
viewDocs(qa_sections, 1)
qa_sections <- tm_map(qa_sections, removeWords, c("b", "q", "a", "i", "e", "font", 
                                                  "style","n","trim","size","font", "can", "also", "e", "mail", "via", "td", "align","border",
                                                  "familytimes", "roman", "p", "tr", "nbsp", "with", "table", "cellspacing", "valign", "cellpadding",
                                                  "width", "top", "left", "sizepx", "telephone", "if", "may", "help", "us", "will", "please", "unless",
                                                  "visit", "thnbsp","toppx","bottompx", "nnn", "address", "nonbsp", "new", "bottom", "em"))
viewDocs(qa_sections, 1)
toString <- content_transformer(function(x, from, to) gsub(from, to, x)) 
qa_sections <- tm_map(qa_sections, toString, "broker bank", "bb")
install.packages("SnowballC") 
qa_sections <-tm_map(qa_sections, stemDocument) 
viewDocs(qa_sections, 1) 
qa_sections <-tm_map(qa_sections, PlainTextDocument) 
qa_dtm <- DocumentTermMatrix(qa_sections)
qa_dtm
inspect(qa_dtm[1:5, 100:105])
freq <- colSums((as.matrix(qa_dtm)))
length(freq) 
ord <- order(freq) 
freq[head(ord)]
freq[tail(ord)]
head(table(freq), 15)
tail(table(freq), 15)
m<-as.matrix(qa_dtm)
dim(m)
write.csv(m,file= "C:/Users/Neel-Megha/Desktop/corpus/qa_dtm.csv")
dtms <- removeSparseTerms(qa_dtm, 0.1) 
dim(dtms)
inspect(dtms)
freq <-colSums(as.matrix(dtms))
freq 
freq <-colSums(as.matrix(dtms))
freq
table(freq)
findFreqTerms(dtm, lowfreq=50)
table(freq)
findFreqTerms(dtm, lowfreq=100)
findAssocs(dtm, "hp", corlimit = .9)
freq <- sort(colSums(as.matrix(dtms)), decreasing = TRUE)
head(freq, 14)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)
install.packages("ggplot2")
library(ggplot2)
subset(wf,freq>700)
subset(wf, freq>700) %>% ggplot(aes(word, freq))
subset(wf, freq>700) %>% ggplot(aes(word, freq))
install.packages("dplyr") 
library(dplyr)
subset(wf, freq>200) %>% ggplot(aes(word, freq))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=45, hjust=1)) 
install.packages("wordcloud")
library(wordcloud) 
set.seed(123) 
wordcloud(names(freq), freq, min.freq=50) 
