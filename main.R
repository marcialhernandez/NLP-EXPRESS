#install.packages("party", dependencies = TRUE)
library(randomForest)
library(party)
library(neuralnet)
library(reshape2)
library(RWeka)
require(lsa)
require(qplot)
require(XML)
require(SnowballC)
require(fastmatch)
library(stringr)
library(twitteR)
library(streamR)
library(tm) # para Text Mining
library(slam) # estructuras y algoritmos para arrays y matrices
library(Matrix) # para trabajar con matrices
library(plyr)
require(devtools)
library(car)
library(caret)
library(reshape2)
library(wordcloud)


stemmerOraciones<-function(stringOracion){
  resultado<-c()
  for (x in strsplit(stringOracion,' ')){
    resultado<-c(resultado,unlist(wordStem(x,language = "es")))
  }
  return (tolower(paste(resultado,collapse=" ")))
}

f <- url("http://uce.uniovi.es/mundor/lematizador.rda")
load(f)  # spdictionary, spcommonwords, spmorphemes
close(f)


####
lematizador <- function(word,
                        all.words = FALSE,
                        commonwords =  spcommonwords,
                        dictionary = spdictionary,
                        morphemes = spmorphemes,
                        ...) {
  
  word <- tolower(as.character(word))
  getcanonicalword <- function(words, database, all.words = FALSE ) {
    pos <- fmatch(words, database$word )
    pos <- pos[!is.na(pos)]
    if( all.words ) database$canonical[pos]
    else database$canonical[ pos[1] ]
  }
  
  ## Is it a spanish common word?
  canonical <-  getcanonicalword(word, commonwords, all.words)
  if( any(!is.na(canonical)) ) return(canonical)
  
  ## Is this word in the dictionary?
  canonical <-  getcanonicalword(word, dictionary, all.words)
  if( any(!is.na(canonical)) ) return(canonical)
  
  ## No. So we have to find out 'similar' words from the dictionary
  ## We split the word in root + desinence.
  ## And we paste root with other possibles desinences.
  
  ## Divide the word into root+desinence
  nch <- nchar(word)
  listroots <- lapply(1:(nch-1), function(i, word, nch) {
    root <- substring(word,1,i)
    desinence <- substring(word,i+1,nch)
    c(root, desinence)
  }, word,nch)
  listroots <- as.data.frame(do.call(rbind, listroots))
  names(listroots) <- c("root","desinence")
  
  getderivational <- function(x, mylist) {
    pos <- fmatch(x, names(mylist))
    tmp <- mylist[[pos]]
    if(is.null(tmp) ) {NA}
    else {tmp}
  }
  
  ## Get the derivational morphemes that correspond to each desinence
  derivational <- lapply(as.character(listroots$desinence), getderivational , spmorphemes)
  names(derivational) <- listroots$root
  
  ## Build the possible words: root + derivational morphemes
  possiblewords <-  (unlist(lapply(names(derivational), function(x) paste(x, derivational[[x]], sep=""))))
  possiblewords <- possiblewords[ !duplicated(possiblewords)]
  
  ## Get the canonical words!
  canonical <- getcanonicalword(possiblewords, dictionary, all.words )
  if( any(!is.na(canonical)) ) return(canonical[!is.na(canonical)])
  
  ## No words until here.
  return(NA)
  
}


lematizadorGRAMPAL <- function(word) {
  
  cambiaracentos <- function(jj){ ## FALTA Ãœ
    jj <- gsub("Ãƒ\\u0081","Ã", jj, fixed=TRUE)
    jj <- gsub("Ãƒ\\u0089","Ã‰", jj, fixed=TRUE)
    jj <- gsub("Ãƒ\\u008d","Ã", jj, fixed=TRUE)
    jj <- gsub("Ãƒ\\u0093","Ã“", jj, fixed=TRUE)
    jj <- gsub("Ãƒ\\u009a","Ãš", jj, fixed=TRUE)
    jj <- gsub("Ãƒ\\u0091","Ã‘", jj, fixed=TRUE)
    jj
  }
  
  lematiza <- function( frase ){
    ## Borrowed from
    ## http://www.datanalytics.com/blog/2011/12/13/un-lematizador-para-el-espanol-con-r-Â¿cutre-Â¿mejorable/
    palabra <- gsub( " ", "+", frase )
    base.url <- paste(
      "http://cartago.lllf.uam.es/grampal/grampal.cgi?m=etiqueta&e=",
      palabra, sep = "" )
    tmp <- readLines( base.url, encoding = 'utf8' )
    tmp <- iconv( tmp, "utf-8" )
    tmp <- gsub( "&nbsp;", " ", tmp )
    tmp <- readHTMLTable( tmp )
    tmp <- as.character( tmp[[1]]$V3 )
    tmp <- do.call( rbind, strsplit( tmp, " " ) )[,4]
    tmp
  }
  
  canonical <- lematiza(word)
  canonical <- tolower(cambiaracentos(canonical))
  if( canonical == tolower("UNKN")) canonical <- NA
  canonical
}
#################
#################
#################
#################
#################


sentiment <- function(text, verbose=FALSE)
{
  require(RCurl)
  require(rjson)
  require(plyr)
  r <- dynCurlReader()  
  
  # get rid of single quote 
  text <- gsub("'", ' ' ,text)
  text <- gsub('"', ' ' ,text)
  text <- tolower(text)
  x  <- paste( sprintf("{'text': '%s'}", text),
               collapse = "," ) 
  
  curlPerform(postfields =   
                sprintf('{"language": "auto", "data": [%s]}', x),
              url = "http://www.sentiment140.com/api/bulkClassifyJson", 
              verbose = verbose,
              post = 1L, 
              writefunction = r$update)
  
  r$value()
  
  # list item to JSON 
  j <- fromJSON(r$value()) 
  j$language <- NULL 
  
  k <-  ldply(j, as.data.frame, stringsAsFactors=FALSE) 
  idx.text     <- grep("text", names(k))
  idx.polarity <- grep("polarity", names(k))
  idx.lang     <- grep("language", names(k))
  
  output <-function()
  {
    #   The polarity values are:
    #   0: negative
    #   2: neutral
    #   4: positive
    #    
    #
    label<-function(key.value,key.string) 
    {
      idx= j$polarity == key.value
      if ( sum(idx) > 0 ) 
        j[idx,]$polarity <<- key.string 
    }
    
    label(0, "negative")
    label(2, "neutral" )
    label(4, "positive" )  
  } # output 
  
  if( length(idx.text) == 1)
  {
    j<-k[,2:4]
    names(j) <-c('text','polarity','language') 
    output()
    return (j )
  } else  # multiple case  
  {
    text=unlist( k[,idx.text] )
    names(text) <- NULL 
    
    polarity=unlist( k[,idx.polarity]) 
    names(polarity) <- NULL 
    
    language=unlist( k[,idx.lang]) 
    names(language) <- NULL 
    
    # note: R crash on wide character, trick them reading by csv file. 
    TFILE=tempfile()
    cat(      
      sprintf('"%s",%s,"%s"\n', text,unlist(polarity),unlist(language), sep=','),
      file=TFILE) 
    
    j<-read.csv(file=TFILE, header=FALSE)
    names(j) <- c('text','polarity','language')
    
    output()
    return ( j )     
  }
}


lematiza <- function( frase ){
  palabra <- gsub( " ", "+", frase )
  base.url <- paste( 
    "http://cartago.lllf.uam.es/grampal/grampal.cgi?m=etiqueta&e=", 
    palabra, sep = "" )
  tmp <- readLines( base.url, encoding = 'utf8' )
  tmp <- iconv( tmp, "utf-8" )
  tmp <- gsub( "&nbsp;", " ", tmp )
  tmp <- readHTMLTable( tmp )
  tmp <- as.character( tmp[[1]]$V3 )
  tmp <- do.call( rbind, strsplit( tmp, " " ) )[,4]
  tmp
}




'Twitter Web Client'#1
"Twitter for iPhone"#2
"Twitter for Android"#3
"Twitter for Mac"#4
"Twitter for iPad"#5
"TweetDeck"#6
"Twitter Lite"#7
'Twitter Web Client'#

stringFormat<- function (x){
  return (toString(paste(x)))
}
sourceFormat<- function (stringEntrada){
  
  if (grepl('Twitter Web Client', stringEntrada,fixed = TRUE)==TRUE){
    return (1)
  }
  else if(grepl('Twitter for i Phone', stringEntrada,fixed = TRUE)==TRUE){
    return (2)
  }
  else if(grepl("Twitter for Android", stringEntrada,fixed = TRUE)==TRUE){
    return (3)
  }
  else if(grepl("Twitter for Mac", stringEntrada,fixed = TRUE)==TRUE){
    return (4)
  }
  else if(grepl("Twitter for iPad", stringEntrada,fixed = TRUE)==TRUE){
    return (5)
  }
  else if(grepl("TweetDeck", stringEntrada,fixed = TRUE)==TRUE){
    return (6)
  }
  else{
    return(7)
  }
}

sentimentalFormat<- function (stringEntrada){
  
  if (grepl('neutral', stringEntrada,fixed = TRUE)==TRUE){
    return (0)
  }
  else if(grepl('positive', stringEntrada,fixed = TRUE)==TRUE){
    return (1)
  }
  else{
    return(-1)
  }
}

remove_1 = function(x) iconv(x, "UTF-8", "UTF-8", sub = " ")
remove_2 = function(x){
  vectorString<-strsplit(x,' ')
  result<-c()
  for (word in vectorString){
    result<-c(result,unlist(strsplit(word, "(?<=[a-z])(?=[A-Z])", perl = TRUE)))
  }
  return (paste(result,collapse = ' '))
}
   #strsplit(gsub("([[:upper:]])", " \\1", x), " ") #strsplit(gsub("([A-Z])", " \\1", x), " ")
remove_3 = function(x) gsub("&amp", " ", x)
remove_4 = function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", x)
remove_5 = function(x) gsub("@\\w+", " ", x)
remove_6 = function(x) gsub("[[:punct:]]", " ", x)
remove_7 = function(x) gsub("[[:digit:]]", " ", x)
remove_8 = function(x) gsub("http\\w+", " ", x)
remove_9 = function(x) gsub("[ \t]{2,}", " ", x)
remove_10 = function(x) gsub("^\\s+|\\s+$", " ", x)
remove_11 <- function(x) stringr::str_replace_all(x," "," ")
# Get rid of URLs
remove_12 <- function(x) gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", x) #stringr::str_replace_all(x, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
remove_13 <- function(x) str_replace(x,"RT @[a-z,A-Z]*: "," ")
# Get rid of hashtags
remove_14 <- function(x) stringr::str_replace_all(x,fixed("#")," ")
# Get rid of references to other screennames
remove_15 <- function(x) stringr::str_replace_all(x,"@[a-z,A-Z]*"," ")
remove_16 <- function(x) stringr::str_replace_all(x, "[^[:alnum:]]", " ")
timeFormat<- function (x){
  timetest<-unlist(strsplit(x," "))
  timetest<-plyr::revalue(timetest,c("Jan"="1",
                                     "Feb"="2",
                                     "Mar"="3",
                                     "Apr"="4",
                                     "May"="5",
                                     "Jun"="6",
                                     "Jul"="7",
                                     "Aug"="8",
                                     "Sept"="9",
                                     "Oct"="10",
                                     "Nov"="11",
                                     "Dec"="12"),warn_missing = FALSE)
  
  #testTime<-paste(timetest[3],'-',timetest[2],'-',timetest[6],' ',timetest[4],sep = "")
  #timetest<-strptime(paste(paste(timetest[6],timetest[2],timetest[3],sep="-"),timetest[4],sep=" "), "%Y-%m-%d %H:%M:%S") 
  return (paste(paste(timetest[6],timetest[2],timetest[3],sep="-"),timetest[4],sep=" "))
  #return (paste(timetest[4],sep=" "))
}

timeSeconds<- function (x){
  #testTime<-paste(timetest[3],'-',timetest[2],'-',timetest[6],' ',timetest[4],sep = "")
  timetest<-strptime(x, "%Y-%m-%d %H:%M:%S") 
  #listaTime<-strsplit(timetest[4],":")
  return (as.numeric(timetest[[3]])*3600+as.numeric(timetest[[2]]*60)+timetest[[1]])
  #return (paste(paste(timetest[6],timetest[2],timetest[3],sep="-"),timetest[4],sep=" "))
  #return (paste(timetest[4],sep=" "))
}

timeToYear<- function (x){
  #testTime<-paste(timetest[3],'-',timetest[2],'-',timetest[6],' ',timetest[4],sep = "")
  timetest<-format(strptime(x, "%Y-%m-%d %H:%M:%S"),"%Y")
  #listaTime<-strsplit(timetest[4],":")
  return (as.numeric(timetest))
  #return (paste(paste(timetest[6],timetest[2],timetest[3],sep="-"),timetest[4],sep=" "))
  #return (paste(timetest[4],sep=" "))
}

#Lectura
tweets.df <- parseTweets("./data-challenge.json")
#eleccion columnas de interes
tweets.df.organized<-tweets.df[,c('user_id_str','name','text','description','geo_enabled','user_created_at','statuses_count','followers_count','favourites_count','friends_count','source','created_at','listed_count','country','location','time_zone')]

tweets.df.organized.clear<-tweets.df.organized
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_5))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_6))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_2))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_3))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_4))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_7))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_8))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_9))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_10))
#tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_11))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_12))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_13))
#tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_14))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_15))
#tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_1))
tweets.df.organized.clear[,'description']<-unlist(lapply(tweets.df.organized.clear[,'description'], remove_16))

tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_5))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_6))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_2))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_3))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_4))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_7))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_8))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_9))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_10))
#tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_11))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_12))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_13))
#tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_14))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_15))
#tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_1))
tweets.df.organized.clear[,'text']<-unlist(lapply(tweets.df.organized.clear[,'text'], remove_16))

tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_5))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_6))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_2))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_3))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_4))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_7))
#tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_8))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_9))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_10))
#tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_11))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_12))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_13))
#tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_14))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_15))
#tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_1))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], remove_16))

#Time format
tweets.df.organized.clear[,'user_created_at']<-unlist(lapply(tweets.df.organized.clear[,'user_created_at'], timeFormat))
tweets.df.organized.clear[,'user_created_at']<-unlist(lapply(tweets.df.organized.clear[,'user_created_at'], timeToYear))
tweets.df.organized.clear[,'created_at']<-unlist(lapply(tweets.df.organized.clear[,'created_at'],timeFormat ))#timeSeconds
tweets.df.organized.clear[,'created_at']<-unlist(lapply(tweets.df.organized.clear[,'created_at'],timeSeconds ))

#tweets.df.organized.clear[,'user_created_at']<-strptime(tweets.df.organized.clear[,'user_created_at'], "%Y-%m-%d %H:%M:%S")
#tweets.df.organized.clear[,'created_at']<-strptime(tweets.df.organized.clear[,'created_at'], "%Y-%m-%d %H:%M:%S")

tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], stringFormat))
tweets.df.organized.clear[,'source']<-unlist(lapply(tweets.df.organized.clear[,'source'], sourceFormat))

data.sentiments.text<-sentiment(tweets.df.organized.clear[,'text'])$polarity
data.sentiments.description<-sentiment(tweets.df.organized.clear[,'description'])$polarity

data.stem.text<-unlist(lapply(tweets.df.organized.clear[,'text'], stemmerOraciones))
data.stem.description<-unlist(lapply(tweets.df.organized.clear[,'description'], stemmerOraciones))

corpus.description <- Corpus(VectorSource(data.stem.description),
                             readerControl = list(reader = readPlain,
                                                  language = "es",
                                                  load = TRUE))

corpus.text <- Corpus(VectorSource(data.stem.text),
                      readerControl = list(reader = readPlain,
                                           language = "es",
                                           load = TRUE))


corpus.description <- tm_map(corpus.description, removeNumbers)
corpus.description <- tm_map(corpus.description, removePunctuation)
#corpus.description <- tm_map(corpus.description, tolower)
corpus.description <- tm_map(corpus.description, removeWords, stopwords("spanish"))
#corpus.description <- tm_map(corpus.description, stemDocument, language="spanish")
corpus.description <- tm_map(corpus.description, stripWhitespace)

corpus.text <- tm_map(corpus.text, removeNumbers)
corpus.text <- tm_map(corpus.text, removePunctuation)
#corpus.description <- tm_map(corpus.description, tolower)
corpus.text <- tm_map(corpus.text, removeWords, stopwords("spanish"))
#corpus.description <- tm_map(corpus.description, stemDocument, language="spanish")
corpus.text <- tm_map(corpus.text, stripWhitespace)

#columnas agregar
#data.sentiments.text #valoracion
#data.sentiments.description #valoracion
#corpus.text$content #stemmed y stop words
#corpus.description$content #stemmed y stop words

tweets.df.organized.clear$sentiments.text<-data.sentiments.text
tweets.df.organized.clear$sentiments.description<-data.sentiments.description
tweets.df.organized.clear$st_text<-corpus.text$content
tweets.df.organized.clear$st_description<-corpus.description$content

dataTest<-tweets.df.organized.clear
dataTest<-dataTest[complete.cases(dataTest$user_created_at), ]
dataTest<-dataTest[order(dataTest$created_at),]
dataTest[,'sentiments.text']<-unlist(lapply(dataTest[,'sentiments.text'],sentimentalFormat ))
dataTest[,'sentiments.description']<-unlist(lapply(dataTest[,'sentiments.description'],sentimentalFormat ))
followers_count.max<-max(dataTest[,"followers_count"])
followers_count.min<-min(dataTest[,"followers_count"])
dataTest[,"followers_count"]<- unlist(lapply(dataTest[,"followers_count"], function(x) round((x-followers_count.min)/(followers_count.max-followers_count.min), 1)))
favourites_count.max<-max(dataTest[,"favourites_count"])
favourites_count.min<-min(dataTest[,"favourites_count"])
dataTest[,"favourites_count"]<- unlist(lapply(dataTest[,"favourites_count"], function(x) round((x-favourites_count.min)/(favourites_count.max-favourites_count.min), 1)))
dataTest<-transform(dataTest,sentiments.text=sentiments.text+sentiments.text*followers_count+sentiments.text*favourites_count)
dataTest<-transform(dataTest,sentiments.description=sentiments.description+sentiments.description*followers_count+sentiments.description*favourites_count)
###primer reporte, la forma en que se comportan los datos

postscript("tendenciaSentimentalText.eps",useKerning=TRUE, fillOddEven=TRUE,horizontal = TRUE,pagecentre = TRUE,paper = "executive")
plot(dataTest$created_at,dataTest$sentiments.text,xlab="Time",ylab="Sentiment punctuation",main="Sentimental text tweets")
dev.off()
postscript("tendenciaSentimentalDescription.eps",useKerning=TRUE, fillOddEven=TRUE,horizontal = TRUE,pagecentre = TRUE,paper = "executive")
plot(dataTest$created_at,dataTest$sentiments.description,xlab="Time",ylab="Sentiment punctuation",main="Sentimental text tweets")
dev.off()

#existe una connotada tendencia negativa
#no existe correlacion entre cor(dataTest$sentiments.text,dataTest$sentiments.description)

dataTest.negative<-dataTest[dataTest$sentiments.text<0,]
corpus.dataTest.negative <- Corpus(VectorSource(dataTest.negative$text),
                      readerControl = list(reader = readPlain,
                                           language = "es",
                                           load = TRUE))

btm <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dataTest.negative.myTdm.inverse<-TermDocumentMatrix(corpus.dataTest.negative,control = list(tokenize = btm))
postscript("wordcloud.eps",useKerning=TRUE, fillOddEven=TRUE,horizontal = TRUE,pagecentre = TRUE,paper = "executive")
wordcloud(corpus.dataTest.negative,min.freq=7)
dev.off()
#TermDocumentMatrix (terms: 609, documents: 161)>>
#Non-/sparse entries: 1988/96061
#Sparsity           : 98%
#Maximal term length: 20
#Weighting          : term frequency (tf)
dataTest.negative.myTdm <- TermDocumentMatrix(corpus.dataTest.negative, control = list(tokenize = btm))
####reporte de top palabras negativas
corpus.dataTest.negative.frq1 <- findFreqTerms(dataTest.negative.myTdm,lowfreq=7)
#####
#inicio prediccion
###
trainIndex.dataTest <- createDataPartition(dataTest$sentiments.text, p=.7, list=F)
dataTest.train <- dataTest[trainIndex.dataTest, ]
dataTest.test <- dataTest[-trainIndex.dataTest, ]

my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(3, 2))
sentiments.text.fit <- train(sentiments.text ~ user_created_at + favourites_count, data = dataTest,
                      method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)

sentiments.text.predict <- predict(sentiments.text.fit, newdata = dataTest.test)
sentiments.text.rmse <- sqrt(mean((sentiments.text.predict - dataTest.test$sentiments.text)^2)) 

### 0.3471803
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,		    # do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)
sentiments.text.fit <- train(sentiments.text ~ source+favourites_count, data = dataTest,
                             method = "pcaNNet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)

sentiments.text.predict <- predict(sentiments.text.fit, newdata = dataTest.test)
sentiments.text.rmse <- sqrt(mean((sentiments.text.predict - dataTest.test$sentiments.text)^2)) 
#0.3425037

#rf da   2     0.3148126  0.02337715
sentiments.text.rf<-train(sentiments.text ~ (user_created_at+source)*friends_count,data=dataTest,method="rf",
                trControl=trainControl(method="cv",number=10),
                prox=TRUE,allowParallel=TRUE)

sentiments.text.predict.rf <- predict(sentiments.text.rf, newdata = dataTest.test)
sentiments.text.rmse.rf <- sqrt(mean((sentiments.text.predict.rf - dataTest.test$sentiments.text)^2)) 
#sentiments.text.rmse.rf

plot(predict(sentiments.text.rf, newdata = dataTest.test),dataTest.test$sentiments.text,xlab="predicted",ylab="actual")

postscript("prediccionRandomForest.eps",useKerning=TRUE, fillOddEven=TRUE,horizontal = TRUE,pagecentre = TRUE,paper = "executive")
plot(sentiments.text.predict.rf,xlab="Time",ylab="Sentiment punctuation",main="Sentimental tweets Prediction with Random Forest")
dev.off()
postscript("real.eps",useKerning=TRUE, fillOddEven=TRUE,horizontal = TRUE,pagecentre = TRUE,paper = "executive")
plot(dataTest.test$sentiments.text,xlab="Time",ylab="Sentiment punctuation",main="Sentimental tweets")
dev.off()

plot.randomForest<-cforest(sentiments.text ~ (user_created_at+source)*friends_count,data=dataTest, controls=cforest_control(mtry=10, mincriterion=0))

pt <- party:::prettytree(plot.randomForest@ensemble[[1]], names(plot.randomForest@data@get("input")))
nt <- new("BinaryTree")
nt@tree <- pt
nt@data <- plot.randomForest@data
nt@responses <- plot.randomForest@responses

postscript("randomForestModel.eps",useKerning=TRUE, fillOddEven=TRUE,horizontal = TRUE,pagecentre = TRUE,paper = "executive")
plot(nt)
dev.off()

#####################################
###HASTA ACA ESTA CON PREDICCION
####################################
dtm.text <- DocumentTermMatrix(corpus.text)
dtm.text.v2<-TermDocumentMatrix(corpus.text)
dtm.text.data <- as.matrix(dtm.text)
lsa.text<-lsa(dtm.text.data)

distancias <- as.matrix(dist(dtm.text.data, 
                             method = "binary", 
                             diag = TRUE, 
                             upper = FALSE, 
                             p = 2))

df <- melt(as.matrix(dtm.text.v2))
df <- df[df$Terms %in% findFreqTerms(dtm.text.v2, lowfreq = 45), ]
postscript("heatmap.eps",useKerning=TRUE, fillOddEven=TRUE,horizontal = TRUE,pagecentre = TRUE,paper = "executive")
ggplot(df, aes(as.factor(Docs), Terms, fill=log(value))) + geom_tile() + xlab("Docs") + scale_fill_continuous(low="#FEE6CE", high="#E6550D")
dev.off()

dist.mat.lsa <- dist(t(as.textmatrix(lsa.text))) # compute distance matrix
fit <- cmdscale(dist.mat.lsa , eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot2::qplot(x, y, data = points, geom = "point", alpha = I(1/5))

#Ahora falta pasar el tiempo a tipo tiempo de r
#de source diferenciar de donde fue escrito el tweet, de iphone pc u otro
#agrupar los chilenos de los no, si el conjunto es muy acotado, quitarlos



