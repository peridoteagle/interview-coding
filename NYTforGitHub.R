#Source
#http://brooksandrew.github.io/simpleblog/articles/new-york-times-api-to-mongodb/

#Please load the following libraries
library(XML)
library(rtimes)
library(stringr)
library(tm)
library(httr)
library(topicmodels)
library(tidytext)
library(dplyr)
library(tibble)
library(quanteda)
library(wordcloud2)
library(collapsibleTree)
library(shiny)
library(htmlwidgets)

#Set the following NYT Key:
Sys.setenv(NYTIMES_AS_KEY = "Your NYT Key")

#Obtaining articles from the NYT Search API
#Search terms and start and end date can be adjusted
urllist<-c()
urls <- c()
mainhead <-c()
headline <- c()
subhead <- c()
sublabel <- c()
dpub <- c()
datepub <- c()
i=0
for (i in 0:73){
  Sys.sleep(1)
  cornerarticles <- as_search(q="corner office adam bryant",start_date ='20090701', end_date = '20171025',page=i)
  urllist <- cornerarticles$data$web_url
  print(length(urllist))
  urls <- c(urls,urllist)
  mainhead <-cornerarticles$data$headline.main
  headline <- c(headline,mainhead)
  subhead <- cornerarticles$data$headline.kicker
  sublabel <- c(sublabel,subhead)
  dpub <- cornerarticles$data$pub_date
  datepub <- c(datepub,dpub)
}

#Selecting only those articles from the Corner Office blog
alldata <-  data.frame(urls,headline,sublabel,datepub)
corneroffice <- subset(alldata,sublabel=="Corner Office")

#Separating relevant information (namely URL, headline, and publication date)
cornerofficeurls <- as.character(corneroffice$urls)
cornerofficeheads <- as.character(corneroffice$headline)
cornerofficedates <- as.character(corneroffice$datepub)

#Defining function to parse URL for article body
parseArticleBody <- function(artHTML) {
  xpath2try <- c('//div[@class="articleBody"]//p',
                 '//p[@class="story-body-text story-content"]',
                 '//p[@class="story-body-text"]'
  )
  for(xp in xpath2try) {
    bodyi <- paste(xpathSApply(htmlParse(artHTML), xp, xmlValue), collapse = "")
    if(nchar(bodyi)>0) break
  }
  return(bodyi)
}


#Loop to extract articles from every relevant URL
#Also separates out bold text (which demarks Adam's questions)
#Replaces specific symbols
#Substitutes headline and date information at the end of every bold line so that it is attached to each answer
articletext1 <- c()
j=1
for (j in 1:length(cornerofficeurls)){
  p <- GET(cornerofficeurls[j])
  html <- content(p, 'text')
  newtry <- str_replace_all(html,"[<]strong[>]"," BOLD ADAMBRYANT ")
  newtry <- str_replace_all(newtry,"[<][/]strong[>]","BOLD HEZDLINE ")
  artBody <- parseArticleBody(newtry)
  artBody <- str_replace_all(artBody,"â\u0080\u0099","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009c","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009d","'")
  artBody <- str_replace_all(artBody,"â\u0080\u0094",",")
  articleinfo<- paste("(",substring(cornerofficedates[j],1,10),": ",cornerofficeheads[j]," [",cornerofficeurls[j],"]",")",sep="")
  artBody <- str_replace_all(artBody,"HEZDLINE",articleinfo)
  test <- strsplit(artBody,split="BOLD")
  articletext1 <- c(articletext1,test)
}

attempt1 <- unlist(articletext1)

#The following code separates the interviewer questions (ADAMBRYANT) from the response questions for all respondents
corp1 <- VCorpus(VectorSource(attempt1))
textVector1 <- sapply(corp1, as.character)
newCorp1 <- Corpus(VectorSource(textVector1[-grep("ADAMBRYANT", textVector1, 
                                                  ignore.case = TRUE)]))
docswithheadlines <- newCorp1

#The following loop repeats above, but does NOT attach headline and date info to each response
#This output is used in term frequency and LDA
articletext2 <- c()
j=1
for (j in 1:540){
  p <- GET(cornerofficeurls[j])
  html <- content(p, 'text')
  newtry <- str_replace_all(html,"[<]strong[>]"," BOLD ADAMBRYANT ")
  newtry <- str_replace_all(newtry,"[<][/]strong[>]","BOLD ")
  artBody <- parseArticleBody(newtry)
  artBody <- str_replace_all(artBody,"â\u0080\u0099","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009c","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009d","'")
  artBody <- str_replace_all(artBody,"â\u0080\u0094",",")
  test <- strsplit(artBody,split="BOLD")
  articletext2 <- c(articletext2,test)
}

attempt2 <- unlist(articletext2)

#The following code separates the interviewer questions from the response questions for all respondents
corp2 <- VCorpus(VectorSource(attempt2))
textVector2 <- sapply(corp2, as.character)
newCorp2 <- Corpus(VectorSource(textVector2[-grep("ADAMBRYANT", textVector2, 
                                                  ignore.case = TRUE)]))
docs <- newCorp2

#Convert symbols to spaces
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "—")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove common English stopwords
docs <- tm_map(docs, removeWords,c("a",	"about",	"above",	"after",	"again",	"against",	"all",	"am",	"an",	"and",	"any",	"are",	"aren't",	"as",	"at",	"be",	"because",	"been",	"before",	"being",	"below",	"between",	"both",	"but",	"by",	"can't",	"cannot",	"could",	"couldn't",	"did",	"didn't",	"do",	"does",	"doesn't",	"doing",	"don't",	"down",	"during",	"each",	"few",	"for",	"from",	"further",	"had",	"hadn't",	"has",	"hasn't",	"have",	"haven't",	"having",	"he",	"he'd",	"he'll",	"he's",	"her",	"here",	"here's",	"hers",	"herself",	"him",	"himself",	"his",	"how",	"how's",	"i",	"i'd",	"i'll",	"i'm",	"i've",	"if",	"in",	"into",	"is",	"isn't",	"it",	"it's",	"its",	"itself",	"let's",	"me",	"more",	"most",	"mustn't",	"my",	"myself",	"no",	"nor",	"not",	"of",	"off",	"on",	"once",	"only",	"or",	"other",	"ought",	"our",	"ours",	"ourselves",	"out",	"over",	"own",	"same",	"shan't",	"she",	"she'd",	"she'll",	"she's",	"should",	"shouldn't",	"so",	"some",	"such",	"than",	"that",	"that's",	"the",	"their",	"theirs",	"them",	"themselves",	"then",	"there",	"there's",	"these",	"they",	"they'd",	"they'll",	"they're",	"they've",	"this",	"those",	"through",	"to",	"too",	"under",	"until",	"up",	"very",	"was",	"wasn't",	"we",	"we'd",	"we'll",	"we're",	"we've",	"were",	"weren't",	"what",	"what's",	"when",	"when's",	"where",	"where's",	"which",	"while",	"who",	"who's",	"whom",	"why",	"why's",	"with",	"won't",	"would",	"wouldn't",	"you",	"you'd",	"you'll",	"you're",	"you've",	"your",	"yours",	"yourself",	"yourselves"))
#Removing contractions since they are generally made of stop words
docs <- tm_map(docs, removeWords,c("ain't",	"aren't",	"can't",	"can't've",	"'cause",	"could've",	"couldn't",	"couldn't've",	"didn't",	"doesn't",	"don't",	"hadn't",	"hadn't've",	"hasn't",	"haven't",	"he'd",	"he'd've",	"he'll",	"he'll've",	"he's",	"how'd",	"how'd'y",	"how'll",	"how's",	"I'd",	"I'd've",	"I'll",	"I'll've",	"I'm",	"I've",	"i'd",	"i'd've",	"i'll",	"i'll've",	"i'm",	"i've",	"isn't",	"it'd",	"it'd've",	"it'll",	"it'll've",	"it's",	"let's",	"ma'am",	"mayn't",	"might've",	"mightn't",	"mightn't've",	"must've",	"mustn't",	"mustn't've",	"needn't",	"needn't've",	"o'clock",	"oughtn't",	"oughtn't've",	"shan't",	"sha'n't",	"shan't've",	"she'd",	"she'd've",	"she'll",	"she'll've",	"she's",	"should've",	"shouldn't",	"shouldn't've",	"so've",	"so's",	"that'd",	"that'd've",	"that's",	"there'd",	"there'd've",	"there's",	"they'd",	"they'd've",	"they'll",	"they'll've",	"they're",	"they've",	"to've",	"wasn't",	"we'd",	"we'd've",	"we'll",	"we'll've",	"we're",	"we've",	"weren't",	"what'll",	"what'll've",	"what're",	"what's",	"what've",	"when's",	"when've",	"where'd",	"where's",	"where've",	"who'll",	"who'll've",	"who's",	"who've",	"why's",	"why've",	"will've",	"won't",	"won't've",	"would've",	"wouldn't",	"wouldn't've",	"y'all",	"y'all'd",	"y'all'd've",	"y'all're",	"y'all've",	"you'd",	"you'd've",	"you'll",	"you'll've",	"you're",	"you've"))

# Remove punctuation marks
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#The following code was written to remove the most common words after looking at the most common words after stemming that did not provide significant interpretation
docs <- tm_map(docs, removeWords,c(" ’s","“","“", "’ve"," ’ll"," ’d"," ’m","didn’t","don’t"," ’re","just","mean","like","yes","thanks","know","get","say","okay","first","mmhmm","well","’ve ","“","—","”","can’t ","â\u0080\u009c"))

#Creating a set of non-stemmed words for a word cloud
docsnostem <-docs

# Text stemming using Porter stemming
docs <- tm_map(docs, stemDocument)

################# TERM FREQUENCY ######################

#Term-document matrix for most frequent words NOT STEMMED:
tdm2 <- TermDocumentMatrix(docsnostem)
#Counting the Top 10 most frequent words
m2 <- as.matrix(tdm2)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
head(d2, 10)
#Wordcloud of these words
wordcloud <- wordcloud2(d2)

saveWidget(wordcloud,file="wordcloud.html")

#Term-document matrix for most frequent words:
tdm <- TermDocumentMatrix(docs)
#Counting the Top 10 most frequent words
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

################ LDA FOR TOPIC ANALYSIS ######################

#Document Term Matrix for LDA
docterm <- DocumentTermMatrix(docs)

#Selecting only the rows greated than 0
rowTotals <- apply(docterm , 1, sum) #Find the sum of words in each Document
ttdm.new   <- docterm[rowTotals> 0, ] 

result <- FindTopicsNumber(
  ttdm.new,
  topics = seq(from = 2, to = 62, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)
#Choosing 40 topics

#Applying LDA to the documents
ap_lda2 <- LDA(ttdm.new, k = 40, control = list(seed = 1234))
chapter_topics <- tidy(ap_lda2, matrix = "beta")

#Finding the top terms per topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(3, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

options(tibble.print_max=Inf)
top_terms

toptermsbytopic <- as.data.frame(top_terms)

cTree<-collapsibleTree(
  toptermsbytopic,
  hierarchy = c("topic", "term"),
  width = 500, height = 500, zoomable = FALSE, tooltip = TRUE
)
saveWidget(cTree,file="ctree.html")

############################################################
# Investigation into the most common word (people)

#Selecting just the documents with people
corp3 <- VCorpus(VectorSource(docswithheadlines))
textVector3 <- sapply(corp3, as.character)
people <- Corpus(VectorSource(textVector3[grep("people", textVector3)]))

#Check Out NYT Shiny App

