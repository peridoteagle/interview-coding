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
library(ldatuning)
library(collapsibleTree)
library(shiny)
library(htmlwidgets)

#Set the following NYT Key:
Sys.setenv(NYTIMES_AS_KEY = "Your NYT API")

#Obtaining articles from the NYT Search API
#Search terms, start and end dates, and number of articles can be adjusted
keywords <- "corner office adam bryant"
startdate <- '20090701'
enddate <- '20171025'
#n is number of desired articles
n <- 730
#nover10 divides n by 10 and rounds up; for use in later for loop
nover10 <- ceiling(n/10)

urllist<-c()
urls <- c()
headlabel <-c()
headline <- c()
kicklabel <- c()
kicker <- c()
seclabel <- c()
section <- c()
dpublabel <- c()
datepub <- c()
newslabel <- c()
newsdesk <- c()
typelabel <-c()
typematerial <- c()
bylinelabel <- c()
byline <- c()
i=0
for (i in 0:nover10){
  Sys.sleep(1)
  tenarticles <- as_search(q=keywords,start_date =startdate, end_date = enddate,page=i)
  urllist <- tenarticles$data$web_url
  print(length(urllist))
  urls <- c(urls,urllist)
  headlabel <-tenarticles$data$headline.main
  headline <- c(headline,headlabel)
  kicklabel <- tenarticles$data$headline.kicker
  kicker <- c(kicker,kicklabel)
  seclabel <- tenarticles$data$section_name
  section <- c(section,seclabel)
  bylinelabel <- tenarticles$data$byline.original
  byline <- c(byline,bylinelabel)
  dpublabel <- tenarticles$data$pub_date
  datepub <- c(datepub,dpublabel)
  newslabel <- tenarticles$data$new_desk
  newsdesk <- c(newsdesk,newslabel)
  typelabel <- tenarticles$data$type_of_material
  typematerial <- c(typematerial,typelabel)
}

#Selecting only those articles from the Corner Office blog
alldata <-  data.frame(urls,headline,kicker,datepub)
relevantarticles <- subset(alldata,kicker=="Corner Office")

#Separating relevant information for this analysis (namely URL, headline, and publication date)
relevanturls <- as.character(relevantarticles$urls)
relevantheads <- as.character(relevantarticles$headline)
relevantdates <- as.character(relevantarticles$datepub)

narticles <- length(relevanturls)

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
#This output is used in term frequency and LDA
articletext1 <- c()
j=1
for (j in 1:narticles){
  p <- GET(relevanturls[j])
  html <- content(p, 'text')
  newtry <- str_replace_all(html,"[<]strong[>]"," BOLD ADAMBRYANT ")
  newtry <- str_replace_all(newtry,"[<][/]strong[>]","BOLD ")
  artBody <- parseArticleBody(newtry)
  artBody <- str_replace_all(artBody,"â\u0080\u0099","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009c","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009d","'")
  artBody <- str_replace_all(artBody,"â\u0080\u0094",",")
  test <- strsplit(artBody,split="BOLD")
  articletext1 <- c(articletext1,test)
}

attempt1 <- unlist(articletext1)

#The following code separates the interviewer questions from the response questions for all respondents
corp1 <- VCorpus(VectorSource(attempt1))
textVector1 <- sapply(corp1, as.character)
newCorp1 <- Corpus(VectorSource(textVector1[-grep("ADAMBRYANT", textVector1, 
                                                  ignore.case = TRUE)]))
docs <- newCorp1

#Loop to extract articles from every relevant URL
#Also separates out bold text (which demarks Adam's questions)
#Replaces specific symbols
#Substitutes headline and date information at the end of every bold line so that it is attached to each answer
#This is used in the Shiny App
articletext2 <- c()
j=1
for (j in 1:narticles){
  p <- GET(relevanturls[j])
  html <- content(p, 'text')
  newtry <- str_replace_all(html,"[<]strong[>]"," BOLD ADAMBRYANT ")
  newtry <- str_replace_all(newtry,"[<][/]strong[>]","BOLD HEZDLINE ")
  artBody <- parseArticleBody(newtry)
  artBody <- str_replace_all(artBody,"â\u0080\u0099","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009c","'")
  artBody <- str_replace_all(artBody,"â\u0080\u009d","'")
  artBody <- str_replace_all(artBody,"â\u0080\u0094",",")
  articleinfo<- paste("(",substring(relevantdates[j],1,10),": ",relevantheads[j]," [",relevanturls[j],"]",")",sep="")
  artBody <- str_replace_all(artBody,"HEZDLINE",articleinfo)
  test <- strsplit(artBody,split="BOLD")
  articletext2 <- c(articletext2,test)
}

attempt2 <- unlist(articletext2)

#The following code separates the interviewer questions (ADAMBRYANT) from the response questions for all respondents
corp2 <- VCorpus(VectorSource(attempt2))
textVector2 <- sapply(corp2, as.character)
newCorp2 <- Corpus(VectorSource(textVector2[-grep("ADAMBRYANT", textVector2, 
                                                  ignore.case = TRUE)]))
docswithheadlines <- newCorp2


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
wordcloud

################ LDA FOR TOPIC ANALYSIS ######################

#Document Term Matrix for LDA
docterm <- DocumentTermMatrix(docs)

#Selecting only the rows greated than 0
rowTotals <- apply(docterm , 1, sum) #Find the sum of words in each Document
ttdm.new   <- docterm[rowTotals> 0, ] 

#The following code helps select the number of topics for LDA
#Note that it can take awhile to run
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
#cTree
saveWidget(cTree,file="ctreecorneroffice.html")

#Seeing which documents are most closely related to a specific topic
ap_documents <- tidy(ap_lda2, matrix = "gamma")

top_docs <- ap_documents %>%
  group_by(topic) %>%
  top_n(3, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

top_docs
#Example interpretation: Doc 1349 is most closely related to topic 27
#What is Doc 1349?
#Doc 1034 in the cleaned data:
docs[[1349]]$content
#Doc 1034 in the original data:
newCorp1[[1349]]$content
#Doc 1034 with Date/Headline/URL:
newCorp2[[1349]]$content

############################################################
# Investigation into the most common word (people)

#Selecting just the documents with people
corp3 <- VCorpus(VectorSource(docswithheadlines))
textVector3 <- sapply(corp3, as.character)
people <- Corpus(VectorSource(textVector3[grep("people", textVector3)]))

#Creating a R-Shiny app
shinyApp(
  ui = fluidPage(
    titlePanel("It's All About People"),
    verbatimTextOutput("text1"),
    actionButton("button1", "Show Me a Quote!"),
    verbatimTextOutput("text2")),
  
  server = function(input, output, session) {
    
    output$text1 <- renderText({
      session$userData$text1 <- "CEO Quotes that Include the Word 'People'"
      session$userData$text1})
    output$text2 <- renderText("Press the button to begin...")
    
    observeEvent(input$button1, {
      llll<-as.character(sample(people,1))
      llll <- str_replace_all(llll,"people","PEOPLE")
      output$text2 <- renderText(llll[1])
    })
  }
)
