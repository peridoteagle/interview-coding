# Investigation into the most common word (people)

#Selecting just the documents with people
corp3 <- VCorpus(VectorSource(docswithheadlines))
textVector3 <- sapply(corp3, as.character)
people <- Corpus(VectorSource(textVector3[grep("people", textVector3)]))

dataframe <- data.frame()
for (i in 1:length(people)){
  dataframe[i,1] <- people[i]$content
}
write.csv(dataframe,'/Users/emilyhadley/Documents/IAA Documents/R Code/CEOsTalkAboutPeople/people.csv')

#Creating a R-Shiny app

ui = fluidPage(
    titlePanel("It's All About People"),
    verbatimTextOutput("text1"),
    actionButton("button1", "Show Me a Quote!"),
    verbatimTextOutput("text2"))
  
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

