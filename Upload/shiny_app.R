library(shiny)
library(here)
library(vroom)
library(tm)
library(RTextTools)
library(dplyr)
library(ggplot2)
library(plotly)
library(syuzhet)
library(e1071)
library(caret)
library(wordcloud)
library(Rstem)
library(sentiment)
library(SentimentAnalysis)

ui <- fluidPage(
  titlePanel("Review Fashion Wanita"),
  sidebarPanel(
    sliderInput(inputId = "num", label = "Choose a number: ",
                value = 5, min = 5, max = 214, step = 5)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Dataset Review", tableOutput("table")),
                  tabPanel("Emotions", plotOutput("barplot1")),
                  tabPanel("Wordcloud", plotOutput("wordcloud"))
    )
  )
)

server <- function(input, output){
  
  # Output Data Reviewclean
  output$table = renderTable({
    datarev <- read.csv('DataSentimen.csv', stringsAsFactors = FALSE)
    review <- datarev$text
    data <- reactive({
      return(head(datarev,input$num))
    })
    data()
  })
  
  # Output Barplot1
  output$barplot1 <- renderPlot({
    data <- data.frame(datarev)
    dataplot <- reactive({
      return(head(data,input$num))
    })
    emos <- function(sentiment_df, title){
      ggplot(dataplot(), aes(x=emotion)) +
        geom_bar(aes(y=..count.., fill=emotion)) +
        scale_fill_brewer(palette="Dark2") +
        labs(x="emotion categories", y="number of review") +
        labs(title = title,
             plot.title = element_text(size=12))
    } 
    emos(dataplot(),"Sentiment Analysis dari Review")
  })
  
  
  # Output Wordcloud
  output$wordcloud <- renderPlot({
    datareview <- read.csv('DataReview.csv')
    review <- datareview$review
    reviewc <- Corpus(VectorSource(review))
    removeall <- function(x)gsub("[^[:alpha:][:space:]]*","",x)
    reviewclean <- tm_map (reviewc,removeall)
    reviewclean <- tm_map(reviewclean,removePunctuation)
    reviewclean <- tm_map(reviewclean,tolower)
    stopwords = readLines("stopwords.txt")
    reviewclean <- tm_map(reviewclean,removeWords,stopwords)
    
    # Build a term-document matrix
    review_dtm <- TermDocumentMatrix(reviewclean)   #dtm = document term matrix
    review_dtm <- as.matrix(review_dtm)
    # Sort by descearing value of frequency
    review_dtm <- sort(rowSums(review_dtm),decreasing=TRUE)
    review_dtm <- data.frame(word = names(review_dtm),freq=review_dtm)
    #generate word cloud
    set.seed(1234)
    wc <- wordcloud(words = head(review_dtm$word, input$num), freq = head(review_dtm$freq, input$num), min.freq = 5,
                    max.words=100, random.order=FALSE, rot.per=0.40, 
                    colors=brewer.pal(8, "Dark2"))
    wc
  })
}

shinyApp(ui = ui, server = server)