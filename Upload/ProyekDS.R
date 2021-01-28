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
datareview <- vroom(here('DataReview.csv'))
class_emo = classify_emotion(datareview,algorithm="bayes",prior=1.0,verbose=TRUE)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"
# classify polarity
class_pol = classify_polarity(datareview, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
View(polarity)
sent_df = data.frame(text=datareview, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

write.csv(sent_df,file = "DataSentimen.csv")
View(sent_df)
head(sent_df,20)
table(sent_df$emotion)

# plot emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of review") +
  labs(title = "Sentiment Analysis dari Review",
       plot.title = element_text(size=12))


# plot polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of review") +
  labs(title = "Sentiment Analysis dari Review",
       plot.title = element_text(size=12))