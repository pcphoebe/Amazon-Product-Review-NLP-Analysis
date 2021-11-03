#import file of product reviews
data<-read.csv("out.csv")

#assign id
data$id<-c(1:64706)

#tokenize sentences
data%>%
  select(review)%>%
  unnest_tokens(output = word,input=preprocessed)%>%

# sum the positive words and negative words in the reviews
data2<-data%>%
  unnest_tokens(output = word, input = preprocessed)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(id)%>%
  summarize(positive_words = sum(sentiment=='positive'),
            negative_words = sum(sentiment=='negative'),
            proportion_positive = positive_words/(positive_words+negative_words))%>% 
  ungroup()

# merge two dfs, exclude ones with both non-positive and non-negative
for (i in 1:64706){
  if(i%in%data2$id){
    data[which(data$id==i),]$positive<-data2[which(data2$id==i),]$positive_words
    data[which(data$id==i),]$negative<-data2[which(data2$id==i),]$negative_words
    data[which(data$id==i),]$proportion<-data2[which(data2$id==i),]$proportion_positive
    print(i)
  }
  else{
    data[which(data$id==i),]$positive<-100000
  }
}

#categorize sentiment based on proportion of positive and nagative words in a review
data[which(data$positive==100000),]$proportion<-0.5
data$sentiment<-data$summary
data[which(data$proportion>=0.6),]$sentiment<-"positive"
data[which(data$proportion<=0.4),]$sentiment<-"negative"
data[which(data$proportion>0.4&data$proportion<0.6),]$sentiment<-"neutral"
data3<-data

#test the percentage of positive reviews and nagative reviews after the categorization 
data3$judge<-data3$sentiment
data3[which(data3$sentiment=='positive'&(data$overall%in%c(4,5))),]$judge="Y"
data3[which(data3$sentiment=='negative'&(data$overall%in%c(1,2))),]$judge="Y"
data3[which(data3$sentiment=='positive'&data$overall%in%c(2,3,4)),]$judge="Y"
data5<-data3[which(data3$judge=='Y'),]