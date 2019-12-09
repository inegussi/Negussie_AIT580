#ISrael Negussie
#G00653210

library("selectr")
library("xml2")
library("rvest")

url <- "https://nytimes.com"
webpage <- read_html(url)

# Just sample code. 
titles <- html_text(html_nodes(webpage, "div h2.css-km70tz"))
without_tags <- gsub("<.*?>", "", titles) 
print(without_tags)
print(titles)
titlesdf<-as.data.frame(titles)
print(titlesdf)


news_summary <- html_text(html_nodes(webpage,"div p.css-gs67ux"))
newssum<-as.data.frame(news_summary)
print(newssum)

final<-cbind(titlesdf,newssum)
write.csv(final,"NYTimes.csv")
