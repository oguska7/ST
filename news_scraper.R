#The function scrapes Reters news from the webpage and assigns polarity 
#for a particular stock
#as described in the sentimentr package

 #tic - ticker of the stock of interest i.e. tic='aapl'
 #exchange - name of the exchange the stock is traded at, i.e exchange = 'nasdaq'
              #can be NASDAQ, NYSE, or AMEX
 #start_date - start date for dates sequense; first date for which the code will look up news, 
              # i.e start_date = "2014/9/20"
              ## format is "2014/12/14"
 #end_date - end date for dates sequense; last date for which the code will look up news
              # i.e. end_date = "2014/10/15" 
              ## format is "2014/12/14"

Newscraper = function(tic, exchange, strat_date, end_date){
  
  require(sentimentr)
  require(RCurl)
  require(httr)
  require(XML)
  tic = toupper(tic)
  # fix exchange
  exchange = tolower(exchange)
  e = ifelse(exchange == 'nyse', '.N', ifelse(exchange == 'nasdaq', '.O', '.A'))
  
  #fix date sequence
  date=seq(as.Date(start_date), as.Date(end_date), "day")
  text_date=paste(substr(date, 6, 7), substr(date, 9, 10), substr(date, 1, 4), sep="")

  #placeholder for news
  news = as.data.frame(matrix(ncol=3, nrow=0))
  colnames(news) = c("news", "date", "tic")
  for(i in 1: length(text_date)){
   SOURCE = getURL(paste("https://www.reuters.com/finance/stocks/company-news/", tic, e,"?date=", text_date[i], sep=""), encoding="UTF-8")
   PARSED = htmlParse(SOURCE)
   newsline = xpathSApply(PARSED, "//h2",xmlValue)
   newsline= trimws(newsline, "right")
   newsline=as.data.frame(newsline)
   newsline$date=date[i]
   newsline$tic=tic
   news=rbind(news, newsline)
  }
  
  # cleanup
  news=subset(news, !news$newsline=="")
  news$newsline=gsub("UPDATE [[:digit:]]-", "", news$newsline)
  news$newsline=gsub("([A-Z\ ]+)(-)","", news$newsline)
  news$newsline = gsub('[[:punct:] ]+',' ',news$newsline)
  news=subset(news, duplicated(news$newsline) == FALSE)
  
  #polarity
  news$sent=sentiment(news$newsline)$sentiment
  return(news)
}




