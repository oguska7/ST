################################################
# dat - contains tweets from ST selected for "AAPL" 
# preprocessed for the classification
# and all information about the authors of the messages 
# including 
            # id - message ID
            # body - message text
            # actor.id - id of author of teh message
            # actor.followersCount - number of author's followers
            # actor.followingCount - number of other authors this author's following
            # object.postedTime - time when message was posted
            # entities.sentiment.basic - bullish or bearish tag
            # inReplyTo.id - id of the message tho which this message was a reply
            # sharedNote.id - id of the message which was retweeted
# the code executes classification of remaining non-tagged messages 
# to get a trader's decision to buy or sell
# loads news polarity (newscraper. code)
# and filters the decision to by or sell by the news polarity 
# for each player separately;
# then runs a spatial lag regression to extract herding component
# **dat  - data is proprietary
library(dplyr)
library(RTextTools)
library(spdep)


tweet=dat[,c("body","entities.sentiment.basic", "id")]
colnames(tweet)=c("body", "sent", "id")

bull_train=tweet[which(tweet$sent=="Bullish"),]
bear_train=tweet[which(tweet$sent=="Bearish"),]

sent_test=tweet[which(is.na(tweet$sent)==TRUE),]

all=rbind(bull_train, bear_train, sent_test)

mat= create_matrix(as.vector(all[, 1]), language="english", minDocFreq=1,
                   removeStopwords=TRUE, removeNumbers=TRUE, 
                   stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, tm::weightTfIdf)

container = create_container(mat, as.numeric(all[,2]),
                             trainSize=1:(dim(bull_train)[1]+dim(bear_train)[1]), testSize=((dim(bull_train)[1]+dim(bear_train)[1])+1):(dim(all)[1]),virgin=FALSE)
legend=as.data.frame(table(all$sent))

legend$tag=as.numeric(legend$Var1)
legend=legend[, c(1,3)]
colnames(legend)=c("tag", "entities.sentiment.basic")
models = train_models(container, algorithms = c( "MAXENT"), use_sgd=TRUE, l1_regularizer=1 )


results = classify_models(container, models)
cross_validate(container, 5, algorithm = c("MAXENT"))
sent_test$sent=results$MAXENTROPY_LABEL
# 
sent_test=sent_test[, -1]

data=merge(dat, sent_test, by=c("id"), all.x = TRUE)
data$entities.sentiment.basic=ifelse(is.na(data$entities.sentiment.basic)==TRUE, data$sent, data$entities.sentiment.basic)
data=merge(data, legend, by=c("entities.sentiment.basic"))
data=data[, c("id", "body","actor.id" ,"actor.followersCount",  "actor.followingCount", "actor.followingStocksCount", "object.postedTime",  "inReplyTo.id" ,  "sharedNote.id" , "tag")]
colnames(data)=c("id", "body","actor.id" ,"actor.followersCount",  "actor.followingCount", "actor.followingStocksCount", "date",  "inReplyTo.id" ,  "sharedNote.id" , "tag")

author=as.data.frame(table(data$actor.id))
author=subset(author, author$Freq>=60)#60 over 3 years
news=read.csv("news.csv") ## the data obtained from the newscrape. code

dat_1=as.data.frame(matrix(ncol=6, nrow=0))
for (i in 1: length(author$Var1)){
  dat=subset(data, data$actor.id==author$Var1[i])
  dat=merge(dat, news, by=c("date"))
  lg=glm(as.numeric(tag)~sent, data=dat)
  da=dat[, c("date", "id", "actor.id", "actor.followersCount")]
  da$interact=ifelse(dat$inReplyTo.id>0, dat$inReplyTo.id,ifelse(dat$sharedNote.id>0,dat$sharedNote.id, 0 ) )
  da$error=lg$residuals
  dat_1=rbind(dat_1, da)
}
dat_1$ac2=0
dat_1$fol2=0 ##followers of the one i'm responding to

for (i in 1: dim(dat_1)[1]){
  dat_1$ac2[i]=ifelse(length(dat_1$actor.id[which(dat_1$id == dat_1$interact[i])])==0, 0,dat_1h$actor.id[which(dat_1$id == dat_1$interact[i])]) 
  dat_1$fol2[i]=ifelse(length(dat_1$actor.id[which(dat_1$id == dat_1$interact[i])])==0, 0,dat_1$actor.followersCount[which(dat_1$id == dat_1$interact[i])]) 
  
}

dates=unique(dat_1$date)
Rho=numeric(length(dates))
for (n in 1: length(dates)){
  oneday=subset(dat_1, dat_1$date==dates[n])
  halfday=subset(dat_1,dat_1$interact %in% oneday$id )
  oneday=rbind(oneday, halfday)
  oneday=subset(oneday, duplicated(oneday)==FALSE)
  aut=unique(oneday$actor.id)
  ### W ###
  w=matrix(0, ncol=length(aut), nrow=length(aut))
  colnames(w)=aut
  rownames(w)=aut
  
  for (i in 1:length(aut)){
    a=subset(oneday, oneday$actor.id==aut[i])
    b=ifelse(aut %in% a$ac2, (a$fol2+1), 0)
    
    w[i,]=b}
  for (i in 1:length(aut)){
    w[i, ]=(1/(sum(w[i, ])))*w[i, ]
    w[is.nan(w)]=0}
  w=w+t(w)
  aut=as.data.frame(colnames(w))
  colnames(aut)=c("aut")
  aut$error=0
  for (i in 1:(dim(aut)[1])){
    aut$error[i]=subset(oneday$error, oneday$actor.id==aut$aut[i])[length(subset(oneday$error, oneday$actor.id==aut$aut[i]))]
    
  }
  mod=lagsarlm( error~error ,data= aut, mat2listw(w), zero.policy = TRUE)
  Rho[n]=mod$rho
}
output=as.data.frame(dates)
output$x=Rho

