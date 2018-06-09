# Twitter Scraping

library(rjson)
library(twitteR)
library(ROAuth)
library(httr)
library(XML)
library(anytime)
library(syuzhet)
ClosestMatch2 =  function(string, stringVector){
  stringVector[amatch(string, stringVector, maxDist=Inf)]}

#library(reticulate)
#os <- import("os")

# Set API Keys
access_token   =   '3705111012-0ZSGhm0Y5XDkptTYfecD8TwXoJTepfQ6fgtkUX2'
access_token_secret  =  'i3EaK25UsGsHvnhJzvyLxTnVOAMusH5giu0oOKf3Y0pJY'
consumer_key = 'kI3TDGtYDpdN5mPWVtZg4E74L'
consumer_secret  = 'Lk9upIVEm5BsiQq1o3KalWDWLxHL2hFnRlzwDJAkIGnSUvkr6Y'

setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_token_secret)

cato_feeds= 'cato-twitter-feeds'
twlist= "cato-policy-scholars"
twowner= "CatoInstitute"
api.url= paste0("https://api.twitter.com/1.1/lists/members.json?slug=",twlist,"&owner_screen_name=",twowner,"&count=5000")
response <- GET(api.url, config(token=twitteR:::get_oauth_sig()))
response

#cato-twitter-feeds

response.list <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
users.names <- sapply(response.list$users, function(i) i$name)
users.screennames <- sapply(response.list$users, function(i) i$screen_name)
users.IDs <- sapply(response.list$users, function(i) i$id_str)
faves <- sapply(response.list$users, function(i) i$favourites_count)
followers <- sapply(response.list$users, function(i) i$followers_count)
date_created <- sapply(response.list$users, function(i) i$created_at)

cato_twitter=cbind(users.names,date_created,users.screennames,users.IDs,faves,followers)
scholars=cato_twitter
names(scholars)[names(scholars) == 'users.names'] ='name.twitter'
names(scholars)[names(scholars) == 'users.IDs'] ='ID.twitter'
names(scholars)[names(scholars) == 'users.screennames'] ='handle.twitter'
write.csv(cato_twitter, "Cato_Scholars.csv")

website.names=list()
for(i in seq_along(scholars$users.names)){
	temp=scholars$users.names[i]
	website.names[i] = ClosestMatch2(temp, df1$author_full) 
}

scholars$name.website=website.names
scholars=as.data.frame(scholars)


tweets.dataframe = data.frame()
tweet_df = data.frame()
query <- unlist(strsplit(users.screennames,","))
query=as.data.frame(query)

output=apply( query, 1,function(i){
	tweets <- userTimeline(i, n = 3200)
	df=do.call("rbind", lapply(tweets, as.data.frame))})

# Loop through the twitter handles & store the results as individual dataframes
length(output[[10]])
str(tweets.dataframe)

tweets.dataframe=data.frame()
for(i in 1:length(output)){
	tweet_info=data.frame()
	for(j in 1:length(output[[i]]$text)){tweet=output[[i]]$text[j]
					author=output[[i]]$screenName[j]
					date=(output[[i]]$created[j])
					retweets=output[[i]]$retweetCount[j]
					faves=output[[i]]$favoriteCount[j]
					reply=output[[i]]$replyToSN[j]
					tweet_info=cbind(author, date, tweet, faves, retweets, reply)
					tweets.dataframe=rbind(tweets.dataframe, tweet_info)}}
					
# Format Data
tweets.dataframe=sapply(tweets.dataframe, as.character)
tweets.dataframe = as.data.frame( tweets.dataframe )
tweets.dataframe$date = as.character( tweets.dataframe$date )
tweets.dataframe$date = as.numeric( tweets.dataframe$date )
tweets.dataframe$date=sapply(tweets.dataframe$date,function(i) format(anydate(i)))
i <- sapply(tweets.dataframe, is.factor) 
tweets.dataframe[i] <- lapply(tweets.dataframe[i], as.character)
tweets.dataframe = as.data.frame( tweets.dataframe )

# Conversation ?
tweets.dataframe$conversation=ifelse(tweets.dataframe$reply==tweets.dataframe$author | is.na(tweets.dataframe$reply), "No", "Yes")
tweets.dataframe$tweet_v <- get_sentences(tweets.dataframe$tweet)

tweets.dataframe$tweet=gsub(' http[^[:blank:]]+', '',  tweets.dataframe$tweet)
tweets.dataframe$tweet=gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', tweets.dataframe$tweet)
# Polarity
tweets.dataframe$syuzhet <- get_sentiment(tweets.dataframe$tweet, method="syuzhet")

tweets.dataframe$emotions <- get_nrc_sentiment(tweets.dataframe$tweet)


min(tweets.dataframe$syuzhet)
library(ggplot2)
test=aggregate(syuzhet ~ author, data= tweets.dataframe, mean)
test$author<-factor(test$author,levels=test$author[order(test$syuzhet)])

test = merge(test, cato_twitter, by.x = 'author', by.y = 'users.screennames' )
test$users.names<-factor(test$users.names,levels=test$users.names[order(test$syuzhet)])

ggplot(data=test, aes(x=users.names,y=syuzhet,colour=users.names) ) +
		geom_bar(aes(fill = users.names),stat="identity",width=.4, position = "dodge") + theme(legend.position="none") + coord_flip()
		
bar +theme(legend.title=element_blank()) + coord_flip()

str(tweets.dataframe)

txt=tweets.dataframe$tweet[i]

pol = 	lapply(tweets.dataframe$tweet, function(i) {txt=tweets.dataframe$tweet[i]
	# strip sentence enders so each tweet is analyzed as a sentence,
	# and +'s which muck up regex
	txt=gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt)
		# strip URLs
		txt=gsub(' http[^[:blank:]]+', '', txt)
		# calculate polarity
		syuzhet_vector <- get_sentiment(txt, method="syuzhet")})

unique(pol)

tweets.dataframe$emotionalValence = sapply(pol, function(x) x$all$polarity)


format(anydate(tweets.dataframe$date), tz = "America/New_York", usetz=F) 
tweets.dataframe$date=as.Date(tweets.dataframe$date)


test=sapply(tweets.dataframe, as.character)
test = as.data.frame( test )

Sys.setlocale()


format.str <- "%a %b %d %H:%M:%S %z %Y"
test=as.POSIXct(strptime(tweets.dataframe$date, format.str, tz = "GMT"), tz = "GMT")

test=as.character(tweets.dataframe$date,"%Y-%m-%d")
str(test)


test=as.POSIXct(test)
	# tweets.dataframe$date=as.character(tweets.dataframe$date)
	# tweets.dataframe$date=as.Date(tweets.dataframe$date, format = "%m/%d/%Y")
	# if(length(tweet_df) >0) {tweets.dataframe <- rbind(tweets.dataframe, (tweet_df))}}





# tweets.dataframe is now a list where each element is a date frame containing
# the results from an individual query; for example...

tweets.dataframe[[1]]

# to combine them into one data frame

lol=do.call(rbind, tweets.dataframe)


twitter_handles <- c("@katyperry","@justinbieber","@Cristiano","@BarackObama")

# Loop through the twitter handles & store the results as individual dataframes
for(handle in query) {
	result <- userTimeline(handle, n = 15 , includeRts = FALSE)
	result$Source <- handle
	
	df_name <- substring(handle, 1)
	
	if(exists(df_name)) {
		assign(df_name, unique(rbind(get(df_name), result)))
	} else {
		assign(df_name, result)}}


for(i in 1:length(lol)){
				polarity=get_sentiment(lol[[i]]$text)
				#Store tweet and polarity in DF 
				sentiments = rbind(polarity, 
																							list(Tweet=lol[[i]]$text,polarity=polarity),
																							stringsAsFactors=FALSE)}
syuzhet_vector <- get_sentiment(poa_v, method="syuzhet")


#Count = 5000 is the number of names per result page,
#        which for this case simplifies things to one page.


