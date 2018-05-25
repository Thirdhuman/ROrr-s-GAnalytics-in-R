# Twitter Scraping

library(rjson)
library(twitteR)
library(ROAuth)
library(httr)
library(XML)
library(anytime)
library(syuzhet)

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

tweets.dataframe = data.frame()
tweet_df = data.frame()
query <- unlist(strsplit(users.screennames,","))
query=as.data.frame(query)

output=apply( query, 1,function(i){
	tweets <- userTimeline(i, n = 100)
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
					tweet_info=cbind(author, date, tweet, faves, retweets)
					tweets.dataframe=rbind(tweets.dataframe, tweet_info)}
					#tweets.dataframe$date=as.Date(as.POSIXct(tweets.dataframe$date))
					}
tweets.dataframe=sapply(tweets.dataframe, as.character)
tweets.dataframe = as.data.frame( tweets.dataframe )

tweets.dataframe$date=format(anytime(tweets.dataframe$date), tz = "America/New_York", usetz=TRUE) 
tweets.dataframe$date=as.Date(tweets.dataframe$date)


test=sapply(tweets.dataframe, as.character)
test = as.data.frame( test )

Sys.setlocale()


format.str <- "%a %b %d %H:%M:%S %z %Y"
test=as.POSIXct(strptime(tweets.dataframe$date, format.str, tz = "GMT"), tz = "GMT")

test=as.character(tweets.dataframe$date,"%Y-%m-%d")
str(test)


test=as.POSIXct(test)
format(anytime(1526935968), tz = "America/New_York", usetz=TRUE) #converting from UTC to EST timezone.
	# tweets.dataframe$date=as.character(tweets.dataframe$date)
	# tweets.dataframe$date=as.Date(tweets.dataframe$date, format = "%m/%d/%Y")
	# if(length(tweet_df) >0) {tweets.dataframe <- rbind(tweets.dataframe, (tweet_df))}}



pol = 	lapply(tweets.dataframe$text, function(txt) {
	# strip sentence enders so each tweet is analyzed as a sentence,
	# and +'s which muck up regex
	gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
		# strip URLs
		gsub(' http[^[:blank:]]+', '', .) %>%
		# calculate polarity
		polarity()
})
tweets.dataframe$emotionalValence = sapply(pol, function(x) x$all$polarity)


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


