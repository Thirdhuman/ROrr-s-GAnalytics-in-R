library(twitteR)
library(ROAuth)
library(httr)
library(rjson)
library(XML)


oauth_endpoints("twitter")

myapp <- oauth_app("twitter",
			 key = "kI3TDGtYDpdN5mPWVtZg4E74L",
			 secret = "Lk9upIVEm5BsiQq1o3KalWDWLxHL2hFnRlzwDJAkIGnSUvkr6Y")

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
	     config(token = twitter_token))
stop_for_status(req)
content(req)

access_token_main  =  '3705111012-0ZSGhm0Y5XDkptTYfecD8TwXoJTepfQ6fgtkUX2'
access_token_secret_main  =  'i3EaK25UsGsHvnhJzvyLxTnVOAMusH5giu0oOKf3Y0pJY'
consumer_key_main =  'kI3TDGtYDpdN5mPWVtZg4E74L'
consumer_secret_main  =  'Lk9upIVEm5BsiQq1o3KalWDWLxHL2hFnRlzwDJAkIGnSUvkr6Y'

# Set API Keys
access_token   =   '3705111012-0ZSGhm0Y5XDkptTYfecD8TwXoJTepfQ6fgtkUX2'
access_token_secret  =  'i3EaK25UsGsHvnhJzvyLxTnVOAMusH5giu0oOKf3Y0pJY'
consumer_key = 'kI3TDGtYDpdN5mPWVtZg4E74L'
consumer_secret  = 'Lk9upIVEm5BsiQq1o3KalWDWLxHL2hFnRlzwDJAkIGnSUvkr6Y'


## whatever name you assigned to your created app
appname <- "rorr_bot"
## api key (example below is not a real key)
key <- "kI3TDGtYDpdN5mPWVtZg4E74L"
## api secret (example below is not a real key)
secret <- "Lk9upIVEm5BsiQq1o3KalWDWLxHL2hFnRlzwDJAkIGnSUvkr6Y"
## create token named "twitter_token"
twitter_token <- create_token(
	app = appname,
	consumer_key = key,
	consumer_secret = secret)

setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_token_secret)

twlist <- "cato-policy-scholars"
twowner <- "CatoInstitute"
api.url <- paste0("https://api.twitter.com/1.1/lists/members.json?slug=",
			twlist, "&owner_screen_name=", twowner,"&count=5000")
response <- GET(api.url, config(token=twitteR:::get_oauth_sig()))
response

response.list <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
users.names <- sapply(response.list$users, function(i) i$name)
users.screennames <- sapply(response.list$users, function(i) i$screen_name)


Cato_list <- lists_users('CatoInstitute', n=5)
feed_sanders = lapply(response, function(t) t$getText())


#"https://api.twitter.com/1.1/lists/statuses.json?slug=teams&owner_screen_name=MLS&count=1"

usernames <- nantes_tag$screenName
temp_df <- twListToDF(lookupUsers(usernames))


#https://twitter.com/CatoInstitute/lists/cato-policy-scholars




#Count = 5000 is the number of names per result page,
#        which for this case simplifies things to one page.




user_id	