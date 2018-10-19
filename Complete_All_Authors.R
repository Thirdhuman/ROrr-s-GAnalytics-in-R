# Google Analytics
setwd("~/Desktop/Welfare_Policy/Data/Data_Explorations/Google_Analytics(Cato)")
# My Packages
library(googleAnalyticsR)
library(tidyverse)
library(httr)
library(RCurl)
library(XML)
library(foreach)
library(stringr)
library(ggplot2)
library(data.table)
library(stringdist)
library(pbmcapply)
library(openxlsx)
#library(plyr)

# Define Date
current_date=format(Sys.time(), "%Y-%m-%d")
current_date=as.Date(current_date)
# Open Google Analytics
account_list=ga_account_list()
ga_id=account_list$viewId[1]
# Choose person(s) of interest
# Establish date range
from="2014-07-01" # (Earliest Available)
#from="2018-7-14" # (Insert Other)
to =as.character(current_date)
#### create filters on dimensions ####
dimf=dim_filter("dimension1","PARTIAL", expressions=name,not = F, caseSensitive = F)
dimf2=dim_filter("countryIsoCode","EXACT","US",not = F)
fc2=filter_clause_ga4(list(# dimf #,dimf2
			), operator = "OR")

#### Construct File Name ####
# from_s = (from);from_m = as.character(from)
# from_y=str_sub(from, start=3, end = 4);from_m=str_sub(from, start=6, end = 7)
# to_y=str_sub(to, start=3, end = 4);to_m=str_sub(to, start=6, end = 7)
# analysis_range=paste0("(",from_m,from_y,'-',to_m,to_y,")")
# initials=function(a, b){
# 	a=str_split(a, "&")
# 	a1=lapply(a, function(x){
# 		x1=str_split(str_trim(x), " ")
# 		paste0(unlist(lapply(x1, str_sub, 1, 2)), collapse="")	})
# 	paste0(unlist(a1), b) }
# analysis_identifier=initials(name,analysis_range)

#################################################################################
#################### Begin Module 1: Google Analytics ###########################
#################################################################################

#### Specify Search terms ####
max = 500000000
met = c("sessions", #"pageviews",
	'timeOnPage','avgTimeOnPage',
	"entrances","bounces", 'exitRate')
dim = c("date", 'pageTitle',
	"ga:dimension1", #'channelGrouping',# 'city', 'region',
	#'ga:dimension2', 
	'pagePath')

#lst=sapply(str_extract_all(name), function(x) substr(x, 0, 2))
# view id of your Google Analytics view where 1 conversion = visit
vid="3016983"
#### Launch Google Analytic Retrieval Function ####
get_data=function(vid,from,to,dim,met,max){df=google_analytics(
			viewId=vid,date_range=c(from,to),metrics=met,dimensions=dim, #met_filters = fc, 
 		dim_filters = fc2,  max = max	,anti_sample = TRUE)
# clean up and set class
		df$dimension1 = gsub('O&#039;Toole', "O'Toole", df$dimension1)
		df$dimension1 = gsub('&#039;', "'", df$dimension1)
		df$dimension1 = gsub('&quot;Chip&quot;',  "'Chip'", df$dimension1)
		df$author_full=df$dimension1
		df$dimension1=NULL
df}
gadata=get_data(vid=vid, from=from, to=to, dim=dim, met=met, max=max) # Run Download function
df1 = as.data.frame(gadata) # Convert to dataframe
df1=df1[-which(!is.na(as.numeric(df1$author_full))),] # Clean author names pt.1
df1$pageTitle=gsub("([ | ].*)[ | ] .*", "\\1", df1$pageTitle) # Clean author names pt.2
authurs=as.list(unique(df1$author_full));name_sample=as.list(unique(df1$author_full))
save(df1, file = "Big_Raw_GA_DAT.RData")
save(name_sample, file = "NAME_SAMPLE.RData")
#####################################################
#### Initialize Cleanup of Google Analytics Data ####
#####################################################
load( file = "Big_Raw_GA_DAT.RData")
df1$obs_day=as.Date(df1$date)
df1$date=NULL
#RemovePopularProxyStrings
df1=df1[!grepl("search/srpcache",df1$pagePath),]
df1=df1[!grepl("www.filterbypass.me",df1$pagePath),]
df1=df1[!grepl("wikipedia.org/secure",df1$pagePath),]
df1=df1[!grepl("www.googleadservices.com",df1$pagePath),]
df1=df1[!grepl("bit.ly",df1$pagePath),]
df1=df1[!grepl("j.mp",df1$pagePath),]
df1=df1[!grepl("nl.hideproxy.me",df1$pagePath),]
df1=df1[!grepl("cc.bingj.com",df1$pagePath),]
df1=df1[!grepl("prolegis/getfile",df1$pagePath),]
df1=df1[!grepl("cluster23-files.instructure",df1$pagePath),]
df1=df1[!grepl("rorr.im/reddit.com",df1$pagePath),]
df1=df1[!grepl("www.duplichecker.com",df1$pagePath),]
df1=df1[!grepl("copyscape",df1$pagePath),]
df1=df1[!grepl("us1.proxysite",df1$pagePath),]
df1=df1[!grepl("eveil.alize",df1$pagePath),]
df1=df1[!grepl("honyaku.yahoofs.jp",df1$pagePath),]
df1=df1[!grepl("ow.ly",df1$pagePath),]
df1=df1[!grepl("searchenginereports.net",df1$pagePath),]
df1=df1[!grepl("xitenow",df1$pagePath),]
df1=df1[!grepl("cato.us1.list-manage.com/track/click",df1$pagePath),]
#MoreReshapingofPagePaths&RemoveTrailingStrings#
df1$pagePath2=gsub(".*www.cato.org","www.cato.org",df1$pagePath)
df1$pagePath2=gsub(pattern="[?].*","",x=df1$pagePath2)
df1$pagePath2=gsub(pattern=".*https://*|&.*","",x=df1$pagePath2)
df1$pagePath2=gsub(pattern=".*genius.it/*|&.*",replacement="",x=df1$pagePath2)
df1$pagePath2=gsub(".*www-cato-org","www.cato.org",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub(".*www.cato.org.*?/publications","www.cato.org/publications",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub("\\.myaccess.library.utoronto.ca","",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub("proxy.earlham.edu","www.cato.org",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub("object.cato.org","www.cato.org",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub("seekingalpha.com","www.cato.org",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub(".ezproxy.wallawalla.edu","",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub(".stfi.re","",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub(".helin.uri.edu","",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub(".proxy.lib.pdx.edu","",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub("www.cato.org:80","www.cato.org",df1$pagePath2,perl=TRUE)#Index.html
df1$pagePath2=gsub("www.cato-at-liberty.org","www.cato.org/blog",df1$pagePath2,perl=TRUE)#Index.html
df1$pagePath2=gsub("www.cato/","www.cato.org/",df1$pagePath2,perl=TRUE)#Index.html
df1$pagePath2=gsub("index.html","",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub(".jpllnet.sfsu.edu","",df1$pagePath2,perl=TRUE)
df1$pagePath2=gsub("what-have-the-politicians-in-washington-given-us/","what-have-politicians-washington-given-us",df1$pagePath2)
df1$pagePath2=gsub(".ezproxy.csusm.edu","",df1$pagePath2,perl=TRUE)#Index.html
df1$pagePath2=gsub("proxy.unfake.us/proxy/350/","www.cato.org/blog/",df1$pagePath2,perl=TRUE)#Index.html
df1$pagePath2=ifelse(grepl("php$",df1$pagePath2)==T,df1$pagePath,df1$pagePath2)
refcols=c("obs_day",'pagePath','pagePath2')
df1=df1[,c(refcols,setdiff(names(df1),refcols))]
df1$pagePath1=df1$pagePath
df1$pagePath=df1$pagePath2
df1$pagePath2=NULL
rm(refcols,get_data)
save(df1, file = "df1.RData")
#######################################################################################
########################## Begin Module 2: Twitter ####################################
#######################################################################################
library(rjson);library(twitteR);library(ROAuth);library(httr);library(XML);library(anytime);library(syuzhet)
ClosestMatch2 =  function(string, stringVector){stringVector[amatch(string, stringVector, maxDist=Inf)]}
access_token   =  '3705111012-0ZSGhm0Y5XDkptTYfecD8TwXoJTepfQ6fgtkUX2' # Set API Keys
access_token_secret  = 'i3EaK25UsGsHvnhJzvyLxTnVOAMusH5giu0oOKf3Y0pJY'
consumer_key = 'kI3TDGtYDpdN5mPWVtZg4E74L'
consumer_secret  = 'Lk9upIVEm5BsiQq1o3KalWDWLxHL2hFnRlzwDJAkIGnSUvkr6Y'
setup_twitter_oauth(consumer_key, consumer_secret,access_token, access_token_secret)
cato_feeds= 'cato-twitter-feeds';twlist= "cato-policy-scholars";twowner= "CatoInstitute"
api.url= paste0("https://api.twitter.com/1.1/lists/members.json?slug=",twlist,"&owner_screen_name=",twowner,"&count=5000")
response=GET(api.url, config(token=twitteR:::get_oauth_sig()))
load( file = "NAME_SAMPLE.RData")
response.list=fromJSON(content(response, as = "text", encoding = "UTF-8"))
users.names=sapply(response.list$users, function(i) i$name)
users.screennames=sapply(response.list$users, function(i) i$screen_name)
users.IDs=sapply(response.list$users, function(i) i$id_str)
faves=sapply(response.list$users, function(i) i$favourites_count)
followers=sapply(response.list$users, function(i) i$followers_count)
date_created=sapply(response.list$users, function(i) i$created_at)
cato_twitter=cbind(users.names,date_created,users.screennames,users.IDs,faves,followers)
scholars=as.data.frame(cato_twitter)
names(scholars)[names(scholars) == 'users.names'] ='name.twitter'
names(scholars)[names(scholars) == 'users.IDs'] ='ID.twitter'
names(scholars)[names(scholars) == 'users.screennames'] ='handle.twitter'
website.names = list();for(i in seq_along(scholars$name.twitter)){
	temp=scholars$name.twitter[i]
	website.names[i] = ClosestMatch2(temp, name_sample) }
scholars$name.website=website.names;scholars=as.data.frame(scholars)
i=sapply(scholars, is.factor);scholars[i]=lapply(scholars[i], as.character) # Change factors to characters
scholars$name.website=unlist(scholars$name.website) # unlist the cato website name column
scholars$title_category_1 = NA;scholars$category_1 = NA # Create Titles and Bins for Categories 1
scholars$title_category_2 = NA;scholars$category_2 = NA # Create Titles and Bins for Categories 2
scholars$title_category_3 = NA;scholars$category_3 = NA # Create Titles and Bins for Categories 3
scholars$title_category_4 = NA;scholars$category_4 = NA # Create Titles and Bins for Categories 4
scholars$title_category_5 = NA;scholars$category_5 = NA # Create Titles and Bins for Categories 5
write.csv(scholars, "Cato_Scholars.csv") # Save
#detach('package:rjson');detach('package:twitteR');detach('package:ROAuth'); # Detach 1
#detach('package:httr');detach('package:XML');detach('package:anytime');detach('package:syuzhet')  # Detach 2
rm(consumer_key, consumer_secret,access_token, access_token_secret,name_sample,faves,twowner) # Clear environment
rm(response,response.list,temp,twlist,to_m,to_y,i,cato_feeds,account_list,api.url,website.names)# Clear environment
rm(cato_twitter,users.names,date_created,users.screennames,users.IDs,followers,from_m,from_s,from_y)# Clear environment
# Generate vector to create column for each author
load( file = "df1.RData")
scholars=read.csv('Cato_Scholars.csv')
authur_row=as.data.frame(scholars$name.website)
colnames(authur_row) = authur_row[1, ] # the first row will be the header
authur_row = authur_row[-1, ]          # removing the first row.
authur_row=as.vector(as.character(authur_row))
df1[as.character(authur_row)] = NA
##################################################################################
###### Warning: Stop here if column #s are altered since previous analysis! ######
##################################################################################
colnames(df1)
df1 = df1[, c(11, 1:10, 12:64)]
df1_1=df1[ ,1:10]
df1_2=df1[ ,11:64]
df1_2[-1] = lapply(names(df1_2[-1]), function(nm) grepl(nm, df1_2[[1]]))
df2=cbind(df1_1,df1_2)
rm(gadata, dim, dimf, dimf2, fc2,ga_id, met, max, vid)
rm(df1_1,df1_2,authur_row,df1)
save(df2, file = "df2.RData")
##################################################################################
##################### Begin Module 3: Cato Sitemap XML ###########################
##################################################################################
library(stringdist)
library(xml2)
library(XML)
library(data.table)
#### Download & Split ####
url_1=download_xml('https://www.cato.org/sitemap.xml?page=1')
url_2=download_xml('https://www.cato.org/sitemap.xml?page=2')
# Read XML 1
xmlfile=xmlParse(url_1) # Parse
tagsList=xmlToList(xmlfile) # Convert to List
tagsList=lapply(tagsList, function(x) as.data.table(as.list(x)))# Each List element is a character vector. Convert them.
tags_1=rbindlist(tagsList, use.names = T, fill = T) # Rbind all the 1-row data.tables into a single data.table
tags_1=as.data.frame(tags_1) # convert to dataframe
# Read XML 2
xmlfile=xmlParse(url_2) # Parse
tagsList=xmlToList(xmlfile)# Convert to List
tagsList=lapply(tagsList, function(x) as.data.table(as.list(x)))# Each List element is a character vector. Convert them.
tags_2=rbindlist(tagsList, use.names = T, fill = T) # Rbind all the 1-row data.tables into a single data.table
tags_2=as.data.frame(tags_2) # convert to dataframe
# Combine XML tag pages
url_vector=merge(tags_1, tags_2, by=c('loc',"priority",'changefreq','lastmod','schemaLocation'), all = T)
url_vector=as.vector(url_vector$loc) # This is your vector of URLs from the XML Files
######## Apply & Save ########
SafeGet = function (x)	{
	tryCatch({
	#short_url_vector
	html=GET(x)
	print(html)
	parsed=htmlParse(html)
	root=xmlRoot(parsed)
	title = xpathSApply(root, "//h1[@class='page-h1'][1]", xmlValue)
	return(title)	
	Sys.sleep(.001)},
	error=function(e){cat("ERROR :", conditionMessage(e))}, '0')}
split_url_vector = split(url_vector, ceiling(seq_along(url_vector)/5000))
split_1=split_url_vector[[1]];responses_1=pbmclapply(split_1,SafeGet,mc.preschedule=T);save(responses_1,file="responses_1.RData");rm(split_1) 
split_2=split_url_vector[[2]];responses_2=pbmclapply(split_2,SafeGet,mc.preschedule=T);save(responses_2,file="responses_2.RData");rm(split_2)
split_3=split_url_vector[[3]];responses_3=pbmclapply(split_3,SafeGet,mc.preschedule=T);save(responses_3,file="responses_3.RData");rm(split_3)
split_4=split_url_vector[[4]];responses_4=pbmclapply(split_4,SafeGet,mc.preschedule=T);save(responses_4,file="responses_4.RData");rm(split_4)
split_5=split_url_vector[[5]];responses_5=pbmclapply(split_5,SafeGet,mc.preschedule=T);save(responses_5,file="responses_5.RData");rm(split_5)
split_6=split_url_vector[[6]];responses_6=pbmclapply(split_6,SafeGet,mc.preschedule=T);save(responses_6,file="responses_6.RData");rm(split_6)
split_7=split_url_vector[[7]];responses_7=pbmclapply(split_7,SafeGet,mc.preschedule=T);save(responses_7,file="responses_7.RData");rm(split_7)
split_8=split_url_vector[[8]];responses_8=pbmclapply(split_8,SafeGet,mc.preschedule=T);save(responses_8,file="responses_8.RData");rm(split_8)
split_9=split_url_vector[[9]];responses_9=pbmclapply(split_9,SafeGet,mc.preschedule=T);save(responses_9,file="responses_9.RData");rm(split_9)
split_10=split_url_vector[[10]];responses_10=pbmclapply(split_10,SafeGet,mc.preschedule=T);save(responses_10,file="responses_10.RData");rm(split_10)
split_11=split_url_vector[[11]];responses_11=pbmclapply(split_11,SafeGet,mc.preschedule=T);save(responses_11,file="responses_11.RData");rm(split_11)
split_12=split_url_vector[[12]];responses_12=pbmclapply(split_12,SafeGet,mc.preschedule=T);save(responses_12,file="responses_12.RData");rm(split_12)
split_13=split_url_vector[[13]];responses_13=pbmclapply(split_13,SafeGet,mc.preschedule=T);save(responses_13,file="responses_13.RData");rm(split_13)
split_14=split_url_vector[[14]];responses_14=pbmclapply(split_14,SafeGet,mc.preschedule=T);save(responses_14,file="responses_14.RData");rm(split_14)
#################### Combine & Clean ########################## 
load(file="responses_1.RData");load(file="responses_2.RData");load(file="responses_3.RData");load(file="responses_4.RData");load(file="responses_5.RData");load(file="responses_6.RData");load(file="responses_7.RData");load(file="responses_8.RData");load(file="responses_9.RData");load(file="responses_10.RData");load(file="responses_11.RData");load(file="responses_12.RData");load(file="responses_13.RData");load(file="responses_14.RData");
website_responses=(c(responses_1,responses_2,responses_3,responses_4,responses_5,responses_6,responses_7,responses_8,responses_9,responses_10,responses_11, responses_12,responses_13,responses_14));rm(responses_1,responses_2,responses_3,responses_4,responses_5,responses_6,responses_7,responses_8,responses_9,responses_10,responses_11, responses_12,responses_13,responses_14)

title=trimws(website_responses);link_title_df=as.data.frame(cbind(title=title, pagePath=url_vector))
save(link_title_df, file = "link_title_df.RData")

#########################################################################################
############# Begin Module 4: Match Google Analytics with Cato Sitemap XML ##############
#########################################################################################
df_intermediate
load(file = "link_title_df.RData") # Load XML Data
is.na(link_title_df) = lengths(link_title_df) == 0
link_title_df[lengths(link_title_df) == 0] = NA
link_title_df=subset(link_title_df, (link_title_df$title)!='NA')
title_list<-link_title_df[["title"]]
url_list=link_title_df[["pagePath"]]
df_intermediate = merge(df1, link_title_df, by.x = 'pagePath', by.y = 'pagePath', all.x=T)

save(df_intermediate, file = "df_intermediate.RData")
############# Save Split-Apply-Combine #################


link_title_df=as.data.frame(cbind(title=title, pagePath=url_vector))

df_intermediate=df_intermediate[(df_intermediate$title)!='NA',]
url_vector_dfi=df_intermediate[["pagePath"]]
url_vector_dfi=as.vector(unique(url_vector_dfi))

title_vector_dfi=df_intermediate[["title"]]
title_vector_dfi=as.vector(unique(title_vector_dfi))

ClosestMatch2 = function(string, stringVector){stringVector[amatch(string, stringVector, maxDist=Inf,nomatch=0)]}
ClosestMatch3 = function(string){url_vector[amatch(string, url_list, maxDist=40,nomatch=0)]}

#ClosestMatch2(url_vector_dfi, url_list)
alt_page=pbmclapply(url_vector_dfi, ClosestMatch2)

is.na(alt_page) = lengths(alt_page) == 0
alt_page[lengths(alt_page) == 0] = 0
alt_page_=as.data.frame(alt_page)

alt_page_=rbindlist(alt_page_, use.names = T, fill = T)
match_output=as.data.frame(cbind(url_vector_dfi,((alt_page))))
match_output$V2=lapply(match_output$V2,unlist)
match_output$V2=ifelse(match_output$V2=='https://www.cato.org/cato40',NA,match_output$V2)

#save(df1, file = "Big_Cleaned_DAT.RData")
load(file = "Big_Cleaned_DAT.RData")

############# Load and Save #################
# Load
title=trimws(website_responses)
link_df=as.data.frame(cbind(title=title, pagePath=url_vector))
link_df_full=as.data.frame(cbind(pagePath=url_vector_full))
linked_title= merge(link_df_full, link_df, by=('pagePath'), all.x=T)
df_intermediate = merge(df1, linked_title, by.x = 'pagePath', by.y = 'pagePath', all.x=T)

############# Save Split-Apply-Combine #################
save(link_df, file = "sitemap.RData")
save(title, file = "Big_Title_Vector.RData")
save(linked_title, file = "Big_LinkedTitle.RData")

load( file = "Big_Title_Vector.RData")
load( file = "Big_LinkedTitle.RData")
linked_title=unique(linked_title)
df_intermediate = merge(df1, linked_title, by.x = 'pagePath', by.y = 'pagePath', all.x=T)
save(df_intermediate, file = "df_intermediate.RData")

#########################################################################
################# Begin Module 5: Scrape Cato Web Data ##################
#########################################################################
load(file = "df_intermediate.RData")

title_vector_dfi=df_intermediate[["title"]]

url_vector_dfi=as.vector(unique(url_vector_dfi))
url_vector_dfi=url_vector_dfi[nchar(url_vector_dfi) > 16]

#ClosestMatch2 = function(string, stringVector){stringVector[amatch(string, stringVector, maxDist=Inf,nomatch=0)]}
url_list = df_intermediate$pagePath

type_list=pbmclapply(url_list, function(url){type = gsub('www.cato.org*/|/.*', "\\1", url)
type = gsub('-', " ", type)
type_2 = gsub('www.cato.org/publications*/|/.*', "\\1", url)
type_2 = gsub('-', " ", type_2)
type=ifelse((type=="publications"), type_2, type)})

type_df=data.frame(cbind(type=type_list))
type_df=as.data.frame(cbind(type=type_list, pagePath=url_vector))
type_df_full=as.data.frame(cbind(pagePath=url_vector_full))
linked_type= merge(type_df_full, type_df, by=('pagePath'), all.x=T)
linked_type=unique(linked_type)
df_intermediate=merge(df_intermediate, linked_type, by.x = 'pagePath', by.y = 'pagePath', all.x=T)
df_intermediate$type = unlist(df_intermediate$type)

# Extract web content from Cato Website
text_content=df_intermediate %>% distinct(pagePath, title, type)
text_content=text_content[!duplicated(text_content),]
text_url_vector=text_content[["pagePath"]]
text_responses=pbmclapply(text_url_vector, GET) 

body_vector = pbmclapply(text_responses, function (filename) {
	doc = htmlParse(filename)
	body = xpathSApply(doc, "//div[@class='field-body'][1]", xmlValue)
	body =  gsub('\nNotes\n.*', '', body)
	body =  gsub("\n", ' ', body)
	body=trimws(body)})
body_count=pbmclapply(gregexpr("[[:alpha:]]+", body_vector), function(x) sum(x > 0))

pub_date_output = pbmclapply(text_responses, function(filename){
	doc = htmlParse(filename)
	pub_date = xpathSApply(doc, "//meta[@name='publication_date'][1]",xmlGetAttr,'content')})

tags_output = pbmclapply(text_responses, function(filename){
	doc = htmlParse(filename)
	tags = xpathSApply(doc, "//div[@class='field-tags inline']", xmlValue)
	tags =  gsub("\n", ' ', tags)
	tags=trimws((tags))})
	
topics_output = pbmclapply(text_responses, function(filename){
		doc = htmlParse(filename)
		topics = xpathSApply(doc, "//div[@class='field-topics inline']", xmlValue)
		topics =  gsub("\n", ' ', topics)
		topics=trimws((topics))})
text_df=data.frame(cbind(body=body_vector,body_count=body_count,topics=topics_output,tags=tags_output,pub_date=pub_date_output))
text_stats=cbind(text_content, text_df)

#######################################################
######## Text Analysis	- Generate Text Wall ###########
#######################################################
split_url_vector = split(, ceiling(seq_along(url_vector)/5000))

# Text Analysis	- Generate Text Wall	
text_wall=text_df %>%
	distinct(title, body,tags)
text_wall=text_wall[!duplicated(text_wall),]
for(i in 1:nrow(text_content)){
if (i==1){save_docs=paste(text_wall$title[i],text_wall$body[i],as.character(text_wall$tags[i]))}
	else{save_docs = paste(save_docs,text_wall$title[i],text_wall$body[i],as.character(text_wall$tags[i]))}
}

library(tm)
library(wordcloud)
library(topicmodels)
library(quanteda)
toSpace=content_transformer(function (x , pattern ) gsub(pattern, " ", x))
toNothing=content_transformer(function (x , pattern ) gsub(pattern, "", x))

text_stats$row_count = NULL
## Text Analysis - Top Terms ##
for(k in 1:nrow(text_stats)){
	keywords_doc = paste(text_stats$title[k],text_stats$body[k],text_stats$tags[k])
	keywords_doc=Corpus(VectorSource(keywords_doc))
	keywords_doc=tm_map(keywords_doc, toSpace, "/")
	keywords_doc=tm_map(keywords_doc, toSpace, "@")
	keywords_doc=tm_map(keywords_doc, toSpace, "\\|")
	keywords_doc=tm_map(keywords_doc, toNothing, "-")
	keywords_doc=tm_map(keywords_doc, toNothing, "—")
	keywords_doc=tm_map(keywords_doc, toNothing, "–")
	keywords_doc=tm_map(keywords_doc, removeNumbers)
	keywords_doc=tm_map(keywords_doc, stripWhitespace)
	keywords_doc=tm_map(keywords_doc, removePunctuation)
	keywords_doc=tm_map(keywords_doc, removeWords, stopwords("english"))
	keywords_doc=tm_map(keywords_doc, removeWords, c("the", "can",'did','like', 'one', 'and', 'use', 'NA')) 
	dtm=DocumentTermMatrix(keywords_doc)
	dtm=removeSparseTerms(dtm, 0.96)
	top_terms=findMostFreqTerms(dtm, n = 20L)
	top_terms=as.data.frame(do.call(rbind, top_terms))
	text_stats$top_terms[k]=paste(colnames(top_terms)[1:15],sep="|-|", collapse=", ")
	}
## Generate Author's Custom Categories ##
unique(text_stats$top_terms[1:50])

stopifnot(cato_scholars$name.website==name&is.na(cato_scholars$category_1))

print(name)
# Michael D. Tanner Categories
# category_1=c('poverty', 'welfare', 'zoning', 'tanf','prwora','snap','dole','racism', 'charity', 'dependency', 'antipoverty', 'poor', 'credit')
# category_2=c('social', 'security' ,'retirement', 'stocks', 'bonds')
# category_3=c('healthcare', 'security', 'medicare','medicaid', 'obamacare', 'aca', 'insurance', 'health')
# category_4=c('debt', 'entitlements','deficits', 'fiscal','liabilities', 'unfunded')


#Alex Nowrasteh Categories
title_category_1=c("Terorism")
title_category_2=c("Crime")
title_category_3=c("Culture/Assimilation")
title_category_4=c("Economy/Employment")
title_category_5=c("Fiscal/Welfare")

# text_stats$author_categories=ifelse(grepl(paste(category_1, collapse = "|"),text_stats$top_terms,fixed=F)==T,"Terorism",
# 																		ifelse(grepl(paste(category_2, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Crime",
# 																		ifelse(grepl(paste(category_3, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Culture/Assimilation",
# 																		ifelse(grepl(paste(category_4, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Economy/Employment",
# 																		ifelse(grepl(paste(category_5, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Fiscal/Welfare",
# 																		"Other")))))

# category_1=c('terrorist', 'terrorism', 'muslim', 'security')
# category_2=c('murder', 'crime','murdered', 'incarceration', 'prison','criminality')
# category_3=c('assimilation', 'generation','descendants', 'generations','vote')
# category_4=c('jobs', 'employment', 'worker','economic','bracero','workers', 'employmentbased','income', 'wage')
# category_5=c('benefits', 'debt', 'welfare','entitlements','deficits', 'fiscal')

# Vanessa B. Calder Categories
category_1=c('housing', 'carson', 'zoning', 'hud','landuse','lihtc','homeownership','mortgage','building')
category_2=c('women', 'family', 'leave','gender','gap')
category_3=c(NA)
category_4=c(NA)
category_5=c(NA)

title_category_1=c("Women's Issues")
title_category_2=c("Housing")
title_category_3=c(NA)
title_category_4=c(NA)
title_category_5=c(NA)


## Generate 
unique(df_final$co_authors)

cato_scholars$category_1=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$category_1),list(category_1),cato_scholars$category_1)
cato_scholars$category_2=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$category_2),list(category_2),cato_scholars$category_2)
cato_scholars$category_3=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$category_3),list(category_3),cato_scholars$category_3)
cato_scholars$category_4=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$category_4),list(category_4),cato_scholars$category_4)
cato_scholars$category_5=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$category_5),list(category_5),cato_scholars$category_5)

cato_scholars$title_category_1=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$title_category_1),list(title_category_1),cato_scholars$title_category_1)
cato_scholars$title_category_2=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$title_category_2),list(title_category_2),cato_scholars$title_category_2)
cato_scholars$title_category_3=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$title_category_3),list(title_category_3),cato_scholars$title_category_3)
cato_scholars$title_category_4=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$title_category_4),list(title_category_4),cato_scholars$title_category_4)
cato_scholars$title_category_5=ifelse(cato_scholars$name.website==name&is.na(cato_scholars$title_category_5),list(title_category_5),cato_scholars$title_category_5)
write.xlsx(cato_scholars, "Cato_Scholars.xlsx")

text_stats$author_categories=ifelse(grepl(paste(cato_scholars$category_1,collapse ="|"),
text_stats$top_terms,fixed=F)==T,cato_scholars$title_category_1,ifelse(grepl(paste(cato_scholars$category_2,collapse = "|"),
text_stats$top_terms,fixed=F)==T, cato_scholars$title_category_2,ifelse(grepl(paste(cato_scholars$category_3,collapse = "|"),
text_stats$top_terms,fixed=F)==T&is.na(cato_scholars$category_3)==F,cato_scholars$title_category_3,ifelse(grepl(paste(cato_scholars$category_4, collapse ="|"),text_stats$top_terms,fixed=F)==T&is.na(cato_scholars$category_3),cato_scholars$title_category_4,ifelse(grepl(paste(cato_scholars$category_5,collapse="|"),text_stats$top_terms,fixed=F)==T&is.na(cato_scholars$category_3),cato_scholars$title_category_5,"Other")))))

text_stats$author_categories=ifelse(grepl(paste(category_2, collapse = "|"),text_stats$top_terms,fixed=F)==T,"Women's Issues",ifelse(grepl(paste(category_1, collapse = "|"),text_stats$top_terms,fixed=F)==T,"Housing","Other"))

names(df_intermediate)
names(text_stats)

all(df_intermediate1==df_intermediate2) 

df_final=merge(df_intermediate, text_stats)

# Fix broken classes
sapply(df_final, class)
df_final$title=as.character(df_final$title)
df_final$topics=as.character(df_final$topics)
df_final$tags=as.character(df_final$tags)
df_final$body=as.character(df_final$body)
df_final$pub_date=as.character(df_final$pub_date)

df_final$body_count=as.numeric(df_final$body_count)
df_final$avg_MinPerWord=(df_final$avgTimeOnPage/df_final$body_count)
df_final$type=as.character(df_final$type)
df_final$one=1

# Fixes - This needs to be substantially filled in
df_finalt=df_final[!(df_final$title==length(0)),]
setDT(df_final)
df_final=df_final[,n:=.N,type][n>10,,][,n:=NULL]
#unique(df_final$type)
df_final$obs_day=as.Date(df_final$obs_day)

df_final$pub_date=as.Date(df_final$pub_date)
df_final$days_aft_pub=(df_final$obs_day-df_final$pub_date)

df_final$collaboration_yn=ifelse(df_final$author==df_final$author_full,"Sole Author",
						ifelse(df_final$author!=df_final$author_full|!is.na(df_final$co_authors),"Co-Authored",0))

unique(df_final$co_authors)
df_final$co_authors=gsub("^,*|(?<=,),|,*$", "", df_final$co_authors, perl=T)

doc=Corpus(VectorSource(save_docs))
doc=tm_map(doc, removeNumbers)
doc=tm_map(doc, tolower)
doc=tm_map(doc, stripWhitespace)
doc=tm_map(doc, removePunctuation)
doc=tm_map(doc, PlainTextDocument)
doc=tm_map(doc, toSpace, "/")
doc=tm_map(doc, toSpace, "@")
doc=tm_map(doc, toSpace, "\\|")
doc=tm_map(doc, toNothing, "-")
doc=tm_map(doc, toNothing, "—")
doc=tm_map(doc, toNothing, "–")
doc=tm_map(doc, removeWords, stopwords("english"))
doc=tm_map(doc, removeWords, c("the", "can",'did','like', 'and', 'null', 'one', 'NA', 'immigrants', 'will'))

df_test=merge(df_final, df_full)

save(df_final, file = paste0(analysis_identifier,".RData"))
save(doc, file = paste0("save_docs(",last_name,").RData"))
print(paste0(analysis_identifier,".RData"))
print(paste0("save_docs(",last_name,").RData"))


# save(df_final, file = paste0(analysis_identifier,".RData"))
# save(doc, file = paste0("save_docs(",last_name,").RData"))
# print(paste0(analysis_identifier,".RData"))
# print(paste0("save_docs(",last_name,").RData"))

# load("AlNo(0515-0518).RData")
# save(df_final, file = 'AlNo(0515-0518).RData')
# 
# unique(df_final$title)
# save(df_final, file = 'AlNo(0515-0518).RData')
# 
# df_final_noTerror=df_final[ which(df_final$title!='Terrorism and Immigration: A Risk Analysis'), ]
# save(df_final_noTerror, file = 'noTerror.RData')
# 
# 
# df_final$author_categories=ifelse(grepl(paste(category_1, collapse = "|"),df_final$top_terms,fixed=F)==T,"Terorism",
# 										ifelse(grepl(paste(category_2, collapse = "|"),df_final$top_terms,fixed=F)==T, "Crime",
# 										ifelse(grepl(paste(category_3, collapse = "|"),df_final$top_terms,fixed=F)==T,"Culture/Assimilation",
# 										ifelse(grepl(paste(category_4, collapse = "|"),df_final$top_terms,fixed=F)==T, "Economy/Employment",
# 										ifelse(grepl(paste(category_5, collapse = "|"),df_final$top_terms,fixed=F)==T, "Fiscal/Welfare","Other")))))

