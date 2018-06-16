# Google Analytics
setwd("~/Desktop/Welfare_Policy/Data/Data_Explorations/Google_Analytics(Cato)")
# Fonts
library(extrafont)
font_import()
loadfonts()
getwd()
# My Packages
library(googleAnalyticsR)
library(dplyr)
library(httr)
library(RCurl)
library(XML)
library(foreach)
library(stringr)
library(ggplot2)
library(data.table)
library(stringdist)
library(pbmcapply)
#library(plyr)

## Define Functions ##
lst <- sapply(stri_extract_all_words(name), function(x) substr(x, 0, 2))
df$ID <- paste0(sapply(lst, function(x) paste(x, collapse = '')), df$Year)

########################################################################################## 
###################################### Begin script ###################################### 
########################################################################################## 

# Define Date
current_date=format(Sys.time(), "%Y-%m-%d")
current_date=as.Date(current_date)
# Open Google Analytics
account_list <- ga_account_list()
ga_id <- account_list$viewId[1]
cato_scholars=read.csv('Cato_Scholars.csv')

# Choose person(s) of interest
targets = cato_scholars %>% filter(str_detect(name.website, 'Nowrasteh'))
name=targets$name.website
name=as.character(name)
last_name=str_extract(name,'[^ ]+$')

# view id of your Google Analytics view where 1 conversion = visit
vid <- "3016983"

# Establish date range
from <- "2014-06-30"
#from <- "2018-04-01"
to   <- as.character(current_date)
## create filters on dimensions
dimf <- dim_filter("dimension1","PARTIAL", expressions=name,not = F, caseSensitive = F)
dimf2 <- dim_filter("countryIsoCode","EXACT","US",not = F)
fc2 <- filter_clause_ga4(list(dimf #,dimf2
																														), operator = "OR")

#### Construct File Name ####
from_s = (from)
from_m = as.character(from)
from_y=str_sub(from, start=3, end = 4)
from_m=str_sub(from, start=6, end = 7)
to_y=str_sub(to, start=3, end = 4)
to_m=str_sub(to, start=6, end = 7)
analysis_range=paste0("(",from_m,from_y,'-',to_m,to_y,")")
initials <- function(a, b){
	a <- str_split(a, "&")
	a1 <- lapply(a, function(x){
		x1 <- str_split(str_trim(x), " ")
		paste0(unlist(lapply(x1, str_sub, 1, 2)), collapse="")	})
	paste0(unlist(a1), b) }
analysis_identifier=initials(name,analysis_range)

#### Specify Search terms ####
max = 50000000
met = c("sessions", #"pageviews",
								'timeOnPage','avgTimeOnPage',
								"entrances","bounces", 'exitRate')
dim = c("date", 
								"ga:dimension1", 'channelGrouping', 'referralPath', 'city', 'region',
								#'ga:dimension2', 
								#'region',
								#'city', 
								'pagePath')

#### Launch Google Analytic Retrieval Function ####
df2 = data.frame()
get_data=function(vid,from,to,dim,met,max){df=google_analytics(
			viewId=vid,date_range=c(from,to),metrics=met,dimensions=dim, #met_filters = fc, 
 		dim_filters = fc2,  max = max	,anti_sample = TRUE)
# clean up and set class
		df$dimension1 = gsub('O&#039;Toole', "O'Toole", df$dimension1)
		df$author_full=df$dimension1
		df$dimension1 <- NULL
		df$author=name
		df$co_authors = gsub(name, '', df$author_full)
		df$co_authors = gsub("^,*|(?<=,),|,*$", "", df$co_authors, perl=T)
		df$co_authors=gsub(', , ', ', ', df$co_authors)
		df$co_authors=trimws(df$co_authors)
		df$collaboration_yn=ifelse(df$author==df$author_full,"Sole Author",
																						ifelse(df$author!=df$author_full|!is.na(df$co_authors),"Co-Authored",0))
df}
gadata=get_data(vid=vid, from=from, to=to, dim=dim, met=met, max=max)
save(gadata, file = "Last_Raw_GA_DAT.RData")
#######
load( file = "Last_Raw_GA_DAT.RData")
#######
df1 = as.data.frame(gadata)
rm(gadata)

#####################################################
#### Initialize Cleanup of Google Analytics Data ####
#####################################################
df1$obs_day=as.Date(df1$date)
df1$date<-NULL
#  Remove Popular Proxy Strings
df1 <- df1[!grepl("search/srpcache", df1$pagePath),]
df1 <- df1[!grepl("www.filterbypass.me", df1$pagePath),]
df1 <- df1[!grepl("wikipedia.org/secure", df1$pagePath),]
df1 <- df1[!grepl("www.googleadservices.com", df1$pagePath),]
df1 <- df1[!grepl("bit.ly", df1$pagePath),]
df1 <- df1[!grepl("j.mp", df1$pagePath),]
df1 <- df1[!grepl("nl.hideproxy.me", df1$pagePath),]
df1 <- df1[!grepl("cc.bingj.com", df1$pagePath),]
df1 <- df1[!grepl("prolegis/getfile", df1$pagePath),]
df1 <- df1[!grepl("cluster23-files.instructure", df1$pagePath),]
df1 <- df1[!grepl("rorr.im/reddit.com", df1$pagePath),]
df1 <- df1[!grepl("www.duplichecker.com", df1$pagePath),]
df1 <- df1[!grepl("copyscape", df1$pagePath),]
df1 <- df1[!grepl("us1.proxysite", df1$pagePath),]
df1 <- df1[!grepl("eveil.alize", df1$pagePath),]
df1 <- df1[!grepl("honyaku.yahoofs.jp", df1$pagePath),]
df1 <- df1[!grepl("ow.ly", df1$pagePath),]
df1 <- df1[!grepl("searchenginereports.net", df1$pagePath),]
df1 <- df1[!grepl("xitenow", df1$pagePath),]
df1 <- df1[!grepl("cato.us1.list-manage.com/track/click", df1$pagePath),]
# More Reshaping of Page Paths & Remove Trailing Strings #
df1$pagePath2= gsub(".*www.cato.org", "www.cato.org", df1$pagePath)
df1$pagePath2= gsub(pattern="[?].*","",x=df1$pagePath2)
df1$pagePath2= gsub(pattern=".*https://*|&.*","",x=df1$pagePath2)
df1$pagePath2= gsub(pattern=".*genius.it/*|&.*",replacement="",x=df1$pagePath2)
df1$pagePath2= gsub(".*www-cato-org","www.cato.org",df1$pagePath2,perl=TRUE)
df1$pagePath2= gsub(".*www.cato.org.*?/publications","www.cato.org/publications",df1$pagePath2,perl=TRUE)
df1$pagePath2= gsub("\\.myaccess.library.utoronto.ca", "", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub("proxy.earlham.edu", "www.cato.org", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub("object.cato.org", "www.cato.org", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub("seekingalpha.com", "www.cato.org", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub(".ezproxy.wallawalla.edu", "", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub(".stfi.re", "", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub(".helin.uri.edu", "", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub(".proxy.lib.pdx.edu", "", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub("www.cato.org:80", "www.cato.org", df1$pagePath2, perl=TRUE) # Index.html 
df1$pagePath2= gsub("www.cato-at-liberty.org", "www.cato.org/blog", df1$pagePath2, perl=TRUE) # Index.html 
df1$pagePath2= gsub("www.cato/", "www.cato.org/", df1$pagePath2, perl=TRUE) # Index.html 
df1$pagePath2= gsub("index.html", "", df1$pagePath2, perl=TRUE)
df1$pagePath2= gsub(".jpllnet.sfsu.edu", "", df1$pagePath2, perl=TRUE)
df1$pagePath2=gsub("what-have-the-politicians-in-washington-given-us/","what-have-politicians-washington-given-us",df1$pagePath2)
df1$pagePath2= gsub(".ezproxy.csusm.edu", "", df1$pagePath2, perl=TRUE) # Index.html 
df1$pagePath2= gsub("proxy.unfake.us/proxy/350/", "www.cato.org/blog/", df1$pagePath2, perl=TRUE) # Index.html 
df1$pagePath2=ifelse(grepl("php$", df1$pagePath2)==T, df1$pagePath, df1$pagePath2)
refcols <- c("obs_day", 'pagePath', 'pagePath2') 
df1 <- df1[, c(refcols, setdiff(names(df1), refcols))]

df1$pagePath1=df1$pagePath
df1$pagePath=df1$pagePath2
df1$pagePath2=NULL

df_final= data.frame()
web_df= data.frame()
df1$ID <- seq.int(nrow(df1))
url_vector=df1[["pagePath"]]

SafeGet = function (x)	{
	tryCatch({
	#	short_url_vector
	html=GET(x)
	parsed=htmlParse(html)
	root=xmlRoot(parsed)
	title = xpathSApply(root, "//h1[@class='page-h1'][1]", xmlValue)
	return(title)},
	error=function(e){cat("ERROR :", conditionMessage(e))}, '0')}

responses <- pbmclapply(url_vector, SafeGet, mc.preschedule=T)
title=trimws(responses)
save(title, file = "Title_Vector.RData")
#######
load( file = "Title_Vector.RData")
web_df=data.frame(cbind(title=title))
df_intermediate = cbind(df1, web_df)

# 
# nrow(test)
# for(value in seq_along(responses)){
# 	tryCatch({
# 		url_mine=test[value]
# 		html<-getURL(url_mine,followlocation=TRUE)
# 		parsed=htmlParse(html)
# 		root=xmlRoot(parsed)
# 		title = xpathSApply(root, "//h1[@class='page-h1'][1]", xmlValue)
# 		#print("________________________")
# 		#print(url_mine)
# 		#print(title)
# 	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n", url_mine, "\n")})
# }
# 
# title = pbmclapply(responses, function (filename) {
# 	doc = htmlParse(filename)
# 	plain_text = xpathSApply(doc, "//h1[@class='page-h1'][1]", xmlValue)})
# web_df=data.frame(cbind(title=title))
# df_intermediate <- cbind(df1, web_df)

type_list <- pbmclapply(url_vector, function(url){
																type = gsub('www.cato.org*/|/.*', "\\1", url)
																type = gsub('-', " ", type)
																type_2 = gsub('www.cato.org/publications*/|/.*', "\\1", url)
																type_2 = gsub('-', " ", type_2)
																type=ifelse((type=="publications"), type_2, type)
})
type_df=data.frame(cbind(type=type_list))
df_intermediate <- cbind(df_intermediate, type_df)

# Extract web content from Cato Website
df3= data.frame()
text_content <- df_intermediate %>%
	distinct(pagePath, title, type)
text_content=text_content[!duplicated(text_content),]
text_url_vector=text_content[["pagePath"]]
text_responses <- pbmclapply(text_url_vector, GET) 

body_vector = pbmclapply(text_responses, function (filename) {
	doc = htmlParse(filename)
	body = xpathSApply(doc, "//div[@class='field-body'][1]", xmlValue)
	body =  gsub('\nNotes\n.*', '', body)
	body =  gsub("\n", ' ', body)
	body=trimws(body)})
body_count=pbmclapply(gregexpr("[[:alpha:]]+", body_vector), function(x) sum(x > 0))

pub_date_output = pbmclapply(text_responses, function (filename) {
	doc = htmlParse(filename)
	pub_date = xpathSApply(doc, "//meta[@name='publication_date'][1]",xmlGetAttr,'content')})

tags_output = pbmclapply(text_responses, function (filename) {
	doc = htmlParse(filename)
	tags = xpathSApply(doc, "//div[@class='field-tags inline']", xmlValue)
	tags =  gsub("\n", ' ', tags)
	tags=trimws((tags))})
	
	topics_output = pbmclapply(text_responses, function (filename) {
		doc = htmlParse(filename)
		topics = xpathSApply(doc, "//div[@class='field-topics inline']", xmlValue)
		topics =  gsub("\n", ' ', topics)
		topics=trimws((topics))})

	text_df=data.frame(cbind(body=body_vector,body_count=body_count,topics=topics_output,tags=tags_output,pub_date=pub_date_output ))
	text_stats <- cbind(text_content, text_df)
	
# Text Analysis	- Generate Text Wall	
text_wall <- text_df %>%
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
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
toNothing <- content_transformer(function (x , pattern ) gsub(pattern, "", x))

text_stats$row_count = NULL
## Text Analysis - Top Terms ##
for(k in 1:nrow(text_stats)){
	keywords_doc = paste(text_stats$title[k],text_stats$body[k],text_stats$tags[k])
	keywords_doc=Corpus(VectorSource(keywords_doc))
	keywords_doc <- tm_map(keywords_doc, toSpace, "/")
	keywords_doc <- tm_map(keywords_doc, toSpace, "@")
	keywords_doc <- tm_map(keywords_doc, toSpace, "\\|")
	keywords_doc <- tm_map(keywords_doc, toNothing, "-")
	keywords_doc <- tm_map(keywords_doc, toNothing, "—")
	keywords_doc <- tm_map(keywords_doc, toNothing, "–")
	keywords_doc <- tm_map(keywords_doc, removeNumbers)
	keywords_doc <- tm_map(keywords_doc, stripWhitespace)
	keywords_doc <- tm_map(keywords_doc, removePunctuation)
	keywords_doc <- tm_map(keywords_doc, removeWords, stopwords("english"))
	keywords_doc <- tm_map(keywords_doc, removeWords, c("the", "can",'did','like', 'one', 'and', 'use', 'NA')) 
	dtm <- DocumentTermMatrix(keywords_doc)
	dtm <- removeSparseTerms(dtm, 0.96)
	top_terms=findMostFreqTerms(dtm, n = 20L)
	top_terms=as.data.frame(do.call(rbind, top_terms))
	text_stats$top_terms[k]=paste(colnames(top_terms)[1:15],sep="|-|", collapse=", ")
	}
## Generate Author's Custom Categories ##
unique(text_stats$top_terms[1:50])

# Michael D. Tanner Categories
category_1=c('poverty', 'welfare', 'zoning', 'tanf','prwora','snap','dole','racism', 'charity', 'dependency', 'antipoverty', 'poor', 'credit')
category_2=c('social', 'security' ,'retirement', 'stocks', 'bonds')
category_3=c('healthcare', 'security', 'medicare','medicaid', 'obamacare', 'aca', 'insurance', 'health')
category_4=c('debt', 'entitlements','deficits', 'fiscal','liabilities', 'unfunded')

text_stats$author_categories=ifelse(grepl(paste(category_1, collapse = "|"),text_stats$top_terms,fixed=F)==T,"Poverty",
							ifelse(grepl(paste(category_2, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Social Security",
							ifelse(grepl(paste(category_3, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Healthcare",
							ifelse(grepl(paste(category_4, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Debt/Deficits","Other"))))


#Alex Nowrasteh Categories
# category_1=c('terrorist', 'terrorism', 'muslim', 'security')
# category_2=c('murder', 'crime','murdered', 'incarceration', 'prison','criminality')
# category_3=c('assimilation', 'generation','descendants', 'generations','vote')
# category_4=c('jobs', 'employment', 'worker','economic','bracero','workers', 'employmentbased','income', 'wage')
# category_5=c('benefits', 'debt', 'welfare','entitlements','deficits', 'fiscal')

# Vanessa B. Calder Categories
#category_1=c('housing', 'carson', 'zoning', 'hud','landuse','lihtc','homeownership','mortgage','building')
#category_2=c('women', 'family', 'leave','gender','gap')
#category_3=c('women', 'family', 'leave','gender','gap')
#category_4=c('women', 'family', 'leave','gender','gap')

## Generate 
unique(df_final$co_authors)
# 
# text_stats$author_categories=ifelse(grepl(paste(category_1, collapse = "|"),text_stats$top_terms,fixed=F)==T,"Terorism",
# 																		ifelse(grepl(paste(category_2, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Crime",
# 																		ifelse(grepl(paste(category_3, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Culture/Assimilation",
# 																		ifelse(grepl(paste(category_4, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Economy/Employment",
# 																		ifelse(grepl(paste(category_5, collapse = "|"),text_stats$top_terms,fixed=F)==T, "Fiscal/Welfare",
# 																		"Other")))))

# text_stats$author_categories=ifelse(grepl(paste(category_2, collapse = "|"),text_stats$title,fixed=F)==T,"Women's Issues"
#  				,ifelse(grepl(paste(category_1, collapse = "|"),text_stats$title,fixed=F)==T,"Housing",text_stats$author_categories))

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
df_final <- df_final[,n:=.N,type][n>10,,][,n:=NULL]
#unique(df_final$type)
df_final$obs_day=as.Date(df_final$obs_day)

df_final$pub_date=as.Date(df_final$pub_date)
df_final$days_aft_pub=(df_final$obs_day-df_final$pub_date)

df_final$collaboration_yn=ifelse(df_final$author==df_final$author_full,"Sole Author",
						ifelse(df_final$author!=df_final$author_full|!is.na(df_final$co_authors),"Co-Authored",0))

unique(df_final$co_authors)
df_final$co_authors=gsub("^,*|(?<=,),|,*$", "", df_final$co_authors, perl=T)

doc=Corpus(VectorSource(save_docs))
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, tolower)
doc <- tm_map(doc, stripWhitespace)
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, PlainTextDocument)
doc <- tm_map(doc, toSpace, "/")
doc <- tm_map(doc, toSpace, "@")
doc <- tm_map(doc, toSpace, "\\|")
doc <- tm_map(doc, toNothing, "-")
doc <- tm_map(doc, toNothing, "—")
doc <- tm_map(doc, toNothing, "–")
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, removeWords, c("the", "can",'did','like', 'and', 'null', 'one', 'NA', 'immigrants', 'will'))


save(df_final, file = paste0(analysis_identifier,".RData"))
save(doc, file = paste0("save_docs(",last_name,").RData"))
print(paste0(analysis_identifier,".RData"))
print(paste0("save_docs(",last_name,").RData"))

# load("AlNo(0515-0518).RData")
# save(df_final, file = 'AlNo(0515-0518).RData')
# 
# unique(df_final$title)
# save(df_final, file = 'AlNo(0515-0518).RData')
# 
# df_final_noTerror <- df_final[ which(df_final$title!='Terrorism and Immigration: A Risk Analysis'), ]
# save(df_final_noTerror, file = 'noTerror.RData')
# 
# 
# df_final$author_categories=ifelse(grepl(paste(category_1, collapse = "|"),df_final$top_terms,fixed=F)==T,"Terorism",
# 										ifelse(grepl(paste(category_2, collapse = "|"),df_final$top_terms,fixed=F)==T, "Crime",
# 										ifelse(grepl(paste(category_3, collapse = "|"),df_final$top_terms,fixed=F)==T,"Culture/Assimilation",
# 										ifelse(grepl(paste(category_4, collapse = "|"),df_final$top_terms,fixed=F)==T, "Economy/Employment",
# 										ifelse(grepl(paste(category_5, collapse = "|"),df_final$top_terms,fixed=F)==T, "Fiscal/Welfare","Other")))))

## Qualitative Text Analyses ##
#custom_bigram <- content_transformer(function (x , pattern ) gsub(pattern, "paid_leave", x))
# Primary WordCloud #
doc=Corpus(VectorSource(save_docs))
doc <- tm_map(doc, removeNumbers)
doc <- tm_map(doc, tolower)
doc <- tm_map(doc, stripWhitespace)
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, PlainTextDocument)
doc <- tm_map(doc, toSpace, "/")
doc <- tm_map(doc, toSpace, "@")
#doc <- tm_map(doc, custom_bigram, "paid leave")
doc <- tm_map(doc, toSpace, "\\|")
doc <- tm_map(doc, toNothing, "-")
doc <- tm_map(doc, toNothing, "—")
doc <- tm_map(doc, toNothing, "–")
doc <- tm_map(doc, removeWords, stopwords("english"))
doc <- tm_map(doc, removeWords, c("the", "can",'did','like', 'and', 'null', 'one'))
# creating of document matrix
tdm <- TermDocumentMatrix(doc, control = list(stemming = TRUE))
#tdm <- TermDocumentMatrix(myCorpus, control = list(stemming = TRUE)) 
tes=cbind(stems = rownames(tdm), completed = stemCompletion(rownames(tdm), doc))  
m <- as.matrix(tes)

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(12)
cloud=wordcloud(words = d$word, freq = d$freq, min.freq = 3,	max.words=150, 
										random.order = FALSE,rot.per=.05,vfont=c("sans serif","plain"),
										colors=brewer.pal(8, "Dark2"))




### Quantitative Analyses ###
my_theme <- function(){
	theme_light() +
		theme(text = element_text(family = "Open Sans"),  
								plot.title = element_text(size = 12, color = "gray30"),   # Set up the title style
								plot.subtitle = element_text(size = 10, color = "black"), # Set up the subtitle style
								plot.margin = unit(c(.1,.1,.1,.1), "cm"),                 # Add white space at the top and left
								panel.grid = element_blank(),
								#panel.border = element_blank(),
								axis.title = element_blank(),
								axis.ticks = element_blank(),
								#axis.text.x = element_blank(),
								axis.text.y = element_text(size = 9, color = "gray10"))
}
names(df_final)

#Bar plot
Bar_type_df =subset(df_final, type != "events") # Remove events
Bar_type_df<-Bar_type_df %>% distinct(type,title,collaboration_yn,one)
title_name=sprintf("%s's Content Breakdown",name)

ggplot(data=Bar_type_df,aes(x=type,y=one,fill=collaboration_yn))+
ggtitle(sprintf("%s's Content Breakdown ", name), # Add the title and subtitle
									subtitle=sprintf("\n Total Content: %s",sum(Bar_type_df$one)))+
	geom_bar(stat="identity", width=0.5)+ my_theme()+
	theme(legend.position="bottom", legend.box = "horizontal")+theme(legend.title=element_blank())

#Time per Article
Audience_Captivation =subset(df_final, type != "events" & type != "multimedia") # Remove Events and Multimedia
Audience_Captivation <- aggregate(avg_MinPerWord ~ title, Audience_Captivation, mean)

# Happy
Happy_audience=filter(Audience_Captivation, row_number(-avg_MinPerWord) <= 10)
Happy_audience <- Happy_audience %>% arrange(avg_MinPerWord)
Happy_audience$title<-factor(Happy_audience$title,levels=Happy_audience$title[order(Happy_audience$avg_MinPerWord)])
ggplot(data=Happy_audience, aes(x=title, y=avg_MinPerWord)) +
	ggtitle(sprintf("10 Best Audience Attrition", name),subtitle = "Time Spent per Word in Aticle Text" ) +
	geom_bar(stat="identity", width=0.5)+ coord_flip() + my_theme()+ 
	theme(legend.position="bottom", legend.box = "horizontal")+theme(legend.title=element_blank())

# Sad Audience
sad_audience<-filter(Audience_Captivation, row_number(avg_MinPerWord) <= 10)
sad_audience <- sad_audience %>% arrange(avg_MinPerWord)
sad_audience$title<-factor(sad_audience$title,levels=sad_audience$title[order(sad_audience$avg_MinPerWord)])
ggplot(data=sad_audience, aes(x=title, y=avg_MinPerWord))+
	ggtitle(sprintf("10 Worst Audience Attrition", name),subtitle="Time Spent per Word in Aticle Text")+
	geom_bar(stat="identity", width=0.5)+ coord_flip() + my_theme()+
	theme(legend.position="bottom", legend.box = "horizontal")+theme(legend.title=element_blank())

# Line Chart
total_line =subset(df_final, type != "events"&type !="multimedia") # Remove events
total_line=aggregate(sessions ~ title + VBC_Issues + obs_day, total_line, sum)
total_line %>%
ggplot(aes(x=obs_day,y=sessions,group=VBC_Issues,colour=VBC_Issues))+geom_line()+
	theme(axis.text.x=element_text(angle=90,hjust = 1),legend.position="none")+my_theme()+
	theme(legend.position="bottom", legend.box = "horizontal")+theme(legend.title=element_blank())

total_line <- total_line %>%
		distinct(type, title, VBC_Issues, sessions, one)
title_name=sprintf("%s's Content Breakdown", name)
ggplot(data=total_line, aes(x=type, y=one, fill=VBC_Issues)) +
	ggtitle(sprintf("%s's Content Breakdown ", name),  # Add the title and subtitle
									subtitle = sprintf("\n Total Content: %s", sum(total_line$one))) +
	geom_line(stat="identity")+
	my_theme()+
	theme(legend.position="bottom", legend.box = "horizontal")+theme(legend.title=element_blank())


page <- aggregate(sessions ~ date + page_name, gadata, sum)
page %>%
	ggplot(aes(x=date,y=sessions
												,group=page_name
												,colour=page_name
	)) + 
	geom_line()  + 
	theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")


### group mtcars by cylinders and return some averages
cars <- df1 %>%
	select(title, pub_date, hp, qsec) %>%
	group_by(cyl) %>%
	summarise(mpg = mean(mpg), hp = mean(hp), qsec = mean(qsec))


gadata = filter(gadata, !grepl("Caleb", dimension1) & !grepl('Cannon', dimension1) & !grepl('Levy', dimension1))
gadata$date <- as.Date(gadata$date , "%Y-%m-%d")
page <- aggregate(sessions ~ date + page_name, gadata, sum)
page %>%
ggplot(aes(x=date,y=sessions
											,group=page_name
											,colour=page_name
											)) + 
  geom_line()  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")
  #+  scale_x_date(date_breaks = "20 days", date_labels = "%Y-%m-%d")
		
  # some styles to rotate x-axis labels

#barchart
bar <- aggregate(page_name ~ sessions, gadata, sum)
ggplot(data=bar, aes(x=page_name, y=sessions)) +
  geom_bar(stat="identity", width=0.5)

page %>%
ggplot(aes(x=date,y=pageviews,group=1)) + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  # some styles to rotate x-axis labels

ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", width=0.5)

gadata3 %>%
ggplot(aes(x=date, y=pageviews)) +
  geom_point()
theme(axis.text.x = element_text(angle = 90, hjust = 1))

gadata %>%
ggplot(aes(x=date, y=pageviews)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
size = pageviews

gadata %>%
ggplot(aes(x=date, y=pageviews, size = pageviews)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


page %>%
ggplot(aes(x = page$date,y = page$pageviews) ) + 
  geom_point() + 
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 90, hjust = 2)) +
  			labs(x = "Date", y = 'Page Views', title = "Vanessa Calder 2018")



Names <- character(length(x))
for(i in seq_along(x)){
	Names[i] <- paste0("A_", i, ".png") 
}


somePDFPath = "C:\\temp\\some.pdf"
pdf(file=somePDFPath)  

for (i in seq(5,10))   
{   
	par(mfrow = c(2,1))
	VAR1=rnorm(i)  
	VAR2=rnorm(i)  
	plot(VAR1,VAR2)   
} 
dev.off() 

