# Google Analytics
setwd("~/Desktop/Welfare_Policy/Data/Data_Explorations/Google_Analytics(Cato)")
# Fonts
library(extrafont)
font_import()
loadfonts()
getwd()
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

## Define Functions ##

########################################################################################## 
###################################### Begin script ###################################### 
########################################################################################## 

# Define Date
current_date=format(Sys.time(), "%Y-%m-%d")
current_date=as.Date(current_date)
# Open Google Analytics
account_list <- ga_account_list()
ga_id <- account_list$viewId[1]
cato_scholars=read.xlsx('Cato_Scholars.xlsx')

# Choose person(s) of interest
targets = cato_scholars %>% filter(str_detect(name.website, 'Vanessa'))
name=targets$name.website
name=as.character(name)
last_name=str_extract(name,'[^ ]+$')

authur_row=as.data.frame(cato_scholars$name.website)
colnames(authur_row) = authur_row[1, ] # the first row will be the header
authur_row = authur_row[-1, ]          # removing the first row.
authur_row=as.vector(as.character(authur_row))

# view id of your Google Analytics view where 1 conversion = visit
vid <- "3016983"
# Establish date range
from <- "2014-06-30" # (Earliest Available)
#from <- "2018-07-01" # (Insert Other)
to   <- as.character(current_date)
#### create filters on dimensions ####
dimf <- dim_filter("dimension1","PARTIAL", expressions=name,not = F, caseSensitive = F)
dimf2 <- dim_filter("countryIsoCode","EXACT","US",not = F)
fc2 <- filter_clause_ga4(list(# dimf #,dimf2
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
max = 500000000
met = c("sessions", #"pageviews",
								'timeOnPage','avgTimeOnPage',
								"entrances","bounces", 'exitRate')
dim = c("date", 
								"ga:dimension1", #'channelGrouping',# 'city', 'region',
								#'ga:dimension2', 
								'pagePath')

#lst <- sapply(str_extract_all(name), function(x) substr(x, 0, 2))

#### Launch Google Analytic Retrieval Function ####
get_data=function(vid,from,to,dim,met,max){df=google_analytics(
			viewId=vid,date_range=c(from,to),metrics=met,dimensions=dim, #met_filters = fc, 
 		dim_filters = fc2,  max = max	,anti_sample = TRUE)
# clean up and set class
		df$dimension1 = gsub('O&#039;Toole', "O'Toole", df$dimension1)
		df$author_full=df$dimension1
		df$dimension1 <- NULL
df}
gadata=get_data(vid=vid, from=from, to=to, dim=dim, met=met, max=max)
save(gadata, file = "Big_Raw_GA_DAT.RData")
#######
load( file = "Big_Raw_GA_DAT.RData")
#######
df1 = as.data.frame(gadata)
df1[as.character(authur_row)] = NA

### Stop here if column #s are altered since previous analysis ###
colnames(df1)
df1$ID=NULL
df1_1=df1[ ,1:8]
df1_2=df1[ ,9:61]
df1_2[-1] = lapply(names(df1_2[-1]), function(nm) grepl(nm, df1_2[[1]]))
df1=cbind(df1_1,df1_2)
rm(gadata, dim, dimf, dimf2, fc2,ga_id, met, max, vid)
rm(df1_1,df1_2)

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

save(df1, file = "Big_Cleaned_DAT.RData")
#######
load( file = "Big_Cleaned_DAT.RData")

#####################################################
################ Scrape Cato Web Data ############### 
#####################################################
#df_final= data.frame()

df1$ID <- seq.int(nrow(df1))
url_vector_full=df1[["pagePath"]]
url_vector=unique(url_vector_full)

SafeGet = function (x)	{
	tryCatch({
	#	short_url_vector
	html=GET(x)
	parsed=htmlParse(html)
	root=xmlRoot(parsed)
	title = xpathSApply(root, "//h1[@class='page-h1'][1]", xmlValue)
	return(title)},
	error=function(e){cat("ERROR :", conditionMessage(e))}, '0')}

  library(curl)
  # Init list of urls to be read
  url_vector_full=df1[["pagePath"]]
  url_vector=unique(url_vector_full)
  parsed_url <- list()

  cb <- function(req){
    parsed_url <<- append(parsed_url, list(rawToChar(req$content)))
  }

  # Specify chunk size to prevent exceeding API rate limit
  chunk_size <- 50
  for (i in 1:ceiling(length(url_vector) / chunk_size)) {
    pool <- new_pool()
    # vector of uris to loop through
    uris <- url_vector[(i + (i - 1) * (chunk_size - 1)):(i * chunk_size)]
    # all scheduled requests are performed concurrently
    sapply(uris, curl_fetch_multi, done=cb, pool=pool)
    # Perform requests
    out <- multi_run(pool = pool)
    # Print out number of successes each round
    cat(sum(out$success))
    # Delay calls to prevent exceeding speed limit
    Sys.sleep(2)
  }

# split_df       <- split(big_df, big_df$ID)
# result_list_df <- lapply(split_df, complex_func)
# result_df      <- do.call(rbind, result_list_df)

website_responses <- pbmclapply(url_vector, SafeGet, mc.preschedule=T)
title=trimws(website_responses)
link_df=as.data.frame(cbind(title=title, pagePath=url_vector))
link_df_full=as.data.frame(cbind(pagePath=url_vector_full))
linked_title= merge(link_df_full, link_df, by=('pagePath'), all.x=T)

save(title, file = "Big_Title_Vector.RData")
#######
load( file = "Big_Title_Vector.RData")

linked_title=unique(linked_title)
df_intermediate = merge(df1, linked_title, by.x = 'pagePath', by.y = 'pagePath', all.x=T)

type_list <- pbmclapply(url_vector, function(url){
																type = gsub('www.cato.org*/|/.*', "\\1", url)
																type = gsub('-', " ", type)
																type_2 = gsub('www.cato.org/publications*/|/.*', "\\1", url)
																type_2 = gsub('-', " ", type_2)
																type=ifelse((type=="publications"), type_2, type)
})
type_df=data.frame(cbind(type=type_list))
type_df=as.data.frame(cbind(type=type_list, pagePath=url_vector))
type_df_full=as.data.frame(cbind(pagePath=url_vector_full))
linked_type= merge(type_df_full, type_df, by=('pagePath'), all.x=T)
linked_type=unique(linked_type)
df_intermediate <- merge(df_intermediate, linked_type, by.x = 'pagePath', by.y = 'pagePath', all.x=T)

# Extract web content from Cato Website
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
																																										text_stats$top_terms,fixed=F)==T,cato_scholars$title_category_1,
																																				ifelse(grepl(paste(cato_scholars$category_2,collapse = "|"),
																																										text_stats$top_terms,fixed=F)==T, cato_scholars$title_category_2,
																																				ifelse(grepl(paste(cato_scholars$category_3,collapse = "|"),
																																									 text_stats$top_terms,fixed=F)==T&is.na(cato_scholars$category_3)==F,cato_scholars$title_category_3,
																																				ifelse(grepl(paste(cato_scholars$category_4, collapse ="|"),
																																										text_stats$top_terms,fixed=F)==T&is.na(cato_scholars$category_3),cato_scholars$title_category_4,
																																				ifelse(grepl(paste(cato_scholars$category_5,collapse="|"),
																																										text_stats$top_terms,fixed=F)==T&is.na(cato_scholars$category_3),cato_scholars$title_category_5,																																				   "Other")))))


text_stats$author_categories=ifelse(grepl(paste(category_2, collapse = "|"),text_stats$top_terms,fixed=F)==T,"Women's Issues",ifelse(grepl(paste(category_1, collapse = "|"),text_stats$top_terms,fixed=F)==T,"Housing",
 																		"Other"))

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
# df_final_noTerror <- df_final[ which(df_final$title!='Terrorism and Immigration: A Risk Analysis'), ]
# save(df_final_noTerror, file = 'noTerror.RData')
# 
# 
# df_final$author_categories=ifelse(grepl(paste(category_1, collapse = "|"),df_final$top_terms,fixed=F)==T,"Terorism",
# 										ifelse(grepl(paste(category_2, collapse = "|"),df_final$top_terms,fixed=F)==T, "Crime",
# 										ifelse(grepl(paste(category_3, collapse = "|"),df_final$top_terms,fixed=F)==T,"Culture/Assimilation",
# 										ifelse(grepl(paste(category_4, collapse = "|"),df_final$top_terms,fixed=F)==T, "Economy/Employment",
# 										ifelse(grepl(paste(category_5, collapse = "|"),df_final$top_terms,fixed=F)==T, "Fiscal/Welfare","Other")))))

