# Google Analytics
#Analytics
#4/AACQY2pkmcHP-LBO1kezweGPQMmoEem6TI53Nn3bTWIcTB7SiI6TVEw
#Auth
#4/AABJlcxvF9CQFwt_CpXgw1gk-YOl0WzDHiwPncsbl3PgBAMciZAGen4

#Intro to text analysis
#https://www.r-bloggers.com/intro-to-text-analysis-with-r/

getwd()
#setwd('/Users/rorr/Desktop/Welfare_Policy/Data/Data_Explorations/Google_Analytics(Cato)')
# This installs googleAnalyticsR if you haven't got it installed already
if(!require(googleAnalyticsR)) install.packages("googleAnalyticsR")
#library("tidyverse")
library(googleAnalyticsR)
library(dplyr)
library(httr)
library(RCurl)
library(XML)
library(lubridate)
library(foreach)
library(stringr)
#library(googleAuthR)
library(ggplot2)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}
is.valid <- function(x) {
	require(shiny)
	is.null(need(x, message = FALSE))  
}
current_date=format(Sys.time(), "%Y-%m-%d")
current_date=ymd(current_date)
account_list <- ga_account_list()
ga_id <- account_list$viewId[1]
#
# Setup script
#

name="Vanessa Brown Calder"

# view id of your Google Analytics view where 1 conversion = visit
vid <- "3016983"
# date range
from <- "2015-01-01"
to   <- "2018-06-01"
## create filters on dimensions
dimf <- dim_filter("countryIsoCode","EXACT","US",not = F)
dimf2 <- dim_filter("dimension1","PARTIAL", expressions=name,not = F, caseSensitive = F)
fc2 <- filter_clause_ga4(list(dimf, dimf2), operator = "AND")
max = 50000
met = c("sessions",
								#"pageviews",
								'timeOnPage','avgTimeOnPage',
								"entrances","bounces")
dim = c(
								"date", 
								"ga:dimension1", 
								#'ga:dimension2', 
								#'region',
								#'city', 
								'pagePath'
								)
# the function
df2 = data.frame()
get_data <- function(vid, from, to, dim, met, max) {
  df <- google_analytics(viewId = vid, 
                         date_range = c(from, to), 
                         metrics = met, 
                         dimensions = dim,
                         #met_filters = fc, 
                         dim_filters = fc2, 
                         max = max)
  # clean up and set class
  df[,1] <- gsub(" / ", "/", df[,1])              # remove spacing
  df[,1] <- gsub(":?(NA|CLICK|NA):?", "", df[,1]) # remove CLICK and NA
  #df[,2] <- as.numeric(df[,2])                    # conversion column is character :-/
  #df$page_name <- sapply(df$pagePath, basename)
  #df$page_name= gsub("-", ' ', df$page_name)
  #df$page_name= sapply(df$page_name, simpleCap)
  refcols <- c("obs_day") 
  df$obs_day=ymd(df$date)
  df <- df[, c(refcols, setdiff(names(df), refcols))]
		df$date <- NULL
		df$dimension1 = gsub('O&#039;Toole', "O'Toole", df$dimension1)
		df$author_full=df$dimension1
		df$dimension1 <- NULL
		df$author=name
		df$co_authors = gsub(name, '', df$author_full)
		df$co_authors=trimws(df1$co_authors)
		df$co_authors = gsub("^,*|(?<=,),|,*$", "", df$co_authors, perl=T)
		df$collaboration=ifelse(length(df$co_authors==0), FALSE, TRUE)
		df}
gadata <- get_data(vid=vid, from=from, to=to, dim=dim, met=met, max=max)
#######
df2 = data.frame()
web_df= data.frame()
df1 = gadata
rm(df)
for(i in 1:nrow(df1)){
			row_numb = i 
			url=df1$pagePath[i]
			url= gsub(pattern=".*https://",replacement="",x=url)
			html<-getURL(url,followlocation=TRUE)
			html= gsub(pattern = "Recent Cato Daily Podcast.*<h4", replacement = "", x = html)
			parsed=htmlParse(html)
			root=xmlRoot(parsed)
# Generate Title
			title = xpathSApply(root, "//h1[@class='page-h1'][1]", xmlValue)
			if (length(title)==0){title = 0	}
# Manipulate Body for Text Analyses			
			body = xpathSApply(root, "//div[@class='field-body'][1]", xmlValue)
			body =  gsub('\nNotes\n.*', '', body)
			body =  gsub("\n", ' ', body)
			body=trimws(body)
			body_count=sapply(gregexpr("[[:alpha:]]+", body), function(x) sum(x > 0))
			if (length(body_count)==0){
				body_count = 0	}
# Grab Publication Date
			pub_date=xpathSApply(root,"//meta[@name='publication_date'][1]",xmlGetAttr,'content')
			if (length(pub_date)==0){pub_date=0}
			if (pub_date!=0){
				pub_date=ymd(pub_date)
				days_10=pub_date+days(10)
				days_90=pub_date+days(90)
				year_1=pub_date+years(1)}
# Grab Author name
#Grab Media type
			type = gsub('www.cato.org*/|/.*', "\\1", df1$pagePath[i])
			type = gsub('-', " ", type)
			type_2 = gsub('www.cato.org/publications*/|/.*', "\\1", df1$pagePath[i])
			type_2 = gsub('-', " ", type_2)
			type=ifelse((type=="publications"), type_2, type)
			web_df=data.frame(title=title,type=type,primary_author=primary_author,pub_date=pub_date,
																						row_numb=row_numb,body_count=body_count,author_full=author_full,co_authored=co_authored,
																					days_90=days_90,year_1=year_1,days_10=days_10)
			df2 <- rbind(df2, web_df)
}			
df1 <- cbind(df1, df2)
df1$days_aft_pub=(df1$obs_day-df1$pub_date)

str(df1)


# Create time periods
Recep_Quick=subset(df1, (pub_date < obs_day) & (obs_day< days_10))
Recep_Medium=subset(df1, (pub_date < obs_day) & (obs_day< days_90))
Recep_Long=subset(df1, (pub_date < obs_day) & (obs_day< year_1))
#Recep_Long3=subset(df1, (pub_date < obs_day) & (obs_day< days_10))

days_90=pub_date+days(90)
year_1=pub_date+years(1)


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

