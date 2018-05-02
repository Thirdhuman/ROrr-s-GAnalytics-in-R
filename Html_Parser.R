library(RCurl)
library(XML)
library(stringr)
library(lubridate)


gadata$pagePath[1]

# download html
html <- getURL(gadata$pagePath[1], followlocation = TRUE)


path = 'https://www.cato.org/blog/must-rising-oil-prices-compel-fed-tighten-more'
html <- getURL(path, followlocation = TRUE)

parsed = htmlParse(html)
root = xmlRoot(parsed)

sig_desc_nodes = xpathSApply(root, "//h1[@class='page-h1'] | //div[@class='field-body'] | //div[@class='story-overline']
																													| //div[@class='byline']  | //div[@class='blog-hdr-hlink']", xmlValue)

title = xpathSApply(root, "//h1[@class='page-h1']", xmlValue)
body = xpathSApply(root, "//div[@class='field-body']", xmlValue)
body =  gsub("\n", ' ', body)
body=trimws(body)
date = xpathSApply(root, "//div[@class='story-overline']", xmlValue)
date =as.Date(date, "%B %d, %Y %I:%M%p")

date = xpathSApply(root, "//meta[@name='publication_date']",  xmlGetAttr, 'content')
date
date=ymd(date)
date

date =as.Date(date, "%B %d, %Y %I:%M%p")
date

#meta name="publication_date" content="2014/2/11"

author = xpathSApply(root, "//div[@class='byline']", xmlValue)
author =  gsub("\n | By | Featuring", ' ', author)
author=trimws(author)

author = xpathSApply(root, "//a[@property='foaf:name']", xmlValue)
author =  gsub("\n | By | Featuring", ' ', author)
author=trimws(author)


#author = gsub("^([^\n]*\n)\n.*$", "\\1",author)
author
type = xpathSApply(root, "//a[@class='blog-hdr-hlink']", xmlGetAttr, 'href')										
type=str_sub(type, 2, str_length(type))
cat(paste(type, collapse = " "))


type = xpathSApply(root, "//meta[@property='og:type']", xmlGetAttr, 'content')										
type=str_sub(type, 2, str_length(type))
cat(paste(type, collapse = " "))


head(sig_desc_nodes, n = 1)


doc = htmlParse(html, query, asText=TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)]
																										[not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
#cat(paste(plain.text, collapse = " "))

cat(paste(sig_desc_nodes, collapse = " "))

#plain.text

#r project - 
# Google Search Web Images Videos Maps News Shopping Gmail More Translate Books Finance Scholar Blogs YouTube 
# Calendar Photos Documents Sites Groups Reader Even more » Account Options Sign in Search settings Web History 
# Advanced Search Results 1 - 10 of about 336,000,000 for r project . Everything More Search Options Show options... 
# Web The R Project for Statistical Computing R , also called GNU S, is a strongly functional language and environment 
# to statistically explore data sets, make many graphical displays of data from custom ... 
# www. r - project .org/ - Cached - Similar [Trunc...]

