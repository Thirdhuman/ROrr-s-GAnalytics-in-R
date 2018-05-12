
urls <- c(
	'http://timesofindia.indiatimes.com//articleshow/2933112.cms',
	'http://timesofindia.indiatimes.com//articleshow/2933019.cms',
	'http://timesofindia.indiatimes.com//articleshow/2933131.cms',
	'http://timesofindia.indiatimes.com//articleshow/2933209.cms',
	'http://timesofindia.indiatimes.com//articleshow/2933277.cms'
)



system.time({ responses <- lapply(url_vector, GET) })
result = lapply(response, function (filename) {
	doc = htmlParse(filename)
	plain_text = xpathSApply(doc, "//h1[@class='page-h1'][1]", xmlValue)
})
print(plain_text)

sapply(responses, http_status)

titles=sapply(responses, function(x) headers(x)$'page-h1')

library(rvest)
library(tidyverse)
library(httr)

library(purrr)

url_list=as.list(url_vector)

title = xpathSApply(root, "//h1[@class='page-h1'][1]", xmlValue)

response=responses[1:20]
pages <- response %>% map(read_html)
responses[1]

url_1=url_vector[1]

result = pbmclapply(response, function (filename) {
	doc = htmlParse(filename)
	plain_text = xpathSApply(doc, "//h1[@class='page-h1'][1]", xmlValue)
	})
print(plain_text)

links <- response %>%
	html_nodes(".titleColumn") %>%
	html_nodes("h1") %>%
	.[1:5] # for testing


title <- response %>% 
	map_chr(. %>% 
										html_nodes("page-h1") %>% 
										html_text()
	)
rating <- pages %>% 
	map_dbl(. %>% 
										html_nodes("strong span") %>% 
										html_text() %>% 
										as.numeric()
	)

