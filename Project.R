
# Setting up WD and loading some packages.

#getwd()
#setwd("C:/Users/anton/OneDrive/Dokument/R/dsss/Scraper")

library(RCurl) 
library(XML) 
library(httr)
library(stringr)

# I am working with a site that collects all QAnon posts. I am interested in posts that has to do
# with the term "conspiracy". 

url <- "https://qalerts.app/?q=conspiracy"

mycurl <- getCurlHandle()
curlSetOpt(cookiejar="~/Rcookies", curl = mycurl)

rawpage <- GET(url,
               useragent = "Mozilla/5.0 (Windows NT 6.3; WOW64; rv:43) Gecko/2010010, Firefox/43.0",
               followlocation = T,
               timeout = 60,
               curl = mycurl
               )

tpage <- htmlParse(rawpage)

value.count <- length(xpathSApply(tpage, "//a", xmlValue)) # Counting values that are hyperlinked.

# Extracting information by the nodes of interest. I collect the text inside the posts and each posts tripcode.

nodes.text <- xpathSApply(tpage, "//div[@class='dont-break-out mb-3 text-accent']", xmlValue)
nodes.tripcode <- xpathSApply(tpage, "//span[@class='d-none d-md-inline-block d-lg-inline-block d-xl-inline-block']", xmlValue)

nodes.text <- nodes.text[!duplicated(nodes.text)] # Removing duplicates.
nodes.tripcode <- nodes.tripcode[!duplicated(nodes.tripcode)] 

# I set up a function to pad the data frame with NA if either text or tripcode is missing.

na.pad <- function(x,len){
  x[1:len]
}

makePaddedDataFrame <- function(l,...){
  maxlen <- max(sapply(l,length))
  data.frame(lapply(l,na.pad,len=maxlen),...)
}

sample <- makePaddedDataFrame(list(text=nodes.text, tripcode=nodes.tripcode))

sample <- na.omit(sample) # Remove all rows that has either missing text or missing tripcode.

# Beginning to clean with regexs. I set up a function for this. 

remove <- function(x){
  res <- gsub(pattern = "\\n", replacement=" ", x) # This regex removes newlines. 
  res <- gsub("http\\S+\\s*", " ", res) # This regex removes http links inside the strings. 
  res <- gsub("[[:punct:]]", " ", res) # This regex removes special characters like punctuations and questionmarks.
  res <- gsub("-"," ", res) # However, I don't know why, but the previous regex did not remove "-" in certain places. So I specified the removal of this character directly. 
  res <- gsub("www\\w+ *", " ", res) # I remove parts of the strings that start with www.
  res <- gsub("^\\s+|\\s+$", " ", res) # I remove trailing and leading spaces. 
return(res)
}

text.clean <- remove(sample$text)
text.clean <- gsub("[[:digit:]]+", " ", text.clean) # Removing digits. I only want text.

tripcode.clean <- remove(sample$tripcode)
tripcode.clean <- gsub("[No]", "", tripcode.clean) # Removing characters from tripcode column. I only want digits.
tripcode.clean <- gsub(" ", "", tripcode.clean) # I remove extra spaces.
tripcode.clean <- as.numeric(tripcode.clean) # Converting class of tripcodes to numeric.

clean.sample <- cbind(text.clean, tripcode.clean) # Binding together by the column.

clean.sample <- data.frame(clean.sample, stringsAsFactors = F) # Converting to a data frame. 

clean.sample[clean.sample==""]<-NA # Cleaning might have lead to some empty rows so I convert these rows to NAs

clean.sample <- na.omit(clean.sample) #.. and remove these NAs.

# Topic model 

#install.packages("tm")

library(tm)
  
clean.sample$doc_id <- seq.int(nrow(clean.sample)) # An ID column is required for TM, so I make this here.

colnames(clean.sample)
names(clean.sample)[1] <- "text" # Renaming columns per necessity in order to utilize TM packages.
names(clean.sample)[2] <- "tripcode"
clean.sample <- clean.sample[c("doc_id", "text", "tripcode")] # I change places of the columns. This is also required. ID column has to be the first column in the data frame.
colnames(clean.sample)

corp1 <- VCorpus(DataframeSource(clean.sample)) # Creating corpuses. 
corp1 <- tm_map(corp1, content_transformer(tolower)) 
corp1 <- tm_map(corp1, stripWhitespace) 
corp1 <- tm_map(corp1, removeWords, stopwords("english")) # I remove common english stopwords from the texts. The link below specifies more exactly which these are.

# http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/a11-smart-stop-list/english.stop

dtm1 <- DocumentTermMatrix(corp1) # Creating a term matrix.
nrow(dtm1)
ui <- unique(dtm1$i) 
dtm1 <- dtm1[ui,]
nrow(dtm1)

#install.packages("topicmodels")

library(topicmodels)

k <- 10 # I set number of topics to 10. 

controlGibbs <- list(seed = 5683,
                     iter = 500)
model1 <- LDA(dtm1, k, method = "Gibbs", control = controlGibbs) # Setting up topic model.
terms(model1,10)

#install.packages(c("slam","LDAvis"))

library(slam)
library(LDAvis)

# Visualization

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(model1))


# Collecting data using RSelenium.

library(rJava)
library(RSelenium)

remDr <- rsDriver(verbose = T,
                  remoteServerAddr = "localhost",
                  port = 4443L,
                  browser=c("firefox"))
rm <-remDr$client

rm$getStatus()

rm$navigate("https://qalerts.app/")

search_box <- rm$findElement(using = 'xpath', '//input[@name = "q"]')

search_box$sendKeysToElement(list("Watergate", "\uE007")) # I want to look at conspiracies promoted by QAnon now. Here I look at posts associated to Watergate.

rm$getCurrentUrl()

elems <- rm$findElements(using = "xpath", "//div[@class='dont-break-out mb-3 text-accent']") # Finding elements with this Xpath.

watergate.text <- lapply(elems, function(elem) {
  elem$getElementText()
}) # Putting the texts in a list. 

rm(elems)

rm$goBack()

search_box <- rm$findElement(using = 'xpath', '//input[@name = "q"]')

search_box$sendKeysToElement(list("Ukraine", "\uE007")) # I want to collect posts associated to Ukraine (Biden - Ukraine conspiracy).

rm$getCurrentUrl()

elems <- rm$findElements(using = 'xpath', "//div[@class='dont-break-out mb-3 text-accent']")

ukraine.text <- lapply(elems, function(elem) {
  elem$getElementText()
}) # Putting the texts in a list.

rm(elems)
rm(search_box)
rm$close()
rm(remDr)
rm(rm)
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE) # Closing RSelenium.

# Regex function. 
remove2 <- function(x){
  res <- gsub("(\\\\n)+", " ", x) # I remove newlines. 
  res <- gsub("[[:digit:]]+", "", res) # Removing digits.
  res <- gsub("[[:punct:]]", "", res) # Removing special characters.
  res <- gsub("http\\S+\\s*", " ", res) # Removing http links.
  res <- gsub("^list","",res) # Explicitly removing the word "list". This word was previously displayed in the beginning of each string.
  return(res)
}

clean.watergate.text <- remove2(watergate.text) # Applying function to texts.
clean.ukraine.text <- remove2(ukraine.text)

View(clean.watergate.text)
View(clean.ukraine.text)

clean.ukraine.text[clean.ukraine.text==""]<-NA  # Removing NAs from Ukraine texts.
clean.ukraine.text <- na.omit(clean.ukraine.text)

ukraine.conspiracy <- data.frame(clean.ukraine.text)
watergate.conspiracy <- data.frame(clean.watergate.text)
ukraine.conspiracy$doc_id <- seq.int(nrow(ukraine.conspiracy)) # Creating ID columns.
watergate.conspiracy$doc_id <- seq.int(nrow(watergate.conspiracy))

names(ukraine.conspiracy)[1] <- "text" # Renaming column that contain the texts. 
names(watergate.conspiracy)[1] <- "text"

ukraine.conspiracy <- ukraine.conspiracy[c("doc_id", "text")] # Reordering columns.
watergate.conspiracy <- watergate.conspiracy[c("doc_id", "text")]

View(ukraine.conspiracy) # All looks good.
View(watergate.conspiracy)

corp.ukraine <- VCorpus(DataframeSource(ukraine.conspiracy)) # Setting up corpus for Ukraine texts.
corp.ukraine <- tm_map(corp.ukraine, content_transformer(tolower))
corp.ukraine <- tm_map(corp.ukraine, stripWhitespace)
corp.ukraine <- tm_map(corp.ukraine, removeWords, stopwords("english"))
text.doc_dtm_ukraine <- TermDocumentMatrix(corp.ukraine) 

corp.watergate <- VCorpus(DataframeSource(watergate.conspiracy)) # Corpus for Watergate texts.
corp.watergate <- tm_map(corp.watergate, content_transformer(tolower))
corp.watergate <- tm_map(corp.watergate, stripWhitespace)
corp.watergate <- tm_map(corp.watergate, removeWords, stopwords("english"))
text.doc_dtm_watergate <- TermDocumentMatrix(corp.watergate)

text.doc_dtm_ukraine.m <- as.matrix(text.doc_dtm_ukraine) # Converting to matrices.
text.doc_dtm_watergate.m <- as.matrix(text.doc_dtm_watergate)

dtm_ukraine.m_v <- sort(rowSums(text.doc_dtm_ukraine.m), decreasing=T)
dtm_watergate.m_v <- sort(rowSums(text.doc_dtm_watergate.m), decreasing=T)

dtm.data.ukraine <- data.frame(word = names(dtm_ukraine.m_v), freq=dtm_ukraine.m_v) # Setting up data frame that classifies number of times words are used.
dtm.data.watergate <- data.frame(word = names(dtm_watergate.m_v), freq=dtm_watergate.m_v)

head(dtm.data.ukraine) # All looks good, however the Watergate sample is much smaller than the Ukraine sample.
head(dtm.data.watergate)

#install.packages("wordcloud")
library("wordcloud")

# I create wordclouds of frequently used terms in both texts. I set minimum frequency to 10 in the Ukraine texts because it is a larger set.
# I set minimum frequency to 5 in Watergate texts. 

set.seed(1234)
ukraine.wc <- wordcloud(words = dtm.data.ukraine$word, freq = dtm.data.ukraine$freq, min.freq = 10,
                   max.words=100, random.order=FALSE, rot.per=0.40, 
                   colors=brewer.pal(8, "Dark2"))
set.seed(1234)
watergate.wc <- wordcloud(words = dtm.data.watergate$word, freq = dtm.data.watergate$freq, min.freq = 5,
                          max.words=100, random.order=FALSE, rot.per=0.40, 
                          colors=brewer.pal(8, "Dark2"))

