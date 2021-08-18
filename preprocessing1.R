########## run 2 times
library(tm)
library(SnowballC)
library(textstem)
################################################ 
#set working directory (modify path as needed)
Path <- setwd("D:/Program Files/RStudio/projects/LDA-implementation/lda-Base/documents")
#get listing of folders
Foldernames <- list.files(getwd())
until <- 0
iter <- 0
for(f in Foldernames){
  Path <- setwd(paste0("D:/Program Files/RStudio/projects/LDA-implementation/lda-Base/documents/",f))
  Path <- setwd(paste0("D:/Program Files/RStudio/projects/LDA-implementation/lda-Base/documents/",f))
  #load files into corpus
  #get listing of .txt files in a Folder
  Filenames <- list.files(getwd(),pattern="*.txt")
  for (d in Filenames) {
    iter <- iter + 1
    until <- until + 1
    ################################################
    #read files into a character vector
    text <- lapply(d,readLines)
    # Text processing
    text <-gsub('\\b+RT', '', text) ## Remove RT
    text <-gsub('#\\w+ *', '', text) ## Remove Hashtags
    text <-gsub("@\\w+ *", "", text) ## Remove Mentions
    text <-gsub('[[:punct:]]', '', text) ## Remove Punctuations
    text <-gsub("http[[:alnum:]]*",'', text) ## Remove URLs
    text <-gsub('[[:cntrl:]]', '', text) ## Remove Controls and special characters
    text <-gsub('\\b\\w{1,2}\\b','',text) ## Remove words that have length 1 or 2
    text <-gsub("^[[:space:]]*","",text) ## Remove leading whitespaces
    text <-gsub("[[:space:]]*$","",text) ## Remove trailing whitespaces
    text <-gsub(' +',' ',text) ## Remove extra whitespaces
    #create corpus from vector
    articles.corpus <- Corpus(VectorSource(text))
    # make each letter lowercase
    articles.corpus <- tm_map(articles.corpus, content_transformer(tolower))
    #remove numbers
    articles.corpus <- tm_map(articles.corpus, removeNumbers)
    # remove generic and custom stopwords
    stopword <- c(stopwords('english'))
    articles.corpus <- tm_map(articles.corpus, removeWords, stopword)
    # remove punctuations
    articles.corpus <- tm_map(articles.corpus, removePunctuation)
    # Eliminate extra white spaces
    articles.corpus <- tm_map(articles.corpus, stripWhitespace) 
    # Text Stemming
    #articles.corpus <- tm_map(articles.corpus, stemDocument)
    # Text Lemmatization
    articles.corpus <- tm_map(articles.corpus, lemmatize_strings)
    # write preprossed corpus
    writeCorpus(articles.corpus, path = Path, filenames = d)
    #################################################
    if(until == 1){
      cat("\r \n ", iter)
      until <- 0
    }
  }
}

