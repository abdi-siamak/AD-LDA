setwd("D:/Program Files/RStudio/projects/LDA-implementation/AD-LDA/documents")
#get listing of folders
Foldernames <- list.files(getwd())
docs_len <- c()
ww <- c()
dd <- c()
cc <- c()
n_c<- c(0)
i <- 1
j <- 1
for(f in Foldernames){
  Path <- setwd(paste0("D:/Program Files/RStudio/projects/LDA-implementation/AD-LDA/documents/",f))
  Path <- setwd(paste0("D:/Program Files/RStudio/projects/LDA-implementation/AD-LDA/documents/",f))
  #get listing of .txt files in Folder
  Filenames <- list.files(getwd(),pattern="*.txt")
  for (d in Filenames) {
    w <- scan(d,what=" ")
    docs_len[i] <- length(w)                               # length of each documnet
    ww <- append(ww,w,after = length(ww))                  # all of the words in all documents
    dd <- append(dd,rep(i,length(w)),after = length(dd))   # assign document name of each word
    i <- i+1
  }
  j <- j+1
}
#################################### creating gibbs matrix #####################################
W <- length(ww)                                 # number of words in all document
gibbs <- matrix(nrow = W, ncol = 5)
colnames(gibbs)<- c("words","document","Topic*","Topic Label","in DataSet")
gibbs[,1] <- ww                                         
gibbs[,2] <- dd
cat("W:\n ",length(ww))
cat("M:\n ",i-1)
M <- i-1                                        # number of all documents
################################################################################################
### output: (docs_len,M,W,gibbs)

