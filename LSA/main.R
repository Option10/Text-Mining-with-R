## Extraction or load
extract_data <- FALSE # TRUE if new load needed
queryPUBMED <- '' # keep empty if you want full database
abstractSize <- c(100,3000) # min and max caracter in abstracts analysed

new_Tokens <- FALSE # if you want to recompute tokenization
stemming <- FALSE # to stem tokens

new_LSA <- FALSE # TRUE if you want to recalculate LSA
nv <- 100 # number of dimensions for LSA
flag <- TRUE # working version   ---------------------> TODO: find the bug in LSA.R

show_topics <- FALSE # to show the best words of the topics
query <- TRUE # to activate queries
interactiveQueries <- TRUE # to activate interactive queries

## ---- QUERIES --------- ##
# give a positive & negative query as a vector of strings ('querry','querry',...)
posQuerry_String <- ('breast cancer')
negQuerry_String <- ('') # '' for no negative query 

if (interactive() & interactiveQueries){
  posQuerry_String <- readline("Give a positive query:") 
  negQuerry_String <- readline("Give a negative query:")
}

############# Data extraction ####################
#------------------------------------------------#

if (extract_data == FALSE & file.exists("Dataframe")){
  df <- readRDS("Dataframe")
}else{
  print("new extraction")
  Extract_Data <- dget("Extract_Data.R")
  df <- Extract_Data(queryPUBMED,abstractSize)
}

############## Tokenization ######################
#------------------------------------------------#
if (new_Tokens | file.exists("tokensDF") == FALSE){
  flag0 <- TRUE
  tokenization <- dget("tokenization.R")
  tokensDF <- tokenization(df,stemming,flag0)
  
  saveRDS(tokensDF, file = "tokensDF", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
} else tokensDF <- readRDS("tokensDF")

################### LSA ##########################
#------------------------------------------------#

if (new_Tokens | new_LSA | file.exists("irlba") == FALSE){

  ## Data processing (preprocessing & SVD)
  if (flag) {
    library(irlba)
    
    ## analyzing Tokens:
    #-------------------
    
    # Our function for calculating relative term frequency (TF)
    term.frequency <- function(row) {
      row / rowSums(row)
    }
    
    # Our function for calculating inverse document frequency (IDF)
    inverse.doc.freq <- function(col) {
      corpus.size <- length(col[,1])
      doc.count <- colSums(col > 0)
      log10(corpus.size / doc.count)
    }
    
    # Our function for calculating TF-IDF.
    tf.idf <- function(tf, idf) {
      tf * idf
    }
    print("tf-idf")
    # First step, normalize all documents via TF.
    tokens.tf <- term.frequency(tokensDF)
    
    # Second step, calculate the IDF vector that we will use - both
    tokens.idf <- inverse.doc.freq(tokensDF)
    
    # Lastly, calculate TF-IDF for our training corpus.
    tokens.tfidf <- tf.idf(tokens.tf,tokensDF)
    
    ## Perform SVD. Specifically, reduce dimensionality down to 'nv' columns
    #-----------------------------------------------------------------------
    
    # for our latent semantic analysis (LSA).
    print("SVD")
    irlba <- irlba(tokens.tfidf, nv = nv, maxit = 1000)
    
    # line names
    rownames(irlba$v) <- colnames(tokensDF)
    rownames(irlba$u) <- row.names(tokensDF)
    
    saveRDS(irlba, file = "irlba", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  } else{
    LSA <- dget("LSA.R")
    irlba <- LSA(df,nv)
  }
}else {
  irlba <- readRDS("irlba")
}

########### Topics visualization #################
#------------------------------------------------#

if (show_topics){
  visu_topic <- dget("visu_topic.R")
  visu_topic(irlba)
}

############### Query system #####################
#------------------------------------------------#

if (query){
  Abstract <- as.character(df$Abstract)
  query_system <- dget("query_system.R")
  query_system(irlba,posQuerry_String,negQuerry_String,Abstract,stemming) # TODO better solution than Abstract
}
