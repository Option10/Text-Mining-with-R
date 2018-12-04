setwd("~/Text-Mining-with-R/Text_mining_project")

## Extraction or load
loadPackage <- dget("Source/loadPackage.R")
loadPackage("quanteda","tm","foreach","doParallel","XML","easyPubMed","dplyr")

extract_data <- FALSE       # TRUE if new load needed
queryPUBMED <- ''           # keep empty if you want full database
abstractSize <- c(100,3000) # min and max caracter in abstracts analysed

new_Tokens <- FALSE         # if you want to recompute tokenization
stemming <- FALSE           # to stem tokens

new_LSA <- FALSE             # TRUE if you want to recalculate LSA
nv <- 100                   # number of dimensions for LSA
flag <- TRUE                # working version   ---------------------> TODO: find the bug in LSA.R

new_LDA <- FALSE             # TRUE if you want to recalculate LDA
k <- 20L                    # hyper parameter for LDA

LSAquery <- TRUE
LDAquery <- FALSE            # to activate queries
interactiveQueries <- TRUE # to activate interactive queries

## ---- QUERIES --------- ##
# give a positive & negative query as a vector of strings ('query','query',...)
posQuery_String <- ('cancer')
negQuery_String <- ('') # '' for no negative query 

if (interactive() & interactiveQueries){
  if (LDAquery) cat("LDA only allows one positive keyword and one negative keyword \n")
  posQuery_String <- readline("Give a positive query:") 
  negQuery_String <- readline("Give a negative query:")
}
########### Package loading function ##############
#-------------------------------------------------#

loadPackage <- dget("Source/loadPackage.R") # use loadPackage instead of library

############# Data extraction ####################
#------------------------------------------------#

if (extract_data == FALSE & file.exists("Data/Dataframe")){
  df <- readRDS("Data/Dataframe")
}else{
  cat("new extraction \n")
  Extract_Data <- dget("Source/Extract_Data.R")
  df <- Extract_Data(queryPUBMED,abstractSize)
  # export dataframe
  saveRDS(df, file = "Data/Dataframe", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
}

############## Tokenization ######################
#------------------------------------------------#
if (new_Tokens | file.exists("Data/tokensDF") == FALSE){
  flag0 <- TRUE
  tokenization <- dget("Source/tokenization.R")
  tokensDF <- tokenization(df,stemming,flag0)
  
  saveRDS(tokensDF, file = "Data/tokensDF", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
} else tokensDF <- readRDS("Data/tokensDF")

################### LSA ##########################
#------------------------------------------------#

if (new_Tokens | new_LSA | file.exists("Data/irlba") == FALSE){

  ## Data processing (preprocessing & SVD)
  if (flag) {
    loadPackage("irlba")
    
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
    cat("Computing: tf-idf \n")
    # First step, normalize all documents via TF.
    tokens.tf <- term.frequency(tokensDF)
    
    # Second step, calculate the IDF vector that we will use - both
    tokens.idf <- inverse.doc.freq(tokensDF)
    
    # Lastly, calculate TF-IDF for our training corpus.
    tokens.tfidf <- tf.idf(tokens.tf,tokensDF)
    
    ## Perform SVD. Specifically, reduce dimensionality down to 'nv' columns
    #-----------------------------------------------------------------------
    
    # for our latent semantic analysis (LSA).
    cat("Computing: SVD \n")
    irlba <- irlba(tokens.tfidf, nv = nv, maxit = 1000)
    
    # line names
    rownames(irlba$v) <- colnames(tokensDF)
    rownames(irlba$u) <- row.names(tokensDF)
    
    saveRDS(irlba, file = "Data/irlba", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  } else{
    LSA <- dget("Source/LSA.R")
    irlba <- LSA(tokensDF,nv)
  }
}else {
  irlba <- readRDS("Data/irlba")
}

#################### LDA #########################
#------------------------------------------------#

if (new_LDA | file.exists("Data/LDAtop_terms") == FALSE){
  runLDA <- dget("Source/LDA.R")
  runLDA(k,tokensDF)
}else {
  LDAtop_terms <- readRDS("Data/LDAtop_terms")
  LDAdoc <- readRDS("Data/LDAdoc")
}

############### Query system #####################
#------------------------------------------------#
Abstract <- as.character(df$Abstract)

if (LSAquery){
  query_system <- dget("Source/LSA_query_system.R")
  
  Result <- query_system(irlba,posQuery_String,negQuery_String,Abstract,stemming) 
  
  cat("Positive queries:",posQuery_String,"\n","Negative queries:",negQuery_String,"\n")
  for (i in (1:10)) {
    cat("Result",i,"\n","Abstract",Result[i],"\n",Abstract[Result[i]],"\n")
  }
}
if (LDAquery){
  query_system <- dget("Source/LDA_query_system.R")
  query_system(posQuery_String,negQuery_String,LDAtop_terms,LDAdoc,Abstract) 
}
