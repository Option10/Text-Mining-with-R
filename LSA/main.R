## Extraction or load
extract_data <- FALSE # TRUE if new load needed
queryPUBMED <- '' # keep empty if you want full database
abstractSize <- c(100,3000) # min and max caracter in abstracts analysed

new_Tokens <- FALSE # if you want to recompute tokenization
stemming <- FALSE # to stem tokens

new_LSA <- TRUE # TRUE if you want to recalculate LSA
nv <- 100 # number of dimensions for LSA
flag <- TRUE # working version   ---------------------> TODO: find the bug in LSA.R

new_LDA <- TRUE # TRUE if you want to recalculate LDA
k <- 20L # hyper parameter for LDA

query <- FALSE # to activate queries
interactiveQueries <- FALSE # to activate interactive queries

## ---- QUERIES --------- ##
# give a positive & negative query as a vector of strings ('query','query',...)
posQuery_String <- ('cancer')
negQuery_String <- ('') # '' for no negative query 

if (interactive() & interactiveQueries){
  posQuery_String <- readline("Give a positive query:") 
  negQuery_String <- readline("Give a negative query:")
}
########### Package loading function##############
#------------------------------------------------#

loadPackage <- dget("loadPackage.R") # use loadPackage instead of library

############# Data extraction ####################
#------------------------------------------------#

if (extract_data == FALSE & file.exists("Dataframe")){
  df <- readRDS("Dataframe")
}else{
  cat("new extraction \n")
  Extract_Data <- dget("Extract_Data.R")
  df <- Extract_Data(queryPUBMED,abstractSize)
}

############## Tokenization ######################
#------------------------------------------------#
if (new_Tokens | new_LSA | file.exists("tokensDF") == FALSE){
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
    cat("tf-idf \n")
    # First step, normalize all documents via TF.
    tokens.tf <- term.frequency(tokensDF)
    
    # Second step, calculate the IDF vector that we will use - both
    tokens.idf <- inverse.doc.freq(tokensDF)
    
    # Lastly, calculate TF-IDF for our training corpus.
    tokens.tfidf <- tf.idf(tokens.tf,tokensDF)
    
    ## Perform SVD. Specifically, reduce dimensionality down to 'nv' columns
    #-----------------------------------------------------------------------
    
    # for our latent semantic analysis (LSA).
    cat("SVD \n")
    irlba <- irlba(tokens.tfidf, nv = nv, maxit = 1000)
    
    # line names
    rownames(irlba$v) <- colnames(tokensDF)
    rownames(irlba$u) <- row.names(tokensDF)
    
    saveRDS(irlba, file = "irlba", ascii = FALSE, version = NULL,
            compress = TRUE, refhook = NULL)
  } else{
    LSA <- dget("LSA.R")
    irlba <- LSA(tokensDF,nv)
  }
}else {
  irlba <- readRDS("irlba")
}

#################### LDA #########################
#------------------------------------------------#

if (new_LDA){
  runLDA <- dget("LDA.R")
  runLDA(k,tokensDF)
  if (1 == 2){
loadPackage("dplyr","quanteda","stringr","tidytext","topicmodels","tictoc")
  
  tic("LDA")
  #--------------------------------------------------------------------------
  cat("LDA \n")
  ap_lda <- LDA(tokensDF, k, control = list(seed = 1234))
  
  ap_topics <- tidy(ap_lda, matrix = "beta")
  
  cat("top terms")
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(dim(tokens.dfm)[2], beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
  ap_documents <- tidy(ap_lda, matrix = "gamma")
  
  saveRDS(ap_top_terms, file = "LDAtop_terms", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
  saveRDS(ap_documents, file = "LDAdoc", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
  
  toc()
  }
}

############### Query system #####################
#------------------------------------------------#

if (query){
  Abstract <- as.character(df$Abstract)
  query_system <- dget("query_system.R")
  query_system(irlba,posQuery_String,negQuery_String,Abstract,stemming) # TODO better solution than Abstract
}
