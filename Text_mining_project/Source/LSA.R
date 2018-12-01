LSA <- function(df,nv){
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
  tokens.tf <- term.frequency(tokens.matrix)
  
  # Second step, calculate the IDF vector that we will use - both
  tokens.idf <- inverse.doc.freq(tokens.matrix)
  
  # Lastly, calculate TF-IDF for our training corpus.
  tokens.tfidf <- tf.idf(tokens.tf,tokens.idf)
  
  ## Perform SVD. Specifically, reduce dimensionality down to 'nv' columns
  #-----------------------------------------------------------------------

  # for our latent semantic analysis (LSA).
  print("SVD")
  irlba <- irlba(tokens.tfidf, nv = nv, maxit = 1000)

  # line names
  rownames(irlba$v) <- colnames(tokens.matrix)
  rownames(irlba$u) <- row.names(tokens.matrix)
  
  saveRDS(irlba, file = "irlba", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
  
  return(irlba)
}