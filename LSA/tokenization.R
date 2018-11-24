tokenization <- function(df,stemming,flag){
  loadPackage("quanteda","tm","foreach","doParallel")

  if (flag) Abstract <- as.character(df$Abstract)
  else Abstract <- df
  
  # NbrDoc <- 1000
  # Abstract <- Abstract[1:NbrDoc]
  
  if (stemming) {
    cores=detectCores()
    cat("Parallel stemming \n",cores,"cores detected \n")
    cl <- makeCluster(cores[1]-1) #not to overload your computer
    registerDoParallel(cl)
    
    Abstract <- foreach(n = 1:length(Abstract),.packages =c("tm")) %dopar% {
      stemDocument(Abstract[n],language = "english")
    }
    stopCluster(cl)
    Abstract <- as.character(Abstract)
  }

  # Tokenize

  tokens <- tokens(Abstract, what = "word", 
                   remove_numbers = TRUE, remove_punct = TRUE,
                   remove_symbols = TRUE, remove_hyphens = FALSE)
  
  # for bigrams.
  # test.tokens <- tokens_ngrams(test.tokens, n = 1:2)
  
  # minimize capital letters
  tokens <- tokens_tolower(tokens)
  
  # Stopwords
  stop<-stopwords()
  new_stopwords<-append(stop,c("fig.","eq.","e.g"))
  tokens <- tokens_select(tokens, new_stopwords, selection = "remove")
  tokens <- tokens_select(tokens,min_nchar = 3, selection ="keep")
  
  # Create our first bag-of-words model dataframe.
  tokensDF <- dfm(tokens) #,stem = stemming)
  
  return(tokensDF)
}