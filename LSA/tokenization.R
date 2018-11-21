tokenization <- function(df,stemming){
  library(quanteda)
  library(tm)
  
  Abstract <- as.character(df$Abstract)
  
  # NbrDoc <- 10000
  # Abstract <- Abstract[1:NbrDoc]
  
  if (stemming){
    for (i in (1:length(Abstract))){
      Abstract <- stemDocument(Abstract)
    }
  }
  
  # Tokenize
  print("tokenization")
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
  tokensDF <- dfm(tokens)
  
  saveRDS(tokensDF, file = "tokensDF", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
  
  return(tokensDF)
}