tokenization <- function(df,stemming,flag){
  library(quanteda)
  library(tm)
  
  if (flag) Abstract <- as.character(df$Abstract)
  else Abstract <- df
  
  # NbrDoc <- 10000
  # Abstract <- Abstract[1:NbrDoc]
  
  if (stemming) Abstract <- stemDocument(Abstract,language = "english")

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
  tokensDF <- dfm(tokens)
  
  return(tokensDF)
}