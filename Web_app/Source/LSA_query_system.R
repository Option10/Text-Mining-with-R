query_system <- function(irlba,posQueryString,negQueryString,Abstract,stemming){
  err <-""
  # Tokenization of the queries (return vector ('query','query',...))
  tokenization <- dget("Source/tokenization.R")
  flag <- FALSE
  posQuery_String <- colnames(tokenization(posQueryString,stemming,flag))
  posQuery_Check <- colnames(tokenization(posQueryString,FALSE,flag))
  
  if (length(posQuery_String) == 0) {
    Result <- NULL # if no positive query is given, no results are chosen.
    err <- c("Please enter a positive query")
  } else{
    for (i in (1:length(posQuery_String))){ # for every words in the query
      if (is.na(match(posQuery_String[i],rownames(irlba$v)))){
        posQuery_String[i] <- 1
        # cat("The word",posQuery_Check[i],"isn't in the abstracts \n")
        err <- c("We couldn't find the word \"",posQuery_Check[i],"\" in any abstract \n")
      }
    }
    posQuery_String <- posQuery_String [! posQuery_String %in% 1]
    
    
    negQuery_String <- colnames(tokenization(negQueryString,stemming,flag))
    negQuery_Check <- colnames(tokenization(negQueryString,FALSE,flag))
    for (i in (1:length(negQuery_String))){
      if (length(negQuery_String) > 0){
        if (is.na(match(negQuery_String[i],rownames(irlba$v)))){
          negQuery_String[i] <- 1
          # cat("The word",negQuery_Check[i],"isn't in the abstracts \n")
          err <-c("We couldn't find the word \"",negQuery_Check[i],"\" in any abstracts \n")
        }
      }
    }
    negQuery_String <- negQuery_String [! negQuery_String %in% 1]
    
    if (length(posQuery_String) > 0) {
      
      # Build the query vector
      # return words vector with 0, 1 (pos query), -1 (neg query) 
      indQuery <- 0                                               
      indPosQuery <- rep(0,length(rownames(irlba$v)))          
      for (i in 1:length(posQuery_String)){
        indQuery <- rownames(irlba$v) == posQuery_String[i]
        indPosQuery <- indPosQuery | indQuery
      }
      
      indNegQuery <- rep(0,length(rownames(irlba$v)))
      if (length(negQuery_String) != 0)
        for (i in 1:length(negQuery_String)){
          indQuery <- rownames(irlba$v) == negQuery_String[i]
          indNegQuery <- indNegQuery | indQuery
        }
      indNegQuery = indNegQuery * -1L
      
      Query <- as.matrix(indPosQuery + indNegQuery)
      
      sigma <- diag(1/irlba$d)
      
      eigQuery <- sigma %*% (aperm(irlba$v) %*% Query)  #compute the query vector
      
      # This function computes the euclidean distance between the queries and each document
      euc.dist <- function(docs,query){ 
        dimDocs <- dim(docs)
        squareSum <- 0
        euc.dist <- vector(length=dimDocs[1])
        for (i in (1:dimDocs[1])) {
         
          squareSum <- squareSum + sum((docs[i,] - query)^2)
            
          euc.dist[i] <- squareSum 
          squareSum <- 0
        }
        return(euc.dist)
      }
      
      # Calculate distance, order and name the rows
      distMatrix <- euc.dist(irlba$u, eigQuery)
      
      names(distMatrix) <- rownames(irlba$u)
      distMatrix <- distMatrix[order(distMatrix),drop=FALSE]
      
      Result <- names(distMatrix)
      Result <- as.numeric(gsub(pattern = 'text',replacement = '',x = Result))
      
      right_text <- NA
      wrong_text <- NA
      buffer <- NA
      tot_text <- NA
      Result <- Result[1:2500]
      
      for (k in Result){
        right_text[match(k,Result)] <- grepl(posQuery_String[1], Abstract[k])
        if (is.null(negQuery_String)){
          wrong_text[match(k,Result)]<- FALSE
        }else{
          wrong_text[match(k,Result)] <- grepl(negQuery_String[1], Abstract[k])
        }
      }

      Result <- setdiff(Result[right_text],Result[wrong_text])
      if (length(Result) == 0) {
        Result <- NULL
        err <- "We could'nt find your query"
      }
    }else{Result <- NULL}
    
  }
  
  res <- list(res = Result, err=err)
  return(res)
# test <- rbind(eig_Query,irlba$u[20129])
# rownames(test) <- c("queries","abstract")
# print(test)
}
