query_system <- function(irlba,posQueryString,negQueryString,Abstract,stemming){
  err <-""
  # Tokenization of the queries (return vector ('query','query',...))
  tokenization <- dget("Source/tokenization.R")
  flag <- FALSE
  posQuery_String <- colnames(tokenization(posQueryString,stemming,flag))
  posQuery_Check <- colnames(tokenization(posQueryString,FALSE,flag))
  
  if (posQueryString == "") {Result <- NULL} # if no positive query is given, no results are chosen.
  else{
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
    
    # Compute the queries' coordinates in SVD matrix
    posIndex <- vector(length = length(posQuery_String))
    for (i in (1:length(posQuery_String))) {
      posIndex[i] <- match(posQuery_String[i], rownames(irlba$v))
    }
    eig_posQuery <- irlba$v[posIndex,]
    
    if (is.null(negQuery_String) == FALSE){
      negIndex <- vector(length = length(negQuery_String))
      for (i in (1:length(negQuery_String))) {
        negIndex[i] <- match(negQuery_String[i], rownames(irlba$v))
      }
      eig_negQuery <- irlba$v[negIndex,]
    }
    
    if (!is.na(eig_posQuery[1])) {
      
      # combine the queries (coordinates mean)
      if (is.null(negQuery_String) == FALSE){
        eig_Query <- colMeans(rbind(eig_posQuery,-eig_negQuery)) # general case
      }else if (length(posQuery_String) == 1){ eig_Query <- eig_posQuery # if only one positive query
      }else {eig_Query <- colMeans(eig_posQuery)} # if no negative query
      
      # # Test with singular values
      # sigma <- diag(irlba$d)
      # eig_Query <- irlba$d %o% irlba$u  
      # test <- eig_Query * posIndex
      
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
      distMatrix <- euc.dist(irlba$u, eig_Query)
      
      names(distMatrix) <- rownames(irlba$u)
      distMatrix <- distMatrix[order(distMatrix),drop=FALSE]
      
      Result <- names(distMatrix)
      Result <- as.numeric(gsub(pattern = 'text',replacement = '',x = Result))
    }else{Result <- NULL}
    
  }
  
  res <- list(res = Result, err=err)
  return(res)
# test <- rbind(eig_Query,irlba$u[20129])
# rownames(test) <- c("queries","abstract")
# print(test)
}
