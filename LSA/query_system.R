query_system <- function(irlba,posQueryString,negQueryString,Abstract,stemming){
  
  # Tokenization of the queries (return vector ('query','query',...))
  tokenization <- dget("tokenization.R")
  flag <- FALSE
  posQuery_String <- colnames(tokenization(posQueryString,stemming,flag))
  posQuery_Check <- colnames(tokenization(posQueryString,FALSE,flag))
  for (i in (1:length(posQuery_String))){
    if (is.na(match(posQuery_String[i],rownames(irlba$v)))){
      posQuery_String[i] <- 1
      cat("The word",posQuery_Check[i],"isn't in the abstracts \n")
    }
  }
  posQuery_String <- posQuery_String [! posQuery_String %in% 1]
  
  negQuery_String <- colnames(tokenization(negQueryString,stemming,flag))
  negQuery_Check <- colnames(tokenization(negQueryString,FALSE,flag))
  for (i in (1:length(negQuery_String))){
    if (is.na(match(negQuery_String[i],rownames(irlba$v)))){
      negQuery_String[i] <- 1
      cat("The word",negQuery_Check[i],"isn't in the abstracts \n")
    }
  }
  negQuery_String <- negQuery_String [! negQuery_String %in% 1]
  
  
  # negQuery_String <- stemDocument(negQuery_String) # IF STEMMING
  # flag <- match(negQuery_String, rownames(irlba$v))
  # try(if(negQuery_String != '' & sum(is.na(flag)) > 0) stop("Query not found"))
  
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
  
  # This function computes the euclidean distance between the queries and each document
  euc.dist <- function(docs,query){ 
    dimDocs <- dim(docs)
    squareSum <- 0
    euc.dist <- vector(length=dimDocs[1])
    for (i in (1:dimDocs[1])) {
      for (j in (1:dimDocs[2])){ # TODO: remove the loop and calculate with the whole vectors
        squareSum <- squareSum + (docs[i,j] - query[j])^2
      }  
      euc.dist[i] <- squareSum ^ 0.5
      squareSum <- 0
    }
    return(euc.dist)
  }
  
  # Calculate distance, order and name the rows
  posdistMatrix <- matrix(nrow = length(posQuery_String),ncol=dim(irlba$u)[1])
  posdist <- rep(1,length = dim(irlba$u)[1])
  if (length(posQuery_String) > 1){
    for (i in (1:length(posQuery_String))) {
      posdistMatrix[i,] <- euc.dist(irlba$u, eig_posQuery[i,])
      posdist <- posdist + posdistMatrix[i,]
    }
  }else{posdist <- euc.dist(irlba$u, eig_posQuery)}
  
  if (is.null(negQuery_String) == FALSE){
    negdistMatrix <- matrix(1L,nrow = length(negQuery_String),ncol=dim(irlba$u)[1])
    negdist <- rep(1,length = dim(irlba$u)[1])
    if (length(negQuery_String) > 1){
      for (i in (1:length(negQuery_String))) {
        negdistMatrix[i,] <- euc.dist(irlba$u, eig_negQuery[i,])
        negdist <- negdist + negdistMatrix[i,]
      }
    }else{negdist <- euc.dist(irlba$u, eig_negQuery)}
    distMatrix <- 0.8*posdist - 0.2*negdist
  }else distMatrix <- posdist
  
  names(distMatrix) <- rownames(irlba$u)
  distMatrix <- distMatrix[order(distMatrix),drop=FALSE]
  
  Result <- names(distMatrix)
  Result <- gsub(pattern = 'text',replacement = '',x = Result)
  
  cat("Positive queries:",posQuery_String,"\n","Negative Queries:",negQuery_String,"\n")
  for (i in (1:10)) {
    num <- as.numeric(Result[i])
    cat("Result",i,"\n","Abstract",num,"\n",Abstract[num],"\n")
  }
}
