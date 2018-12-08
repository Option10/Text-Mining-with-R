function(posQueryString,negQueryString,LDAtop_terms,LDAdoc,Abstract,Strings,stemming){
  err <-""
  beta1 <-0
  
  ap_top_terms <- LDAtop_terms
  ap_documents <- LDAdoc
  
  # Tokenization of the queries (return vector ('query','query',...))
  tokenization <- dget("Source/tokenization.R")
  flag <- FALSE
  posQuery_String <- colnames(tokenization(posQueryString,stemming,flag))
  posQuery_Check <- colnames(tokenization(posQueryString,FALSE,flag))
  for (i in (1:length(posQuery_String))){ # for every words in the query
    if (!is.null(posQuery_String)){
      if (is.na(match(posQuery_String[i],ap_top_terms$term))){
        posQuery_String[i] <- 1
        # cat("The word",posQuery_Check[i],"isn't in the abstracts \n")
        err <- c("We couldn't find the word \"",posQuery_Check[i],"\" in any abstract \n")
      }
    }
  }
  posQuery_String <- posQuery_String [! posQuery_String %in% 1]
  
  negQuery_String <- colnames(tokenization(negQueryString,stemming,flag))
  negQuery_Check <- colnames(tokenization(negQueryString,FALSE,flag))
  for (i in (1:length(negQuery_String))){
    if (length(negQuery_String) > 0){
      if (is.na(match(negQuery_String[i],ap_top_terms$term))){
        negQuery_String[i] <- 1
        # cat("The word",negQuery_Check[i],"isn't in the abstracts \n")
        err <-c("We couldn't find the word \"",negQuery_Check[i],"\" in any abstracts \n")
      }
    }
  }
  negQuery_String <- negQuery_String [! negQuery_String %in% 1]
  
  if (length(posQuery_String) == 0) {
    tot_text <- NULL   # if no positive query is given, no results are chosen.
    err <- c("Please enter a positive query")
  } else{
    if (length(posQuery_String) > 1 | length(negQuery_String) > 1){
      err <- c("LDA only support one positive query and one negative query. The result is based on the first word")
      posQuery_String <- posQuery_String[1]
      negQuery_String <- negQuery_String[1]
    }
  
    # flag topics that contain the positive query in the 200 top terms
    topic_term <- 0
    ind_pos <- 0
    for (i in 1:20){
      topic_term <- ap_top_terms$term[which(ap_top_terms$topic == i)]
      if ( which(topic_term == posQuery_String) < 15000){
        ind_pos[i] <- 1
      }else{ind_pos[i] <- 0}
    }
  
    topic_ind_pos <- which(ind_pos == 1)
    
    # oposite with neg query
    topic_term <- 0
    ind_neg <- 0
    if (length(negQuery_String) > 0){
      for (i in 1:20){
        topic_term <- ap_top_terms$term[which(ap_top_terms$topic == i)]
        if ( which(topic_term == negQuery_String) < 200){
          ind_neg[i] <- 0
        }else{ind_neg[i] <- 1}
      }
      topic_ind_neg <- which(ind_neg == 1)
      topic_ind <- setdiff(topic_ind_pos,topic_ind_neg)
    }else{
      topic_ind <- topic_ind_pos
      topic_ind_neg <- c(NULL, NULL)
    }
  
    # errors
    if(length(topic_ind_pos)<1) err <- c(as.character(Strings$noPosQuery))
    if(length(topic_ind_neg)<1 & length(negQuery_String) > 0) err <- c(as.character(Strings$noNegQuery))
    if(length(topic_ind)<1 & length(negQuery_String) > 0) err <- c(as.character(Strings$insignificantNegQuery))
  
    ind3 <- which(ap_documents$topic %in% topic_ind) # concerned by selected topics abstracts
    
    dfr <- ap_documents[c(ind3),]
    dfr$beta1=0
   
    for (j in 1:length(topic_ind)) {
      ind_topic <- dfr$topic == topic_ind[j]
      if (ind_pos[j] != 0){
        dfr$beta1[ind_topic] <- ap_top_terms$beta[ind_pos[j]]
      }else{dfr$beta1[ind_topic] <- 0}
    }
    
    dfr$gamma <- dfr$gamma/sum(dfr$gamma[1:length(dfr$gamma)])          
    dfr$pond1 <- 0
    dfr$pond1 <- dfr$gamma+dfr$beta1
    
    result <- dfr[order(-dfr$pond1),]
    
    # Result <- as.numeric(gsub(pattern = 'text',replacement = '',x = result$document))
    
   
    top_text <- select(head(result,2500),"document")
    top_text
    top_text_number <- 0
    right_text <- NA
    wrong_text <- NA
    tot_text <- NA
    perfect_match <- 0
    
    for (j in 1:2500) {
      top_text_number[j] <- as.numeric(as.character(gsub("text",'',top_text[j,1])))
    }
    
    for (k in top_text_number){
      right_text[match(k,top_text_number)]<-grepl(posQuery_String, Abstract[k])
      if (is.null(negQuery_String)){
        wrong_text[match(k,top_text_number)]<- FALSE
      }else{
        wrong_text[match(k,top_text_number)]<-grepl(negQuery_String, Abstract[k])
      }
    }
  
    tot_text <- setdiff(top_text_number[right_text],top_text_number[wrong_text])
  }
  
  res <- list(res = tot_text, err=err)
  return(res)

}