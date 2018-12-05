function(posQuery_String,negQuery_String,LDAtop_terms,LDAdoc,Abstract){

  beta1 <-0
  
  ap_top_terms <- LDAtop_terms
  ap_documents <- LDAdoc
  
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
  
  # opisite with neg query
  topic_term <- 0
  ind_neg <- 0
  if (negQuery_String != ""){
    for (i in 1:20){
      topic_term <- ap_top_terms$term[which(ap_top_terms$topic == i)]
      if ( which(topic_term == negQuery_String) < 200){
        ind_neg[i] <- 0
      }else{ind_neg[i] <- 1}
    }
  topic_ind_neg <- which(ind_neg == 1)
  topic_ind <- setdiff(topic_ind_pos,topic_ind_neg)
  }else{topic_ind <- topic_ind_pos}

  # error <- renderPrint({ 
  #   tryCatch(if(length(topic_int_pos)<1) cat(as.character(Strings$noPosQuery)))
  #   tryCatch(if(length(topic_int_neg)<1 & negQuery_String!="") cat(as.character(Strings$noNegQuery)))
  #   tryCatch(if(length(topic_int_tot)<1 & negQuery_String!="") cat(as.character(Strings$insignificantNegQuery)))
  #   
  # })
  
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
  
  Abstract <- as.character(df$Abstract)

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
    if (negQuery_String == ""){
      wrong_text[match(k,top_text_number)]<- FALSE
    }else{
      wrong_text[match(k,top_text_number)]<-grepl(negQuery_String, Abstract[k])
    }
  }

  tot_text <- setdiff(top_text_number[right_text],top_text_number[wrong_text])

  return(tot_text)
  
}