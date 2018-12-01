function(posQuery_String,negQuery_String,LDAtop_terms,LDAdoc,Abstract){
  
  beta1 <-0
  
  ap_top_terms <- LDAtop_terms
  ap_documents <- LDAdoc
  
  
  ind_pos <- which(ap_top_terms$term==posQuery_String)
  topic_int_pos <- ap_top_terms$topic[ind_pos]
  
  ind_neg <- which(ap_top_terms$term[1:200]==negQuery_String)
  topic_int_neg <- ap_top_terms$topic[ind_neg]
  
  topic_int_tot <- setdiff(topic_int_pos,topic_int_neg)
  
  if (length(topic_int_tot)<1) {
    topic_int_tot <- topic_int_pos
  }
  
  # error <- renderPrint({ 
  #   tryCatch(if(length(topic_int_pos)<1) cat(as.character(Strings$noPosQuery)))
  #   tryCatch(if(length(topic_int_neg)<1 & negQuery_String!="") cat(as.character(Strings$noNegQuery)))
  #   tryCatch(if(length(topic_int_tot)<1 & negQuery_String!="") cat(as.character(Strings$insignificantNegQuery)))
  #   
  # })
  
  ind3 <- which(ap_documents$topic %in% c(topic_int_tot))
  
  dfr <- ap_documents[c(ind3),]
  dfr$beta1=0
 
  for (j in 1:length(topic_int_tot)) {
    ind_topic <- dfr$topic == topic_int_tot[j]
    dfr$beta1[ind_topic] <- ap_top_terms$beta[ind_pos[j]]
  }
  
  dfr$gamma <- dfr$gamma/sum(dfr$gamma[1:length(dfr$gamma)])          
  dfr$pond1 <- 0
  dfr$pond1 <- dfr$gamma+dfr$beta1
  
  result <- dfr[order(-dfr$pond1),]
  
  Result <- as.numeric(gsub(pattern = 'text',replacement = '',x = result$document))
  
  # Abstract <- as.character(df$Abstract)
  # 
  # for (k in top_text_number){
  #   right_text[match(k,top_text_number)]<-grepl(posQuery_String, Abstract[k])
  #   wrong_text[match(k,top_text_number)]<-grepl(negQuery_String, Abstract[k])
  # }
  # 
  # tot_text <- setdiff(top_text_number[right_text],top_text_number[wrong_text])
  
  return(Result)
  
}